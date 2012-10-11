//===------- TypeConversion.cpp - Converting GCC types to LLVM types ------===//
//
// Copyright (C) 2005 to 2012  Chris Lattner, Duncan Sands et al.
//
// This file is part of DragonEgg.
//
// DragonEgg is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later version.
//
// DragonEgg is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// DragonEgg; see the file COPYING.  If not, write to the Free Software
// Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA.
//
//===----------------------------------------------------------------------===//
// This is the code that converts GCC tree types into LLVM types.
//===----------------------------------------------------------------------===//

// Plugin headers
#include "dragonegg/ABI.h"
#include "dragonegg/Cache.h"
#include "dragonegg/TypeConversion.h"
#include "dragonegg/ADT/IntervalList.h"
#include "dragonegg/ADT/Range.h"

// LLVM headers
#include "llvm/ADT/SCCIterator.h"

// System headers
#include <gmp.h>
#include <map>

// GCC headers
#include "auto-host.h"
#ifndef ENABLE_BUILD_WITH_CXX
#include <cstring> // Otherwise included by system.h with C linkage.
extern "C" {
#endif
#include "config.h"
// Stop GCC declaring 'getopt' as it can clash with the system's declaration.
#undef HAVE_DECL_GETOPT
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"

#include "flags.h"
#ifndef ENABLE_BUILD_WITH_CXX
} // extern "C"
#endif

// Trees header.
#include "dragonegg/Trees.h"

static LLVMContext &Context = getGlobalContext();

/// SCCInProgress - Set of mutually dependent types currently being converted.
static const std::vector<tree_node*> *SCCInProgress;

//===----------------------------------------------------------------------===//
//                       ... ContainedTypeIterator ...
//===----------------------------------------------------------------------===//

/// ContainedTypeIterator - A convenience class for viewing a type as a graph,
/// where the nodes of the graph are types and there is an edge from type A to
/// type B iff A "contains" B.  A record type contains the types of its fields,
/// an array type contains the element type, a pointer type contains the type
/// pointed to and so on.  Use the begin, end and increment methods to iterate
/// over all of the types contained in a given type.
namespace {

  class ContainedTypeIterator {
    /// type_ref - Either a TREE_LIST node, in which case TREE_VALUE gives the
    /// contained type, or some other kind of tree node and TREE_TYPE gives the
    /// contained type.  A null value indicates the end iterator.
    tree type_ref;

    /// ContainedTypeIterator - Convenience constructor for internal use.
    explicit ContainedTypeIterator(const tree& t) : type_ref(t) {}

  public:
    /// Dereference operator.
    tree operator*() {
      return isa<TREE_LIST>(type_ref) ?
        TREE_VALUE(type_ref) : TREE_TYPE(type_ref);
    };

    /// Comparison operators.
    bool operator==(const ContainedTypeIterator &other) const {
      return other.type_ref == this->type_ref;
    }
    bool operator!=(const ContainedTypeIterator &other) const {
      return !(*this == other);
    }

    /// Prefix increment operator.
    ContainedTypeIterator& operator++() {
      assert(type_ref && "Incrementing end iterator!");

      switch (TREE_CODE(type_ref)) {
      default:
        debug_tree(type_ref);
        llvm_unreachable("Unexpected tree kind!");
      case ARRAY_TYPE:
      case COMPLEX_TYPE:
      case POINTER_TYPE:
      case REFERENCE_TYPE:
      case VECTOR_TYPE:
        // Here type_ref is the type being iterated over.  These types all have
        // only one contained type, so incrementing returns the end iterator.
        type_ref = 0;
        break;

      case FIELD_DECL:
        // Here type_ref is a field of the record or union type being iterated
        // over.  Move on to the next field.
        do
          type_ref = TREE_CHAIN(type_ref);
        while (type_ref && !isa<FIELD_DECL>(type_ref));
        break;

      case FUNCTION_TYPE:
      case METHOD_TYPE:
        // Here type_ref is the type being iterated over and the iterator refers
        // to the function return type.  Move on to the first function argument
        // (a TREE_LIST node).
        type_ref = TYPE_ARG_TYPES(type_ref);
        break;

      case TREE_LIST:
        // Here type_ref belongs to the argument list of the function or method
        // being iterated over.  Move on to the next function argument.
        type_ref = TREE_CHAIN(type_ref);
        // If the function takes a fixed number of arguments then the argument
        // list is terminated by void_list_node.  This is not a real argument.
        if (type_ref == void_list_node)
          type_ref = 0;
        break;
      }

      return *this;
    }

    /// begin - Return an iterator referring to the first type contained in the
    /// given type.
    static ContainedTypeIterator begin(tree type) {
      switch (TREE_CODE(type)) {
      default:
        llvm_unreachable("Unknown type!");

      case BOOLEAN_TYPE:
      case ENUMERAL_TYPE:
      case FIXED_POINT_TYPE:
      case INTEGER_TYPE:
      case OFFSET_TYPE:
      case REAL_TYPE:
      case VOID_TYPE:
        // No contained types.
        return end();

      case ARRAY_TYPE:
      case COMPLEX_TYPE:
      case POINTER_TYPE:
      case REFERENCE_TYPE:
      case VECTOR_TYPE:
        // Use the type itself as the "pointer" to the contained type.
        return ContainedTypeIterator(type);

      case QUAL_UNION_TYPE:
      case RECORD_TYPE:
      case UNION_TYPE:
        // The contained types are the types of the record's fields.  Use the
        // first FIELD_DECL as the "pointer" to the first contained type.
        for (tree field = TYPE_FIELDS(type); field; field = TREE_CHAIN(field))
          if (isa<FIELD_DECL>(field))
            return ContainedTypeIterator(field);
        return end();

      case FUNCTION_TYPE:
      case METHOD_TYPE:
        // The contained types are the return type and the argument types (in
        // the case of METHOD_TYPE nothing special needs to be done for 'this'
        // since it occurs explicitly in the argument list).  Return the type
        // itself as the "pointer" to the return type; incrementing will move
        // the iterator on to the argument types.
        // Note that static chains for nested functions cannot be obtained from
        // the function type which is why there is no attempt to handle them.
        return ContainedTypeIterator(type);
      }
    }

    /// end - Return the end iterator for contained type iteration.
    static ContainedTypeIterator end() {
      return ContainedTypeIterator(0);
    }
  };

} // Unnamed namespace.


//===----------------------------------------------------------------------===//
//                                 Utilities
//===----------------------------------------------------------------------===//

/// ArrayLengthOf - Returns the length of the given gcc array type, or NO_LENGTH
/// if the array has variable or unknown length.
uint64_t ArrayLengthOf(tree type) {
  assert(isa<ARRAY_TYPE>(type) && "Only for array types!");
  // Workaround for missing sanity checks in older versions of GCC.
  if ((GCC_MINOR == 5 && GCC_MICRO < 3) || (GCC_MINOR == 6 && GCC_MICRO < 2))
    if (!TYPE_DOMAIN(type) || !TYPE_MAX_VALUE(TYPE_DOMAIN(type)))
      return NO_LENGTH;
  tree range = array_type_nelts(type); // The number of elements minus one.
  // Bail out if the array has variable or unknown length.
  if (!isInt64(range, false))
    return NO_LENGTH;
  int64_t Range = (int64_t)getInt64(range, false);
  return Range < 0 ? 0 : 1 + (uint64_t)Range;
}

/// set_decl_index - Associate a non-negative number with the given GCC
/// field declaration.
static int set_decl_index(tree t, int i) {
  assert(i >= 0 && "Negative indices not allowed!");
  setCachedInteger(t, i);
  return i;
}

/// get_decl_index - Get the non-negative number associated with the given GCC
/// field decl.  Returns a negative value if no such association has been made.
static int get_decl_index(tree t) {
  int Idx;
  if (getCachedInteger(t, Idx))
    return Idx;
  return -1;
}

/// GetFieldIndex - Return the index of the field in the given LLVM type that
/// corresponds to the GCC field declaration 'decl'.  This means that the LLVM
/// and GCC fields start in the same byte (if 'decl' is a bitfield, this means
/// that its first bit is within the byte the LLVM field starts at).  Returns
/// INT_MAX if there is no such LLVM field.
int GetFieldIndex(tree decl, Type *Ty) {
  assert(isa<FIELD_DECL>(decl) && "Expected a FIELD_DECL!");
  // FIXME: The following test sometimes fails when compiling Fortran90 because
  // DECL_CONTEXT does not point to the containing type, but some other type!
//  assert(Ty == ConvertType(DECL_CONTEXT(decl)) && "Field not for this type!");

  // If we previously cached the field index, return the cached value.
  unsigned Index = (unsigned)get_decl_index(decl);
  if (Index <= INT_MAX)
    return Index;

  // TODO: At this point we could process all fields of DECL_CONTEXT(decl), and
  // incrementally advance over the StructLayout.  This would make indexing be
  // O(N) rather than O(N log N) if all N fields are used.  It's not clear if it
  // would really be a win though.

  StructType *STy = dyn_cast<StructType>(Ty);
  // If this is not a struct type, then for sure there is no corresponding LLVM
  // field (we do not require GCC record types to be converted to LLVM structs).
  if (!STy)
    return set_decl_index(decl, INT_MAX);

  // If this is an empty struct then there is no corresponding LLVM field.
  if (STy->element_begin() == STy->element_end())
    return set_decl_index(decl, INT_MAX);

  // If the field declaration is at a variable or humongous offset then there
  // can be no corresponding LLVM field.
  if (!OffsetIsLLVMCompatible(decl))
    return set_decl_index(decl, INT_MAX);

  // Find the LLVM field that contains the first bit of the GCC field.
  uint64_t OffsetInBytes = getFieldOffsetInBits(decl) / 8; // Ignore bit in byte
  const StructLayout *SL = getDataLayout().getStructLayout(STy);
  Index = SL->getElementContainingOffset(OffsetInBytes);

  // The GCC field must start in the first byte of the LLVM field.
  if (OffsetInBytes != SL->getElementOffset(Index))
    return set_decl_index(decl, INT_MAX);

  // We are not able to cache values bigger than INT_MAX, so bail out if the
  // LLVM field index is that huge.
  if (Index >= INT_MAX)
    return set_decl_index(decl, INT_MAX);

  // Found an appropriate LLVM field - return it.
  return set_decl_index(decl, Index);
}

/// getPointerToType - Returns the LLVM register type to use for a pointer to
/// the given GCC type.
Type *getPointerToType(tree type) {
  if (isa<VOID_TYPE>(type))
    // void* -> byte*
    return GetUnitPointerType(Context);
  // FIXME: Handle address spaces.
  return ConvertType(type)->getPointerTo();
}

/// GetUnitType - Returns an integer one address unit wide if 'NumUnits' is 1;
/// otherwise returns an array of such integers with 'NumUnits' elements.  For
/// example, on a machine which has 16 bit bytes returns an i16 or an array of
/// i16.
Type *GetUnitType(LLVMContext &C, unsigned NumUnits) {
  Type *UnitTy = IntegerType::get(C, BITS_PER_UNIT);
  if (NumUnits == 1)
    return UnitTy;
  return ArrayType::get(UnitTy, NumUnits);
}

/// GetUnitPointerType - Returns an LLVM pointer type which points to memory one
/// address unit wide.  For example, on a machine which has 16 bit bytes returns
/// an i16*.
Type *GetUnitPointerType(LLVMContext &C, unsigned AddrSpace) {
  return GetUnitType(C)->getPointerTo(AddrSpace);
}

/// isSized - Return true if the GCC type has a size, perhaps variable.  Note
/// that this returns false for function types, for which the GCC type size
/// doesn't represent anything useful for us.
static bool isSized(tree type) {
  if (isa<FUNCTION_TYPE>(type) || isa<METHOD_TYPE>(type))
    return false;
  return TYPE_SIZE(type);
}

/// isSizeCompatible - Return true if the specified gcc type is guaranteed to be
/// turned by ConvertType into an LLVM type of the same size (i.e. TYPE_SIZE the
/// same as getTypeAllocSizeInBits).
bool isSizeCompatible(tree type) {
  if (!isSized(type))
    return false;
  return isInt64(TYPE_SIZE(type), true);
}


//===----------------------------------------------------------------------===//
//                   Matching LLVM types with GCC trees
//===----------------------------------------------------------------------===//

static Type *CheckTypeConversion(tree type, Type *Ty) {
#ifndef NDEBUG
  bool Mismatch = false;
  // If the GCC type has a size, check that the LLVM type does too.  Note that
  // the LLVM type may have a size when the GCC type does not.  For example a
  // C variable length array int[] may be converted into [0 x i32].
  if (isSized(type) && !Ty->isSized()) {
    Mismatch = true;
    errs() << "The GCC type has a size but the LLVM type does not!\n";
  }
  // Check that the LLVM and GCC types really do have the same size when we say
  // they do.
  if (isSizeCompatible(type) && Ty->isSized()) {
    uint64_t GCCSize = getInt64(TYPE_SIZE(type), true);
    uint64_t LLVMSize = getDataLayout().getTypeAllocSizeInBits(Ty);
    if (LLVMSize != GCCSize) {
      Mismatch = true;
      errs() << "GCC size: " << GCCSize << "; LLVM size: " << LLVMSize
        << "!\n";
    }
  }
  // Check that the LLVM type has the same alignment or less than the GCC type.
  if (Ty->isSized()) {
    unsigned GCCAlign = TYPE_ALIGN(type);
    unsigned LLVMAlign = getDataLayout().getABITypeAlignment(Ty) * 8;
    if (LLVMAlign > GCCAlign) {
      Mismatch = true;
      errs() << "GCC align: " << GCCAlign << "; LLVM align: " << LLVMAlign
        << "\n";
    }
  }
  if (Mismatch) {
    errs() << "GCC: ";
    debug_tree(type);
    errs() << "LLVM: ";
    Ty->print(errs());
    llvm_unreachable("\nLLVM type doesn't represent GCC type!");
  }
#endif

  (void)type;
  return Ty;
}

// RememberTypeConversion - Associate an LLVM type with a GCC type.
// These are lazily computed by ConvertType.
static Type *RememberTypeConversion(tree type, Type *Ty) {
  CheckTypeConversion(type, Ty);
  setCachedType(type, Ty);
  return Ty;
}


//===----------------------------------------------------------------------===//
//                       Type Conversion Utilities
//===----------------------------------------------------------------------===//

// isPassedByInvisibleReference - Return true if an argument of the specified
// type should be passed in by invisible reference.
//
bool isPassedByInvisibleReference(tree Type) {
  // Don't crash in this case.
  if (Type == error_mark_node)
    return false;

  // FIXME: Search for TREE_ADDRESSABLE in calls.c, and see if there are other
  // cases that make arguments automatically passed in by reference.
  return TREE_ADDRESSABLE(Type) || TYPE_SIZE(Type) == 0 ||
         !isa<INTEGER_CST>(TYPE_SIZE(Type));
}


//===----------------------------------------------------------------------===//
//                             ... getRegType ...
//===----------------------------------------------------------------------===//

/// getRegType - Returns the LLVM type to use for registers that hold a value
/// of the scalar GCC type 'type'.  All of the EmitReg* routines use this to
/// determine the LLVM type to return.
Type *getRegType(tree type) {
  // Check that the type mode doesn't depend on the type variant (various bits
  // of the plugin rely on this).
  assert(TYPE_MODE(type) == TYPE_MODE(TYPE_MAIN_VARIANT(type))
         && "Type mode differs between variants!");

  // LLVM doesn't care about variants such as const, volatile, or restrict.
  type = TYPE_MAIN_VARIANT(type);

  // NOTE: Any changes made here need to be reflected in LoadRegisterFromMemory,
  // StoreRegisterToMemory and ExtractRegisterFromConstant.
  assert(!isa<AGGREGATE_TYPE>(type) && "Registers must have a scalar type!");
  assert(!isa<VOID_TYPE>(type) && "Registers cannot have void type!");

  switch (TREE_CODE(type)) {

  default:
    debug_tree(type);
    llvm_unreachable("Unknown register type!");

  case BOOLEAN_TYPE:
  case ENUMERAL_TYPE:
  case INTEGER_TYPE:
    // For integral types, convert based on the type precision.  For example,
    // this turns bool into i1 while ConvertType probably turns it into i8 or
    // i32.
    return IntegerType::get(Context, TYPE_PRECISION(type));

  case COMPLEX_TYPE: {
    Type *EltTy = getRegType(TREE_TYPE(type));
    return StructType::get(EltTy, EltTy, NULL);
  }

  case OFFSET_TYPE:
    return getDataLayout().getIntPtrType(Context);

  case POINTER_TYPE:
  case REFERENCE_TYPE:
    // void* -> byte*
    return isa<VOID_TYPE>(TREE_TYPE(type)) ?  GetUnitPointerType(Context) :
      ConvertType(TREE_TYPE(type))->getPointerTo();

  case REAL_TYPE:
    if (TYPE_PRECISION(type) == 32)
      return Type::getFloatTy(Context);
    if (TYPE_PRECISION(type) == 64)
      return Type::getDoubleTy(Context);
    if (TYPE_PRECISION(type) == 80)
      return Type::getX86_FP80Ty(Context);
    if (TYPE_PRECISION(type) == 128)
#ifdef TARGET_POWERPC
      return Type::getPPC_FP128Ty(Context);
#else
      // IEEE quad precision.
      return Type::getFP128Ty(Context);
#endif
    debug_tree(type);
    llvm_unreachable("Unknown FP type!");

  case VECTOR_TYPE: {
    // LLVM does not support vectors of pointers, so turn any pointers into
    // integers.
    Type *EltTy = isa<ACCESS_TYPE>(TREE_TYPE(type)) ?
      getDataLayout().getIntPtrType(Context) : getRegType(TREE_TYPE(type));
    return VectorType::get(EltTy, TYPE_VECTOR_SUBPARTS(type));
  }

  }
}


//===----------------------------------------------------------------------===//
//                            ... ConvertType ...
//===----------------------------------------------------------------------===//

static Type *ConvertArrayTypeRecursive(tree type) {
  Type *ElementTy = ConvertType(TREE_TYPE(type));
  uint64_t NumElements = ArrayLengthOf(type);

  if (NumElements == NO_LENGTH) // Variable length array?
    NumElements = 0;

  // Create the array type.
  Type *Ty = ArrayType::get(ElementTy, NumElements);

  // If the array is underaligned, wrap it in a packed struct.
  if (TYPE_ALIGN(type) < TYPE_ALIGN(main_type(type)))
    Ty = StructType::get(Context, Ty, /*isPacked*/ true);

  // If the user increased the alignment of the array element type, then the
  // size of the array is rounded up by that alignment even though the size
  // of the array element type is not (!).  Correct for this if necessary by
  // adding padding.  May also need padding if the element type has variable
  // size and the array type has variable length, but by a miracle the product
  // gives a constant size.
  if (isInt64(TYPE_SIZE(type), true)) {
    uint64_t PadBits = getInt64(TYPE_SIZE(type), true) -
      getDataLayout().getTypeAllocSizeInBits(Ty);
    if (PadBits) {
      Type *Padding = ArrayType::get(Type::getInt8Ty(Context), PadBits / 8);
      Ty = StructType::get(Ty, Padding, NULL);
    }
  }

  return Ty;
}

namespace {
  class FunctionTypeConversion : public DefaultABIClient {
    Type *&RetTy;
    SmallVectorImpl<Type*> &ArgTypes;
    CallingConv::ID &CallingConv;
    unsigned Offset;
    bool isShadowRet;
    bool KNRPromotion;
  public:
    FunctionTypeConversion(Type *&retty, SmallVectorImpl<Type*> &AT,
                           CallingConv::ID &CC, bool KNR)
      : RetTy(retty), ArgTypes(AT), CallingConv(CC), Offset(0),
        KNRPromotion(KNR) {
      CallingConv = CallingConv::C;
      isShadowRet = false;
    }

    /// getCallingConv - This provides the desired CallingConv for the function.
    CallingConv::ID getCallingConv(void) { return CallingConv; }

    bool isShadowReturn() const { return isShadowRet; }

    /// HandleScalarResult - This callback is invoked if the function returns a
    /// simple scalar result value.
    void HandleScalarResult(Type *RTy) {
      this->RetTy = RTy;
    }

    /// HandleAggregateResultAsScalar - This callback is invoked if the function
    /// returns an aggregate value by bit converting it to the specified scalar
    /// type and returning that.
    void HandleAggregateResultAsScalar(Type *ScalarTy, unsigned Off=0) {
      RetTy = ScalarTy;
      this->Offset = Off;
    }

    /// HandleAggregateResultAsAggregate - This callback is invoked if the function
    /// returns an aggregate value using multiple return values.
    void HandleAggregateResultAsAggregate(Type *AggrTy) {
      RetTy = AggrTy;
    }

    /// HandleShadowResult - Handle an aggregate or scalar shadow argument.
    void HandleShadowResult(PointerType *PtrArgTy, bool RetPtr) {
      // This function either returns void or the shadow argument,
      // depending on the target.
      RetTy = RetPtr ? PtrArgTy : Type::getVoidTy(Context);

      // In any case, there is a dummy shadow argument though!
      ArgTypes.push_back(PtrArgTy);

      // Also, note the use of a shadow argument.
      isShadowRet = true;
    }

    /// HandleAggregateShadowResult - This callback is invoked if the function
    /// returns an aggregate value by using a "shadow" first parameter, which is
    /// a pointer to the aggregate, of type PtrArgTy.  If RetPtr is set to true,
    /// the pointer argument itself is returned from the function.
    void HandleAggregateShadowResult(PointerType *PtrArgTy,
                                       bool RetPtr) {
      HandleShadowResult(PtrArgTy, RetPtr);
    }

    /// HandleScalarShadowResult - This callback is invoked if the function
    /// returns a scalar value by using a "shadow" first parameter, which is a
    /// pointer to the scalar, of type PtrArgTy.  If RetPtr is set to true,
    /// the pointer argument itself is returned from the function.
    void HandleScalarShadowResult(PointerType *PtrArgTy, bool RetPtr) {
      HandleShadowResult(PtrArgTy, RetPtr);
    }

    void HandlePad(llvm::Type *LLVMTy) {
      HandleScalarArgument(LLVMTy, 0, 0);
    }

    void HandleScalarArgument(llvm::Type *LLVMTy, tree type,
                              unsigned /*RealSize*/ = 0) {
      if (KNRPromotion) {
        if (type == float_type_node)
          LLVMTy = ConvertType(double_type_node);
        else if (LLVMTy->isIntegerTy(16) || LLVMTy->isIntegerTy(8) ||
                 LLVMTy->isIntegerTy(1))
          LLVMTy = Type::getInt32Ty(Context);
      }
      ArgTypes.push_back(LLVMTy);
    }

    /// HandleByInvisibleReferenceArgument - This callback is invoked if a pointer
    /// (of type PtrTy) to the argument is passed rather than the argument itself.
    void HandleByInvisibleReferenceArgument(llvm::Type *PtrTy,
                                            tree /*type*/) {
      ArgTypes.push_back(PtrTy);
    }

    /// HandleByValArgument - This callback is invoked if the aggregate function
    /// argument is passed by value. It is lowered to a parameter passed by
    /// reference with an additional parameter attribute "ByVal".
    void HandleByValArgument(llvm::Type *LLVMTy, tree type) {
      HandleScalarArgument(LLVMTy->getPointerTo(), type);
    }

    /// HandleFCAArgument - This callback is invoked if the aggregate function
    /// argument is a first class aggregate passed by value.
    void HandleFCAArgument(llvm::Type *LLVMTy, tree /*type*/) {
      ArgTypes.push_back(LLVMTy);
    }
  };
}

static Attributes HandleArgumentExtension(tree ArgTy) {
  if (isa<BOOLEAN_TYPE>(ArgTy)) {
    if (TREE_INT_CST_LOW(TYPE_SIZE(ArgTy)) < INT_TYPE_SIZE)
      return Attributes::get(Attributes::Builder().addAttribute(Attributes::ZExt));
  } else if (isa<INTEGER_TYPE>(ArgTy) &&
             TREE_INT_CST_LOW(TYPE_SIZE(ArgTy)) < INT_TYPE_SIZE) {
    if (TYPE_UNSIGNED(ArgTy))
      return Attributes::get(Attributes::Builder().addAttribute(Attributes::ZExt));
    else
      return Attributes::get(Attributes::Builder().addAttribute(Attributes::SExt));
  }

  return Attributes();
}

/// ConvertParamListToLLVMSignature - This method is used to build the argument
/// type list for K&R prototyped functions.  In this case, we have to figure out
/// the type list (to build a FunctionType) from the actual DECL_ARGUMENTS list
/// for the function.  This method takes the DECL_ARGUMENTS list (Args), and
/// fills in Result with the argument types for the function.  It returns the
/// specified result type for the function.
FunctionType *ConvertArgListToFnType(tree type, ArrayRef<tree> Args,
                                     tree static_chain, bool KNRPromotion,
                                     CallingConv::ID &CallingConv,
                                     AttrListPtr &PAL) {
  tree ReturnType = TREE_TYPE(type);
  SmallVector<Type*, 8> ArgTys;
  Type *RetTy(Type::getVoidTy(Context));

  FunctionTypeConversion Client(RetTy, ArgTys, CallingConv, KNRPromotion);
  DefaultABI ABIConverter(Client);

#ifdef TARGET_ADJUST_LLVM_CC
  TARGET_ADJUST_LLVM_CC(CallingConv, type);
#endif

  // Builtins are always prototyped, so this isn't one.
  ABIConverter.HandleReturnType(ReturnType, current_function_decl, false);

  SmallVector<AttributeWithIndex, 8> Attrs;

  // Compute whether the result needs to be zext or sext'd.
  Attributes RAttributes = HandleArgumentExtension(ReturnType);

  // Allow the target to change the attributes.
#ifdef TARGET_ADJUST_LLVM_RETATTR
  TARGET_ADJUST_LLVM_RETATTR(RAttributes, type);
#endif

  if (RAttributes.hasAttributes())
    Attrs.push_back(AttributeWithIndex::get(0, RAttributes));

  // If this function returns via a shadow argument, the dest loc is passed
  // in as a pointer.  Mark that pointer as struct-ret and noalias.
  if (ABIConverter.isShadowReturn()) {
    Attributes::Builder B;
    B.addAttribute(Attributes::StructRet)
      .addAttribute(Attributes::NoAlias);
    Attrs.push_back(AttributeWithIndex::get(ArgTys.size(),
                                            Attributes::get(B)));
  }

  std::vector<Type*> ScalarArgs;
  if (static_chain) {
    // Pass the static chain as the first parameter.
    ABIConverter.HandleArgument(TREE_TYPE(static_chain), ScalarArgs);
    // Mark it as the chain argument.
    Attributes::Builder B;
    B.addAttribute(Attributes::Nest);
    Attrs.push_back(AttributeWithIndex::get(ArgTys.size(),
                                            Attributes::get(B)));
  }

  for (ArrayRef<tree>::iterator I = Args.begin(), E = Args.end(); I != E; ++I) {
    tree ArgTy = TREE_TYPE(*I);

    // Determine if there are any attributes for this param.
    Attributes PAttributes;

    ABIConverter.HandleArgument(ArgTy, ScalarArgs, &PAttributes);

    // Compute zext/sext attributes.
    PAttributes |= HandleArgumentExtension(ArgTy);

    // Compute noalias attributes.
    Attributes::Builder B;
    if (isa<ACCESS_TYPE>(ArgTy) && TYPE_RESTRICT(ArgTy))
      B.addAttribute(Attributes::NoAlias);

    PAttributes |= Attributes::get(B);
    if (PAttributes.hasAttributes())
      Attrs.push_back(AttributeWithIndex::get(ArgTys.size(), PAttributes));
  }

  PAL = AttrListPtr::get(Attrs);
  return FunctionType::get(RetTy, ArgTys, false);
}

FunctionType *ConvertFunctionType(tree type, tree decl, tree static_chain,
                                  CallingConv::ID &CallingConv,
                                  AttrListPtr &PAL) {
  Type *RetTy = Type::getVoidTy(Context);
  SmallVector<Type*, 8> ArgTypes;
  FunctionTypeConversion Client(RetTy, ArgTypes, CallingConv, false/*not K&R*/);
  DefaultABI ABIConverter(Client);

  // Allow the target to set the CC for things like fastcall etc.
#ifdef TARGET_ADJUST_LLVM_CC
  TARGET_ADJUST_LLVM_CC(CallingConv, type);
#endif

  ABIConverter.HandleReturnType(TREE_TYPE(type), current_function_decl,
                                decl ? DECL_BUILT_IN(decl) : false);

  // Compute attributes for return type (and function attributes).
  SmallVector<AttributeWithIndex, 8> Attrs;
  Attributes::Builder FnAttributes;

  int flags = flags_from_decl_or_type(decl ? decl : type);

  // Check for 'readnone' and 'readonly' function attributes.
  if (flags & ECF_CONST)
    FnAttributes.addAttribute(Attributes::ReadNone);
  else if (flags & ECF_PURE)
    FnAttributes.addAttribute(Attributes::ReadOnly);

  // TODO: Handle ECF_LOOPING_CONST_OR_PURE

  // Check for 'noreturn' function attribute.
  if (flags & ECF_NORETURN)
    FnAttributes.addAttribute(Attributes::NoReturn);

  // Check for 'nounwind' function attribute.
  if (flags & ECF_NOTHROW)
    FnAttributes.addAttribute(Attributes::NoUnwind);

  // Check for 'returnstwice' function attribute.
  if (flags & ECF_RETURNS_TWICE)
    FnAttributes.addAttribute(Attributes::ReturnsTwice);

  // Since they write the return value through a pointer,
  // 'sret' functions cannot be 'readnone' or 'readonly'.
  if (ABIConverter.isShadowReturn()) {
    FnAttributes.removeAttribute(Attributes::ReadNone)
      .removeAttribute(Attributes::ReadOnly);
  }

  // Demote 'readnone' nested functions to 'readonly' since
  // they may need to read through the static chain.
  if (static_chain && FnAttributes.hasAttribute(Attributes::ReadNone)) {
    FnAttributes.removeAttribute(Attributes::ReadNone);
    FnAttributes.addAttribute(Attributes::ReadOnly);
  }

  // Compute whether the result needs to be zext or sext'd.
  Attributes RAttributes;
  RAttributes |= HandleArgumentExtension(TREE_TYPE(type));

  // Allow the target to change the attributes.
#ifdef TARGET_ADJUST_LLVM_RETATTR
  TARGET_ADJUST_LLVM_RETATTR(RAttributes, type);
#endif

  // The value returned by a 'malloc' function does not alias anything.
  if (flags & ECF_MALLOC)
    RAttributes |= Attributes::get(Attributes::Builder(Attributes::NoAlias));

  if (RAttributes.hasAttributes())
    Attrs.push_back(AttributeWithIndex::get(0, RAttributes));

  // If this function returns via a shadow argument, the dest loc is passed
  // in as a pointer.  Mark that pointer as struct-ret and noalias.
  if (ABIConverter.isShadowReturn()) {
    Attributes::Builder B;
    B.addAttribute(Attributes::StructRet)
      .addAttribute(Attributes::NoAlias);
    Attrs.push_back(AttributeWithIndex::get(ArgTypes.size(),
                                            Attributes::get(B)));
  }

  std::vector<Type*> ScalarArgs;
  if (static_chain) {
    // Pass the static chain as the first parameter.
    ABIConverter.HandleArgument(TREE_TYPE(static_chain), ScalarArgs);
    // Mark it as the chain argument.
    Attributes::Builder B;
    B.addAttribute(Attributes::Nest);
    Attrs.push_back(AttributeWithIndex::get(ArgTypes.size(),
                                            Attributes::get(B)));
  }

#ifdef LLVM_TARGET_ENABLE_REGPARM
  // If the target has regparam parameters, allow it to inspect the function
  // type.
  int local_regparam = 0;
  int local_fp_regparam = 0;
  LLVM_TARGET_INIT_REGPARM(local_regparam, local_fp_regparam, type);
#endif // LLVM_TARGET_ENABLE_REGPARM

  // Keep track of whether we see a byval argument.
  bool HasByVal = false;

  // Check if we have a corresponding decl to inspect.
  tree DeclArgs = (decl) ? DECL_ARGUMENTS(decl) : NULL;
  // Loop over all of the arguments, adding them as we go.
  tree Args = TYPE_ARG_TYPES(type);
  for (; Args && TREE_VALUE(Args) != void_type_node; Args = TREE_CHAIN(Args)){
    tree ArgTy = TREE_VALUE(Args);
    if (!isPassedByInvisibleReference(ArgTy))
      if (const StructType *STy = dyn_cast<StructType>(ConvertType(ArgTy)))
        if (STy->isOpaque()) {
          // If we are passing an opaque struct by value, we don't know how many
          // arguments it will turn into.  Because we can't handle this yet,
          // codegen the prototype as (...).
          if (CallingConv == CallingConv::C)
            ArgTypes.clear();
          else
            // Don't nuke last argument.
            ArgTypes.erase(ArgTypes.begin()+1, ArgTypes.end());
          Args = 0;
          break;
        }

    // Determine if there are any attributes for this param.
    Attributes PAttributes;

    unsigned OldSize = ArgTypes.size();

    ABIConverter.HandleArgument(ArgTy, ScalarArgs, &PAttributes);

    // Compute zext/sext attributes.
    PAttributes |= HandleArgumentExtension(ArgTy);

    // Compute noalias attributes. If we have a decl for the function
    // inspect it for restrict qualifiers, otherwise try the argument
    // types.
    tree RestrictArgTy = (DeclArgs) ? TREE_TYPE(DeclArgs) : ArgTy;
    if (isa<ACCESS_TYPE>(RestrictArgTy) && TYPE_RESTRICT(RestrictArgTy))
      PAttributes |= Attributes::get(Attributes::Builder(Attributes::NoAlias));

#ifdef LLVM_TARGET_ENABLE_REGPARM
    // Allow the target to mark this as inreg.
    if (isa<INTEGRAL_TYPE>(ArgTy) || isa<ACCESS_TYPE>(ArgTy) ||
        isa<REAL_TYPE>(ArgTy))
      LLVM_ADJUST_REGPARM_ATTRIBUTE(PAttributes, ArgTy,
                                    TREE_INT_CST_LOW(TYPE_SIZE(ArgTy)),
                                    local_regparam, local_fp_regparam);
#endif // LLVM_TARGET_ENABLE_REGPARM

    if (PAttributes.hasAttributes()) {
      HasByVal |= PAttributes.hasAttribute(Attributes::ByVal);

      // If the argument is split into multiple scalars, assign the
      // attributes to all scalars of the aggregate.
      for (unsigned i = OldSize + 1; i <= ArgTypes.size(); ++i) {
        Attrs.push_back(AttributeWithIndex::get(i, PAttributes));
      }
    }

    if (DeclArgs)
      DeclArgs = TREE_CHAIN(DeclArgs);
  }

  // If there is a byval argument then it is not safe to mark the function
  // 'readnone' or 'readonly': gcc permits a 'const' or 'pure' function to
  // write to struct arguments passed by value, but in LLVM this becomes a
  // write through the byval pointer argument, which LLVM does not allow for
  // readonly/readnone functions.
  if (HasByVal) {
    FnAttributes.removeAttribute(Attributes::ReadNone)
      .removeAttribute(Attributes::ReadOnly);

  assert(RetTy && "Return type not specified!");

  if (FnAttributes.hasAttribute())
    Attrs.push_back(AttributeWithIndex::get(~0, Attributes::get(FnAttributes)));

  // Finally, make the function type and result attributes.
  PAL = AttrListPtr::get(Attrs);
  return FunctionType::get(RetTy, ArgTypes, Args == 0);
}

static Type *ConvertPointerTypeRecursive(tree type) {
  // This is where self-recursion loops are broken, by not converting the type
  // pointed to if this would cause trouble (the pointer type is turned into
  // {}* instead).
  tree pointee = main_type(type);

  // The pointer type is in the strongly connected component (SCC) currently
  // being converted.  Check whether the pointee is as well.  If there is more
  // than one type in the SCC then necessarily the pointee type is in the SCC
  // since any path from the pointer type to the other type necessarily passes
  // via the pointee.  If the pointer type is the only element of the SCC then
  // the pointee is only in the SCC if it is equal to the pointer.
  bool bothInSCC = SCCInProgress->size() != 1 || pointee == type;

  Type *PointeeTy;
  if (!bothInSCC) {
    // It is safe to convert the pointee.  This is the common case, as we get
    // here for pointers to integers and so on.
    PointeeTy = ConvertType(pointee);
    if (PointeeTy->isVoidTy())
      PointeeTy = GetUnitType(Context); // void* -> byte*.
  } else {
    // Both the pointer and the pointee type are in the SCC so it is not safe
    // to convert the pointee type - otherwise we would get an infinite loop.
    // However if a type, for example an opaque struct placeholder, has been
    // registered for the pointee then we can return a pointer to it, giving
    // nicer IR (this is not needed for correctness).  Note that some members
    // of the SCC may have been converted already at this point (for this to
    // happen there must be more than one pointer type in the SCC), and thus
    // will have LLVM types registered for them.  Unfortunately which types
    // have been converted depends on the order in which we visit the SCC, and
    // that is not an intrinsic property of the SCC.  This is why we choose to
    // only use the types registered for records and unions - these are always
    // available.  As a further attempt to improve the IR, we return an S* for
    // an array type S[N] if (recursively) S is a record or union type.

    // Drill down through nested arrays to the ultimate element type.  Thanks
    // to this we may return S* for a (S[])*, which is better than {}*.
    while (isa<ARRAY_TYPE>(pointee))
      pointee = main_type(pointee);

    // If the pointee is a record or union type then return a pointer to its
    // placeholder type.  Otherwise return {}*.
    if (isa<RECORD_OR_UNION_TYPE>(pointee))
      PointeeTy = getCachedType(pointee);
    else
      PointeeTy = StructType::get(Context);
  }

  return PointeeTy->getPointerTo();
}

typedef Range<uint64_t> BitRange;

/// TypedRange - A type that applies to a range of bits.  Any part of the type
/// outside of the range is discarded.  The range may be bigger than the type
/// in which case any extra bits have an undefined type.
namespace {

class TypedRange {
  BitRange R;      // The range of bits occupied by the type.
  Type *Ty;        // The type.  May be null if the range is empty.
  uint64_t Starts; // The first bit of the type is positioned at this offset.

  TypedRange(BitRange r, Type *t, uint64_t starts) :
    R(r), Ty(t), Starts(starts) {
    assert((R.empty() || Ty) && "Need type when range not empty!");
  }

  /// isSafeToReturnContentsDirectly - Return whether the current value for the
  /// type properly represents the bits in the range and so can be handed to the
  /// user as is.
  bool isSafeToReturnContentsDirectly(const DataLayout &TD) const {
    // If there is no type (allowed when the range is empty) then one needs to
    // be created.
    if (!Ty)
      return false;
    // If the first bit of the type is not the first bit of the range then it
    // needs to be displaced before being passed to the user.
    if (!R.empty() && R.getFirst() != Starts)
      return false;
    // Check that the type isn't something like i17.  Avoiding types like this
    // is not needed for correctness, but makes life easier for the optimizers.
    if ((Ty->getPrimitiveSizeInBits() % BITS_PER_UNIT) != 0)
      return false;
    // If the type is wider than the range then it needs to be truncated before
    // being passed to the user.
    uint64_t AllocBits = TD.getTypeAllocSizeInBits(Ty);
    return AllocBits <= R.getWidth();
  }

public:
  /// get - Use the given type for the range [first, last).
  static TypedRange get(uint64_t first, uint64_t last, Type *Ty) {
    return TypedRange(BitRange(first, last), Ty, first);
  }

  // Copy assignment operator.
  TypedRange &operator=(const TypedRange &other) {
    R = other.R; Ty = other.Ty; Starts = other.Starts;
    return *this;
  }

  /// getRange - Return the range occupied by this field.
  BitRange getRange() const { return R; }

  /// ChangeRangeTo - Change the range occupied by this field.
  void ChangeRangeTo(BitRange r) { R = r; }

  /// JoinWith - Form the union of this field with another field (which must be
  /// disjoint from this one).  After this the range will be the convex hull of
  /// the ranges of the two fields.
  void JoinWith(const TypedRange &S);

  /// extractContents - Return the contained bits as a type which covers every
  /// defined bit in the range, yet is guaranteed to have alloc size no larger
  /// than the width of the range.  Unlike the other methods for this class this
  /// one requires that the width of the range be a multiple of an address unit,
  /// which usually means a multiple of 8.
  Type *extractContents(const DataLayout &TD) {
    assert(R.getWidth() % BITS_PER_UNIT == 0 && "Boundaries not aligned?");
    /// If the current value for the type can be used to represent the bits in
    /// the range then just return it.
    if (isSafeToReturnContentsDirectly(TD))
      return Ty;
    // If the range is empty then return a type with zero size.
    if (R.empty()) {
      // Return an empty array.  Remember the returned value as an optimization
      // in case we are called again.
      Ty = GetUnitType(Context, 0);
      assert(isSafeToReturnContentsDirectly(TD) && "Unit over aligned?");
      return Ty;
    }
    // If the type is something like i17 then round it up to a multiple of a
    // byte.  This is not needed for correctness, but helps the optimizers.
    if ((Ty->getPrimitiveSizeInBits() % BITS_PER_UNIT) != 0) {
      unsigned BitWidth = RoundUpToAlignment(Ty->getPrimitiveSizeInBits(),
                                             BITS_PER_UNIT);
      Ty = IntegerType::get(Context, BitWidth);
      if (isSafeToReturnContentsDirectly(TD))
        return Ty;
    }
    // Represent the range using an array of bytes.  Remember the returned type
    // as an optimization in case we are called again.
    // TODO: If the type only needs to be truncated and has struct or array type
    // then we could try to do the truncation by dropping or modifying the last
    // elements of the type, maybe yielding something less horrible.
    uint64_t Units = R.getWidth() / BITS_PER_UNIT;
    Ty = GetUnitType(Context, Units);
    Starts = R.getFirst();
    assert(isSafeToReturnContentsDirectly(TD) && "Unit over aligned?");
    return Ty;
  }
};

} // Unnamed namespace.

/// JoinWith - Form the union of this field with another field (which must be
/// disjoint from this one).  After this the range will be the convex hull of
/// the ranges of the two fields.
void TypedRange::JoinWith(const TypedRange &S) {
  if (S.R.empty())
    return;
  if (R.empty()) {
    *this = S;
    return;
  }
  // Use an integer type that covers both ranges.  Turning everything into an
  // integer like this is pretty nasty, but as we only get here for bitfields
  // it is fairly harmless.
  R = R.Join(S.R);
  Ty = IntegerType::get(Context, R.getWidth());
  Starts = R.getFirst();
}

static Type *ConvertRecordTypeRecursive(tree type) {
  // FIXME: This new logic, especially the handling of bitfields, is untested
  // and probably wrong on big-endian machines.
  assert(TYPE_SIZE(type) && "Incomplete types should be handled elsewhere!");

  IntervalList<TypedRange, uint64_t, 8> Layout;
  const DataLayout &TD = getDataLayout();

  // Get the size of the type in bits.  If the type has variable or ginormous
  // size then it is convenient to pretend it is "infinitely" big.
  uint64_t TypeSize = isInt64(TYPE_SIZE(type), true) ?
    getInt64(TYPE_SIZE(type), true) : ~0UL;

  // Record all interesting fields so they can easily be visited backwards.
  SmallVector<tree, 16> Fields;
  for (tree field = TYPE_FIELDS(type); field; field = TREE_CHAIN(field)) {
    if (!isa<FIELD_DECL>(field)) continue;
    // Ignore fields with variable or unknown position since they cannot be
    // represented by the LLVM type system.
    if (!OffsetIsLLVMCompatible(field))
      continue;
    Fields.push_back(field);
  }

  // Process the fields in reverse order.  This is for the benefit of union
  // types since it means that a zero constant of the LLVM type will fully
  // initialize the first union member, which is needed if the zero constant
  // is to be used as the default value for the union type.
  for (SmallVector<tree, 16>::reverse_iterator I = Fields.rbegin(),
       E = Fields.rend(); I != E; ++I) {
    tree field = *I;
    uint64_t FirstBit = getFieldOffsetInBits(field);
    assert(FirstBit <= TypeSize && "Field off end of type!");
    // Determine the width of the field.
    uint64_t BitWidth;
    Type *FieldTy = ConvertType(TREE_TYPE(field));
    if (isInt64(DECL_SIZE(field), true)) {
      // The field has a size and it is a constant, so use it.  Note that
      // this size may be smaller than the type size.  For example, if the
      // next field starts inside alignment padding at the end of this one
      // then DECL_SIZE will be the size with the padding used by the next
      // field not included.
      BitWidth = getInt64(DECL_SIZE(field), true);
    } else {
      // If the field has variable or unknown size then use the size of the
      // LLVM type instead as it gives the minimum size the field may have.
      assert(FieldTy->isSized() && "Type field has no size!");
      BitWidth = TD.getTypeAllocSizeInBits(FieldTy);
      if (FirstBit + BitWidth > TypeSize)
        BitWidth = TypeSize - FirstBit;
    }
    uint64_t LastBit = FirstBit + BitWidth;

    if (LastBit > TypeSize) {
      // Qualified union types may list fields that cannot be present, but that
      // the optimizers weren't smart enough to remove.  It can sometimes happen
      // that the optimizers nonetheless managed to simplify the type size to a
      // constant, which can be smaller than the size of the non-present fields
      // if were larger than the rest.
      assert(isa<QUAL_UNION_TYPE>(type) && "Field runs off end of type!");
      LastBit = TypeSize;
    }

    // Set the type of the range of bits occupied by the field to the LLVM type
    // for the field.
    Layout.AddInterval(TypedRange::get(FirstBit, LastBit, FieldTy));
  }

  // Force all fields to begin and end on a byte boundary.  This automagically
  // takes care of bitfields.
  Layout.AlignBoundaries(BITS_PER_UNIT);

  // Determine whether to return a packed struct type.  If returning an ordinary
  // struct would result in a type that is more aligned than the GCC type then
  // return a packed struct instead.  If a field's alignment would make it start
  // after its desired position then also use a packed struct type.
  bool Pack = false;
  unsigned MaxAlign = TYPE_ALIGN(type);
  for (unsigned i = 0, e = Layout.getNumIntervals(); i != e; ++i) {
    TypedRange F = Layout.getInterval(i);
    uint64_t First = F.getRange().getFirst();
    Type *Ty = F.extractContents(TD);
    unsigned Alignment = TD.getABITypeAlignment(Ty) * 8;
    if (Alignment > MaxAlign || First % Alignment) {
      Pack = true;
      break;
    }
  }

  // Create the elements that will make up the struct type.  As well as the
  // fields themselves there may also be padding elements.
  std::vector<Type*> Elts;
  Elts.reserve(Layout.getNumIntervals());
  uint64_t EndOfPrevious = 0; // Offset of first bit after previous element.
  for (unsigned i = 0, e = Layout.getNumIntervals(); i != e; ++i) {
    TypedRange F = Layout.getInterval(i);
    uint64_t First = F.getRange().getFirst();
    Type *Ty = F.extractContents(TD);
    assert(EndOfPrevious <= First && "Previous field too big!");

    // If there is a gap then we may need to fill it with padding.
    if (First > EndOfPrevious) {
      // There is a gap between the end of the previous field and the start of
      // this one.  The alignment of the field contents may mean that it will
      // start at the right offset anyway, but if not then insert padding.
      bool NeedPadding = true;
      if (!Pack) {
        // If the field's alignment will take care of the gap then there is no
        // need for padding.
        unsigned Alignment = TD.getABITypeAlignment(Ty) * 8;
        if (First == (EndOfPrevious + Alignment - 1) / Alignment * Alignment)
          NeedPadding = false;
      }
      if (NeedPadding) {
        // Fill the gap with an array of bytes.
        assert((First - EndOfPrevious) % BITS_PER_UNIT == 0 &&
               "Non-unit field boundaries!");
        uint64_t Units = (First - EndOfPrevious) / BITS_PER_UNIT;
        Elts.push_back(GetUnitType(Context, Units));
      }
    }

    // Append the field.
    Elts.push_back(Ty);
    EndOfPrevious = First + TD.getTypeAllocSizeInBits(Ty);
  }

  // If the GCC type has a sensible size then we guarantee that LLVM type has
  // the same size.  If needed, append padding to ensure this.
  if (TypeSize != ~0UL && EndOfPrevious < TypeSize) {
    assert((TypeSize - EndOfPrevious) % BITS_PER_UNIT == 0 &&
           "Non-unit type size?");
    uint64_t Units = (TypeSize - EndOfPrevious) / BITS_PER_UNIT;
    Elts.push_back(GetUnitType(Context, Units));
  }

  // OK, we're done.  Add the fields to the struct type and return it.
  Type *STy = getCachedType(type);
  assert(STy && isa<StructType>(STy) && cast<StructType>(STy)->isOpaque() &&
         "Incorrect placeholder for struct type!");
  cast<StructType>(STy)->setBody(Elts, Pack);
  return STy;
}

/// mayRecurse - Return true if converting this type may require breaking a
/// self-referential type loop.  For example, converting the struct type
///   struct S;
///   struct S {
///     struct S* s;
///   };
/// requires converting the "struct S*" field type; converting that pointer
/// type requires converting "struct S", leading to an infinite loop.  On the
/// other hand simple types like integers are never self-referential.  As this
/// routine is intended to be quick and simple, it returns true when in doubt.
/// Note that if a complicated type has already been converted then false is
/// usually returned, since type conversion doesn't have to do anything except
/// return the previously computed LLVM type.  The exception is record or union
/// types which were first converted when incomplete but that are now complete
/// so need to be converted again.
static bool mayRecurse(tree type) {
  assert(type == TYPE_MAIN_VARIANT(type) && "Not converting the main variant!");
  switch (TREE_CODE(type)) {
  default:
    llvm_unreachable("Unknown type!");

  case BOOLEAN_TYPE:
  case ENUMERAL_TYPE:
  case FIXED_POINT_TYPE:
  case INTEGER_TYPE:
  case OFFSET_TYPE:
  case REAL_TYPE:
  case VOID_TYPE:
    // Simple types that are never self-referential.
    return false;

  case COMPLEX_TYPE:
  case VECTOR_TYPE:
    // Converting these types does involve converting another type, however that
    // conversion cannot refer back to the initial type.
    // NOTE: GCC supports vectors of pointers, and the pointer could refer back
    // to the vector.  However as LLVM does not support vectors of pointers we
    // don't convert the pointer type and just use an integer instead, so as far
    // as we are concerned such vector types are not self-referential.
    return false;

  case ARRAY_TYPE:
  case FUNCTION_TYPE:
  case METHOD_TYPE:
  case POINTER_TYPE:
  case REFERENCE_TYPE:
    // Converting these types may recurse unless the type was already converted.
    return getCachedType(type) == 0;

  case QUAL_UNION_TYPE:
  case RECORD_TYPE:
  case UNION_TYPE: {
    // Converting these types may recurse unless already converted.  However if
    // the type was converted when still incomplete but is now complete then it
    // needs to be converted again, which might recurse.

    // If the type is incomplete then converting it will not recurse (conversion
    // just returns an opaque type).
    if (!TYPE_SIZE(type))
      return false;

    // If the type was not previously converted then converting it may recurse.
    Type *Ty = getCachedType(type);
    if (!Ty)
      return true;

    // If the type was previously converted when incomplete then converting it
    // may recurse as the type is now complete so needs to be converted again.
    if (cast<StructType>(Ty)->isOpaque())
      return true;

    // The type was already converted and does not need to be converted again.
    return false;
  }
  }
}

/// ConvertTypeRecursive - Convert a type when conversion may require breaking
/// type conversion loops, see mayRecurse.  Note that all types used by but not
/// in the current strongly connected component (SCC) must have been converted
/// already.
static Type *ConvertTypeRecursive(tree type) {
  assert(type == TYPE_MAIN_VARIANT(type) && "Not converting the main variant!");
  assert(mayRecurse(type) && "Expected a recursive type!");
  assert(SCCInProgress && "Missing recursion data!");

#ifndef NDEBUG
  // Check that the given type is in the current strongly connected component
  // (SCC) of the type graph.  This should always be the case because SCCs are
  // visited bottom up.
  bool inSCC = false;
  for (size_t i = 0, e = SCCInProgress->size(); i != e; ++i)
    if ((*SCCInProgress)[i] == type) {
      inSCC = true;
      break;
    }
  if (!inSCC) {
    debug_tree(type);
    llvm_unreachable("Type not in SCC!");
  }
#endif

  switch (TREE_CODE(type)) {
  default:
    debug_tree(type);
    llvm_unreachable("Unexpected type!");

  case ARRAY_TYPE:
    return RememberTypeConversion(type, ConvertArrayTypeRecursive(type));

  case FUNCTION_TYPE:
  case METHOD_TYPE: {
    CallingConv::ID CallingConv;
    AttrListPtr PAL;
    // No declaration to pass through, passing NULL.
    return RememberTypeConversion(type, ConvertFunctionType(type, NULL, NULL,
                                                            CallingConv, PAL));
  }

  case POINTER_TYPE:
  case REFERENCE_TYPE:
    return RememberTypeConversion(type, ConvertPointerTypeRecursive(type));

  case RECORD_TYPE:
  case UNION_TYPE:
  case QUAL_UNION_TYPE:
    return RememberTypeConversion(type, ConvertRecordTypeRecursive(type));
  }
}

/// ConvertTypeNonRecursive - Convert a type when this is known to not require
/// breaking type conversion loops, see mayRecurse.
static Type *ConvertTypeNonRecursive(tree type) {
  assert(type == TYPE_MAIN_VARIANT(type) && "Not converting the main variant!");
  assert(!mayRecurse(type) && "Expected a non-recursive type!");

  switch (TREE_CODE(type)) {
  default:
    debug_tree(type);
    llvm_unreachable("Unknown or recursive type!");

  case ARRAY_TYPE:
  case FUNCTION_TYPE:
  case METHOD_TYPE:
  case POINTER_TYPE:
  case REFERENCE_TYPE: {
    // If these types are not recursive it can only be because they were already
    // converted and we can safely return the result of the previous conversion.
    Type *Ty = getCachedType(type);
    assert(Ty && "Type not already converted!");
    return CheckTypeConversion(type, Ty);
  }

  case ENUMERAL_TYPE:
    // If the enum is incomplete return a placeholder type.
    if (!TYPE_SIZE(type))
      return CheckTypeConversion(type, GetUnitType(Context));
    // Otherwise fall through.
  case BOOLEAN_TYPE:
  case INTEGER_TYPE: {
    uint64_t Size = getInt64(TYPE_SIZE(type), true);
    // Caching the type conversion is not worth it.
    return CheckTypeConversion(type, IntegerType::get(Context, Size));
  }

  case COMPLEX_TYPE: {
    if (Type *Ty = getCachedType(type)) return Ty;
    Type *Ty = ConvertTypeNonRecursive(main_type(type));
    Ty = StructType::get(Ty, Ty, NULL);
    return RememberTypeConversion(type, Ty);
  }

  case OFFSET_TYPE:
    // Handle OFFSET_TYPE specially.  This is used for pointers to members,
    // which are really just integer offsets.  Return the appropriate integer
    // type directly.
    // Caching the type conversion is not worth it.
    return CheckTypeConversion(type, getDataLayout().getIntPtrType(Context));

  case REAL_TYPE:
    // Caching the type conversion is not worth it.
    switch (TYPE_PRECISION(type)) {
    default:
      debug_tree(type);
      llvm_unreachable("Unknown FP type!");
    case 32: return CheckTypeConversion(type, Type::getFloatTy(Context));
    case 64: return CheckTypeConversion(type, Type::getDoubleTy(Context));
    case 80: return CheckTypeConversion(type, Type::getX86_FP80Ty(Context));
    case 128:
#ifdef TARGET_POWERPC
      return CheckTypeConversion(type, Type::getPPC_FP128Ty(Context));
#else
      // IEEE quad precision.
      return CheckTypeConversion(type, Type::getFP128Ty(Context));
#endif
    }

  case RECORD_TYPE:
  case QUAL_UNION_TYPE:
  case UNION_TYPE:
    // If the type was already converted then return the already computed type.
    if (Type *Ty = getCachedType(type))
      return CheckTypeConversion(type, Ty);

    // Otherwise this must be an incomplete type - return an opaque struct.
    assert(!TYPE_SIZE(type) && "Expected an incomplete type!");
    return RememberTypeConversion(type,
                                  StructType::create(Context,
                                                     getDescriptiveName(type)));

  case VECTOR_TYPE: {
    if (Type *Ty = getCachedType(type)) return Ty;
    Type *Ty;
    // LLVM does not support vectors of pointers, so turn any pointers into
    // integers.
    if (isa<ACCESS_TYPE>(TREE_TYPE(type)))
      Ty = getDataLayout().getIntPtrType(Context);
    else
      Ty = ConvertTypeNonRecursive(main_type(type));
    Ty = VectorType::get(Ty, TYPE_VECTOR_SUBPARTS(type));
    return RememberTypeConversion(type, Ty);
  }

  case VOID_TYPE:
    // Caching the type conversion is not worth it.
    return CheckTypeConversion(type, Type::getVoidTy(Context));
  }
}

/// RecursiveTypeIterator - A convenience class that visits only those nodes
/// in the type graph that mayRecurse thinks might be self-referential.  Note
/// that dereferencing returns the main variant of the contained type rather
/// than the contained type itself.  See ContainedTypeIterator and mayRecurse
/// for more information about the type graph and self-referential types.
namespace {

  class RecursiveTypeIterator {
    // This class wraps an iterator that visits all contained types, and just
    // increments the iterator over any contained types that will not recurse.
    ContainedTypeIterator I;

    /// SkipNonRecursiveTypes - Increment the wrapped iterator over any types
    /// that mayRecurse says can be converted directly without having to worry
    /// about self-recursion.
    void SkipNonRecursiveTypes() {
      while (I != ContainedTypeIterator::end() &&
             !(isa<TYPE>(*I) && mayRecurse(TYPE_MAIN_VARIANT(*I))))
        ++I;
    }

    /// RecursiveTypeIterator - Convenience constructor for internal use.
    explicit RecursiveTypeIterator(const ContainedTypeIterator& i) : I(i) {}

  public:

    /// Dereference operator returning the main variant of the contained type.
    tree operator*() {
      return TYPE_MAIN_VARIANT(*I);
    };

    /// Comparison operators.
    bool operator==(const RecursiveTypeIterator &other) const {
      return other.I == this->I;
    }
    bool operator!=(const RecursiveTypeIterator &other) const {
      return !(*this == other);
    }

    /// Postfix increment operator.
    RecursiveTypeIterator operator++(int) {
      RecursiveTypeIterator Result(*this);
      ++(*this);
      return Result;
    }

    /// Prefix increment operator.
    RecursiveTypeIterator& operator++() {
      ++I;
      SkipNonRecursiveTypes();
      return *this;
    }

    /// begin - Return an iterator referring to the first type contained in the
    /// given type.
    static RecursiveTypeIterator begin(tree type) {
      RecursiveTypeIterator R(ContainedTypeIterator::begin(type));
      R.SkipNonRecursiveTypes();
      return R;
    }

    /// end - Return the end iterator for contained type iteration.
    static RecursiveTypeIterator end() {
      return RecursiveTypeIterator(ContainedTypeIterator::end());
    }
  };

} // Unnamed namespace.

// Traits for working with the graph of possibly self-referential type nodes,
// see RecursiveTypeIterator.
namespace llvm {
  template <> struct GraphTraits<tree> {
    typedef tree_node NodeType;
    typedef RecursiveTypeIterator ChildIteratorType;
    static inline NodeType *getEntryNode(tree t) {
      assert(TYPE_P(t) && "Expected a type!");
      return t;
    }
    static inline ChildIteratorType child_begin(tree type) {
      return ChildIteratorType::begin(type);
    }
    static inline ChildIteratorType child_end(tree) {
      return ChildIteratorType::end();
    }
  };
}

Type *ConvertType(tree type) {
  if (type == error_mark_node) return Type::getInt32Ty(Context);

  // Check that the type mode doesn't depend on the type variant (various bits
  // of the plugin rely on this).
  assert(TYPE_MODE(type) == TYPE_MODE(TYPE_MAIN_VARIANT(type))
         && "Type mode differs between variants!");

  // LLVM doesn't care about variants such as const, volatile, or restrict.
  type = TYPE_MAIN_VARIANT(type);

  // If this type can be converted without special action being needed to avoid
  // conversion loops coming from self-referential types, then convert it.
  if (!mayRecurse(type))
    return ConvertTypeNonRecursive(type);

  // If we already started a possibly looping type conversion, continue with it.
  if (SCCInProgress)
    return ConvertTypeRecursive(type);

  // Begin converting a type for which the conversion may require breaking type
  // conversion loops coming from self-referential types, see mayRecurse.  First
  // analyse all of the types that will need to be converted in order to convert
  // this one, finding sets of types that must be converted simultaneously (i.e.
  // for which converting any one of them requires converting all of the others;
  // these sets are the strongly connected components (SCCs) of the type graph),
  // then visit them bottom up, converting all types in them.  "Bottom up" means
  // that if a type in a SCC makes use of a type T that is not in the SCC then T
  // will be visited first.  Note that this analysis is performed only once: the
  // results of the type conversion are cached, and any future conversion of one
  // of the visited types will just return the cached value.
  for (scc_iterator<tree> I = scc_begin(type), E = scc_end(type); I != E; ++I) {
    const std::vector<tree> &SCC = *I;

    // First create a placeholder opaque struct for every record or union type
    // in the SCC.  This way, if we have both "struct S" and "struct S*" in the
    // SCC then we can return an LLVM "%struct.s*" for the pointer rather than
    // the nasty {}* type we are obliged to return in general.
    for (size_t i = 0, e = SCC.size(); i != e; ++i) {
      tree some_type = SCC[i];
      if (!isa<RECORD_OR_UNION_TYPE>(some_type)) {
        assert(!getCachedType(some_type) && "Type already converted!");
        continue;
      }
      // If the type used to be incomplete then a opaque struct placeholder may
      // have been created for it already.
      Type *Ty = getCachedType(some_type);
      if (Ty) {
        assert(isa<StructType>(Ty) && cast<StructType>(Ty)->isOpaque() &&
               "Recursive struct already fully converted!");
        continue;
      }
      // Otherwise register a placeholder for this type.
      Ty = StructType::create(Context, getDescriptiveName(some_type));
      // Associate the placeholder with the GCC type without sanity checking
      // since the type sizes won't match yet.
      setCachedType(some_type, Ty);
    }

    // Now convert every type in the SCC, filling in the placeholders created
    // above.  In the common case there is only one type in the SCC, meaning
    // that the type turned out not to be self-recursive and can be converted
    // without having to worry about type conversion loops.  If there is more
    // than one type in the SCC then self-recursion is overcome by returning
    // {}* for the pointer types if nothing better can be done.  As back edges
    // in the type graph can only be created by pointer types, "removing" such
    // edges like this destroys all cycles allowing the other types in the SCC
    // to be converted straightforwardly.
    SCCInProgress = &SCC;
    for (size_t i = 0, e = SCC.size(); i != e; ++i)
      ConvertType(SCC[i]);
    SCCInProgress = 0;

    // Finally, replace pointer types with a pointer to the pointee type (which
    // has now been computed).  This means that while uses of the pointer type
    // by types in the SCC will most likely have been converted into nasty {}*,
    // uses by types outside the SCC will see a sensible pointer type.  This is
    // not needed for correctness - it just makes the IR nicer.
    if (SCC.size() > 1)
      for (size_t i = 0, e = SCC.size(); i != e; ++i) {
        tree some_type = SCC[i];
        if (isa<ACCESS_TYPE>(some_type)) {
          tree pointee = main_type(some_type);
          // The pointee cannot have void type since the SCC contains more than
          // one type.
          assert(!isa<VOID_TYPE>(pointee) && "Self-recursive void*!");
          // The pointee must have been converted since it has to be in the same
          // SCC as the pointer (since the SCC contains more than one type).
          Type *PointeeTy = getCachedType(pointee);
          assert(PointeeTy && "Pointee not converted!");
          RememberTypeConversion(some_type, PointeeTy->getPointerTo());
        }
      }
  }

  // At this point every type reachable from this one has been converted, and
  // the conversion results cached.  Return the value computed for the type.
  Type *Ty = getCachedType(type);
  assert(Ty && "Type not converted!");
  return Ty;
}
