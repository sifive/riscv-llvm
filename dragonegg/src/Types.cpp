//===----------- Types.cpp - Converting GCC types to LLVM types -----------===//
//
// Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011  Chris Lattner,
// Duncan Sands et al.
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
#include "dragonegg/Trees.h"
#include "dragonegg/Types.h"
extern "C" {
#include "dragonegg/cache.h"
}

// LLVM headers
#include "llvm/Module.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/Assembly/Writer.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"

// System headers
#include <gmp.h>
#include <map>

// GCC headers
extern "C" {
#include "config.h"
// Stop GCC declaring 'getopt' as it can clash with the system's declaration.
#undef HAVE_DECL_GETOPT
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
}

static LLVMContext &Context = getGlobalContext();

/// SCCInProgress - Set of mutually dependent types currently being converted.
static const std::vector<tree_node*> *SCCInProgress;

//===----------------------------------------------------------------------===//
//                          Viewing types as graphs
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
      return TREE_CODE(type_ref) == TREE_LIST ?
        TREE_VALUE(type_ref) : TREE_TYPE(type_ref);
    };

    /// Comparison operators.
    bool operator==(const ContainedTypeIterator &other) const {
      return other.type_ref == this->type_ref;
    }
    bool operator!=(const ContainedTypeIterator &other) const {
      return !(*this == other);
    }

    /// Postfix increment operator.
    ContainedTypeIterator operator++(int) {
      ContainedTypeIterator Result(*this);
      ++(*this);
      return Result;
    }

    /// Prefix increment operator.
    ContainedTypeIterator& operator++() {
      assert(type_ref && "Incrementing end iterator!");

      switch (TREE_CODE(type_ref)) {
      default:
        assert(false && "Unexpected tree kind!");
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
        type_ref = TREE_CHAIN(type_ref);
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
        assert(false && "Unknown type!");

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
        return ContainedTypeIterator(TYPE_FIELDS(type));

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
  assert(TREE_CODE(type) == ARRAY_TYPE && "Only for array types!");
  tree range = array_type_nelts(type); // The number of elements minus one.
  // Bail out if the array has variable or unknown length.
  if (!isInt64(range, false))
    return NO_LENGTH;
  int64_t Range = getInt64(range, false);
  return Range < 0 ? 0 : 1 + (uint64_t)Range;
}

/// GetFieldIndex - Return the index of the field in the given LLVM type that
/// corresponds to the GCC field declaration 'decl'.  This means that the LLVM
/// and GCC fields start in the same byte (if 'decl' is a bitfield, this means
/// that its first bit is within the byte the LLVM field starts at).  Returns
/// INT_MAX if there is no such LLVM field.
int GetFieldIndex(tree decl, Type *Ty) {
  assert(TREE_CODE(decl) == FIELD_DECL && "Expected a FIELD_DECL!");
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

  // If the field declaration is at a variable or humongous offset then there
  // can be no corresponding LLVM field.
  if (!OffsetIsLLVMCompatible(decl))
    return set_decl_index(decl, INT_MAX);

  // Find the LLVM field that contains the first bit of the GCC field.
  uint64_t OffsetInBytes = getFieldOffsetInBits(decl) / 8; // Ignore bit in byte
  const StructLayout *SL = getTargetData().getStructLayout(STy);
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

/// GetUnitType - Returns an integer one address unit wide if 'NumUnits' is 1;
/// otherwise returns an array of such integers with 'NumUnits' elements.  For
/// example, on a machine which has 16 bit bytes returns an i16 or an array of
/// i16.
Type *GetUnitType(LLVMContext &C, unsigned NumUnits) {
  // The following assertion is here because just about every place that calls
  // this routine implicitly assumes this.
  assert(!(BITS_PER_UNIT & 7) && "Unit size not a multiple of 8 bits!");
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

/// isSequentialCompatible - Return true if the specified gcc array, pointer or
/// vector type and the corresponding LLVM SequentialType lay out their elements
/// identically in memory, so doing a GEP accesses the right memory location.
/// We assume that objects without a known size do not.
bool isSequentialCompatible(tree type) {
  assert((TREE_CODE(type) == ARRAY_TYPE ||
          TREE_CODE(type) == POINTER_TYPE ||
          TREE_CODE(type) == VECTOR_TYPE ||
          TREE_CODE(type) == REFERENCE_TYPE) && "not a sequential type!");
  // This relies on gcc types with constant size mapping to LLVM types with the
  // same size.  It is possible for the component type not to have a size:
  // struct foo;  extern foo bar[];
  return isInt64(TYPE_SIZE(TREE_TYPE(type)), true);
}

//===----------------------------------------------------------------------===//
//                   Matching LLVM types with GCC trees
//===----------------------------------------------------------------------===//

// llvm_get_type/llvm_set_type - Associate an LLVM type with each TREE type.
// These are lazily computed by ConvertType.

static Type *llvm_set_type(tree Tr, Type *Ty) {
  assert(TYPE_P(Tr) && "Expected a gcc type!");

  // Check that the LLVM and GCC types have the same size, or, if the type has
  // variable size, that the LLVM type is not bigger than any possible value of
  // the GCC type.
#ifndef NDEBUG
  if (Ty->isSized() && isInt64(TYPE_SIZE(Tr), true)) {
    uint64_t LLVMSize = getTargetData().getTypeAllocSizeInBits(Ty);
    if (getInt64(TYPE_SIZE(Tr), true) != LLVMSize) {
      errs() << "GCC: ";
      debug_tree(Tr);
      errs() << "LLVM: ";
      Ty->print(errs());
      errs() << " (" << LLVMSize << " bits)\n";
      DieAbjectly("LLVM type size doesn't match GCC type size!");
    }
  }
#endif

  return (Type *)llvm_set_cached(Tr, Ty);
}

static Type *llvm_get_type(tree Tr) {
  assert(TYPE_P(Tr) && "Expected a gcc type!");
  return (Type *)llvm_get_cached(Tr);
}

static bool llvm_has_type(tree Tr) {
  assert(TYPE_P(Tr) && "Expected a gcc type!");
  return llvm_has_cached(Tr);
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
         TREE_CODE(TYPE_SIZE(Type)) != INTEGER_CST;
}


//===----------------------------------------------------------------------===//
//                      Main Type Conversion Routines
//===----------------------------------------------------------------------===//

/// getRegType - Returns the LLVM type to use for registers that hold a value
/// of the scalar GCC type 'type'.  All of the EmitReg* routines use this to
/// determine the LLVM type to return.
Type *getRegType(tree type) {
  // NOTE: Any changes made here need to be reflected in LoadRegisterFromMemory,
  // StoreRegisterToMemory and ExtractRegisterFromConstant.
  assert(!AGGREGATE_TYPE_P(type) && "Registers must have a scalar type!");
  assert(TREE_CODE(type) != VOID_TYPE && "Registers cannot have void type!");

  switch (TREE_CODE(type)) {

  default:
    DieAbjectly("Unknown register type!", type);

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
    return getTargetData().getIntPtrType(Context);

  case POINTER_TYPE:
  case REFERENCE_TYPE:
    // void* -> byte*
    return VOID_TYPE_P(TREE_TYPE(type)) ?  GetUnitPointerType(Context) :
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
      DieAbjectly("Unknown FP type!", type);

  case VECTOR_TYPE: {
    // LLVM does not support vectors of pointers, so turn any pointers into
    // integers.
    Type *EltTy = POINTER_TYPE_P(TREE_TYPE(type)) ?
      getTargetData().getIntPtrType(Context) : getRegType(TREE_TYPE(type));
    return VectorType::get(EltTy, TYPE_VECTOR_SUBPARTS(type));
  }

  }
}

/// getPointerToType - Returns the LLVM register type to use for a pointer to
/// the given GCC type.
Type *getPointerToType(tree type) {
  if (VOID_TYPE_P(type))
    // void* -> byte*
    return GetUnitPointerType(Context);
  // FIXME: Handle address spaces.
  return ConvertType(type)->getPointerTo();
}


//===----------------------------------------------------------------------===//
//                  FUNCTION/METHOD_TYPE Conversion Routines
//===----------------------------------------------------------------------===//

namespace {
  class FunctionTypeConversion : public DefaultABIClient {
    Type *&RetTy;
    SmallVectorImpl<Type*> &ArgTypes;
    CallingConv::ID &CallingConv;
    bool isShadowRet;
    bool KNRPromotion;
    unsigned Offset;
  public:
    FunctionTypeConversion(Type *&retty, SmallVectorImpl<Type*> &AT,
                           CallingConv::ID &CC, bool KNR)
      : RetTy(retty), ArgTypes(AT), CallingConv(CC), KNRPromotion(KNR),
        Offset(0) {
      CallingConv = CallingConv::C;
      isShadowRet = false;
    }

    /// getCallingConv - This provides the desired CallingConv for the function.
    CallingConv::ID getCallingConv(void) { return CallingConv; }

    bool isShadowReturn() const { return isShadowRet; }

    /// HandleScalarResult - This callback is invoked if the function returns a
    /// simple scalar result value.
    void HandleScalarResult(Type *RetTy) {
      this->RetTy = RetTy;
    }

    /// HandleAggregateResultAsScalar - This callback is invoked if the function
    /// returns an aggregate value by bit converting it to the specified scalar
    /// type and returning that.
    void HandleAggregateResultAsScalar(Type *ScalarTy, unsigned Offset=0) {
      RetTy = ScalarTy;
      this->Offset = Offset;
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
  if (TREE_CODE(ArgTy) == BOOLEAN_TYPE) {
    if (TREE_INT_CST_LOW(TYPE_SIZE(ArgTy)) < INT_TYPE_SIZE)
      return Attribute::ZExt;
  } else if (TREE_CODE(ArgTy) == INTEGER_TYPE &&
             TREE_INT_CST_LOW(TYPE_SIZE(ArgTy)) < INT_TYPE_SIZE) {
    if (TYPE_UNSIGNED(ArgTy))
      return Attribute::ZExt;
    else
      return Attribute::SExt;
  }

  return Attribute::None;
}

/// ConvertParamListToLLVMSignature - This method is used to build the argument
/// type list for K&R prototyped functions.  In this case, we have to figure out
/// the type list (to build a FunctionType) from the actual DECL_ARGUMENTS list
/// for the function.  This method takes the DECL_ARGUMENTS list (Args), and
/// fills in Result with the argument types for the function.  It returns the
/// specified result type for the function.
FunctionType *ConvertArgListToFnType(tree type, tree Args, tree static_chain,
                                     CallingConv::ID &CallingConv,
                                     AttrListPtr &PAL) {
  tree ReturnType = TREE_TYPE(type);
  SmallVector<Type*, 8> ArgTys;
  Type *RetTy(Type::getVoidTy(Context));

  FunctionTypeConversion Client(RetTy, ArgTys, CallingConv, true /*K&R*/);
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

  if (RAttributes != Attribute::None)
    Attrs.push_back(AttributeWithIndex::get(0, RAttributes));

  // If this function returns via a shadow argument, the dest loc is passed
  // in as a pointer.  Mark that pointer as struct-ret and noalias.
  if (ABIConverter.isShadowReturn())
    Attrs.push_back(AttributeWithIndex::get(ArgTys.size(),
                                    Attribute::StructRet | Attribute::NoAlias));

  std::vector<Type*> ScalarArgs;
  if (static_chain) {
    // Pass the static chain as the first parameter.
    ABIConverter.HandleArgument(TREE_TYPE(static_chain), ScalarArgs);
    // Mark it as the chain argument.
    Attrs.push_back(AttributeWithIndex::get(ArgTys.size(),
                                             Attribute::Nest));
  }

  for (; Args && TREE_TYPE(Args) != void_type_node; Args = TREE_CHAIN(Args)) {
    tree ArgTy = TREE_TYPE(Args);

    // Determine if there are any attributes for this param.
    Attributes PAttributes = Attribute::None;

    ABIConverter.HandleArgument(ArgTy, ScalarArgs, &PAttributes);

    // Compute zext/sext attributes.
    PAttributes |= HandleArgumentExtension(ArgTy);

    if (PAttributes != Attribute::None)
      Attrs.push_back(AttributeWithIndex::get(ArgTys.size(), PAttributes));
  }

  PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());
  return FunctionType::get(RetTy, ArgTys, false);
}

FunctionType *ConvertFunctionType(tree type, tree decl, tree static_chain,
                                  CallingConv::ID &CallingConv,
                                  AttrListPtr &PAL) {
  Type *RetTy = Type::getVoidTy(Context);
  SmallVector<Type*, 8> ArgTypes;
  bool isVarArg = false;
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
  Attributes FnAttributes = Attribute::None;

  int flags = flags_from_decl_or_type(decl ? decl : type);

  // Check for 'noreturn' function attribute.
  if (flags & ECF_NORETURN)
    FnAttributes |= Attribute::NoReturn;

  // Check for 'nounwind' function attribute.
  if (flags & ECF_NOTHROW)
    FnAttributes |= Attribute::NoUnwind;

  // Check for 'readnone' function attribute.
  // Both PURE and CONST will be set if the user applied
  // __attribute__((const)) to a function the compiler
  // knows to be pure, such as log.  A user or (more
  // likely) libm implementor might know their local log
  // is in fact const, so this should be valid (and gcc
  // accepts it).  But llvm IR does not allow both, so
  // set only ReadNone.
  if (flags & ECF_CONST)
    FnAttributes |= Attribute::ReadNone;

  // Check for 'readonly' function attribute.
  if (flags & ECF_PURE && !(flags & ECF_CONST))
    FnAttributes |= Attribute::ReadOnly;

  // Since they write the return value through a pointer,
  // 'sret' functions cannot be 'readnone' or 'readonly'.
  if (ABIConverter.isShadowReturn())
    FnAttributes &= ~(Attribute::ReadNone|Attribute::ReadOnly);

  // Demote 'readnone' nested functions to 'readonly' since
  // they may need to read through the static chain.
  if (static_chain && (FnAttributes & Attribute::ReadNone)) {
    FnAttributes &= ~Attribute::ReadNone;
    FnAttributes |= Attribute::ReadOnly;
  }

  // Compute whether the result needs to be zext or sext'd.
  Attributes RAttributes = Attribute::None;
  RAttributes |= HandleArgumentExtension(TREE_TYPE(type));

  // Allow the target to change the attributes.
#ifdef TARGET_ADJUST_LLVM_RETATTR
  TARGET_ADJUST_LLVM_RETATTR(RAttributes, type);
#endif

  // The value returned by a 'malloc' function does not alias anything.
  if (flags & ECF_MALLOC)
    RAttributes |= Attribute::NoAlias;

  if (RAttributes != Attribute::None)
    Attrs.push_back(AttributeWithIndex::get(0, RAttributes));

  // If this function returns via a shadow argument, the dest loc is passed
  // in as a pointer.  Mark that pointer as struct-ret and noalias.
  if (ABIConverter.isShadowReturn())
    Attrs.push_back(AttributeWithIndex::get(ArgTypes.size(),
                                    Attribute::StructRet | Attribute::NoAlias));

  std::vector<Type*> ScalarArgs;
  if (static_chain) {
    // Pass the static chain as the first parameter.
    ABIConverter.HandleArgument(TREE_TYPE(static_chain), ScalarArgs);
    // Mark it as the chain argument.
    Attrs.push_back(AttributeWithIndex::get(ArgTypes.size(),
                                             Attribute::Nest));
  }

  // If the target has regparam parameters, allow it to inspect the function
  // type.
  int local_regparam = 0;
  int local_fp_regparam = 0;
#ifdef LLVM_TARGET_ENABLE_REGPARM
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
    Attributes PAttributes = Attribute::None;

    unsigned OldSize = ArgTypes.size();

    ABIConverter.HandleArgument(ArgTy, ScalarArgs, &PAttributes);

    // Compute zext/sext attributes.
    PAttributes |= HandleArgumentExtension(ArgTy);

    // Compute noalias attributes. If we have a decl for the function
    // inspect it for restrict qualifiers, otherwise try the argument
    // types.
    tree RestrictArgTy = (DeclArgs) ? TREE_TYPE(DeclArgs) : ArgTy;
    if (TREE_CODE(RestrictArgTy) == POINTER_TYPE ||
        TREE_CODE(RestrictArgTy) == REFERENCE_TYPE) {
      if (TYPE_RESTRICT(RestrictArgTy))
        PAttributes |= Attribute::NoAlias;
    }

#ifdef LLVM_TARGET_ENABLE_REGPARM
    // Allow the target to mark this as inreg.
    if (INTEGRAL_TYPE_P(ArgTy) || POINTER_TYPE_P(ArgTy) ||
        SCALAR_FLOAT_TYPE_P(ArgTy))
      LLVM_ADJUST_REGPARM_ATTRIBUTE(PAttributes, ArgTy,
                                    TREE_INT_CST_LOW(TYPE_SIZE(ArgTy)),
                                    local_regparam, local_fp_regparam);
#endif // LLVM_TARGET_ENABLE_REGPARM

    if (PAttributes != Attribute::None) {
      HasByVal |= PAttributes & Attribute::ByVal;

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
  if (HasByVal)
    FnAttributes &= ~(Attribute::ReadNone | Attribute::ReadOnly);

  if (flag_force_vararg_prototypes)
    // If forcing prototypes to be varargs, make all function types varargs
    // except those for builtin functions.
    isVarArg = decl ? !DECL_BUILT_IN(decl) : true;
  else
    // If the argument list ends with a void type node, it isn't vararg.
    isVarArg = (Args == 0);
  assert(RetTy && "Return type not specified!");

  if (FnAttributes != Attribute::None)
    Attrs.push_back(AttributeWithIndex::get(~0, FnAttributes));

  // Finally, make the function type and result attributes.
  PAL = AttrListPtr::get(Attrs.begin(), Attrs.end());
  return FunctionType::get(RetTy, ArgTypes, isVarArg);
}

//===----------------------------------------------------------------------===//
//                      RECORD/Struct Conversion Routines
//===----------------------------------------------------------------------===//

/// StructTypeConversionInfo - A temporary structure that is used when
/// translating a RECORD_TYPE to an LLVM type.
namespace {

  struct StructTypeConversionInfo {
    std::vector<Type*> Elements;
    std::vector<uint64_t> ElementOffsetInBytes;
    std::vector<uint64_t> ElementSizeInBytes;
    std::vector<bool> PaddingElement; // True if field is used for padding
    const TargetData &TD;
    unsigned GCCStructAlignmentInBytes;
    bool Packed; // True if struct is packed
    bool AllBitFields; // True if all struct fields are bit fields
    bool LastFieldStartsAtNonByteBoundry;
    unsigned ExtraBitsAvailable; // Non-zero if last field is bit field and it
                                 // does not use all allocated bits

    StructTypeConversionInfo(TargetMachine &TM, unsigned GCCAlign, bool P)
      : TD(*TM.getTargetData()), GCCStructAlignmentInBytes(GCCAlign),
        Packed(P), AllBitFields(true), LastFieldStartsAtNonByteBoundry(false),
        ExtraBitsAvailable(0) {}

    void lastFieldStartsAtNonByteBoundry(bool value) {
      LastFieldStartsAtNonByteBoundry = value;
    }

    void extraBitsAvailable (unsigned E) {
      ExtraBitsAvailable = E;
    }

    bool isPacked() { return Packed; }

    void markAsPacked() {
      Packed = true;
    }

    void allFieldsAreNotBitFields() {
      AllBitFields = false;
      // Next field is not a bitfield.
      LastFieldStartsAtNonByteBoundry = false;
    }

    unsigned getGCCStructAlignmentInBytes() const {
      return GCCStructAlignmentInBytes;
    }

    /// getTypeAlignment - Return the alignment of the specified type in bytes.
    ///
    unsigned getTypeAlignment(Type *Ty) const {
      return Packed ? 1 : TD.getABITypeAlignment(Ty);
    }

    /// getTypeSize - Return the size of the specified type in bytes.
    ///
    uint64_t getTypeSize(Type *Ty) const {
      return TD.getTypeAllocSize(Ty);
    }

    /// fillInLLVMType - Return the LLVM type for the specified object.
    ///
    void fillInLLVMType(StructType *STy) const {
      // Use Packed type if Packed is set or all struct fields are bitfields.
      // Empty struct is not packed unless packed is set.
      STy->setBody(Elements, Packed || (!Elements.empty() && AllBitFields));
    }

    /// getAlignmentAsLLVMStruct - Return the alignment of this struct if it were
    /// converted to an LLVM type.
    uint64_t getAlignmentAsLLVMStruct() const {
      if (Packed || AllBitFields) return 1;
      unsigned MaxAlign = 1;
      for (unsigned i = 0, e = Elements.size(); i != e; ++i)
        MaxAlign = std::max(MaxAlign, getTypeAlignment(Elements[i]));
      return MaxAlign;
    }

    /// getSizeAsLLVMStruct - Return the size of this struct if it were converted
    /// to an LLVM type.  This is the end of last element push an alignment pad at
    /// the end.
    uint64_t getSizeAsLLVMStruct() const {
      if (Elements.empty()) return 0;
      unsigned MaxAlign = getAlignmentAsLLVMStruct();
      uint64_t Size = ElementOffsetInBytes.back()+ElementSizeInBytes.back();
      return (Size+MaxAlign-1) & ~(MaxAlign-1);
    }

    // If this is a Packed struct and ExtraBitsAvailable is not zero then
    // remove Extra bytes if ExtraBitsAvailable > 8.
    void RemoveExtraBytes () {

      unsigned NoOfBytesToRemove = ExtraBitsAvailable/8;

      if (!Packed && !AllBitFields)
        return;

      if (NoOfBytesToRemove == 0)
        return;

      Type *LastType = Elements.back();
      unsigned PadBytes = 0;

      if (LastType->isIntegerTy(8))
        PadBytes = 1 - NoOfBytesToRemove;
      else if (LastType->isIntegerTy(16))
        PadBytes = 2 - NoOfBytesToRemove;
      else if (LastType->isIntegerTy(32))
        PadBytes = 4 - NoOfBytesToRemove;
      else if (LastType->isIntegerTy(64))
        PadBytes = 8 - NoOfBytesToRemove;
      else
        return;

      assert (PadBytes > 0 && "Unable to remove extra bytes");

      // Update last element type and size, element offset is unchanged.
      Type *Pad =  ArrayType::get(Type::getInt8Ty(Context), PadBytes);
      unsigned OriginalSize = ElementSizeInBytes.back();
      Elements.pop_back();
      Elements.push_back(Pad);

      ElementSizeInBytes.pop_back();
      ElementSizeInBytes.push_back(OriginalSize - NoOfBytesToRemove);
    }

    /// ResizeLastElementIfOverlapsWith - If the last element in the struct
    /// includes the specified byte, remove it. Return true struct
    /// layout is sized properly. Return false if unable to handle ByteOffset.
    /// In this case caller should redo this struct as a packed structure.
    bool ResizeLastElementIfOverlapsWith(uint64_t ByteOffset, tree /*Field*/,
                                         Type *Ty) {
      Type *SavedTy = NULL;

      if (!Elements.empty()) {
        assert(ElementOffsetInBytes.back() <= ByteOffset &&
               "Cannot go backwards in struct");

        SavedTy = Elements.back();
        if (ElementOffsetInBytes.back()+ElementSizeInBytes.back() > ByteOffset) {
          // The last element overlapped with this one, remove it.
          uint64_t PoppedOffset = ElementOffsetInBytes.back();
          Elements.pop_back();
          ElementOffsetInBytes.pop_back();
          ElementSizeInBytes.pop_back();
          PaddingElement.pop_back();
          uint64_t EndOffset = getNewElementByteOffset(1);
          if (EndOffset < PoppedOffset) {
            // Make sure that some field starts at the position of the
            // field we just popped.  Otherwise we might end up with a
            // gcc non-bitfield being mapped to an LLVM field with a
            // different offset.
            Type *Pad = Type::getInt8Ty(Context);
            if (PoppedOffset != EndOffset + 1)
              Pad = ArrayType::get(Pad, PoppedOffset - EndOffset);
            addElement(Pad, EndOffset, PoppedOffset - EndOffset);
          }
        }
      }

      // Get the LLVM type for the field.  If this field is a bitfield, use the
      // declared type, not the shrunk-to-fit type that GCC gives us in TREE_TYPE.
      unsigned ByteAlignment = getTypeAlignment(Ty);
      uint64_t NextByteOffset = getNewElementByteOffset(ByteAlignment);
      if (NextByteOffset > ByteOffset ||
          ByteAlignment > getGCCStructAlignmentInBytes()) {
        // LLVM disagrees as to where this field should go in the natural field
        // ordering.  Therefore convert to a packed struct and try again.
        return false;
      }

      // If alignment won't round us up to the right boundary, insert explicit
      // padding.
      if (NextByteOffset < ByteOffset) {
        uint64_t CurOffset = getNewElementByteOffset(1);
        Type *Pad = Type::getInt8Ty(Context);
        if (SavedTy && LastFieldStartsAtNonByteBoundry)
          // We want to reuse SavedType to access this bit field.
          // e.g. struct __attribute__((packed)) {
          //  unsigned int A,
          //  unsigned short B : 6,
          //                 C : 15;
          //  char D; };
          //  In this example, previous field is C and D is current field.
          addElement(SavedTy, CurOffset, ByteOffset - CurOffset);
        else if (ByteOffset - CurOffset != 1)
          Pad = ArrayType::get(Pad, ByteOffset - CurOffset);
        addElement(Pad, CurOffset, ByteOffset - CurOffset);
      }
      return true;
    }

    /// FieldNo - Remove the specified field and all of the fields that come after
    /// it.
    void RemoveFieldsAfter(unsigned FieldNo) {
      Elements.erase(Elements.begin()+FieldNo, Elements.end());
      ElementOffsetInBytes.erase(ElementOffsetInBytes.begin()+FieldNo,
                                 ElementOffsetInBytes.end());
      ElementSizeInBytes.erase(ElementSizeInBytes.begin()+FieldNo,
                               ElementSizeInBytes.end());
      PaddingElement.erase(PaddingElement.begin()+FieldNo,
                           PaddingElement.end());
    }

    /// getNewElementByteOffset - If we add a new element with the specified
    /// alignment, what byte offset will it land at?
    uint64_t getNewElementByteOffset(unsigned ByteAlignment) {
      if (Elements.empty()) return 0;
      uint64_t LastElementEnd =
        ElementOffsetInBytes.back() + ElementSizeInBytes.back();

      return (LastElementEnd+ByteAlignment-1) & ~(ByteAlignment-1);
    }

    /// addElement - Add an element to the structure with the specified type,
    /// offset and size.
    void addElement(Type *Ty, uint64_t Offset, uint64_t Size,
                    bool ExtraPadding = false) {
      Elements.push_back(Ty);
      ElementOffsetInBytes.push_back(Offset);
      ElementSizeInBytes.push_back(Size);
      PaddingElement.push_back(ExtraPadding);
      lastFieldStartsAtNonByteBoundry(false);
      ExtraBitsAvailable = 0;
    }

    /// getFieldEndOffsetInBytes - Return the byte offset of the byte immediately
    /// after the specified field.  For example, if FieldNo is 0 and the field
    /// is 4 bytes in size, this will return 4.
    uint64_t getFieldEndOffsetInBytes(unsigned FieldNo) const {
      assert(FieldNo < ElementOffsetInBytes.size() && "Invalid field #!");
      return ElementOffsetInBytes[FieldNo]+ElementSizeInBytes[FieldNo];
    }

    /// getEndUnallocatedByte - Return the first byte that isn't allocated at the
    /// end of a structure.  For example, for {}, it's 0, for {int} it is 4, for
    /// {int,short}, it is 6.
    uint64_t getEndUnallocatedByte() const {
      if (ElementOffsetInBytes.empty()) return 0;
      return getFieldEndOffsetInBytes(ElementOffsetInBytes.size()-1);
    }

    void addNewBitField(uint64_t Size, uint64_t Extra,
                        uint64_t FirstUnallocatedByte);

    void dump() const;
  };

} // Unnamed namespace.

// Add new element which is a bit field. Size is not the size of bit field,
// but size of bits required to determine type of new Field which will be
// used to access this bit field.
// If possible, allocate a field with room for Size+Extra bits.
void StructTypeConversionInfo::addNewBitField(uint64_t Size, uint64_t Extra,
                                              uint64_t FirstUnallocatedByte) {

  // Figure out the LLVM type that we will use for the new field.
  // Note, Size is not necessarily size of the new field. It indicates
  // additional bits required after FirstunallocatedByte to cover new field.
  Type *NewFieldTy = 0;

  // First try an ABI-aligned field including (some of) the Extra bits.
  // This field must satisfy Size <= w && w <= XSize.
  uint64_t XSize = Size + Extra;
  for (unsigned w = NextPowerOf2(std::min(UINT64_C(64), XSize))/2;
       w >= Size && w >= 8; w /= 2) {
    if (TD.isIllegalInteger(w))
      continue;
    // Would a w-sized integer field be aligned here?
    const unsigned a = TD.getABIIntegerTypeAlignment(w);
    if (FirstUnallocatedByte & (a-1) || a > getGCCStructAlignmentInBytes())
      continue;
    // OK, use w-sized integer.
    NewFieldTy = IntegerType::get(Context, w);
    break;
  }

  // Try an integer field that holds Size bits.
  if (!NewFieldTy) {
    if (Size <= 8)
      NewFieldTy = Type::getInt8Ty(Context);
    else if (Size <= 16)
      NewFieldTy = Type::getInt16Ty(Context);
    else if (Size <= 32)
      NewFieldTy = Type::getInt32Ty(Context);
    else {
      assert(Size <= 64 && "Bitfield too large!");
      NewFieldTy = Type::getInt64Ty(Context);
    }
  }

  // Check that the alignment of NewFieldTy won't cause a gap in the structure!
  unsigned ByteAlignment = getTypeAlignment(NewFieldTy);
  if (FirstUnallocatedByte & (ByteAlignment-1) ||
      ByteAlignment > getGCCStructAlignmentInBytes()) {
    // Instead of inserting a nice whole field, insert a small array of ubytes.
    NewFieldTy = ArrayType::get(Type::getInt8Ty(Context), (Size+7)/8);
  }

  // Finally, add the new field.
  addElement(NewFieldTy, FirstUnallocatedByte, getTypeSize(NewFieldTy));
  ExtraBitsAvailable = NewFieldTy->getPrimitiveSizeInBits() - Size;
}

void StructTypeConversionInfo::dump() const {
  raw_ostream &OS = outs();
  OS << "Info has " << Elements.size() << " fields:\n";
  for (unsigned i = 0, e = Elements.size(); i != e; ++i) {
    OS << "  Offset = " << ElementOffsetInBytes[i]
       << " Size = " << ElementSizeInBytes[i]
       << " Type = " << *Elements[i] << "\n";
  }
  OS.flush();
}

/// DecodeStructBitField - This method decodes the specified bit-field, adding
/// or updating the specified StructTypeConversionInfo to reflect it.
///
/// Note that in general, we cannot produce a good covering of struct fields for
/// bitfields.  As such, we only make sure that all bits in a struct that
/// correspond to a bitfield are represented in the LLVM struct with
/// (potentially multiple) integer fields of integer type.  This ensures that
/// initialized globals with bitfields can have the initializers for the
/// bitfields specified.
static void DecodeStructBitField(tree Field, StructTypeConversionInfo &Info) {
  unsigned FieldSizeInBits = TREE_INT_CST_LOW(DECL_SIZE(Field));

  if (FieldSizeInBits == 0)   // Ignore 'int:0', which just affects layout.
    return;

  // Get the starting offset in the record.
  uint64_t StartOffsetInBits = getFieldOffsetInBits(Field);
  uint64_t EndBitOffset    = FieldSizeInBits+StartOffsetInBits;

  // If the last inserted LLVM field completely contains this bitfield, just
  // ignore this field.
  if (!Info.Elements.empty()) {
    uint64_t LastFieldBitOffset = Info.ElementOffsetInBytes.back()*8;
    unsigned LastFieldBitSize   = Info.ElementSizeInBytes.back()*8;
    assert(LastFieldBitOffset <= StartOffsetInBits &&
           "This bitfield isn't part of the last field!");
    if (EndBitOffset <= LastFieldBitOffset+LastFieldBitSize &&
        LastFieldBitOffset+LastFieldBitSize >= StartOffsetInBits) {
      // Already contained in previous field. Update remaining extra bits that
      // are available.
      Info.extraBitsAvailable(Info.getEndUnallocatedByte()*8 - EndBitOffset);
      return;
    }
  }

  // Otherwise, this bitfield lives (potentially) partially in the preceding
  // field and in fields that exist after it.  Add integer-typed fields to the
  // LLVM struct such that there are no holes in the struct where the bitfield
  // is: these holes would make it impossible to statically initialize a global
  // of this type that has an initializer for the bitfield.

  // We want the integer-typed fields as large as possible up to the machine
  // word size. If there are more bitfields following this one, try to include
  // them in the same field.

  // Calculate the total number of bits in the continuous group of bitfields
  // following this one. This is the number of bits that addNewBitField should
  // try to include.
  unsigned ExtraSizeInBits = 0;
  tree LastBitField = 0;
  for (tree f = TREE_CHAIN(Field); f; f = TREE_CHAIN(f)) {
    assert(TREE_CODE(Field) == FIELD_DECL && "Lang data not freed?");
    if (TREE_CODE(DECL_FIELD_OFFSET(f)) != INTEGER_CST)
      break;
    if (isBitfield(f))
      LastBitField = f;
    else {
      // We can use all this bits up to the next non-bitfield.
      LastBitField = 0;
      ExtraSizeInBits = getFieldOffsetInBits(f) - EndBitOffset;
      break;
    }
  }
  // Record ended in a bitfield? Use all of the last byte.
  if (LastBitField)
    ExtraSizeInBits = RoundUpToAlignment(getFieldOffsetInBits(LastBitField) +
      TREE_INT_CST_LOW(DECL_SIZE(LastBitField)), 8) - EndBitOffset;

  // Compute the number of bits that we need to add to this struct to cover
  // this field.
  uint64_t FirstUnallocatedByte = Info.getEndUnallocatedByte();
  uint64_t StartOffsetFromByteBoundry = StartOffsetInBits & 7;

  if (StartOffsetInBits < FirstUnallocatedByte*8) {

    uint64_t AvailableBits = FirstUnallocatedByte * 8 - StartOffsetInBits;
    // This field's starting point is already allocated.
    if (StartOffsetFromByteBoundry == 0) {
      // This field starts at byte boundary. Need to allocate space
      // for additional bytes not yet allocated.
      unsigned NumBitsToAdd = FieldSizeInBits - AvailableBits;
      Info.addNewBitField(NumBitsToAdd, ExtraSizeInBits, FirstUnallocatedByte);
      return;
    }

    // Otherwise, this field's starting point is inside previously used byte.
    // This happens with Packed bit fields. In this case one LLVM Field is
    // used to access previous field and current field.
    unsigned prevFieldTypeSizeInBits =
      Info.ElementSizeInBytes[Info.Elements.size() - 1] * 8;

    unsigned NumBitsRequired = prevFieldTypeSizeInBits
      + (FieldSizeInBits - AvailableBits);

    if (NumBitsRequired > 64) {
      // Use bits from previous field.
      NumBitsRequired = FieldSizeInBits - AvailableBits;
    } else {
      // If type used to access previous field is not large enough then
      // remove previous field and insert new field that is large enough to
      // hold both fields.
      Info.RemoveFieldsAfter(Info.Elements.size() - 1);
      for (unsigned idx = 0; idx < (prevFieldTypeSizeInBits/8); ++idx)
        FirstUnallocatedByte--;
    }
    Info.addNewBitField(NumBitsRequired, ExtraSizeInBits, FirstUnallocatedByte);
    // Do this after adding Field.
    Info.lastFieldStartsAtNonByteBoundry(true);
    return;
  }

  if (StartOffsetInBits > FirstUnallocatedByte*8) {
    // If there is padding between the last field and the struct, insert
    // explicit bytes into the field to represent it.
    unsigned PadBytes = 0;
    unsigned PadBits = 0;
    if (StartOffsetFromByteBoundry != 0) {
      // New field does not start at byte boundary.
      PadBits = StartOffsetInBits - (FirstUnallocatedByte*8);
      PadBytes = PadBits/8;
      PadBits = PadBits - PadBytes*8;
    } else
      PadBytes = StartOffsetInBits/8-FirstUnallocatedByte;

    if (PadBytes) {
      Type *Pad = Type::getInt8Ty(Context);
      if (PadBytes != 1)
        Pad = ArrayType::get(Pad, PadBytes);
      Info.addElement(Pad, FirstUnallocatedByte, PadBytes);
    }

    FirstUnallocatedByte = StartOffsetInBits/8;
    // This field will use some of the bits from this PadBytes, if
    // starting offset is not at byte boundary.
    if (StartOffsetFromByteBoundry != 0)
      FieldSizeInBits += PadBits;
  }

  // Now, Field starts at FirstUnallocatedByte and everything is aligned.
  Info.addNewBitField(FieldSizeInBits, ExtraSizeInBits, FirstUnallocatedByte);
}

/// DecodeStructFields - This method decodes the specified field, if it is a
/// FIELD_DECL, adding or updating the specified StructTypeConversionInfo to
/// reflect it.  Return true if field is decoded correctly. Otherwise return
/// false.
static bool DecodeStructFields(tree Field, StructTypeConversionInfo &Info) {
  // Handle bit-fields specially.
  if (isBitfield(Field)) {
    // If this field is forcing packed llvm struct then retry entire struct
    // layout.
    if (!Info.isPacked()) {
      // Unnamed bitfield type does not contribute in struct alignment
      // computations. Use packed llvm structure in such cases.
      if (!DECL_NAME(Field))
        return false;
      // If this field is packed then the struct may need padding fields
      // before this field.
      if (DECL_PACKED(Field))
        return false;
      // If Field has user defined alignment and it does not match Ty alignment
      // then convert to a packed struct and try again.
      if (TYPE_USER_ALIGN(TREE_TYPE(Field))) {
        Type *Ty = ConvertType(TREE_TYPE(Field));
        if (TYPE_ALIGN(TREE_TYPE(Field)) !=
            8 * Info.getTypeAlignment(Ty))
          return false;
      }
    }
    DecodeStructBitField(Field, Info);
    return true;
  }

  Info.allFieldsAreNotBitFields();

  // Get the starting offset in the record.
  uint64_t StartOffsetInBits = getFieldOffsetInBits(Field);
  assert((StartOffsetInBits & 7) == 0 && "Non-bit-field has non-byte offset!");
  uint64_t StartOffsetInBytes = StartOffsetInBits/8;

  Type *Ty = ConvertType(TREE_TYPE(Field));

  // If this field is packed then the struct may need padding fields
  // before this field.
  if (DECL_PACKED(Field) && !Info.isPacked())
    return false;
  // Pop any previous elements out of the struct if they overlap with this one.
  // This can happen when the C++ front-end overlaps fields with tail padding in
  // C++ classes.
  else if (!Info.ResizeLastElementIfOverlapsWith(StartOffsetInBytes, Field, Ty)) {
    // LLVM disagrees as to where this field should go in the natural field
    // ordering.  Therefore convert to a packed struct and try again.
    return false;
  }
  else if (TYPE_USER_ALIGN(TREE_TYPE(Field))
           && (unsigned)DECL_ALIGN(Field) != 8 * Info.getTypeAlignment(Ty)
           && !Info.isPacked()) {
    // If Field has user defined alignment and it does not match Ty alignment
    // then convert to a packed struct and try again.
    return false;
  } else
    // At this point, we know that adding the element will happen at the right
    // offset.  Add it.
    Info.addElement(Ty, StartOffsetInBytes, Info.getTypeSize(Ty));
  return true;
}

/// UnionHasOnlyZeroOffsets - Check if a union type has only members with
/// offsets that are zero, e.g., no Fortran equivalences.
static bool UnionHasOnlyZeroOffsets(tree type) {
  for (tree Field = TYPE_FIELDS(type); Field; Field = TREE_CHAIN(Field)) {
    assert(TREE_CODE(Field) == FIELD_DECL && "Lang data not freed?");
    if (!OffsetIsLLVMCompatible(Field))
      return false;
    if (getFieldOffsetInBits(Field) != 0)
      return false;
  }
  return true;
}

/// SelectUnionMember - Find the union member with the largest aligment.  If
/// there are multiple types with the same alignment, select the one with
/// the largest size. If the type with max. align is smaller than other types,
/// then we will add padding later on anyway to match union size.
static void SelectUnionMember(tree type, StructTypeConversionInfo &Info) {
  bool FindBiggest = TREE_CODE(type) != QUAL_UNION_TYPE;

  Type *UnionTy = 0;
  tree UnionField = 0;
  unsigned MinAlign = ~0U;
  uint64_t BestSize = FindBiggest ? 0 : ~(uint64_t)0;
  for (tree Field = TYPE_FIELDS(type); Field; Field = TREE_CHAIN(Field)) {
    assert(TREE_CODE(Field) == FIELD_DECL && "Lang data not freed?");
    assert(DECL_FIELD_OFFSET(Field) && integer_zerop(DECL_FIELD_OFFSET(Field))
           && "Union with non-zero offset?");

    // Skip fields that are known not to be present.
    if (TREE_CODE(type) == QUAL_UNION_TYPE &&
        integer_zerop(DECL_QUALIFIER(Field)))
      continue;

    tree TheGccTy = TREE_TYPE(Field);

    // Skip zero-length bitfields.  These are only used for setting the
    // alignment.
    if (DECL_BIT_FIELD(Field) && DECL_SIZE(Field) &&
        integer_zerop(DECL_SIZE(Field)))
      continue;

    Type *TheTy = ConvertType(TheGccTy);
    unsigned Align = Info.getTypeAlignment(TheTy);
    uint64_t Size  = Info.getTypeSize(TheTy);

    // Select TheTy as union type if it is the biggest/smallest field (depending
    // on the value of FindBiggest).  If more than one field achieves this size
    // then choose the least aligned.
    if ((Size == BestSize && Align < MinAlign) ||
        (FindBiggest && Size > BestSize) ||
        (!FindBiggest && Size < BestSize)) {
      UnionTy = TheTy;
      UnionField = Field;
      BestSize = Size;
      MinAlign = Align;
    }

    // Skip remaining fields if this one is known to be present.
    if (TREE_CODE(type) == QUAL_UNION_TYPE &&
        integer_onep(DECL_QUALIFIER(Field)))
      break;
  }

  if (UnionTy) {            // Not an empty union.
    if (8 * Info.getTypeAlignment(UnionTy) > TYPE_ALIGN(type))
      Info.markAsPacked();

    if (isBitfield(UnionField)) {
      unsigned FieldSizeInBits = TREE_INT_CST_LOW(DECL_SIZE(UnionField));
      Info.addNewBitField(FieldSizeInBits, 0, 0);
    } else {
      Info.allFieldsAreNotBitFields();
      Info.addElement(UnionTy, 0, Info.getTypeSize(UnionTy));
    }
  }
}

/// ConvertRECORD - Convert a RECORD_TYPE, UNION_TYPE or QUAL_UNION_TYPE to
/// an LLVM type.
// A note on C++ virtual base class layout.  Consider the following example:
// class A { public: int i0; };
// class B : public virtual A { public: int i1; };
// class C : public virtual A { public: int i2; };
// class D : public virtual B, public virtual C { public: int i3; };
//
// The TYPE nodes gcc builds for classes represent that class as it looks
// standing alone.  Thus B is size 12 and looks like { vptr; i2; baseclass A; }
// However, this is not the layout used when that class is a base class for
// some other class, yet the same TYPE node is still used.  D in the above has
// both a BINFO list entry and a FIELD that reference type B, but the virtual
// base class A within B is not allocated in that case; B-within-D is only
// size 8.  The correct size is in the FIELD node (does not match the size
// in its child TYPE node.)  The fields to be omitted from the child TYPE,
// as far as I can tell, are always the last ones; but also, there is a
// TYPE_DECL node sitting in the middle of the FIELD list separating virtual
// base classes from everything else.
//
// Similarly, a nonvirtual base class which has virtual base classes might
// not contain those virtual base classes when used as a nonvirtual base class.
// There is seemingly no way to detect this except for the size differential.
//
// For LLVM purposes, we build a new type for B-within-D that
// has the correct size and layout for that usage.
static Type *ConvertRECORD(tree type) {
  assert(TYPE_SIZE(type) && "Incomplete types should be handled elsewhere!");

  assert(llvm_get_type(type) && isa<StructType>(llvm_get_type(type)) &&
         cast<StructType>(llvm_get_type(type))->isOpaque() &&
         "Incorrect placeholder for struct type!");

  // Record those fields which will be converted to LLVM fields.
  SmallVector<std::pair<tree, uint64_t>, 32> Fields;
  for (tree Field = TYPE_FIELDS(type); Field; Field = TREE_CHAIN(Field)) {
    assert(TREE_CODE(Field) == FIELD_DECL && "Lang data not freed?");
    if (OffsetIsLLVMCompatible(Field))
      Fields.push_back(std::make_pair(Field, getFieldOffsetInBits(Field)));
  }

  // The fields are almost always sorted, but occasionally not.  Sort them by
  // field offset.
  for (unsigned i = 1, e = Fields.size(); i < e; i++)
    for (unsigned j = i; j && Fields[j].second < Fields[j-1].second; j--)
      std::swap(Fields[j], Fields[j-1]);

  StructTypeConversionInfo *Info =
    new StructTypeConversionInfo(*TheTarget, TYPE_ALIGN(type) / 8,
                                 TYPE_PACKED(type));

  // Convert over all of the elements of the struct.
  // Workaround to get Fortran EQUIVALENCE working.
  // TODO: Unify record and union logic and handle this optimally.
  bool HasOnlyZeroOffsets = TREE_CODE(type) != RECORD_TYPE &&
    UnionHasOnlyZeroOffsets(type);
  if (HasOnlyZeroOffsets) {
    SelectUnionMember(type, *Info);
  } else {
    // Convert over all of the elements of the struct.
    bool retryAsPackedStruct = false;
    for (unsigned i = 0, e = Fields.size(); i < e; i++)
      if (DecodeStructFields(Fields[i].first, *Info) == false) {
        retryAsPackedStruct = true;
        break;
      }

    if (retryAsPackedStruct) {
      delete Info;
      Info = new StructTypeConversionInfo(*TheTarget, TYPE_ALIGN(type) / 8,
                                          true);
      for (unsigned i = 0, e = Fields.size(); i < e; i++)
        if (DecodeStructFields(Fields[i].first, *Info) == false) {
          assert(0 && "Unable to decode struct fields.");
        }
    }
  }

  // Insert tail padding if the LLVM struct requires explicit tail padding to
  // be the same size as the GCC struct or union.  This handles, e.g., "{}" in
  // C++, and cases where a union has larger alignment than the largest member
  // does.
  if (TYPE_SIZE(type) && TREE_CODE(TYPE_SIZE(type)) == INTEGER_CST) {
    uint64_t GCCTypeSize = getInt64(TYPE_SIZE_UNIT(type), true);
    uint64_t LLVMStructSize = Info->getSizeAsLLVMStruct();

    if (LLVMStructSize > GCCTypeSize) {
      Info->RemoveExtraBytes();
      LLVMStructSize = Info->getSizeAsLLVMStruct();
    }

    if (LLVMStructSize != GCCTypeSize) {
      assert(LLVMStructSize < GCCTypeSize &&
             "LLVM type size doesn't match GCC type size!");
      uint64_t LLVMLastElementEnd = Info->getNewElementByteOffset(1);

      // If only one byte is needed then insert i8.
      if (GCCTypeSize-LLVMLastElementEnd == 1)
        Info->addElement(Type::getInt8Ty(Context), 1, 1);
      else {
        if (((GCCTypeSize-LLVMStructSize) % 4) == 0 &&
            (Info->getAlignmentAsLLVMStruct() %
             Info->getTypeAlignment(Type::getInt32Ty(Context))) == 0) {
          // Insert array of i32.
          unsigned Int32ArraySize = (GCCTypeSize-LLVMStructSize) / 4;
          Type *PadTy =
            ArrayType::get(Type::getInt32Ty(Context), Int32ArraySize);
          Info->addElement(PadTy, GCCTypeSize - LLVMLastElementEnd,
                           Int32ArraySize, true /* Padding Element */);
        } else {
          Type *PadTy = ArrayType::get(Type::getInt8Ty(Context),
                                             GCCTypeSize-LLVMStructSize);
          Info->addElement(PadTy, GCCTypeSize - LLVMLastElementEnd,
                           GCCTypeSize - LLVMLastElementEnd,
                           true /* Padding Element */);
        }
      }
    }
  } else
    Info->RemoveExtraBytes();

  StructType *ResultTy = cast<StructType>(llvm_get_type(type));
  Info->fillInLLVMType(ResultTy);

  return ResultTy;
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
    assert(false && "Unknown type!");

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
    return !llvm_has_type(type);

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
    Type *Ty = llvm_get_type(type);
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
             !mayRecurse(TYPE_MAIN_VARIANT(*I)))
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

/// ConvertNonRecursiveType - Convert a type when this is known to not require
/// breaking type conversion loops, see mayRecurse.
static Type *ConvertNonRecursiveType(tree type) {
  assert(type == TYPE_MAIN_VARIANT(type) && "Not converting the main variant!");
  assert(!mayRecurse(type) && "Expected a non-recursive type!");

  switch (TREE_CODE(type)) {
  default:
    DieAbjectly("Unknown or recursive type!", type);

  case ARRAY_TYPE:
  case FUNCTION_TYPE:
  case METHOD_TYPE:
  case POINTER_TYPE:
  case REFERENCE_TYPE: {
    // If these types are not recursive it can only be because they were already
    // converted and we can safely return the result of the previous conversion.
    Type *Ty = llvm_get_type(type);
    assert(Ty && "Type not already converted!");
    return Ty;
  }

  case ENUMERAL_TYPE:
    // If the enum is incomplete return a placeholder type.
    if (!TYPE_SIZE(type))
      return Type::getInt32Ty(Context);
    // Otherwise fall through.
  case BOOLEAN_TYPE:
  case INTEGER_TYPE: {
    uint64_t Size = getInt64(TYPE_SIZE(type), true);
    return IntegerType::get(Context, Size); // Not worth caching.
  }

  case COMPLEX_TYPE: {
    if (Type *Ty = llvm_get_type(type)) return Ty;
    Type *Ty = ConvertNonRecursiveType(TYPE_MAIN_VARIANT(TREE_TYPE(type)));
    Ty = StructType::get(Ty, Ty, NULL);
    return llvm_set_type(type, Ty);
  }

  case OFFSET_TYPE:
    // Handle OFFSET_TYPE specially.  This is used for pointers to members,
    // which are really just integer offsets.  Return the appropriate integer
    // type directly.
    return getTargetData().getIntPtrType(Context); // Not worth caching.

  case REAL_TYPE:
    // It is not worth caching the result of this type conversion.
    switch (TYPE_PRECISION(type)) {
    default:
      DieAbjectly("Unknown FP type!", type);
    case 32: return Type::getFloatTy(Context);
    case 64: return Type::getDoubleTy(Context);
    case 80: return Type::getX86_FP80Ty(Context);
    case 128:
#ifdef TARGET_POWERPC
      return Type::getPPC_FP128Ty(Context);
#else
      // IEEE quad precision.
      return Type::getFP128Ty(Context);
#endif
    }

  case RECORD_TYPE:
  case QUAL_UNION_TYPE:
  case UNION_TYPE:
    // If the type was already converted then return the already computed type.
    if (Type *Ty = llvm_get_type(type)) return Ty;

    // Otherwise this must be an incomplete type - return an opaque struct.
    assert(!TYPE_SIZE(type) && "Expected an incomplete type!");
    return llvm_set_type(type, StructType::createNamed(Context,
                                                     getDescriptiveName(type)));

  case VECTOR_TYPE: {
    if (Type *Ty = llvm_get_type(type)) return Ty;
    Type *Ty;
    // LLVM does not support vectors of pointers, so turn any pointers into
    // integers.
    if (POINTER_TYPE_P(TREE_TYPE(type)))
      Ty = getTargetData().getIntPtrType(Context);
    else
      Ty = ConvertNonRecursiveType(TYPE_MAIN_VARIANT(TREE_TYPE(type)));
    Ty = VectorType::get(Ty, TYPE_VECTOR_SUBPARTS(type));
    return llvm_set_type(type, Ty);
  }

  case VOID_TYPE:
    return Type::getVoidTy(Context); // Not worth caching.
  }
}

/// ConvertRecursiveType - Convert a type when conversion may require breaking
/// type conversion loops, see mayRecurse.  Note that all types used by but not
/// in the current strongly connected component (SCC) must have been converted
/// already.
static Type *ConvertRecursiveType(tree type) {
  assert(type == TYPE_MAIN_VARIANT(type) && "Not converting the main variant!");
  assert(mayRecurse(type) && "Expected a recursive type!");
  assert(SCCInProgress && "Missing recursion data!");

#ifndef NDEBUG
  // Check that the given type is in the current strongly connected component
  // (SCC) of the type graph.  This should always be the case because SCCs are
  // visited bottom up.
  bool inSCC = false;
  for (unsigned i = 0, e = SCCInProgress->size(); i != e; ++i)
    if ((*SCCInProgress)[i] == type) {
      inSCC = true;
      break;
    }
  if (!inSCC)
    DieAbjectly("Type not in SCC!", type);
#endif

  switch (TREE_CODE(type)) {
  default:
    DieAbjectly("Unexpected type!", type);

  case QUAL_UNION_TYPE:
  case RECORD_TYPE:
  case UNION_TYPE:
    return llvm_set_type(type, ConvertRECORD(type));

  case POINTER_TYPE:
  case REFERENCE_TYPE: {
    // This is where self-recursion loops are broken, by not converting the type
    // pointed to if this would cause trouble (the pointer type is turned into
    // {}* instead).
    tree pointee = TYPE_MAIN_VARIANT(TREE_TYPE(type));

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
      while (TREE_CODE(pointee) == ARRAY_TYPE)
        pointee = TYPE_MAIN_VARIANT(TREE_TYPE(pointee));

      // If the pointee is a record or union type then return a pointer to its
      // placeholder type.  Otherwise return {}*.
      if (TREE_CODE(pointee) == QUAL_UNION_TYPE ||
          TREE_CODE(pointee) == RECORD_TYPE ||
          TREE_CODE(pointee) == UNION_TYPE)
        PointeeTy = llvm_get_type(pointee);
      else
        PointeeTy = StructType::get(Context);
    }

    return llvm_set_type(type, PointeeTy->getPointerTo());
  }

  case METHOD_TYPE:
  case FUNCTION_TYPE: {
    CallingConv::ID CallingConv;
    AttrListPtr PAL;
    // No declaration to pass through, passing NULL.
    return llvm_set_type(type, ConvertFunctionType(type, NULL, NULL,
                                                   CallingConv, PAL));
  }

  case ARRAY_TYPE: {
    Type *ElementTy = ConvertType(TREE_TYPE(type));
    uint64_t NumElements = ArrayLengthOf(type);

    if (NumElements == NO_LENGTH) // Variable length array?
      NumElements = 0;

    // Create the array type.
    Type *Ty = ArrayType::get(ElementTy, NumElements);

    // If the user increased the alignment of the array element type, then the
    // size of the array is rounded up by that alignment even though the size
    // of the array element type is not (!).  Correct for this if necessary by
    // adding padding.  May also need padding if the element type has variable
    // size and the array type has variable length, but by a miracle the product
    // gives a constant size.
    if (isInt64(TYPE_SIZE(type), true)) {
      uint64_t PadBits = getInt64(TYPE_SIZE(type), true) -
        getTargetData().getTypeAllocSizeInBits(Ty);
      if (PadBits) {
        Type *Padding = ArrayType::get(Type::getInt8Ty(Context), PadBits / 8);
        Ty = StructType::get(Ty, Padding, NULL);
      }
    }

    return llvm_set_type(type, Ty);
  }
  }
}

Type *ConvertType(tree type) {
  if (type == error_mark_node) return Type::getInt32Ty(Context);

  // LLVM doesn't care about variants such as const, volatile, or restrict.
  type = TYPE_MAIN_VARIANT(type);

  // If this type can be converted without special action being needed to avoid
  // conversion loops coming from self-referential types, then convert it.
  if (!mayRecurse(type))
    return ConvertNonRecursiveType(type);

  // If we already started a possibly looping type conversion, continue with it.
  if (SCCInProgress)
    return ConvertRecursiveType(type);

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
    for (unsigned i = 0, e = SCC.size(); i != e; ++i) {
      tree some_type = SCC[i];
      if (TREE_CODE(some_type) != QUAL_UNION_TYPE &&
          TREE_CODE(some_type) != RECORD_TYPE &&
          TREE_CODE(some_type) != UNION_TYPE) {
        assert(!llvm_has_type(some_type) && "Type already converted!");
        continue;
      }
      // If the type used to be incomplete then a opaque struct placeholder may
      // have been created for it already.
      Type *Ty = llvm_get_type(some_type);
      if (Ty) {
        assert(isa<StructType>(Ty) && cast<StructType>(Ty)->isOpaque() &&
               "Recursive struct already fully converted!");
        continue;
      }
      // Otherwise register a placeholder for this type.
      Ty = StructType::createNamed(Context, getDescriptiveName(some_type));
      llvm_set_type(some_type, Ty);
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
    for (unsigned i = 0, e = SCC.size(); i != e; ++i)
      ConvertType(SCC[i]);
    SCCInProgress = 0;
  }

  // At this point every type reachable from this one has been converted, and
  // the conversion results cached.  Return the value computed for the type.
  Type *Ty = llvm_get_type(type);
  assert(Ty && "Type not converted!");
  return Ty;
}
