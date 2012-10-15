//===------------ DefaultABI.cpp - Default ABI implementation -------------===//
//
// Copyright (C) 2010 to 2012  Rafael Espindola, Duncan Sands et al.
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
// This file implements the default ABI.
//===----------------------------------------------------------------------===//

// Plugin headers
#include "dragonegg/ABI.h"

// System headers
#include <gmp.h>

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
#ifndef ENABLE_BUILD_WITH_CXX
} // extern "C"
#endif

// Trees header.
#include "dragonegg/Trees.h"

void DefaultABIClient::anchor() {}

// doNotUseShadowReturn - Return true if the specified GCC type
// should not be returned using a pointer to struct parameter.
bool doNotUseShadowReturn(tree type, tree fndecl, CallingConv::ID CC) {
  (void)CC; // Not used by all ABI macros.
  if (!TYPE_SIZE(type))
    return false;
  if (!isa<INTEGER_CST>(TYPE_SIZE(type)))
    return false;
  // LLVM says do not use shadow argument.
  if (LLVM_SHOULD_NOT_RETURN_COMPLEX_IN_MEMORY(type) ||
      LLVM_SHOULD_NOT_USE_SHADOW_RETURN(type, CC))
    return true;
  // GCC says use shadow argument.
  if (aggregate_value_p(type, fndecl))
    return false;
  return true;
}

/// isSingleElementStructOrArray - If this is (recursively) a structure with one
/// field or an array with one element, return the field type, otherwise return
/// null.  Returns null for complex number types.  If ignoreZeroLength, the
/// struct (recursively) may include zero-length fields in addition to the
/// single element that has data.  If rejectFatBitField, and the single element
/// is a bitfield of a type that's bigger than the struct, return null anyway.
tree isSingleElementStructOrArray(tree type, bool ignoreZeroLength,
                                  bool rejectFatBitfield) {
  // Complex numbers have two fields.
  if (isa<COMPLEX_TYPE>(type)) return 0;
  // All other scalars are good.
  if (!isa<AGGREGATE_TYPE>(type)) return type;

  tree FoundField = 0;
  switch (TREE_CODE(type)) {
  case QUAL_UNION_TYPE:
  case UNION_TYPE:     // Single element unions don't count.
  case COMPLEX_TYPE:   // Complex values are like 2-element records.
  default:
    return 0;
  case RECORD_TYPE:
    // If this record has variable length, reject it.
    if (!isa<INTEGER_CST>(TYPE_SIZE(type)))
      return 0;

    for (tree Field = TYPE_FIELDS(type); Field; Field = TREE_CHAIN(Field))
      if (isa<FIELD_DECL>(Field)) {
        if (ignoreZeroLength) {
          if (DECL_SIZE(Field) && isa<INTEGER_CST>(DECL_SIZE(Field)) &&
              TREE_INT_CST_LOW(DECL_SIZE(Field)) == 0)
            continue;
        }
        if (!FoundField) {
          if (rejectFatBitfield &&
              isa<INTEGER_CST>(TYPE_SIZE(type)) &&
              TREE_INT_CST_LOW(TYPE_SIZE(TREE_TYPE(Field))) >
              TREE_INT_CST_LOW(TYPE_SIZE(type)))
            return 0;
          FoundField = TREE_TYPE(Field);
        } else {
          return 0;   // More than one field.
        }
      }
    return FoundField ? isSingleElementStructOrArray(FoundField,
                                                     ignoreZeroLength, false)
                      : 0;
  case ARRAY_TYPE:
    ArrayType *Ty = dyn_cast<ArrayType>(ConvertType(type));
    if (!Ty || Ty->getNumElements() != 1)
      return 0;
    return isSingleElementStructOrArray(TREE_TYPE(type), false, false);
  }
}

/// isZeroSizedStructOrUnion - Returns true if this is a struct or union
/// which is zero bits wide.
bool isZeroSizedStructOrUnion(tree type) {
  if (!isa<RECORD_OR_UNION_TYPE>(type))
    return false;
  return int_size_in_bytes(type) == 0;
}

DefaultABI::DefaultABI(DefaultABIClient &c) : C(c) {}

bool DefaultABI::isShadowReturn() const { return C.isShadowReturn(); }

/// HandleReturnType - This is invoked by the target-independent code for the
/// return type. It potentially breaks down the argument and invokes methods
/// on the client that indicate how its pieces should be handled.  This
/// handles things like returning structures via hidden parameters.
void DefaultABI::HandleReturnType(tree type, tree fn, bool isBuiltin) {
  (void)isBuiltin; // Not used by all ABI macros.
  unsigned Offset = 0;
  Type *Ty = ConvertType(type);
  if (Ty->isVectorTy()) {
    // Vector handling is weird on x86.  In particular builtin and
    // non-builtin function of the same return types can use different
    // calling conventions.
    tree ScalarType = LLVM_SHOULD_RETURN_VECTOR_AS_SCALAR(type, isBuiltin);
    if (ScalarType)
      C.HandleAggregateResultAsScalar(ConvertType(ScalarType));
    else if (LLVM_SHOULD_RETURN_VECTOR_AS_SHADOW(type, isBuiltin))
      C.HandleScalarShadowResult(Ty->getPointerTo(), false);
    else
      C.HandleScalarResult(Ty);
  } else if (Ty->isSingleValueType() || Ty->isVoidTy()) {
    // Return scalar values normally.
    C.HandleScalarResult(Ty);
  } else if (doNotUseShadowReturn(type, fn, C.getCallingConv())) {
    tree SingleElt = LLVM_SHOULD_RETURN_SELT_STRUCT_AS_SCALAR(type);
    if (SingleElt && TYPE_SIZE(SingleElt) &&
        isa<INTEGER_CST>(TYPE_SIZE(SingleElt)) &&
        TREE_INT_CST_LOW(TYPE_SIZE_UNIT(type)) ==
        TREE_INT_CST_LOW(TYPE_SIZE_UNIT(SingleElt))) {
      C.HandleAggregateResultAsScalar(ConvertType(SingleElt));
    } else {
      // Otherwise return as an integer value large enough to hold the entire
      // aggregate.
      if (Type *AggrTy = LLVM_AGGR_TYPE_FOR_STRUCT_RETURN(type,
                                  C.getCallingConv()))
        C.HandleAggregateResultAsAggregate(AggrTy);
      else if (Type* ScalarTy =
               LLVM_SCALAR_TYPE_FOR_STRUCT_RETURN(type, &Offset))
        C.HandleAggregateResultAsScalar(ScalarTy, Offset);
      else
        llvm_unreachable("Unable to determine how to return this aggregate!");
    }
  } else {
    // If the function is returning a struct or union, we pass the pointer to
    // the struct as the first argument to the function.

    // FIXME: should return the hidden first argument for some targets
    // (e.g. ELF i386).
    if (isa<AGGREGATE_TYPE>(type))
      C.HandleAggregateShadowResult(Ty->getPointerTo(), false);
    else
      C.HandleScalarShadowResult(Ty->getPointerTo(), false);
  }
}

/// HandleArgument - This is invoked by the target-independent code for each
/// argument type passed into the function.  It potentially breaks down the
/// argument and invokes methods on the client that indicate how its pieces
/// should be handled.  This handles things like decimating structures into
/// their fields.
void DefaultABI::HandleArgument(tree type, std::vector<Type*> &ScalarElts,
                                Attributes *Attributes) {
  unsigned Size = 0;
  bool DontCheckAlignment = false;
  Type *Ty = ConvertType(type);
  // Figure out if this field is zero bits wide, e.g. {} or [0 x int].  Do
  // not include variable sized fields here.
  std::vector<Type*> Elts;
  if (Ty->isVoidTy()) {
    // Handle void explicitly as a {} type.
    Type *OpTy = StructType::get(getGlobalContext());
    C.HandleScalarArgument(OpTy, type);
    ScalarElts.push_back(OpTy);
  } else if (isPassedByInvisibleReference(type)) { // variable size -> by-ref.
    Type *PtrTy = Ty->getPointerTo();
    C.HandleByInvisibleReferenceArgument(PtrTy, type);
    ScalarElts.push_back(PtrTy);
  } else if (Ty->isVectorTy()) {
    if (LLVM_SHOULD_PASS_VECTOR_IN_INTEGER_REGS(type)) {
      PassInIntegerRegisters(type, ScalarElts, 0, false);
    } else if (LLVM_SHOULD_PASS_VECTOR_USING_BYVAL_ATTR(type)) {
      C.HandleByValArgument(Ty, type);
      if (Attributes) {
        Attributes::Builder B;
        B.addAttribute(Attributes::ByVal);
        B.addAlignmentAttr(LLVM_BYVAL_ALIGNMENT(type));
        *Attributes = Attributes::get(B.addAttributes(*Attributes));
      }
    } else {
      C.HandleScalarArgument(Ty, type);
      ScalarElts.push_back(Ty);
    }
  } else if (LLVM_TRY_PASS_AGGREGATE_CUSTOM(type, ScalarElts,
                                            C.getCallingConv(), &C)) {
    // Nothing to do.
  } else if (Ty->isSingleValueType()) {
    C.HandleScalarArgument(Ty, type);
    ScalarElts.push_back(Ty);
  } else if (LLVM_SHOULD_PASS_AGGREGATE_AS_FCA(type, Ty)) {
    C.HandleFCAArgument(Ty, type);
  } else if (LLVM_SHOULD_PASS_AGGREGATE_IN_MIXED_REGS(type, Ty,
                                                      C.getCallingConv(),
                                                      Elts)) {
    if (!LLVM_AGGREGATE_PARTIALLY_PASSED_IN_REGS(Elts, ScalarElts,
                                                 C.isShadowReturn(),
                                                 C.getCallingConv()))
      PassInMixedRegisters(Ty, Elts, ScalarElts);
    else {
      C.HandleByValArgument(Ty, type);
      if (Attributes) {
        Attributes::Builder B;
        B.addAttribute(Attributes::ByVal);
        B.addAlignmentAttr(LLVM_BYVAL_ALIGNMENT(type));
        *Attributes = Attributes::get(B.addAttributes(*Attributes));
      }
    }
  } else if (LLVM_SHOULD_PASS_AGGREGATE_USING_BYVAL_ATTR(type, Ty)) {
    C.HandleByValArgument(Ty, type);
    if (Attributes) {
      Attributes::Builder B;
      B.addAttribute(Attributes::ByVal);
      B.addAlignmentAttr(LLVM_BYVAL_ALIGNMENT(type));
      *Attributes = Attributes::get(B.addAttributes(*Attributes));
    }
  } else if (LLVM_SHOULD_PASS_AGGREGATE_IN_INTEGER_REGS(type, &Size,
                                                        &DontCheckAlignment)) {
    PassInIntegerRegisters(type, ScalarElts, Size, DontCheckAlignment);
  } else if (isZeroSizedStructOrUnion(type)) {
    // Zero sized struct or union, just drop it!
    ;
  } else if (isa<RECORD_TYPE>(type)) {
    for (tree Field = TYPE_FIELDS(type); Field; Field = TREE_CHAIN(Field))
      if (isa<FIELD_DECL>(Field)) {
        const tree Ftype = TREE_TYPE(Field);
        unsigned FNo = GetFieldIndex(Field, Ty);
        assert(FNo < INT_MAX && "Case not handled yet!");

        // Currently, a bvyal type inside a non-byval struct is a zero-length
        // object inside a bigger object on x86-64.  This type should be
        // skipped (but only when it is inside a bigger object).
        // (We know there currently are no other such cases active because
        // they would hit the assert in FunctionPrologArgumentConversion::
        // HandleByValArgument.)
        Type *FTy = ConvertType(Ftype);
        (void)FTy; // Not used by all ABI macros.
        if (!LLVM_SHOULD_PASS_AGGREGATE_USING_BYVAL_ATTR(Ftype, FTy)) {
          C.EnterField(FNo, Ty);
          HandleArgument(TREE_TYPE(Field), ScalarElts);
          C.ExitField();
        }
      }
  } else if (isa<COMPLEX_TYPE>(type)) {
    C.EnterField(0, Ty);
    HandleArgument(TREE_TYPE(type), ScalarElts);
    C.ExitField();
    C.EnterField(1, Ty);
    HandleArgument(TREE_TYPE(type), ScalarElts);
    C.ExitField();
  } else if ((isa<UNION_TYPE>(type)) ||
             (isa<QUAL_UNION_TYPE>(type))) {
    HandleUnion(type, ScalarElts);
  } else if (isa<ARRAY_TYPE>(type)) {
    // Array with padding?
    if (Ty->isStructTy())
      Ty = cast<StructType>(Ty)->getTypeAtIndex(0U);
    ArrayType *ATy = cast<ArrayType>(Ty);
    for (unsigned i = 0, e = ATy->getNumElements(); i != e; ++i) {
      C.EnterField(i, Ty);
      HandleArgument(TREE_TYPE(type), ScalarElts);
      C.ExitField();
    }
  } else {
    llvm_unreachable("Unknown aggregate type!");
  }
}

/// HandleUnion - Handle a UNION_TYPE or QUAL_UNION_TYPE tree.
void DefaultABI::HandleUnion(tree type, std::vector<Type*> &ScalarElts) {
  if (TYPE_TRANSPARENT_AGGR(type)) {
    tree Field = TYPE_FIELDS(type);
    assert(Field && "Transparent union must have some elements!");
    while (!isa<FIELD_DECL>(Field)) {
      Field = TREE_CHAIN(Field);
      assert(Field && "Transparent union must have some elements!");
    }

    HandleArgument(TREE_TYPE(Field), ScalarElts);
  } else {
    // Unions pass the largest element.
    unsigned MaxSize = 0;
    tree MaxElt = 0;
    for (tree Field = TYPE_FIELDS(type); Field; Field = TREE_CHAIN(Field)) {
      if (isa<FIELD_DECL>(Field)) {
        tree SizeTree = TYPE_SIZE(TREE_TYPE(Field));
        unsigned Size = ((unsigned)TREE_INT_CST_LOW(SizeTree)+7)/8;
        if (Size > MaxSize) {
          MaxSize = Size;
          MaxElt = Field;
        }
      }
    }

    if (MaxElt)
      HandleArgument(TREE_TYPE(MaxElt), ScalarElts);
  }
}

/// PassInIntegerRegisters - Given an aggregate value that should be passed in
/// integer registers, convert it to a structure containing ints and pass all
/// of the struct elements in.  If Size is set we pass only that many bytes.
void DefaultABI::PassInIntegerRegisters(tree type,
                                        std::vector<Type*> &ScalarElts,
                                        unsigned origSize,
                                        bool DontCheckAlignment) {
  unsigned Size;
  if (origSize)
    Size = origSize;
  else
    Size = TREE_INT_CST_LOW(TYPE_SIZE(type))/8;

  // FIXME: We should preserve all aggregate value alignment information.
  // Work around to preserve some aggregate value alignment information:
  // don't bitcast aggregate value to Int64 if its alignment is different
  // from Int64 alignment. ARM backend needs this.
  unsigned Align = TYPE_ALIGN(type)/8;
  unsigned Int64Align =
    getDataLayout().getABITypeAlignment(Type::getInt64Ty(getGlobalContext()));
  bool UseInt64 = (DontCheckAlignment || Align >= Int64Align);

  unsigned ElementSize = UseInt64 ? 8:4;
  unsigned ArraySize = Size / ElementSize;

  // Put as much of the aggregate as possible into an array.
  Type *ATy = NULL;
  Type *ArrayElementType = NULL;
  if (ArraySize) {
    Size = Size % ElementSize;
    ArrayElementType = (UseInt64 ?
                        Type::getInt64Ty(getGlobalContext()) :
                        Type::getInt32Ty(getGlobalContext()));
    ATy = ArrayType::get(ArrayElementType, ArraySize);
  }

  // Pass any leftover bytes as a separate element following the array.
  unsigned LastEltRealSize = 0;
  llvm::Type *LastEltTy = 0;
  if (Size > 4) {
    LastEltTy = Type::getInt64Ty(getGlobalContext());
  } else if (Size > 2) {
    LastEltTy = Type::getInt32Ty(getGlobalContext());
  } else if (Size > 1) {
    LastEltTy = Type::getInt16Ty(getGlobalContext());
  } else if (Size > 0) {
    LastEltTy = Type::getInt8Ty(getGlobalContext());
  }
  if (LastEltTy) {
    if (Size != getDataLayout().getTypeAllocSize(LastEltTy))
      LastEltRealSize = Size;
  }

  std::vector<Type*> Elts;
  if (ATy)
    Elts.push_back(ATy);
  if (LastEltTy)
    Elts.push_back(LastEltTy);
  StructType *STy = StructType::get(getGlobalContext(), Elts, false);

  unsigned i = 0;
  if (ArraySize) {
    C.EnterField(0, STy);
    for (unsigned j = 0; j < ArraySize; ++j) {
      C.EnterField(j, ATy);
      C.HandleScalarArgument(ArrayElementType, 0);
      ScalarElts.push_back(ArrayElementType);
      C.ExitField();
    }
    C.ExitField();
    ++i;
  }
  if (LastEltTy) {
    C.EnterField(i, STy);
    C.HandleScalarArgument(LastEltTy, 0, LastEltRealSize);
    ScalarElts.push_back(LastEltTy);
    C.ExitField();
  }
}

/// PassInMixedRegisters - Given an aggregate value that should be passed in
/// mixed integer, floating point, and vector registers, convert it to a
/// structure containing the specified struct elements in.
void DefaultABI::PassInMixedRegisters(Type *Ty,
                                      std::vector<Type*> &OrigElts,
                                      std::vector<Type*> &ScalarElts) {
  // We use VoidTy in OrigElts to mean "this is a word in the aggregate
  // that occupies storage but has no useful information, and is not passed
  // anywhere".  Happens on x86-64.
  std::vector<Type*> Elts(OrigElts);
  Type* wordType = getDataLayout().getPointerSize(0) == 4 ?
    Type::getInt32Ty(getGlobalContext()) : Type::getInt64Ty(getGlobalContext());
  for (unsigned i=0, e=Elts.size(); i!=e; ++i)
    if (OrigElts[i]->isVoidTy())
      Elts[i] = wordType;

  StructType *STy = StructType::get(getGlobalContext(), Elts, false);

  unsigned Size = getDataLayout().getTypeAllocSize(STy);
  unsigned InSize = 0;
  // If Ty and STy size does not match then last element is accessing
  // extra bits.
  unsigned LastEltSizeDiff = 0;
  if (isa<StructType>(Ty) || isa<ArrayType>(Ty)) {
    InSize = getDataLayout().getTypeAllocSize(Ty);
    if (InSize < Size) {
      unsigned N = STy->getNumElements();
      llvm::Type *LastEltTy = STy->getElementType(N-1);
      if (LastEltTy->isIntegerTy())
        LastEltSizeDiff =
          getDataLayout().getTypeAllocSize(LastEltTy) - (Size - InSize);
    }
  }
  for (unsigned i = 0, e = Elts.size(); i != e; ++i) {
    if (!OrigElts[i]->isVoidTy()) {
      C.EnterField(i, STy);
      unsigned RealSize = 0;
      if (LastEltSizeDiff && i == (e - 1))
        RealSize = LastEltSizeDiff;
      C.HandleScalarArgument(Elts[i], 0, RealSize);
      ScalarElts.push_back(Elts[i]);
      C.ExitField();
    }
  }
}
