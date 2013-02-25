//===---------------- Target.cpp - Implements the ARM ABI. ----------------===//
//
// Copyright (C) 2005 to 2013  Evan Cheng, Jin Gu Kang, Duncan Sands et al.
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
// This file implements specific LLVM ARM ABI.
// It was derived from llvm-arm.cpp on llvm-gcc.4.2.
//===----------------------------------------------------------------------===//

// Plugin headers
#include "dragonegg/ABI.h"
#include "dragonegg/Target.h"

// LLVM headers
#include "llvm/IR/Module.h"

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
#include "target.h"
#include "tree.h"

#include "diagnostic.h"
#include "gimple.h"
#include "toplev.h"
#ifndef ENABLE_BUILD_WITH_CXX
} // extern "C"
#endif

// Trees header.
#include "dragonegg/Trees.h"

static LLVMContext &Context = getGlobalContext();

// "Fundamental Data Types" according to the AAPCS spec.  These are used
// to check that a given aggregate meets the criteria for a "homogeneous
// aggregate."
enum arm_fdts {
  ARM_FDT_INVALID,

  ARM_FDT_HALF_FLOAT,
  ARM_FDT_FLOAT,
  ARM_FDT_DOUBLE,

  ARM_FDT_VECTOR_64,
  ARM_FDT_VECTOR_128,

  ARM_FDT_MAX
};

// Classify type according to the number of fundamental data types contained
// among its members.  Returns true if type is a homogeneous aggregate.
static bool vfp_arg_homogeneous_aggregate_p(enum machine_mode mode, tree type,
                                            int *fdt_counts) {
  bool result = false;
  HOST_WIDE_INT bytes =
      (mode == BLKmode) ? int_size_in_bytes(type) : (int) GET_MODE_SIZE(mode);

  if (type && isa<AGGREGATE_TYPE>(type)) {
    int i;
    int cnt = 0;
    tree field;

    // Zero sized arrays or structures are not homogeneous aggregates.
    if (!bytes)
      return 0;

    // Classify each field of records.
    switch (TREE_CODE(type)) {
    case RECORD_TYPE:
      // For classes first merge in the field of the subclasses.
      if (TYPE_BINFO(type)) {
        tree binfo, base_binfo;
        int basenum;

        for (binfo = TYPE_BINFO(type), basenum = 0;
             BINFO_BASE_ITERATE(binfo, basenum, base_binfo); basenum++) {
          tree type = BINFO_TYPE(base_binfo);

          result = vfp_arg_homogeneous_aggregate_p(TYPE_MODE(type), type,
                                                   fdt_counts);
          if (!result)
            return false;
        }
      }
      // And now merge the fields of structure.
      for (field = TYPE_FIELDS(type); field; field = TREE_CHAIN(field)) {
        if (isa<FIELD_DECL>(field)) {
          if (TREE_TYPE(field) == error_mark_node)
            continue;

          result = vfp_arg_homogeneous_aggregate_p(
              TYPE_MODE(TREE_TYPE(field)), TREE_TYPE(field), fdt_counts);
          if (!result)
            return false;
        }
      }
      break;

    case ARRAY_TYPE:
        // Arrays are handled as small records.
        {
      int array_fdt_counts[ARM_FDT_MAX] = { 0 };

      result = vfp_arg_homogeneous_aggregate_p(
          TYPE_MODE(TREE_TYPE(type)), TREE_TYPE(type), array_fdt_counts);

      cnt = bytes / int_size_in_bytes(TREE_TYPE(type));
      for (i = 0; i < ARM_FDT_MAX; ++i)
        fdt_counts[i] += array_fdt_counts[i] * cnt;

      if (!result)
        return false;
    } break;

    case UNION_TYPE:
    case QUAL_UNION_TYPE: {
      // Unions are similar to RECORD_TYPE.
      int union_fdt_counts[ARM_FDT_MAX] = { 0 };

      // Unions are not derived.
      gcc_assert(!TYPE_BINFO(type) || !BINFO_N_BASE_BINFOS(TYPE_BINFO(type)));
      for (field = TYPE_FIELDS(type); field; field = TREE_CHAIN(field)) {
        int union_field_fdt_counts[ARM_FDT_MAX] = { 0 };

        if (isa<FIELD_DECL>(field)) {
          if (TREE_TYPE(field) == error_mark_node)
            continue;

          result = vfp_arg_homogeneous_aggregate_p(TYPE_MODE(TREE_TYPE(field)),
                                                   TREE_TYPE(field),
                                                   union_field_fdt_counts);
          if (!result)
            return false;

          // track largest union field
          for (i = 0; i < ARM_FDT_MAX; ++i) {
            if (union_field_fdt_counts[i] > 4) // bail early if we can
              return false;

            union_fdt_counts[i] =
                MAX(union_fdt_counts[i], union_field_fdt_counts[i]);
            union_field_fdt_counts[i] = 0; // clear it out for next iter
          }
        }
      }

      // check for only one type across all union fields
      cnt = 0;
      for (i = 0; i < ARM_FDT_MAX; ++i) {
        if (union_fdt_counts[i])
          ++cnt;

        if (cnt > 1)
          return false;

        fdt_counts[i] += union_fdt_counts[i];
      }
    } break;

    default:
      llvm_unreachable("What type is this?");
    }

    // Walk through fdt_counts.  This is a homogeneous aggregate if
    // only one FDT is used.
    cnt = 0;
    for (i = 0; i < ARM_FDT_MAX; ++i) {
      if (fdt_counts[i]) {
        // Make sure that one FDT is 4 or less elements in size.
        if (fdt_counts[i] > 4)
          return false;
        ++cnt;
      }

      if (cnt > 1)
        return false;
    }

    if (cnt == 0)
      return false;

    return true;
  }

  if (type) {
    int idx = 0;
    int cnt = 0;

    switch (TREE_CODE(type)) {
    case REAL_TYPE:
      idx = (TYPE_PRECISION(type) == 32)
            ? ARM_FDT_FLOAT
            : ((TYPE_PRECISION(type) == 64) ? ARM_FDT_DOUBLE : ARM_FDT_INVALID);
      cnt = 1;
      break;

    case COMPLEX_TYPE: {
      tree subtype = TREE_TYPE(type);
      idx = (TYPE_PRECISION(subtype) == 32)
            ? ARM_FDT_FLOAT
            : ((TYPE_PRECISION(subtype) == 64) ? ARM_FDT_DOUBLE
                                               : ARM_FDT_INVALID);
      cnt = 2;
    } break;

    case VECTOR_TYPE:
      idx = (bytes == 8) ? ARM_FDT_VECTOR_64
                         : (bytes == 16) ? ARM_FDT_VECTOR_128 : ARM_FDT_INVALID;
      cnt = 1;
      break;

    case INTEGER_TYPE:
    case POINTER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case REFERENCE_TYPE:
    case FUNCTION_TYPE:
    case METHOD_TYPE:
    default:
      return false; // All of these disqualify.
    }

    fdt_counts[idx] += cnt;
    return true;
  } else
    llvm_unreachable("what type was this?");

  return false;
}

// Walk over an LLVM Type that we know is a homogeneous aggregate and
// push the proper LLVM Types that represent the register types to pass
// that struct member in.
static void push_elts(Type *Ty, std::vector<Type *> &Elts) {
  for (Type::subtype_iterator I = Ty->subtype_begin(), E = Ty->subtype_end();
       I != E; ++I) {
    Type *STy = *I;
    if (const VectorType *VTy = dyn_cast<VectorType>(STy)) {
      switch (VTy->getBitWidth()) {
      case 64: // v2f32
        Elts.push_back(VectorType::get(Type::getFloatTy(Context), 2));
        break;
      case 128: // v2f64
        Elts.push_back(VectorType::get(Type::getDoubleTy(Context), 2));
        break;
      default:
        assert(0 && "invalid vector type");
      }
    } else if (ArrayType *ATy = dyn_cast<ArrayType>(STy)) {
      Type *ETy = ATy->getElementType();

      for (uint64_t i = ATy->getNumElements(); i > 0; --i)
        Elts.push_back(ETy);
    } else if (STy->getNumContainedTypes())
      push_elts(STy, Elts);
    else
      Elts.push_back(STy);
  }
}

static unsigned count_num_words(std::vector<Type *> &ScalarElts) {
  unsigned NumWords = 0;
  for (unsigned i = 0, e = ScalarElts.size(); i != e; ++i) {
    Type *Ty = ScalarElts[i];
    if (Ty->isPointerTy()) {
      NumWords++;
    } else if (Ty->isIntegerTy()) {
      const unsigned TypeSize = Ty->getPrimitiveSizeInBits();
      const unsigned NumWordsForType = (TypeSize + 31) / 32;

      NumWords += NumWordsForType;
    } else {
      assert(0 && "Unexpected type.");
    }
  }
  return NumWords;
}

// This function is used only on AAPCS. The difference from the generic
// handling of arguments is that arguments larger than 32 bits are split
// and padding arguments are added as necessary for alignment. This makes
// the IL a bit more explicit about how arguments are handled.
extern bool llvm_arm_try_pass_aggregate_custom(
    tree type, std::vector<Type *> &ScalarElts, CallingConv::ID CC,
    struct DefaultABIClient *C) {
  if (CC != CallingConv::ARM_AAPCS && CC != CallingConv::C)
    return false;

  if (CC == CallingConv::C && !TARGET_AAPCS_BASED)
    return false;

  if (TARGET_HARD_FLOAT_ABI)
    return false;
  Type *Ty = ConvertType(type);
  if (Ty->isPointerTy())
    return false;

  const unsigned Size = TREE_INT_CST_LOW(TYPE_SIZE(type)) / 8;
  const unsigned Alignment = TYPE_ALIGN(type) / 8;
  const unsigned NumWords = count_num_words(ScalarElts);
  const bool AddPad = Alignment >= 8 && (NumWords % 2);

  // First, build a type that will be bitcast to the original one and
  // from where elements will be extracted.
  std::vector<Type *> Elts;
  Type *Int32Ty = Type::getInt32Ty(getGlobalContext());
  const unsigned NumRegularArgs = Size / 4;
  for (unsigned i = 0; i < NumRegularArgs; ++i) {
    Elts.push_back(Int32Ty);
  }
  const unsigned RestSize = Size % 4;
  llvm::Type *RestType = NULL;
  if (RestSize > 2) {
    RestType = Type::getInt32Ty(getGlobalContext());
  } else if (RestSize > 1) {
    RestType = Type::getInt16Ty(getGlobalContext());
  } else if (RestSize > 0) {
    RestType = Type::getInt8Ty(getGlobalContext());
  }
  if (RestType)
    Elts.push_back(RestType);
  StructType *STy = StructType::get(getGlobalContext(), Elts, false);

  if (AddPad) {
    ScalarElts.push_back(Int32Ty);
    C->HandlePad(Int32Ty);
  }

  for (unsigned i = 0; i < NumRegularArgs; ++i) {
    C->EnterField(i, STy);
    C->HandleScalarArgument(Int32Ty, 0);
    ScalarElts.push_back(Int32Ty);
    C->ExitField();
  }
  if (RestType) {
    C->EnterField(NumRegularArgs, STy);
    C->HandleScalarArgument(RestType, 0, RestSize);
    ScalarElts.push_back(RestType);
    C->ExitField();
  }
  return true;
}

// Target hook for llvm-abi.h. It returns true if an aggregate of the
// specified type should be passed in a number of registers of mixed types.
// It also returns a vector of types that correspond to the registers used
// for parameter passing. This only applies to AAPCS-VFP "homogeneous
// aggregates" as specified in 4.3.5 of the AAPCS spec.
bool llvm_arm_should_pass_aggregate_in_mixed_regs(
    tree TreeType, Type *Ty, CallingConv::ID CC, std::vector<Type *> &Elts) {
  if (!llvm_arm_should_pass_or_return_aggregate_in_regs(TreeType, CC))
    return false;

  // Walk Ty and push LLVM types corresponding to register types onto
  // Elts.
  push_elts(Ty, Elts);

  return true;
}

static bool alloc_next_spr(bool *SPRs) {
  for (int i = 0; i < 16; ++i)
    if (!SPRs[i]) {
      SPRs[i] = true;
      return true;
    }
  return false;
}

static bool alloc_next_dpr(bool *SPRs) {
  for (int i = 0; i < 16; i += 2)
    if (!SPRs[i]) {
      SPRs[i] = SPRs[i + 1] = true;
      return true;
    }
  return false;
}

static bool alloc_next_qpr(bool *SPRs) {
  for (int i = 0; i < 16; i += 4)
    if (!SPRs[i]) {
      SPRs[i] = SPRs[i + 1] = SPRs[i + 2] = SPRs[i + 3] = true;
      return true;
    }
  return false;
}

// count_num_registers_uses - Simulate argument passing reg allocation in SPRs.
// Caller is expected to zero out SPRs.  Returns true if all of ScalarElts fit
// in registers.
static bool count_num_registers_uses(std::vector<Type *> &ScalarElts,
                                     bool *SPRs) {
  for (unsigned i = 0, e = ScalarElts.size(); i != e; ++i) {
    Type *Ty = ScalarElts[i];
    if (const VectorType *VTy = dyn_cast<VectorType>(Ty)) {
      switch (VTy->getBitWidth()) {
      case 64:
        if (!alloc_next_dpr(SPRs))
          return false;
        break;
      case 128:
        if (!alloc_next_qpr(SPRs))
          return false;
        break;
      default:
        assert(0);
      }
    } else if (Ty->isIntegerTy() || Ty->isPointerTy() ||
               Ty == Type::getVoidTy(Context)) {
      ;
    } else {
      // Floating point scalar argument.
      assert(Ty->isFloatingPointTy() && Ty->isPrimitiveType() &&
             "Expecting a floating point primitive type!");
      switch (Ty->getTypeID()) {
      case Type::FloatTyID:
        if (!alloc_next_spr(SPRs))
          return false;
        break;
      case Type::DoubleTyID:
        if (!alloc_next_spr(SPRs))
          return false;
        break;
      default:
        assert(0);
      }
    }
  }
  return true;
}

// Target hook for llvm-abi.h. This is called when an aggregate is being passed
// in registers. If there are only enough available parameter registers to pass
// part of the aggregate, return true. That means the aggregate should instead
// be passed in memory.
bool llvm_arm_aggregate_partially_passed_in_regs(
    std::vector<Type *> &Elts, std::vector<Type *> &ScalarElts,
    CallingConv::ID CC) {
  // Homogeneous aggregates are an AAPCS-VFP feature.
  if ((CC != CallingConv::ARM_AAPCS_VFP) ||
      !(TARGET_AAPCS_BASED && TARGET_VFP && TARGET_HARD_FLOAT_ABI))
    return true;

  bool SPRs[16] = { 0 }; // represents S0-S16

  // Figure out which SPRs are available.
  if (!count_num_registers_uses(ScalarElts, SPRs))
    return true;

  if (!count_num_registers_uses(Elts, SPRs))
    return true;

  return false; // it all fit in registers!
}

// Return LLVM Type if TYPE can be returned as an aggregate,
// otherwise return NULL.
Type *llvm_arm_aggr_type_for_struct_return(tree TreeType, CallingConv::ID CC) {
  if (!llvm_arm_should_pass_or_return_aggregate_in_regs(TreeType, CC))
    return NULL;

  // Walk Ty and push LLVM types corresponding to register types onto
  // Elts.
  std::vector<Type *> Elts;
  Type *Ty = ConvertType(TreeType);
  push_elts(Ty, Elts);

  return StructType::get(Context, Elts, false);
}

// llvm_arm_extract_mrv_array_element - Helper function that helps extract
// an array element from multiple return value.
//
// Here, SRC is returning multiple values. DEST's DESTFIELDNO field is an array.
// Extract SRCFIELDNO's ELEMENO value and store it in DEST's FIELDNO field's
// ELEMENTNO.
//
static void llvm_arm_extract_mrv_array_element(
    Value *Src, Value *Dest, unsigned SrcFieldNo, unsigned SrcElemNo,
    unsigned DestFieldNo, unsigned DestElemNo, LLVMBuilder &Builder,
    bool isVolatile) {
  Value *EVI = Builder.CreateExtractValue(Src, SrcFieldNo, "mrv_gr");
  const StructType *STy = cast<StructType>(Src->getType());
  llvm::Value *Idxs[3];
  Idxs[0] = ConstantInt::get(llvm::Type::getInt32Ty(Context), 0);
  Idxs[1] = ConstantInt::get(llvm::Type::getInt32Ty(Context), DestFieldNo);
  Idxs[2] = ConstantInt::get(llvm::Type::getInt32Ty(Context), DestElemNo);
  Value *GEP = Builder.CreateGEP(Dest, Idxs, "mrv_gep");
  if (STy->getElementType(SrcFieldNo)->isVectorTy()) {
    Value *ElemIndex = ConstantInt::get(Type::getInt32Ty(Context), SrcElemNo);
    Value *EVIElem = Builder.CreateExtractElement(EVI, ElemIndex, "mrv");
    Builder.CreateAlignedStore(EVIElem, GEP, 1, isVolatile);
  } else {
    Builder.CreateAlignedStore(EVI, GEP, 1, isVolatile);
  }
}

// llvm_arm_extract_multiple_return_value - Extract multiple values returned
// by SRC and store them in DEST. It is expected that SRC and
// DEST types are StructType, but they may not match.
void llvm_arm_extract_multiple_return_value(
    Value *Src, Value *Dest, bool isVolatile, LLVMBuilder &Builder) {
  const StructType *STy = cast<StructType>(Src->getType());
  unsigned NumElements = STy->getNumElements();

  const PointerType *PTy = cast<PointerType>(Dest->getType());
  const StructType *DestTy = cast<StructType>(PTy->getElementType());

  unsigned SNO = 0;
  unsigned DNO = 0;

  while (SNO < NumElements) {

    Type *DestElemType = DestTy->getElementType(DNO);

    // Directly access first class values.
    if (DestElemType->isSingleValueType()) {
      Value *GEP = Builder.CreateStructGEP(Dest, DNO, "mrv_gep");
      Value *EVI = Builder.CreateExtractValue(Src, SNO, "mrv_gr");
      Builder.CreateAlignedStore(EVI, GEP, 1, isVolatile);
      ++DNO;
      ++SNO;
      continue;
    }

    // Access array elements individually. Note, Src and Dest type may
    // not match. For example { <2 x float>, float } and { float[3]; }
    const ArrayType *ATy = cast<ArrayType>(DestElemType);
    unsigned ArraySize = ATy->getNumElements();
    unsigned DElemNo = 0; // DestTy's DNO field's element number
    while (DElemNo < ArraySize) {
      unsigned i = 0;
      unsigned Size = 1;

      if (const VectorType *SElemTy =
              dyn_cast<VectorType>(STy->getElementType(SNO))) {
        Size = SElemTy->getNumElements();
      }
      while (i < Size) {
        llvm_arm_extract_mrv_array_element(Src, Dest, SNO, i++, DNO, DElemNo++,
                                           Builder, isVolatile);
      }
      // Consumed this src field. Try next one.
      ++SNO;
    }
    // Finished building current dest field.
    ++DNO;
  }
}

// Target hook for llvm-abi.h for LLVM_SHOULD_NOT_USE_SHADOW_RETURN and is
// also a utility function used for other target hooks in this file. Returns
// true if the aggregate should be passed or returned in registers.
bool llvm_arm_should_pass_or_return_aggregate_in_regs(tree TreeType,
                                                      CallingConv::ID CC) {
  // Homogeneous aggregates are an AAPCS-VFP feature.
  if ((CC != CallingConv::ARM_AAPCS_VFP) ||
      !(TARGET_AAPCS_BASED && TARGET_VFP && TARGET_HARD_FLOAT_ABI))
    return false;

  // Alas, we can't use LLVM Types to figure this out because we need to
  // examine unions closely.  We'll have to walk the GCC TreeType.
  int fdt_counts[ARM_FDT_MAX] = { 0 };
  bool result = false;
  result = vfp_arg_homogeneous_aggregate_p(TYPE_MODE(TreeType), TreeType,
                                           fdt_counts);
  return result && !TREE_ADDRESSABLE(TreeType);
}
