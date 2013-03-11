//==----- Target.h - Target hooks for GCC to LLVM conversion -----*- C++ -*-==//
//
// Copyright (C) 2007 to 2013  Jin Gu Kang, Anton Korobeynikov, Duncan Sands
// et al.
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
// This file declares some target-specific hooks for GCC to LLVM conversion.
// It was derived from llvm-arm-target.h and arm.h on llvm-gcc.4.2.
//===----------------------------------------------------------------------===//

#ifndef DRAGONEGG_TARGET_H
#define DRAGONEGG_TARGET_H

/* LLVM specific code to select the calling conventions. The AAPCS
   specification says that varargs functions must use the base standard
   instead of the VFP hard float variant. We check for that with
   (isVoid || hasArgList). */

/* from  TARGET_AAPCS_BASED */
#define DEFAULT_TARGET_AAPCS_BASED(ARM_DEFAULT_ABI != ARM_ABI_APCS &&          \
                                   ARM_DEFAULT_ABI != ARM_ABI_ATPCS)

#define TARGET_ADJUST_LLVM_CC(CC, type)                                        \
  {                                                                            \
    if (TARGET_AAPCS_BASED) {                                                  \
      if (TARGET_VFP && TARGET_HARD_FLOAT_ABI &&                               \
          ((TYPE_ARG_TYPES(type) == 0) ||                                      \
           (TREE_VALUE(tree_last(TYPE_ARG_TYPES(type))) == void_type_node)))   \
        CC = CallingConv::ARM_AAPCS_VFP;                                       \
      else if (!DEFAULT_TARGET_AAPCS_BASED)                                    \
        CC = CallingConv::ARM_AAPCS;                                           \
    } else if (DEFAULT_TARGET_AAPCS_BASED) {                                   \
      CC = CallingConv::ARM_APCS;                                              \
    }                                                                          \
  }

#ifdef DRAGONEGG_ABI_H

extern bool llvm_arm_should_pass_aggregate_in_mixed_regs(
    tree_node *, Type *Ty, CallingConv::ID, std::vector<Type *> &);

#define LLVM_SHOULD_PASS_AGGREGATE_IN_MIXED_REGS(T, TY, CC, E)                 \
  llvm_arm_should_pass_aggregate_in_mixed_regs((T), (TY), (CC), (E))

struct DefaultABIClient;
extern bool llvm_arm_try_pass_aggregate_custom(
    tree_node *, std::vector<Type *> &, CallingConv::ID CC,
    struct DefaultABIClient *);

#define LLVM_TRY_PASS_AGGREGATE_CUSTOM(T, E, CC, C)                            \
  llvm_arm_try_pass_aggregate_custom((T), (E), (CC), (C))

extern bool llvm_arm_aggregate_partially_passed_in_regs(
    std::vector<Type *> &, std::vector<Type *> &, CallingConv::ID CC);

#define LLVM_AGGREGATE_PARTIALLY_PASSED_IN_REGS(E, SE, ISR, CC)                \
  llvm_arm_aggregate_partially_passed_in_regs((E), (SE), (CC))

extern Type *
llvm_arm_aggr_type_for_struct_return(tree_node *type, CallingConv::ID CC);

/* LLVM_AGGR_TYPE_FOR_STRUCT_RETURN - Return LLVM Type if X can be
  returned as an aggregate, otherwise return NULL. */
#define LLVM_AGGR_TYPE_FOR_STRUCT_RETURN(X, CC)                                \
  llvm_arm_aggr_type_for_struct_return((X), (CC))

extern void llvm_arm_extract_multiple_return_value(
    Value *Src, Value *Dest, bool isVolatile, LLVMBuilder &B);

/* LLVM_EXTRACT_MULTIPLE_RETURN_VALUE - Extract multiple return value from
  SRC and assign it to DEST. */
#define LLVM_EXTRACT_MULTIPLE_RETURN_VALUE(Src, Dest, V, B)                    \
  llvm_arm_extract_multiple_return_value((Src), (Dest), (V), (B))

extern bool llvm_arm_should_pass_or_return_aggregate_in_regs(
    tree_node *TreeType, CallingConv::ID CC);

/* LLVM_SHOULD_NOT_USE_SHADOW_RETURN = Return true is the given type should
  not be returned via a shadow parameter with the given calling conventions. */
#define LLVM_SHOULD_NOT_USE_SHADOW_RETURN(X, CC)                               \
  llvm_arm_should_pass_or_return_aggregate_in_regs((X), (CC))

/* Vectors bigger than 128 are returned using sret.  */
#define LLVM_SHOULD_RETURN_VECTOR_AS_SHADOW(X, isBuiltin)                      \
  (TREE_INT_CST_LOW(TYPE_SIZE(X)) > 128)

#endif /* DRAGONEGG_ABI_H */

#define LLVM_TARGET_INTRINSIC_PREFIX "arm"

/* LLVM_TARGET_NAME - This specifies the name of the target, which correlates to
 * the llvm::InitializeXXXTarget() function.
 */
#define LLVM_TARGET_NAME ARM

/* Turn -march=xx into a CPU type.
 */
#define LLVM_SET_SUBTARGET_FEATURES(C, F)                                      \
  {                                                                            \
    switch (arm_tune) {                                                        \
    case arm8:                                                                 \
      C = ("arm8");                                                            \
      break;                                                                   \
    case arm810:                                                               \
      C = ("arm810");                                                          \
      break;                                                                   \
    case strongarm:                                                            \
      C = ("strongarm");                                                       \
      break;                                                                   \
    case strongarm110:                                                         \
      C = ("strongarm110");                                                    \
      break;                                                                   \
    case strongarm1100:                                                        \
      C = ("strongarm1100");                                                   \
      break;                                                                   \
    case strongarm1110:                                                        \
      C = ("strongarm1110");                                                   \
      break;                                                                   \
    case arm7tdmi:                                                             \
      C = ("arm7tdmi");                                                        \
      break;                                                                   \
    case arm7tdmis:                                                            \
      C = ("arm7tdmi-s");                                                      \
      break;                                                                   \
    case arm710t:                                                              \
      C = ("arm710t");                                                         \
      break;                                                                   \
    case arm720t:                                                              \
      C = ("arm720t");                                                         \
      break;                                                                   \
    case arm740t:                                                              \
      C = ("arm740t");                                                         \
      break;                                                                   \
    case arm9:                                                                 \
      C = ("arm9");                                                            \
      break;                                                                   \
    case arm9tdmi:                                                             \
      C = ("arm9tdmi");                                                        \
      break;                                                                   \
    case arm920:                                                               \
      C = ("arm920");                                                          \
      break;                                                                   \
    case arm920t:                                                              \
      C = ("arm920t");                                                         \
      break;                                                                   \
    case arm922t:                                                              \
      C = ("arm922t");                                                         \
      break;                                                                   \
    case arm940t:                                                              \
      C = ("arm940t");                                                         \
      break;                                                                   \
    case ep9312:                                                               \
      C = ("ep9312");                                                          \
      break;                                                                   \
    case arm10tdmi:                                                            \
      C = ("arm10tdmi");                                                       \
      break;                                                                   \
    case arm1020t:                                                             \
      C = ("arm1020t");                                                        \
      break;                                                                   \
    case arm9e:                                                                \
      C = ("arm9e");                                                           \
      break;                                                                   \
    case arm946es:                                                             \
      C = ("arm946e-s");                                                       \
      break;                                                                   \
    case arm966es:                                                             \
      C = ("arm966e-s");                                                       \
      break;                                                                   \
    case arm968es:                                                             \
      C = ("arm968e-s");                                                       \
      break;                                                                   \
    case arm10e:                                                               \
      C = ("arm10e");                                                          \
      break;                                                                   \
    case arm1020e:                                                             \
      C = ("arm1020e");                                                        \
      break;                                                                   \
    case arm1022e:                                                             \
      C = ("arm1022e");                                                        \
      break;                                                                   \
    case xscale:                                                               \
      C = ("xscale");                                                          \
      break;                                                                   \
    case iwmmxt:                                                               \
      C = ("iwmmxt");                                                          \
      break;                                                                   \
    case arm926ejs:                                                            \
      C = ("arm926ej-s");                                                      \
      break;                                                                   \
    case arm1026ejs:                                                           \
      C = ("arm1026ej-s");                                                     \
      break;                                                                   \
    case arm1136js:                                                            \
      C = ("arm1136j-s");                                                      \
      break;                                                                   \
    case arm1136jfs:                                                           \
      C = ("arm1136jf-s");                                                     \
      break;                                                                   \
    case arm1176jzs:                                                           \
      C = ("arm1176jz-s");                                                     \
      break;                                                                   \
    case arm1176jzfs:                                                          \
      C = ("arm1176jzf-s");                                                    \
      break;                                                                   \
    case mpcorenovfp:                                                          \
      C = ("mpcorenovfp");                                                     \
      break;                                                                   \
    case mpcore:                                                               \
      C = ("mpcore");                                                          \
      break;                                                                   \
    case arm1156t2s:                                                           \
      C = ("arm1156t2-s");                                                     \
      break;                                                                   \
    case arm1156t2fs:                                                          \
      C = ("arm1156t2f-s");                                                    \
      break;                                                                   \
    case cortexa8:                                                             \
      C = ("cortex-a8");                                                       \
      break;                                                                   \
    case cortexa9:                                                             \
      C = ("cortex-a9");                                                       \
      break;                                                                   \
    case cortexr4:                                                             \
      C = ("cortex-r4");                                                       \
      break;                                                                   \
    case cortexm3:                                                             \
      C = ("cortex-m3");                                                       \
      break;                                                                   \
    case cortexm4:                                                             \
      C = ("cortex-m4");                                                       \
      break;                                                                   \
    case cortexm0:                                                             \
      C = ("cortex-m0");                                                       \
      break;                                                                   \
    default:                                                                   \
      C = ("arm7tdmi");                                                        \
      break;                                                                   \
    }                                                                          \
    F.AddFeature("vfp3", TARGET_VFP3);                                         \
    if (!TARGET_VFP3)                                                          \
      F.AddFeature("vfp2", TARGET_VFP && TARGET_HARD_FLOAT);                   \
    F.AddFeature("neon", TARGET_NEON);                                         \
    F.AddFeature("fp16", TARGET_FP16);                                         \
  }

/* Encode arm / thumb modes and arm subversion number in the triplet. e.g.
 * armv6-apple-darwin, thumbv5-apple-darwin. FIXME: Replace thumb triplets
 * with function notes.
 */
#define LLVM_OVERRIDE_TARGET_ARCH()                                            \
  (TARGET_THUMB                                                                \
       ? (arm_arch7                                                            \
              ? "thumbv7"                                                      \
              : (arm_arch_thumb2                                               \
                     ? "thumbv6t2"                                             \
                     : (arm_tune == cortexm0                                   \
                            ? "thumbv6m"                                       \
                            : (arm_arch6                                       \
                                   ? "thumbv6"                                 \
                                   : (arm_arch5e                               \
                                          ? "thumbv5e"                         \
                                          : (arm_arch5                         \
                                                 ? "thumbv5"                   \
                                                 : (arm_arch4t ? "thumbv4t"    \
                                                               : "")))))))     \
       : (arm_arch7                                                            \
              ? "armv7"                                                        \
              : (arm_arch_thumb2                                               \
                     ? "armv6t2"                                               \
                     : (arm_arch6                                              \
                            ? "armv6"                                          \
                            : (arm_arch5e                                      \
                                   ? "armv5e"                                  \
                                   : (arm_arch5                                \
                                          ? "armv5"                            \
                                          : (arm_arch4t                        \
                                                 ? "armv4t"                    \
                                                 : (arm_arch4 ? "armv4"        \
                                                              : ""))))))))

#if 0
// Dragonegg should make flag_mkernel and flag_apple_kext option later on.
// We didn't decide place to make these flags.
#define LLVM_SET_MACHINE_OPTIONS(argvec)                                       \
  if (flag_mkernel || flag_apple_kext) {                                       \
    argvec.push_back("-arm-long-calls");                                       \
    argvec.push_back("-arm-strict-align");                                     \
  }
#endif

#define LLVM_SET_TARGET_MACHINE_OPTIONS(options)                               \
  do {                                                                         \
    options.UseSoftFloat = TARGET_SOFT_FLOAT;                                  \
    if (TARGET_HARD_FLOAT_ABI)                                                 \
      options.FloatABIType = llvm::FloatABI::Hard;                             \
  } while (0)

/* These are a couple of extensions to the asm formats
     %@ prints out ASM_COMMENT_START
     TODO: %r prints out REGISTER_PREFIX reg_names[arg]  */
#define LLVM_ASM_EXTENSIONS(ESCAPED_CHAR, ASM, RESULT)                         \
  else if ((ESCAPED_CHAR) == '@') {                                            \
    (RESULT) += ASM_COMMENT_START;                                             \
  }

/* LLVM_TARGET_INTRINSIC_LOWER - To handle builtins, we want to expand the
   invocation into normal LLVM code.  If the target can handle the builtin, this
   macro should call the target TreeToLLVM::TargetIntrinsicLower method and
   return true.  This macro is invoked from a method in the TreeToLLVM class. */
#if 0
// Because of data dependency, we will implement later on.
#define LLVM_TARGET_INTRINSIC_LOWER(EXP, BUILTIN_CODE, DESTLOC, RESULT,        \
                                    DESTTY, OPS)                               \
  TargetIntrinsicLower(EXP, BUILTIN_CODE, DESTLOC, RESULT, DESTTY, OPS);
#endif

/* LLVM_GET_REG_NAME - The registers known to llvm as "r10", "r11", and "r12"
   may have different names in GCC.  Register "r12" is called "ip", and on
   non-Darwin OSs, "r10" is "sl" and "r11" is "fp".  Translate those names.
   For VFP registers, GCC doesn't distinguish between the q and d registers
   so use the incoming register name if it exists.  Otherwise, use the default
   register names to match the backend.  */
#define LLVM_GET_REG_NAME(REG_NAME, REG_NUM)                                   \
  ((REG_NUM) == 10 ? "r10" : (REG_NUM) == 11                                   \
                                 ? "r11"                                       \
                                 : (REG_NUM) == 12                             \
                                       ? "r12"                                 \
                                       : (REG_NUM) >= FIRST_VFP_REGNUM &&      \
                                         REG_NAME != 0                         \
                                             ? REG_NAME                        \
                                             : reg_names[REG_NUM])

/* Define a static enumeration of the NEON builtins to be used when
   converting to LLVM intrinsics.  These names are derived from the
   neon_builtin_data table in arm.c and should be kept in sync with that.  */

#endif /* DRAGONEGG_TARGET_H */
