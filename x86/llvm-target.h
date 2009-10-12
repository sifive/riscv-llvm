/* Some target-specific hooks for gcc->llvm conversion
Copyright (C) 2007 Free Software Foundation, Inc.
Contributed by Anton Korobeynikov (asl@math.spbu.ru)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef LLVM_TARGET_H
#define LLVM_TARGET_H

/* LLVM specific stuff for supporting calling convention output */
#define TARGET_ADJUST_LLVM_CC(CC, type)                         \
  {                                                             \
    tree type_attributes = TYPE_ATTRIBUTES (type);              \
    if (lookup_attribute ("stdcall", type_attributes)) {        \
      CC = CallingConv::X86_StdCall;                            \
    } else if (lookup_attribute("fastcall", type_attributes)) { \
      CC = CallingConv::X86_FastCall;                           \
    }                                                           \
  }

#define TARGET_ADJUST_LLVM_RETATTR(Rattributes, type)           \
  {                                                             \
    tree type_attributes = TYPE_ATTRIBUTES (type);              \
    if (!TARGET_64BIT && (TARGET_SSEREGPARM ||                  \
               lookup_attribute("sseregparm", type_attributes)))\
      RAttributes |= Attribute::InReg;                          \
  }

/* LLVM specific stuff for converting gcc's `regparm` attribute to LLVM's
   `inreg` parameter attribute */
#define LLVM_TARGET_ENABLE_REGPARM

extern "C" int ix86_regparm;

#define LLVM_TARGET_INIT_REGPARM(local_regparm, local_fp_regparm, type) \
  {                                                             \
    tree attr;                                                  \
    local_regparm = ix86_regparm;                               \
    local_fp_regparm = TARGET_SSEREGPARM ? 3 : 0;               \
    attr = lookup_attribute ("regparm",                         \
                              TYPE_ATTRIBUTES (type));          \
    if (attr) {                                                 \
      local_regparm = TREE_INT_CST_LOW (TREE_VALUE              \
                                        (TREE_VALUE (attr)));   \
    }                                                           \
    attr = lookup_attribute("sseregparm",                       \
                              TYPE_ATTRIBUTES (type));          \
    if (attr)                                                   \
      local_fp_regparm = 3;                                     \
  }

#define LLVM_ADJUST_REGPARM_ATTRIBUTE(PAttribute, Type, Size,   \
                                      local_regparm,            \
                                      local_fp_regparm)         \
  {                                                             \
    if (!TARGET_64BIT) {                                        \
      if (TREE_CODE(Type) == REAL_TYPE &&                       \
          (TYPE_PRECISION(Type)==32 ||                          \
           TYPE_PRECISION(Type)==64)) {                         \
          local_fp_regparm -= 1;                                \
          if (local_fp_regparm >= 0)                            \
            PAttribute |= Attribute::InReg;                     \
          else                                                  \
            local_fp_regparm = 0;                               \
      } else if (INTEGRAL_TYPE_P(Type) ||                       \
                 POINTER_TYPE_P(Type)) {                        \
          int words =                                           \
                  (Size + BITS_PER_WORD - 1) / BITS_PER_WORD;   \
          local_regparm -= words;                               \
          if (local_regparm>=0)                                 \
            PAttribute |= Attribute::InReg;                     \
          else                                                  \
            local_regparm = 0;                                  \
      }                                                         \
    }                                                           \
  }

#define LLVM_SET_RED_ZONE_FLAG(disable_red_zone)                \
  if (TARGET_64BIT && TARGET_NO_RED_ZONE)                       \
    disable_red_zone = 1;

#ifdef LLVM_ABI_H

/* On x86-32 objects containing SSE vectors are 16 byte aligned, everything
   else 4.  On x86-64 vectors are 8-byte aligned, everything else can
   be figured out by the back end. */
extern "C" bool contains_aligned_value_p(tree);
#define LLVM_BYVAL_ALIGNMENT(T) \
  (TARGET_64BIT ? (TREE_CODE(T)==VECTOR_TYPE ? 8 : 0) : \
   TARGET_SSE && contains_aligned_value_p(T) ? 16 : 4)

extern tree llvm_x86_should_return_selt_struct_as_scalar(tree);

/* Structs containing a single data field plus zero-length fields are
   considered as if they were the type of the data field.  On x86-64,
   if the element type is an MMX vector, return it as double (which will
   get it into XMM0). */

#define LLVM_SHOULD_RETURN_SELT_STRUCT_AS_SCALAR(X) \
  llvm_x86_should_return_selt_struct_as_scalar((X))

extern bool llvm_x86_should_pass_aggregate_in_integer_regs(tree, 
                                                          unsigned*, bool*);

/* LLVM_SHOULD_PASS_AGGREGATE_IN_INTEGER_REGS - Return true if this aggregate
   value should be passed in integer registers.  This differs from the usual
   handling in that x86-64 passes 128-bit structs and unions which only
   contain data in the first 64 bits, as 64-bit objects.  (These can be
   created by abusing __attribute__((aligned)).  */
#define LLVM_SHOULD_PASS_AGGREGATE_IN_INTEGER_REGS(X, Y, Z)             \
  llvm_x86_should_pass_aggregate_in_integer_regs((X), (Y), (Z))

extern const Type *llvm_x86_scalar_type_for_struct_return(tree type, 
                                                          unsigned *Offset);

/* LLVM_SCALAR_TYPE_FOR_STRUCT_RETURN - Return LLVM Type if X can be 
   returned as a scalar, otherwise return NULL. */
#define LLVM_SCALAR_TYPE_FOR_STRUCT_RETURN(X, Y) \
  llvm_x86_scalar_type_for_struct_return((X), (Y))

extern const Type *llvm_x86_aggr_type_for_struct_return(tree type);

/* LLVM_AGGR_TYPE_FOR_STRUCT_RETURN - Return LLVM Type if X can be 
   returned as an aggregate, otherwise return NULL. */
#define LLVM_AGGR_TYPE_FOR_STRUCT_RETURN(X) \
  llvm_x86_aggr_type_for_struct_return(X)

extern void llvm_x86_extract_multiple_return_value(Value *Src, Value *Dest,
                                                   bool isVolatile,
                                                   LLVMBuilder &B);

/* LLVM_EXTRACT_MULTIPLE_RETURN_VALUE - Extract multiple return value from
   SRC and assign it to DEST. */
#define LLVM_EXTRACT_MULTIPLE_RETURN_VALUE(Src,Dest,V,B)       \
  llvm_x86_extract_multiple_return_value((Src),(Dest),(V),(B))

extern bool llvm_x86_should_pass_vector_using_byval_attr(tree);

/* On x86-64, vectors which are not MMX nor SSE should be passed byval. */
#define LLVM_SHOULD_PASS_VECTOR_USING_BYVAL_ATTR(X)      \
  llvm_x86_should_pass_vector_using_byval_attr((X))

extern bool llvm_x86_should_pass_vector_in_integer_regs(tree);

/* On x86-32, vectors which are not MMX nor SSE should be passed as integers. */
#define LLVM_SHOULD_PASS_VECTOR_IN_INTEGER_REGS(X)      \
  llvm_x86_should_pass_vector_in_integer_regs((X))

extern tree llvm_x86_should_return_vector_as_scalar(tree, bool);

/* The MMX vector v1i64 is returned in EAX and EDX on Darwin.  Communicate
    this by returning i64 here.  Likewise, (generic) vectors such as v2i16
    are returned in EAX.  
    On Darwin x86-64, MMX vectors are returned in XMM0.  Communicate this by
    returning f64.  */
#define LLVM_SHOULD_RETURN_VECTOR_AS_SCALAR(X,isBuiltin)\
  llvm_x86_should_return_vector_as_scalar((X), (isBuiltin))

extern bool llvm_x86_should_return_vector_as_shadow(tree, bool);

/* MMX vectors v2i32, v4i16, v8i8, v2f32 are returned using sret on Darwin
   32-bit.  Vectors bigger than 128 are returned using sret.  */
#define LLVM_SHOULD_RETURN_VECTOR_AS_SHADOW(X,isBuiltin)\
  llvm_x86_should_return_vector_as_shadow((X),(isBuiltin))

extern bool
llvm_x86_should_not_return_complex_in_memory(tree type);

/* LLVM_SHOULD_NOT_RETURN_COMPLEX_IN_MEMORY - A hook to allow
   special _Complex handling. Return true if X should be returned using
   multiple value return instruction.  */
#define LLVM_SHOULD_NOT_RETURN_COMPLEX_IN_MEMORY(X) \
  llvm_x86_should_not_return_complex_in_memory((X))

extern bool
llvm_x86_should_pass_aggregate_as_fca(tree type, const Type *);

/* LLVM_SHOULD_PASS_AGGREGATE_AS_FCA - Return true if an aggregate of the
   specified type should be passed as a first-class aggregate. */
#ifndef LLVM_SHOULD_PASS_AGGREGATE_AS_FCA
#define LLVM_SHOULD_PASS_AGGREGATE_AS_FCA(X, TY) \
  llvm_x86_should_pass_aggregate_as_fca(X, TY)
#endif

extern bool llvm_x86_should_pass_aggregate_in_memory(tree, const Type *);

#define LLVM_SHOULD_PASS_AGGREGATE_USING_BYVAL_ATTR(X, TY)      \
  llvm_x86_should_pass_aggregate_in_memory(X, TY)


extern bool
llvm_x86_64_should_pass_aggregate_in_mixed_regs(tree, const Type *Ty,
                                                std::vector<const Type*>&);
extern bool
llvm_x86_32_should_pass_aggregate_in_mixed_regs(tree, const Type *Ty,
                                                std::vector<const Type*>&);

#define LLVM_SHOULD_PASS_AGGREGATE_IN_MIXED_REGS(T, TY, CC, E)       \
  (TARGET_64BIT ?                                                    \
   llvm_x86_64_should_pass_aggregate_in_mixed_regs((T), (TY), (E)) : \
   llvm_x86_32_should_pass_aggregate_in_mixed_regs((T), (TY), (E)))

extern
bool llvm_x86_64_aggregate_partially_passed_in_regs(std::vector<const Type*>&,
                                                    std::vector<const Type*>&,
                                                    bool);

#define LLVM_AGGREGATE_PARTIALLY_PASSED_IN_REGS(E, SE, ISR, CC)       \
  (TARGET_64BIT ?                                                     \
   llvm_x86_64_aggregate_partially_passed_in_regs((E), (SE), (ISR)) : \
   false)

#endif /* LLVM_ABI_H */

/* Register class used for passing given 64bit part of the argument.
   These represent classes as documented by the PS ABI, with the exception
   of SSESF, SSEDF classes, that are basically SSE class, just gcc will
   use SF or DFmode move instead of DImode to avoid reformatting penalties.

   Similarly we play games with INTEGERSI_CLASS to use cheaper SImode moves
   whenever possible (upper half does contain padding).
 */
enum x86_64_reg_class
  {
    X86_64_NO_CLASS,
    X86_64_INTEGER_CLASS,
    X86_64_INTEGERSI_CLASS,
    X86_64_SSE_CLASS,
    X86_64_SSESF_CLASS,
    X86_64_SSEDF_CLASS,
    X86_64_SSEUP_CLASS,
    X86_64_X87_CLASS,
    X86_64_X87UP_CLASS,
    X86_64_COMPLEX_X87_CLASS,
    X86_64_MEMORY_CLASS
  };

/* Codes for all the SSE/MMX builtins.  */
enum ix86_builtins
{
  IX86_BUILTIN_ADDPS,
  IX86_BUILTIN_ADDSS,
  IX86_BUILTIN_DIVPS,
  IX86_BUILTIN_DIVSS,
  IX86_BUILTIN_MULPS,
  IX86_BUILTIN_MULSS,
  IX86_BUILTIN_SUBPS,
  IX86_BUILTIN_SUBSS,

  IX86_BUILTIN_CMPEQPS,
  IX86_BUILTIN_CMPLTPS,
  IX86_BUILTIN_CMPLEPS,
  IX86_BUILTIN_CMPGTPS,
  IX86_BUILTIN_CMPGEPS,
  IX86_BUILTIN_CMPNEQPS,
  IX86_BUILTIN_CMPNLTPS,
  IX86_BUILTIN_CMPNLEPS,
  IX86_BUILTIN_CMPNGTPS,
  IX86_BUILTIN_CMPNGEPS,
  IX86_BUILTIN_CMPORDPS,
  IX86_BUILTIN_CMPUNORDPS,
  IX86_BUILTIN_CMPNEPS,
  IX86_BUILTIN_CMPEQSS,
  IX86_BUILTIN_CMPLTSS,
  IX86_BUILTIN_CMPLESS,
  IX86_BUILTIN_CMPNEQSS,
  IX86_BUILTIN_CMPNLTSS,
  IX86_BUILTIN_CMPNLESS,
  IX86_BUILTIN_CMPNGTSS,
  IX86_BUILTIN_CMPNGESS,
  IX86_BUILTIN_CMPORDSS,
  IX86_BUILTIN_CMPUNORDSS,
  IX86_BUILTIN_CMPNESS,

  IX86_BUILTIN_COMIEQSS,
  IX86_BUILTIN_COMILTSS,
  IX86_BUILTIN_COMILESS,
  IX86_BUILTIN_COMIGTSS,
  IX86_BUILTIN_COMIGESS,
  IX86_BUILTIN_COMINEQSS,
  IX86_BUILTIN_UCOMIEQSS,
  IX86_BUILTIN_UCOMILTSS,
  IX86_BUILTIN_UCOMILESS,
  IX86_BUILTIN_UCOMIGTSS,
  IX86_BUILTIN_UCOMIGESS,
  IX86_BUILTIN_UCOMINEQSS,

  IX86_BUILTIN_CVTPI2PS,
  IX86_BUILTIN_CVTPS2PI,
  IX86_BUILTIN_CVTSI2SS,
  IX86_BUILTIN_CVTSI642SS,
  IX86_BUILTIN_CVTSS2SI,
  IX86_BUILTIN_CVTSS2SI64,
  IX86_BUILTIN_CVTTPS2PI,
  IX86_BUILTIN_CVTTSS2SI,
  IX86_BUILTIN_CVTTSS2SI64,

  IX86_BUILTIN_MAXPS,
  IX86_BUILTIN_MAXSS,
  IX86_BUILTIN_MINPS,
  IX86_BUILTIN_MINSS,

  IX86_BUILTIN_LOADUPS,
  IX86_BUILTIN_STOREUPS,
  IX86_BUILTIN_MOVSS,

  IX86_BUILTIN_MOVHLPS,
  IX86_BUILTIN_MOVLHPS,
  IX86_BUILTIN_LOADHPS,
  IX86_BUILTIN_LOADLPS,
  IX86_BUILTIN_STOREHPS,
  IX86_BUILTIN_STORELPS,

  IX86_BUILTIN_MASKMOVQ,
  IX86_BUILTIN_MOVMSKPS,
  IX86_BUILTIN_PMOVMSKB,

  IX86_BUILTIN_MOVNTPS,
  IX86_BUILTIN_MOVNTQ,

  IX86_BUILTIN_LOADDQU,
  IX86_BUILTIN_STOREDQU,

  IX86_BUILTIN_PACKSSWB,
  IX86_BUILTIN_PACKSSDW,
  IX86_BUILTIN_PACKUSWB,

  IX86_BUILTIN_PADDB,
  IX86_BUILTIN_PADDW,
  IX86_BUILTIN_PADDD,
  IX86_BUILTIN_PADDQ,
  IX86_BUILTIN_PADDSB,
  IX86_BUILTIN_PADDSW,
  IX86_BUILTIN_PADDUSB,
  IX86_BUILTIN_PADDUSW,
  IX86_BUILTIN_PSUBB,
  IX86_BUILTIN_PSUBW,
  IX86_BUILTIN_PSUBD,
  IX86_BUILTIN_PSUBQ,
  IX86_BUILTIN_PSUBSB,
  IX86_BUILTIN_PSUBSW,
  IX86_BUILTIN_PSUBUSB,
  IX86_BUILTIN_PSUBUSW,

  IX86_BUILTIN_PAND,
  IX86_BUILTIN_PANDN,
  IX86_BUILTIN_POR,
  IX86_BUILTIN_PXOR,

  IX86_BUILTIN_PAVGB,
  IX86_BUILTIN_PAVGW,

  IX86_BUILTIN_PCMPEQB,
  IX86_BUILTIN_PCMPEQW,
  IX86_BUILTIN_PCMPEQD,
  IX86_BUILTIN_PCMPGTB,
  IX86_BUILTIN_PCMPGTW,
  IX86_BUILTIN_PCMPGTD,

  IX86_BUILTIN_PMADDWD,

  IX86_BUILTIN_PMAXSW,
  IX86_BUILTIN_PMAXUB,
  IX86_BUILTIN_PMINSW,
  IX86_BUILTIN_PMINUB,

  IX86_BUILTIN_PMULHUW,
  IX86_BUILTIN_PMULHW,
  IX86_BUILTIN_PMULLW,

  IX86_BUILTIN_PSADBW,
  IX86_BUILTIN_PSHUFW,

  IX86_BUILTIN_PSLLW,
  IX86_BUILTIN_PSLLD,
  IX86_BUILTIN_PSLLQ,
  IX86_BUILTIN_PSRAW,
  IX86_BUILTIN_PSRAD,
  IX86_BUILTIN_PSRLW,
  IX86_BUILTIN_PSRLD,
  IX86_BUILTIN_PSRLQ,
  IX86_BUILTIN_PSLLWI,
  IX86_BUILTIN_PSLLDI,
  IX86_BUILTIN_PSLLQI,
  IX86_BUILTIN_PSRAWI,
  IX86_BUILTIN_PSRADI,
  IX86_BUILTIN_PSRLWI,
  IX86_BUILTIN_PSRLDI,
  IX86_BUILTIN_PSRLQI,

  IX86_BUILTIN_PUNPCKHBW,
  IX86_BUILTIN_PUNPCKHWD,
  IX86_BUILTIN_PUNPCKHDQ,
  IX86_BUILTIN_PUNPCKLBW,
  IX86_BUILTIN_PUNPCKLWD,
  IX86_BUILTIN_PUNPCKLDQ,

  IX86_BUILTIN_SHUFPS,

  IX86_BUILTIN_RCPPS,
  IX86_BUILTIN_RCPSS,
  IX86_BUILTIN_RSQRTPS,
  IX86_BUILTIN_RSQRTSS,
  IX86_BUILTIN_SQRTPS,
  IX86_BUILTIN_SQRTSS,

  IX86_BUILTIN_UNPCKHPS,
  IX86_BUILTIN_UNPCKLPS,

  IX86_BUILTIN_ANDPS,
  IX86_BUILTIN_ANDNPS,
  IX86_BUILTIN_ORPS,
  IX86_BUILTIN_XORPS,

  IX86_BUILTIN_EMMS,
  IX86_BUILTIN_LDMXCSR,
  IX86_BUILTIN_STMXCSR,
  IX86_BUILTIN_SFENCE,

  /* 3DNow! Original */
  IX86_BUILTIN_FEMMS,
  IX86_BUILTIN_PAVGUSB,
  IX86_BUILTIN_PF2ID,
  IX86_BUILTIN_PFACC,
  IX86_BUILTIN_PFADD,
  IX86_BUILTIN_PFCMPEQ,
  IX86_BUILTIN_PFCMPGE,
  IX86_BUILTIN_PFCMPGT,
  IX86_BUILTIN_PFMAX,
  IX86_BUILTIN_PFMIN,
  IX86_BUILTIN_PFMUL,
  IX86_BUILTIN_PFRCP,
  IX86_BUILTIN_PFRCPIT1,
  IX86_BUILTIN_PFRCPIT2,
  IX86_BUILTIN_PFRSQIT1,
  IX86_BUILTIN_PFRSQRT,
  IX86_BUILTIN_PFSUB,
  IX86_BUILTIN_PFSUBR,
  IX86_BUILTIN_PI2FD,
  IX86_BUILTIN_PMULHRW,

  /* 3DNow! Athlon Extensions */
  IX86_BUILTIN_PF2IW,
  IX86_BUILTIN_PFNACC,
  IX86_BUILTIN_PFPNACC,
  IX86_BUILTIN_PI2FW,
  IX86_BUILTIN_PSWAPDSI,
  IX86_BUILTIN_PSWAPDSF,

  /* SSE2 */
  IX86_BUILTIN_ADDPD,
  IX86_BUILTIN_ADDSD,
  IX86_BUILTIN_DIVPD,
  IX86_BUILTIN_DIVSD,
  IX86_BUILTIN_MULPD,
  IX86_BUILTIN_MULSD,
  IX86_BUILTIN_SUBPD,
  IX86_BUILTIN_SUBSD,

  IX86_BUILTIN_CMPEQPD,
  IX86_BUILTIN_CMPLTPD,
  IX86_BUILTIN_CMPLEPD,
  IX86_BUILTIN_CMPGTPD,
  IX86_BUILTIN_CMPGEPD,
  IX86_BUILTIN_CMPNEQPD,
  IX86_BUILTIN_CMPNLTPD,
  IX86_BUILTIN_CMPNLEPD,
  IX86_BUILTIN_CMPNGTPD,
  IX86_BUILTIN_CMPNGEPD,
  IX86_BUILTIN_CMPORDPD,
  IX86_BUILTIN_CMPUNORDPD,
  IX86_BUILTIN_CMPNEPD,
  IX86_BUILTIN_CMPEQSD,
  IX86_BUILTIN_CMPLTSD,
  IX86_BUILTIN_CMPLESD,
  IX86_BUILTIN_CMPNEQSD,
  IX86_BUILTIN_CMPNLTSD,
  IX86_BUILTIN_CMPNLESD,
  IX86_BUILTIN_CMPORDSD,
  IX86_BUILTIN_CMPUNORDSD,
  IX86_BUILTIN_CMPNESD,

  IX86_BUILTIN_COMIEQSD,
  IX86_BUILTIN_COMILTSD,
  IX86_BUILTIN_COMILESD,
  IX86_BUILTIN_COMIGTSD,
  IX86_BUILTIN_COMIGESD,
  IX86_BUILTIN_COMINEQSD,
  IX86_BUILTIN_UCOMIEQSD,
  IX86_BUILTIN_UCOMILTSD,
  IX86_BUILTIN_UCOMILESD,
  IX86_BUILTIN_UCOMIGTSD,
  IX86_BUILTIN_UCOMIGESD,
  IX86_BUILTIN_UCOMINEQSD,

  IX86_BUILTIN_MAXPD,
  IX86_BUILTIN_MAXSD,
  IX86_BUILTIN_MINPD,
  IX86_BUILTIN_MINSD,

  IX86_BUILTIN_ANDPD,
  IX86_BUILTIN_ANDNPD,
  IX86_BUILTIN_ORPD,
  IX86_BUILTIN_XORPD,

  IX86_BUILTIN_SQRTPD,
  IX86_BUILTIN_SQRTSD,

  IX86_BUILTIN_UNPCKHPD,
  IX86_BUILTIN_UNPCKLPD,

  IX86_BUILTIN_SHUFPD,

  IX86_BUILTIN_LOADUPD,
  IX86_BUILTIN_STOREUPD,
  IX86_BUILTIN_MOVSD,

  IX86_BUILTIN_LOADHPD,
  IX86_BUILTIN_LOADLPD,

  IX86_BUILTIN_CVTDQ2PD,
  IX86_BUILTIN_CVTDQ2PS,

  IX86_BUILTIN_CVTPD2DQ,
  IX86_BUILTIN_CVTPD2PI,
  IX86_BUILTIN_CVTPD2PS,
  IX86_BUILTIN_CVTTPD2DQ,
  IX86_BUILTIN_CVTTPD2PI,

  IX86_BUILTIN_CVTPI2PD,
  IX86_BUILTIN_CVTSI2SD,
  IX86_BUILTIN_CVTSI642SD,

  IX86_BUILTIN_CVTSD2SI,
  IX86_BUILTIN_CVTSD2SI64,
  IX86_BUILTIN_CVTSD2SS,
  IX86_BUILTIN_CVTSS2SD,
  IX86_BUILTIN_CVTTSD2SI,
  IX86_BUILTIN_CVTTSD2SI64,

  IX86_BUILTIN_CVTPS2DQ,
  IX86_BUILTIN_CVTPS2PD,
  IX86_BUILTIN_CVTTPS2DQ,

  IX86_BUILTIN_MOVNTI,
  IX86_BUILTIN_MOVNTPD,
  IX86_BUILTIN_MOVNTDQ,

  /* SSE2 MMX */
  IX86_BUILTIN_MASKMOVDQU,
  IX86_BUILTIN_MOVMSKPD,
  IX86_BUILTIN_PMOVMSKB128,

  /* APPLE LOCAL begin 4099020 */
  IX86_BUILTIN_MOVQ,
  IX86_BUILTIN_LOADQ,
  IX86_BUILTIN_STOREQ,
  /* APPLE LOCAL end 4099020 */

  IX86_BUILTIN_PACKSSWB128,
  IX86_BUILTIN_PACKSSDW128,
  IX86_BUILTIN_PACKUSWB128,

  IX86_BUILTIN_PADDB128,
  IX86_BUILTIN_PADDW128,
  IX86_BUILTIN_PADDD128,
  IX86_BUILTIN_PADDQ128,
  IX86_BUILTIN_PADDSB128,
  IX86_BUILTIN_PADDSW128,
  IX86_BUILTIN_PADDUSB128,
  IX86_BUILTIN_PADDUSW128,
  IX86_BUILTIN_PSUBB128,
  IX86_BUILTIN_PSUBW128,
  IX86_BUILTIN_PSUBD128,
  IX86_BUILTIN_PSUBQ128,
  IX86_BUILTIN_PSUBSB128,
  IX86_BUILTIN_PSUBSW128,
  IX86_BUILTIN_PSUBUSB128,
  IX86_BUILTIN_PSUBUSW128,

  IX86_BUILTIN_PAND128,
  IX86_BUILTIN_PANDN128,
  IX86_BUILTIN_POR128,
  IX86_BUILTIN_PXOR128,

  IX86_BUILTIN_PAVGB128,
  IX86_BUILTIN_PAVGW128,

  IX86_BUILTIN_PCMPEQB128,
  IX86_BUILTIN_PCMPEQW128,
  IX86_BUILTIN_PCMPEQD128,
  IX86_BUILTIN_PCMPGTB128,
  IX86_BUILTIN_PCMPGTW128,
  IX86_BUILTIN_PCMPGTD128,

  IX86_BUILTIN_PMADDWD128,

  IX86_BUILTIN_PMAXSW128,
  IX86_BUILTIN_PMAXUB128,
  IX86_BUILTIN_PMINSW128,
  IX86_BUILTIN_PMINUB128,

  IX86_BUILTIN_PMULUDQ,
  IX86_BUILTIN_PMULUDQ128,
  IX86_BUILTIN_PMULHUW128,
  IX86_BUILTIN_PMULHW128,
  IX86_BUILTIN_PMULLW128,

  IX86_BUILTIN_PSADBW128,
  IX86_BUILTIN_PSHUFHW,
  IX86_BUILTIN_PSHUFLW,
  IX86_BUILTIN_PSHUFD,

  IX86_BUILTIN_PSLLW128,
  IX86_BUILTIN_PSLLD128,
  IX86_BUILTIN_PSLLQ128,
  IX86_BUILTIN_PSRAW128,
  IX86_BUILTIN_PSRAD128,
  IX86_BUILTIN_PSRLW128,
  IX86_BUILTIN_PSRLD128,
  IX86_BUILTIN_PSRLQ128,
  IX86_BUILTIN_PSLLDQI128,
  /* APPLE LOCAL 591583 */
  IX86_BUILTIN_PSLLDQI128_BYTESHIFT,
  IX86_BUILTIN_PSLLWI128,
  IX86_BUILTIN_PSLLDI128,
  IX86_BUILTIN_PSLLQI128,
  IX86_BUILTIN_PSRAWI128,
  IX86_BUILTIN_PSRADI128,
  IX86_BUILTIN_PSRLDQI128,
  /* APPLE LOCAL 591583 */
  IX86_BUILTIN_PSRLDQI128_BYTESHIFT,
  IX86_BUILTIN_PSRLWI128,
  IX86_BUILTIN_PSRLDI128,
  IX86_BUILTIN_PSRLQI128,

  IX86_BUILTIN_PUNPCKHBW128,
  IX86_BUILTIN_PUNPCKHWD128,
  IX86_BUILTIN_PUNPCKHDQ128,
  IX86_BUILTIN_PUNPCKHQDQ128,
  IX86_BUILTIN_PUNPCKLBW128,
  IX86_BUILTIN_PUNPCKLWD128,
  IX86_BUILTIN_PUNPCKLDQ128,
  IX86_BUILTIN_PUNPCKLQDQ128,

  IX86_BUILTIN_CLFLUSH,
  IX86_BUILTIN_MFENCE,
  IX86_BUILTIN_LFENCE,

  /* Prescott New Instructions.  */
  IX86_BUILTIN_ADDSUBPS,
  IX86_BUILTIN_HADDPS,
  IX86_BUILTIN_HSUBPS,
  IX86_BUILTIN_MOVSHDUP,
  IX86_BUILTIN_MOVSLDUP,
  IX86_BUILTIN_ADDSUBPD,
  IX86_BUILTIN_HADDPD,
  IX86_BUILTIN_HSUBPD,
  IX86_BUILTIN_LDDQU,

  IX86_BUILTIN_MONITOR,
  IX86_BUILTIN_MWAIT,

  /* Merom New Instructions.  */
  IX86_BUILTIN_PHADDW,
  IX86_BUILTIN_PHADDD,
  IX86_BUILTIN_PHADDSW,
  IX86_BUILTIN_PHSUBW,
  IX86_BUILTIN_PHSUBD,
  IX86_BUILTIN_PHSUBSW,
  IX86_BUILTIN_PMADDUBSW,
  IX86_BUILTIN_PMULHRSW,
  IX86_BUILTIN_PSHUFB,
  IX86_BUILTIN_PSIGNB,
  IX86_BUILTIN_PSIGNW,
  IX86_BUILTIN_PSIGND,
  IX86_BUILTIN_PALIGNR,
  IX86_BUILTIN_PABSB,
  IX86_BUILTIN_PABSW,
  IX86_BUILTIN_PABSD,

  IX86_BUILTIN_PHADDW128,
  IX86_BUILTIN_PHADDD128,
  IX86_BUILTIN_PHADDSW128,
  IX86_BUILTIN_PHSUBW128,
  IX86_BUILTIN_PHSUBD128,
  IX86_BUILTIN_PHSUBSW128,
  IX86_BUILTIN_PMADDUBSW128,
  IX86_BUILTIN_PMULHRSW128,
  IX86_BUILTIN_PSHUFB128,
  IX86_BUILTIN_PSIGNB128,
  IX86_BUILTIN_PSIGNW128,
  IX86_BUILTIN_PSIGND128,
  IX86_BUILTIN_PALIGNR128,
  IX86_BUILTIN_PABSB128,
  IX86_BUILTIN_PABSW128,
  IX86_BUILTIN_PABSD128,
  /* APPLE LOCAL begin 5612787 mainline sse4 */
  /* AMDFAM10 - SSE4A New Instructions.  */
  IX86_BUILTIN_MOVNTSD,
  IX86_BUILTIN_MOVNTSS,
  IX86_BUILTIN_EXTRQI,
  IX86_BUILTIN_EXTRQ,
  IX86_BUILTIN_INSERTQI,
  IX86_BUILTIN_INSERTQ,

  /* SSE4.1.  */
  IX86_BUILTIN_BLENDPD,
  IX86_BUILTIN_BLENDPS,
  IX86_BUILTIN_BLENDVPD,
  IX86_BUILTIN_BLENDVPS,
  IX86_BUILTIN_PBLENDVB128,
  IX86_BUILTIN_PBLENDW128,

  IX86_BUILTIN_DPPD,
  IX86_BUILTIN_DPPS,

  IX86_BUILTIN_INSERTPS128,

  IX86_BUILTIN_MOVNTDQA,
  IX86_BUILTIN_MPSADBW128,
  IX86_BUILTIN_PACKUSDW128,
  IX86_BUILTIN_PCMPEQQ,
  IX86_BUILTIN_PHMINPOSUW128,

  IX86_BUILTIN_PMAXSB128,
  IX86_BUILTIN_PMAXSD128,
  IX86_BUILTIN_PMAXUD128,
  IX86_BUILTIN_PMAXUW128,

  IX86_BUILTIN_PMINSB128,
  IX86_BUILTIN_PMINSD128,
  IX86_BUILTIN_PMINUD128,
  IX86_BUILTIN_PMINUW128,

  IX86_BUILTIN_PMOVSXBW128,
  IX86_BUILTIN_PMOVSXBD128,
  IX86_BUILTIN_PMOVSXBQ128,
  IX86_BUILTIN_PMOVSXWD128,
  IX86_BUILTIN_PMOVSXWQ128,
  IX86_BUILTIN_PMOVSXDQ128,

  IX86_BUILTIN_PMOVZXBW128,
  IX86_BUILTIN_PMOVZXBD128,
  IX86_BUILTIN_PMOVZXBQ128,
  IX86_BUILTIN_PMOVZXWD128,
  IX86_BUILTIN_PMOVZXWQ128,
  IX86_BUILTIN_PMOVZXDQ128,

  IX86_BUILTIN_PMULDQ128,
  IX86_BUILTIN_PMULLD128,

  IX86_BUILTIN_ROUNDPD,
  IX86_BUILTIN_ROUNDPS,
  IX86_BUILTIN_ROUNDSD,
  IX86_BUILTIN_ROUNDSS,

  IX86_BUILTIN_PTESTZ,
  IX86_BUILTIN_PTESTC,
  IX86_BUILTIN_PTESTNZC,
  /* APPLE LOCAL end 5612787 mainline sse4 */
  /* APPLE LOCAL end mainline */
  IX86_BUILTIN_VEC_INIT_V2SI,
  IX86_BUILTIN_VEC_INIT_V4HI,
  IX86_BUILTIN_VEC_INIT_V8QI,
  IX86_BUILTIN_VEC_EXT_V2DF,
  IX86_BUILTIN_VEC_EXT_V2DI,
  IX86_BUILTIN_VEC_EXT_V4SF,
  IX86_BUILTIN_VEC_EXT_V4SI,
  IX86_BUILTIN_VEC_EXT_V8HI,
  /* APPLE LOCAL begin 5612787 mainline sse4 */
  /* deletion */
  /* APPLE LOCAL end 5612787 mainline sse4 */
  IX86_BUILTIN_VEC_EXT_V2SI,
  IX86_BUILTIN_VEC_EXT_V4HI,
  /* APPLE LOCAL begin 5612787 mainline sse4 */
  IX86_BUILTIN_VEC_EXT_V16QI,
  IX86_BUILTIN_VEC_SET_V2DI,
  IX86_BUILTIN_VEC_SET_V4SF,
  IX86_BUILTIN_VEC_SET_V4SI,
  /* APPLE LOCAL end 5612787 mainline sse4 */
  IX86_BUILTIN_VEC_SET_V8HI,
  IX86_BUILTIN_VEC_SET_V4HI,
  /* APPLE LOCAL begin 5612787 mainline sse4 */
  IX86_BUILTIN_VEC_SET_V16QI,

  IX86_BUILTIN_VEC_PACK_SFIX,

  /* SSE4.2.  */
  IX86_BUILTIN_CRC32QI,
  IX86_BUILTIN_CRC32HI,
  IX86_BUILTIN_CRC32SI,
  IX86_BUILTIN_CRC32DI,

  IX86_BUILTIN_PCMPESTRI128,
  IX86_BUILTIN_PCMPESTRM128,
  IX86_BUILTIN_PCMPESTRA128,
  IX86_BUILTIN_PCMPESTRC128,
  IX86_BUILTIN_PCMPESTRO128,
  IX86_BUILTIN_PCMPESTRS128,
  IX86_BUILTIN_PCMPESTRZ128,
  IX86_BUILTIN_PCMPISTRI128,
  IX86_BUILTIN_PCMPISTRM128,
  IX86_BUILTIN_PCMPISTRA128,
  IX86_BUILTIN_PCMPISTRC128,
  IX86_BUILTIN_PCMPISTRO128,
  IX86_BUILTIN_PCMPISTRS128,
  IX86_BUILTIN_PCMPISTRZ128,

  IX86_BUILTIN_PCMPGTQ,

  /* TFmode support builtins.  */
  IX86_BUILTIN_INFQ,
  IX86_BUILTIN_FABSQ,
  IX86_BUILTIN_COPYSIGNQ,
  /* APPLE LOCAL end 5612787 mainline sse4 */

  IX86_BUILTIN_MAX
};

/* LLVM_TARGET_INTRINSIC_PREFIX - Specify what prefix this target uses for its
 * intrinsics.
 */
#define LLVM_TARGET_INTRINSIC_PREFIX "x86"

/* LLVM_TARGET_NAME - This specifies the name of the target, which correlates to
 * the llvm::InitializeXXXTarget() function.
 */
#define LLVM_TARGET_NAME X86

/* Turn -march=xx into a CPU type.
 */
#define LLVM_SET_SUBTARGET_FEATURES(F) \
  { if (TARGET_MACHO && ! strcmp (ix86_arch_string, "apple")) \
      F.setCPU(TARGET_64BIT ? "core2" : "yonah");             \
    else                                                      \
      F.setCPU(ix86_arch_string);                             \
                                                              \
    if (TARGET_64BIT)                                         \
      F.AddFeature("64bit");                                  \
    else if (target_flags_explicit & OPTION_MASK_ISA_64BIT)   \
      F.AddFeature("64bit", false);                           \
                                                              \
    if (TARGET_MMX)                                           \
      F.AddFeature("mmx");                                    \
    else if (target_flags_explicit & OPTION_MASK_ISA_MMX)     \
      F.AddFeature("mmx", false);                             \
                                                              \
    if (TARGET_3DNOW)                                         \
      F.AddFeature("3dnow");                                  \
    else if (target_flags_explicit & OPTION_MASK_ISA_3DNOW)   \
      F.AddFeature("3dnow", false);                           \
                                                              \
    if (TARGET_3DNOW_A)                                       \
      F.AddFeature("3dnowa");                                 \
    else if (target_flags_explicit & OPTION_MASK_ISA_3DNOW_A) \
      F.AddFeature("3dnowa", false);                          \
                                                              \
    if (TARGET_SSE)                                           \
      F.AddFeature("sse");                                    \
    else if (target_flags_explicit & OPTION_MASK_ISA_SSE)     \
      F.AddFeature("sse", false);                             \
                                                              \
    if (TARGET_SSE2)                                          \
      F.AddFeature("sse2");                                   \
    else if (target_flags_explicit & OPTION_MASK_ISA_SSE2)    \
      F.AddFeature("sse2", false);                            \
                                                              \
    if (TARGET_SSE3)                                          \
      F.AddFeature("sse3");                                   \
    else if (target_flags_explicit & OPTION_MASK_ISA_SSE3)    \
      F.AddFeature("sse3", false);                            \
                                                              \
    if (TARGET_SSSE3)                                         \
      F.AddFeature("ssse3");                                  \
    else if (target_flags_explicit & OPTION_MASK_ISA_SSSE3)   \
      F.AddFeature("ssse3", false);                           \
                                                              \
    if (TARGET_SSE4_1)                                        \
      F.AddFeature("sse41");                                  \
    else if (target_flags_explicit & OPTION_MASK_ISA_SSE4_1)  \
      F.AddFeature("sse41", false);                           \
                                                              \
    if (TARGET_SSE4_2)                                        \
      F.AddFeature("sse42");                                  \
    else if (target_flags_explicit & OPTION_MASK_ISA_SSE4_2)  \
      F.AddFeature("sse42", false);                           \
                                                              \
    if (TARGET_AVX)                                           \
      F.AddFeature("avx");                                    \
    else if (target_flags_explicit & OPTION_MASK_ISA_AVX)     \
      F.AddFeature("avx", false);                             \
                                                              \
    if (TARGET_FMA)                                           \
      F.AddFeature("fma3");                                   \
    else if (target_flags_explicit & OPTION_MASK_ISA_FMA)     \
      F.AddFeature("fma3", false);                            \
                                                              \
    if (TARGET_SSE4A)                                         \
      F.AddFeature("sse4a");                                  \
    else if (target_flags_explicit & OPTION_MASK_ISA_SSE4A)   \
      F.AddFeature("sse4a", false);                           \
  }

#define LLVM_SET_IMPLICIT_FLOAT(flag_no_implicit_float)       \
  if (!TARGET_80387)                                          \
    flag_no_implicit_float = 1;                               \
  else                                                        \
    flag_no_implicit_float = 0;                               
    
/* LLVM ABI definition macros. */

/* When -m64 is specified, set the architecture to x86_64-os-blah even if the
 * compiler was configured for i[3456]86-os-blah.
 */
#define LLVM_OVERRIDE_TARGET_ARCH() \
  (TARGET_64BIT ? "x86_64" : "i386")

/* LLVM_TARGET_INTRINSIC_LOWER - To handle builtins, we want to expand the
 * invocation into normal LLVM code.  If the target can handle the builtin, this
 * macro should call the target TreeToLLVM::TargetIntrinsicLower method and
 *  return true.This macro is invoked from a method in the TreeToLLVM class.
 */
#define LLVM_TARGET_INTRINSIC_LOWER(STMT, BUILTIN_CODE, DESTLOC, RESULT,      \
                                    DESTTY, OPS)                              \
        TargetIntrinsicLower(STMT, BUILTIN_CODE, DESTLOC, RESULT, DESTTY, OPS);

/* When extracting a register name for a constraint, use the string extracted
   from the magic symbol built for that register, rather than reg_names.
   The latter maps both AH and AL to the same thing, which means we can't
   distinguish them. */
#define LLVM_DO_NOT_USE_REG_NAMES

/* Propagate code model setting to backend */
#define LLVM_SET_MACHINE_OPTIONS(argvec)           \
  switch (ix86_cmodel) {                           \
  default:                                         \
    sorry ("code model %<%s%> not supported yet", ix86_cmodel_string);  \
    break;                                         \
  case CM_SMALL:                                   \
  case CM_SMALL_PIC:                               \
    argvec.push_back("--code-model=small");        \
    break;                                         \
  case CM_KERNEL:                                  \
    argvec.push_back("--code-model=kernel");       \
    break;                                         \
  case CM_MEDIUM:                                  \
  case CM_MEDIUM_PIC:                              \
    argvec.push_back("--code-model=medium");       \
    break;                                         \
  case CM_32:                                      \
    argvec.push_back("--code-model=default");      \
    break;                                         \
  }

#endif /* LLVM_TARGET_H */
