//===--------------- Target.cpp - Implements the IA-32 ABI. ---------------===//
//
// Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011  Evan Cheng,
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
// This file implements specific LLVM IA-32 ABI.
//===----------------------------------------------------------------------===//

// Plugin headers
#include "dragonegg/ABI.h"
#include "dragonegg/Target.h"

// LLVM headers
#include "llvm/Module.h"

// System headers
#include <gmp.h>

// GCC headers
extern "C" {
#include "config.h"
// Stop GCC declaring 'getopt' as it can clash with the system's declaration.
#undef HAVE_DECL_GETOPT
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"

#include "gimple.h"
#include "toplev.h"
}

static LLVMContext &Context = getGlobalContext();

/// BitCastToIntVector - Bitcast the vector operand to a vector of integers of
//  the same length.
static Value *BitCastToIntVector(Value *Op, LLVMBuilder &Builder) {
  const VectorType *VecTy = cast<VectorType>(Op->getType());
  const Type *EltTy = VecTy->getElementType();
  const Type *IntTy = IntegerType::get(Context,EltTy->getPrimitiveSizeInBits());
  return Builder.CreateBitCast(Op, VectorType::get(IntTy,
                                                   VecTy->getNumElements()));
}

/// BuiltinCode - A enumerated type with one value for each supported builtin.
enum BuiltinCode {
  SearchForHandler, // Builtin not seen before - search for a handler.
#define DEFINE_BUILTIN(x) x
#include "x86_builtins"
#undef DEFINE_BUILTIN
  , UnsupportedBuiltin // There is no handler for this builtin.
};

struct HandlerEntry {
  const char *Name; BuiltinCode Handler;
};

static bool LT(const HandlerEntry &E, const HandlerEntry &F) {
  return strcmp(E.Name, F.Name) < 0;
}

/* TargetIntrinsicLower - For builtins that we want to expand to normal LLVM
 * code, emit the code now.  If we can handle the code, this macro should emit
 * the code, return true.
 */
bool TreeToLLVM::TargetIntrinsicLower(gimple stmt,
                                      tree fndecl,
                                      const MemRef * /*DestLoc*/,
                                      Value *&Result,
                                      const Type *ResultType,
                                      std::vector<Value*> &Ops) {
  // DECL_FUNCTION_CODE contains a value of the enumerated type ix86_builtins,
  // declared in i386.c.  If this type was visible to us then we could simply
  // use a switch statement on DECL_FUNCTION_CODE to jump to the right code for
  // handling the builtin.  But the type isn't visible, so instead we generate
  // at run-time a map from the values of DECL_FUNCTION_CODE to values of the
  // enumerated type BuiltinCode (defined above), the analog of ix86_builtins,
  // and do the switch on the BuiltinCode value instead.

  // The map from DECL_FUNCTION_CODE values to BuiltinCode.
  static std::vector<BuiltinCode> FunctionCodeMap;
  if (FunctionCodeMap.size() <= DECL_FUNCTION_CODE(fndecl))
      FunctionCodeMap.resize(DECL_FUNCTION_CODE(fndecl) + 1);

  // See if we already associated a BuiltinCode with this DECL_FUNCTION_CODE.
  BuiltinCode &Handler = FunctionCodeMap[DECL_FUNCTION_CODE(fndecl)];
  if (Handler == SearchForHandler) {
    // No associated BuiltinCode.  Work out what value to use based on the
    // builtin's name.

    // List of builtin names (w/o '__builtin_ia32_') and associated BuiltinCode.
    static const HandlerEntry Handlers[] = {
#define DEFINE_BUILTIN(x) {#x, x}
#include "x86_builtins"
#undef DEFINE_BUILTIN
    };
    size_t N = sizeof(Handlers) / sizeof(Handlers[0]);
#ifndef NDEBUG
    // Check that the list of handlers is sorted by name.
    static bool Checked = false;
    if (!Checked) {
      for (unsigned i = 1; i < N; ++i)
        assert(LT(Handlers[i-1], Handlers[i]) && "Handlers not sorted!");
      Checked = true;
    }
#endif

    Handler = UnsupportedBuiltin;
    const char *Identifier = IDENTIFIER_POINTER(DECL_NAME(fndecl));
    // All builtins handled here have a name starting with __builtin_ia32_.
    if (!strncmp(Identifier, "__builtin_ia32_", 15)) {
      HandlerEntry ToFind = { Identifier + 15, SearchForHandler };
      const HandlerEntry *E = std::lower_bound(Handlers, Handlers + N, ToFind, LT);
      if ((E < Handlers + N) && !strcmp(E->Name, ToFind.Name))
        Handler = E->Handler;
    }
  }

  bool flip = false;
  unsigned PredCode;

  switch (Handler) {
  case SearchForHandler:
    assert(false && "Unexpected builtin code!");
  case UnsupportedBuiltin: return false;
  case addps:
  case addps256:
  case addpd:
  case addpd256:
    Result = Builder.CreateFAdd(Ops[0], Ops[1]);
    return true;
  case paddb:
  case paddw:
  case paddd:
  case paddq:
  case paddb128:
  case paddw128:
  case paddd128:
  case paddq128:
    Result = Builder.CreateAdd(Ops[0], Ops[1]);
    return true;
  case subps:
  case subps256:
  case subpd:
  case subpd256:
    Result = Builder.CreateFSub(Ops[0], Ops[1]);
    return true;
  case psubb:
  case psubw:
  case psubd:
  case psubq:
  case psubb128:
  case psubw128:
  case psubd128:
  case psubq128:
    Result = Builder.CreateSub(Ops[0], Ops[1]);
    return true;
  case mulps:
  case mulps256:
  case mulpd:
  case mulpd256:
    Result = Builder.CreateFMul(Ops[0], Ops[1]);
    return true;
  case pmullw:
  case pmullw128:
  case pmulld128:
    Result = Builder.CreateMul(Ops[0], Ops[1]);
    return true;
  case divps:
  case divps256:
  case divpd:
  case divpd256:
    Result = Builder.CreateFDiv(Ops[0], Ops[1]);
    return true;
  case pand:
  case pand128:
    Result = Builder.CreateAnd(Ops[0], Ops[1]);
    return true;
  case pandn:
  case pandn128:
    Ops[0] = Builder.CreateNot(Ops[0]);
    Result = Builder.CreateAnd(Ops[0], Ops[1]);
    return true;
  case por:
  case por128:
    Result = Builder.CreateOr(Ops[0], Ops[1]);
    return true;
  case pxor:
  case pxor128:
    Result = Builder.CreateXor(Ops[0], Ops[1]);
    return true;
  case andps:
  case andps256:
  case andpd:
  case andpd256:
    Ops[0] = BitCastToIntVector(Ops[0], Builder);
    Ops[1] = Builder.CreateBitCast(Ops[1], Ops[0]->getType());
    Result = Builder.CreateAnd(Ops[0], Ops[1]);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  case orps:
  case orps256:
  case orpd:
  case orpd256:
    Ops[0] = BitCastToIntVector(Ops[0], Builder);
    Ops[1] = Builder.CreateBitCast(Ops[1], Ops[0]->getType());
    Result = Builder.CreateOr(Ops[0], Ops[1]);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  case xorps:
  case xorps256:
  case xorpd:
  case xorpd256:
    Ops[0] = BitCastToIntVector(Ops[0], Builder);
    Ops[1] = Builder.CreateBitCast(Ops[1], Ops[0]->getType());
    Result = Builder.CreateXor(Ops[0], Ops[1]);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  case andnps:
  case andnps256:
  case andnpd:
  case andnpd256:
    Ops[0] = BitCastToIntVector(Ops[0], Builder);
    Ops[1] = Builder.CreateBitCast(Ops[1], Ops[0]->getType());
    Ops[0] = Builder.CreateNot(Ops[0]);
    Result = Builder.CreateAnd(Ops[0], Ops[1]);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  case shufps:
    if (ConstantInt *Elt = dyn_cast<ConstantInt>(Ops[2])) {
      int EV = Elt->getZExtValue();
      Result = BuildVectorShuffle(Ops[0], Ops[1],
                                  ((EV & 0x03) >> 0),   ((EV & 0x0c) >> 2),
                                  ((EV & 0x30) >> 4)+4, ((EV & 0xc0) >> 6)+4);
    } else {
      error_at(gimple_location(stmt), "mask must be an immediate");
      Result = Ops[0];
    }
    return true;
  case shufpd:
    if (ConstantInt *Elt = dyn_cast<ConstantInt>(Ops[2])) {
      int EV = Elt->getZExtValue();
      Result = BuildVectorShuffle(Ops[0], Ops[1],
                                  ((EV & 0x01) >> 0),   ((EV & 0x02) >> 1)+2);
    } else {
      error_at(gimple_location(stmt), "mask must be an immediate");
      Result = Ops[0];
    }
    return true;
  case pshufw:
  case pshufd:
    if (ConstantInt *Elt = dyn_cast<ConstantInt>(Ops[1])) {
      int EV = Elt->getZExtValue();
      Result = BuildVectorShuffle(Ops[0], Ops[0],
                                  ((EV & 0x03) >> 0),   ((EV & 0x0c) >> 2),
                                  ((EV & 0x30) >> 4),   ((EV & 0xc0) >> 6));
    } else {
      error_at(gimple_location(stmt), "mask must be an immediate");
      Result = Ops[0];
    }
    return true;
  case pshufhw:
    if (ConstantInt *Elt = dyn_cast<ConstantInt>(Ops[1])) {
      int EV = Elt->getZExtValue();
      Result = BuildVectorShuffle(Ops[0], Ops[0],
                                  0, 1, 2, 3,
                                  ((EV & 0x03) >> 0)+4, ((EV & 0x0c) >> 2)+4,
                                  ((EV & 0x30) >> 4)+4, ((EV & 0xc0) >> 6)+4);
      return true;
    }
    return false;
  case pshuflw:
    if (ConstantInt *Elt = dyn_cast<ConstantInt>(Ops[1])) {
      int EV = Elt->getZExtValue();
      Result = BuildVectorShuffle(Ops[0], Ops[0],
                                  ((EV & 0x03) >> 0),   ((EV & 0x0c) >> 2),
                                  ((EV & 0x30) >> 4),   ((EV & 0xc0) >> 6),
                                  4, 5, 6, 7);
    } else {
      error_at(gimple_location(stmt), "mask must be an immediate");
      Result = Ops[0];
    }

    return true;
  case punpckhbw:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 4, 12, 5, 13,
                                                6, 14, 7, 15);
    return true;
  case punpckhwd:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 2, 6, 3, 7);
    return true;
  case punpckhdq:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 1, 3);
    return true;
  case punpcklbw:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 0,  8, 1,  9,
                                                2, 10, 3, 11);
    return true;
  case punpcklwd:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 0, 4, 1, 5);
    return true;
  case punpckldq:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 0, 2);
    return true;
  case punpckhbw128:
    Result = BuildVectorShuffle(Ops[0], Ops[1],  8, 24,  9, 25,
                                                10, 26, 11, 27,
                                                12, 28, 13, 29,
                                                14, 30, 15, 31);
    return true;
  case punpckhwd128:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 4, 12, 5, 13, 6, 14, 7, 15);
    return true;
  case punpckhdq128:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 2, 6, 3, 7);
    return true;
  case punpckhqdq128:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 1, 3);
    return true;
  case punpcklbw128:
    Result = BuildVectorShuffle(Ops[0], Ops[1],  0, 16,  1, 17,
                                                 2, 18,  3, 19,
                                                 4, 20,  5, 21,
                                                 6, 22,  7, 23);
    return true;
  case punpcklwd128:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 0, 8, 1, 9, 2, 10, 3, 11);
    return true;
  case punpckldq128:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 0, 4, 1, 5);
    return true;
  case punpcklqdq128:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 0, 2);
    return true;
  case unpckhps:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 2, 6, 3, 7);
    return true;
  case unpckhpd:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 1, 3);
    return true;
  case unpcklps:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 0, 4, 1, 5);
    return true;
  case unpcklpd:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 0, 2);
    return true;
  case movhlps:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 6, 7, 2, 3);
    return true;
  case movlhps:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 0, 1, 4, 5);
    return true;
  case movss:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 4, 1, 2, 3);
    return true;
  case movsd:
    Result = BuildVectorShuffle(Ops[0], Ops[1], 2, 1);
    return true;
  case movq128: {
    Value *Zero = Constant::getNullValue(Ops[0]->getType());
    Result = BuildVectorShuffle(Zero, Ops[0], 2, 1);
    return true;
  }
//TODO  IX86_BUILTIN_LOADQ: {
//TODO    const PointerType *i64Ptr = Type::getInt64PtrTy(Context);
//TODO    Ops[0] = Builder.CreateBitCast(Ops[0], i64Ptr);
//TODO    Ops[0] = Builder.CreateLoad(Ops[0]);
//TODO    Value *Zero = ConstantInt::get(Type::getInt64Ty(Context), 0);
//TODO    Result = BuildVector(Zero, Zero, NULL);
//TODO    Value *Idx = ConstantInt::get(Type::getInt32Ty(Context), 0);
//TODO    Result = Builder.CreateInsertElement(Result, Ops[0], Idx);
//TODO    Result = Builder.CreateBitCast(Result, ResultType);
//TODO    return true;
//TODO  }
  case loadups: {
    VectorType *v4f32 = VectorType::get(Type::getFloatTy(Context), 4);
    const PointerType *v4f32Ptr = v4f32->getPointerTo();
    Value *BC = Builder.CreateBitCast(Ops[0], v4f32Ptr);
    LoadInst *LI = Builder.CreateLoad(BC);
    LI->setAlignment(1);
    Result = LI;
    return true;
  }
  case loadupd: {
    VectorType *v2f64 = VectorType::get(Type::getDoubleTy(Context), 2);
    const PointerType *v2f64Ptr = v2f64->getPointerTo();
    Value *BC = Builder.CreateBitCast(Ops[0], v2f64Ptr);
    LoadInst *LI = Builder.CreateLoad(BC);
    LI->setAlignment(1);
    Result = LI;
    return true;
  }
  case loaddqu: {
    VectorType *v16i8 = VectorType::get(Type::getInt8Ty(Context), 16);
    const PointerType *v16i8Ptr = v16i8->getPointerTo();
    Value *BC = Builder.CreateBitCast(Ops[0], v16i8Ptr);
    LoadInst *LI = Builder.CreateLoad(BC);
    LI->setAlignment(1);
    Result = LI;
    return true;
  }
  case storeups: {
    VectorType *v4f32 = VectorType::get(Type::getFloatTy(Context), 4);
    const PointerType *v4f32Ptr = v4f32->getPointerTo();
    Value *BC = Builder.CreateBitCast(Ops[0], v4f32Ptr);
    StoreInst *SI = Builder.CreateStore(Ops[1], BC);
    SI->setAlignment(1);
    return true;
  }
  case storeupd: {
    VectorType *v2f64 = VectorType::get(Type::getDoubleTy(Context), 2);
    const PointerType *v2f64Ptr = v2f64->getPointerTo();
    Value *BC = Builder.CreateBitCast(Ops[0], v2f64Ptr);
    StoreInst *SI = Builder.CreateStore(Ops[1], BC);
    SI->setAlignment(1);
    return true;
  }
  case storedqu: {
    VectorType *v16i8 = VectorType::get(Type::getInt8Ty(Context), 16);
    const PointerType *v16i8Ptr = v16i8->getPointerTo();
    Value *BC = Builder.CreateBitCast(Ops[0], v16i8Ptr);
    StoreInst *SI = Builder.CreateStore(Ops[1], BC);
    SI->setAlignment(1);
    return true;
  }
  case loadhps: {
    const PointerType *f64Ptr = Type::getDoublePtrTy(Context);
    Ops[1] = Builder.CreateBitCast(Ops[1], f64Ptr);
    Value *Load = Builder.CreateLoad(Ops[1]);
    Ops[1] = BuildVector(Load, UndefValue::get(Type::getDoubleTy(Context)), NULL);
    Ops[1] = Builder.CreateBitCast(Ops[1], ResultType);
    Result = BuildVectorShuffle(Ops[0], Ops[1], 0, 1, 4, 5);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  }
  case loadlps: {
    const PointerType *f64Ptr = Type::getDoublePtrTy(Context);
    Ops[1] = Builder.CreateBitCast(Ops[1], f64Ptr);
    Value *Load = Builder.CreateLoad(Ops[1]);
    Ops[1] = BuildVector(Load, UndefValue::get(Type::getDoubleTy(Context)), NULL);
    Ops[1] = Builder.CreateBitCast(Ops[1], ResultType);
    Result = BuildVectorShuffle(Ops[0], Ops[1], 4, 5, 2, 3);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  }
  case loadhpd: {
    Value *Load = Builder.CreateLoad(Ops[1]);
    Ops[1] = BuildVector(Load, UndefValue::get(Type::getDoubleTy(Context)), NULL);
    Ops[1] = Builder.CreateBitCast(Ops[1], ResultType);
    Result = BuildVectorShuffle(Ops[0], Ops[1], 0, 2);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  }
  case loadlpd: {
    Value *Load = Builder.CreateLoad(Ops[1]);
    Ops[1] = BuildVector(Load, UndefValue::get(Type::getDoubleTy(Context)), NULL);
    Ops[1] = Builder.CreateBitCast(Ops[1], ResultType);
    Result = BuildVectorShuffle(Ops[0], Ops[1], 2, 1);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  }
  case storehps: {
    VectorType *v2f64 = VectorType::get(Type::getDoubleTy(Context), 2);
    const PointerType *f64Ptr = Type::getDoublePtrTy(Context);
    Ops[0] = Builder.CreateBitCast(Ops[0], f64Ptr);
    Value *Idx = ConstantInt::get(Type::getInt32Ty(Context), 1);
    Ops[1] = Builder.CreateBitCast(Ops[1], v2f64);
    Ops[1] = Builder.CreateExtractElement(Ops[1], Idx);
    Builder.CreateStore(Ops[1], Ops[0]);
    return true;
  }
  case storelps: {
    VectorType *v2f64 = VectorType::get(Type::getDoubleTy(Context), 2);
    const PointerType *f64Ptr = Type::getDoublePtrTy(Context);
    Ops[0] = Builder.CreateBitCast(Ops[0], f64Ptr);
    Value *Idx = ConstantInt::get(Type::getInt32Ty(Context), 0);
    Ops[1] = Builder.CreateBitCast(Ops[1], v2f64);
    Ops[1] = Builder.CreateExtractElement(Ops[1], Idx);
    Builder.CreateStore(Ops[1], Ops[0]);
    return true;
  }
  case movshdup:
    Result = BuildVectorShuffle(Ops[0], Ops[0], 1, 1, 3, 3);
    return true;
  case movsldup:
    Result = BuildVectorShuffle(Ops[0], Ops[0], 0, 0, 2, 2);
    return true;
  case vec_init_v2si:
    Result = BuildVector(Ops[0], Ops[1], NULL);
    return true;
  case vec_init_v4hi:
    // Sometimes G++ promotes arguments to int.
    for (unsigned i = 0; i != 4; ++i)
      Ops[i] = Builder.CreateIntCast(Ops[i], Type::getInt16Ty(Context),
                                     /*isSigned*/false);
    Result = BuildVector(Ops[0], Ops[1], Ops[2], Ops[3], NULL);
    return true;
  case vec_init_v8qi:
    // Sometimes G++ promotes arguments to int.
    for (unsigned i = 0; i != 8; ++i)
      Ops[i] = Builder.CreateIntCast(Ops[i], Type::getInt8Ty(Context),
                                     /*isSigned*/false);
    Result = BuildVector(Ops[0], Ops[1], Ops[2], Ops[3],
                         Ops[4], Ops[5], Ops[6], Ops[7], NULL);
    return true;
  case vec_ext_v2si:
  case vec_ext_v4hi:
  case vec_ext_v2df:
  case vec_ext_v2di:
  case vec_ext_v4si:
  case vec_ext_v4sf:
  case vec_ext_v8hi:
  case vec_ext_v16qi:
    Result = Builder.CreateExtractElement(Ops[0], Ops[1]);
    return true;
  case vec_set_v16qi:
    // Sometimes G++ promotes arguments to int.
    Ops[1] = Builder.CreateIntCast(Ops[1], Type::getInt8Ty(Context),
                                   /*isSigned*/false);
    Result = Builder.CreateInsertElement(Ops[0], Ops[1], Ops[2]);
    return true;
  case vec_set_v4hi:
  case vec_set_v8hi:
    // GCC sometimes doesn't produce the right element type.
    Ops[1] = Builder.CreateIntCast(Ops[1], Type::getInt16Ty(Context),
                                   /*isSigned*/false);
    Result = Builder.CreateInsertElement(Ops[0], Ops[1], Ops[2]);
    return true;
  case vec_set_v4si:
    Result = Builder.CreateInsertElement(Ops[0], Ops[1], Ops[2]);
    return true;
  case vec_set_v2di:
    Result = Builder.CreateInsertElement(Ops[0], Ops[1], Ops[2]);
    return true;

  case cmpeqps: PredCode = 0; goto CMPXXPS;
  case cmpltps: PredCode = 1; goto CMPXXPS;
  case cmpgtps: PredCode = 1; flip = true; goto CMPXXPS;
  case cmpleps: PredCode = 2; goto CMPXXPS;
  case cmpgeps: PredCode = 2; flip = true; goto CMPXXPS;
  case cmpunordps: PredCode = 3; goto CMPXXPS;
  case cmpneqps: PredCode = 4; goto CMPXXPS;
  case cmpnltps: PredCode = 5; goto CMPXXPS;
  case cmpngtps: PredCode = 5; flip = true; goto CMPXXPS;
  case cmpnleps: PredCode = 6; goto CMPXXPS;
  case cmpngeps: PredCode = 6; flip = true; goto CMPXXPS;
  case cmpordps: PredCode = 7; goto CMPXXPS;
  CMPXXPS: {
    Function *cmpps =
      Intrinsic::getDeclaration(TheModule, Intrinsic::x86_sse_cmp_ps);
    Value *Pred = ConstantInt::get(Type::getInt8Ty(Context), PredCode);
    Value *Arg0 = Ops[0];
    Value *Arg1 = Ops[1];
    if (flip) std::swap(Arg0, Arg1);
    Value *CallOps[3] = { Arg0, Arg1, Pred };
    Result = Builder.CreateCall(cmpps, CallOps, CallOps+3);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  }
  case cmpeqss:    PredCode = 0; goto CMPXXSS;
  case cmpltss:    PredCode = 1; goto CMPXXSS;
  case cmpless:    PredCode = 2; goto CMPXXSS;
  case cmpunordss: PredCode = 3; goto CMPXXSS;
  case cmpneqss:   PredCode = 4; goto CMPXXSS;
  case cmpnltss:   PredCode = 5; goto CMPXXSS;
  case cmpnless:   PredCode = 6; goto CMPXXSS;
  case cmpordss:   PredCode = 7; goto CMPXXSS;
  CMPXXSS: {
    Function *cmpss =
      Intrinsic::getDeclaration(TheModule, Intrinsic::x86_sse_cmp_ss);
    Value *Pred = ConstantInt::get(Type::getInt8Ty(Context), PredCode);
    Value *CallOps[3] = { Ops[0], Ops[1], Pred };
    Result = Builder.CreateCall(cmpss, CallOps, CallOps+3);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  }
  case cmpeqpd:    PredCode = 0; goto CMPXXPD;
  case cmpltpd:    PredCode = 1; goto CMPXXPD;
  case cmpgtpd:    PredCode = 1; flip = true; goto CMPXXPD;
  case cmplepd:    PredCode = 2; goto CMPXXPD;
  case cmpgepd:    PredCode = 2; flip = true; goto CMPXXPD;
  case cmpunordpd: PredCode = 3; goto CMPXXPD;
  case cmpneqpd:   PredCode = 4; goto CMPXXPD;
  case cmpnltpd:   PredCode = 5; goto CMPXXPD;
  case cmpngtpd:   PredCode = 5; flip = true; goto CMPXXPD;
  case cmpnlepd:   PredCode = 6; goto CMPXXPD;
  case cmpngepd:   PredCode = 6; flip = true; goto CMPXXPD;
  case cmpordpd:   PredCode = 7; goto CMPXXPD;
  CMPXXPD: {
    Function *cmppd =
      Intrinsic::getDeclaration(TheModule, Intrinsic::x86_sse2_cmp_pd);
    Value *Pred = ConstantInt::get(Type::getInt8Ty(Context), PredCode);
    Value *Arg0 = Ops[0];
    Value *Arg1 = Ops[1];
    if (flip) std::swap(Arg0, Arg1);

    Value *CallOps[3] = { Arg0, Arg1, Pred };
    Result = Builder.CreateCall(cmppd, CallOps, CallOps+3);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  }
  case cmpeqsd:    PredCode = 0; goto CMPXXSD;
  case cmpltsd:    PredCode = 1; goto CMPXXSD;
  case cmplesd:    PredCode = 2; goto CMPXXSD;
  case cmpunordsd: PredCode = 3; goto CMPXXSD;
  case cmpneqsd:   PredCode = 4; goto CMPXXSD;
  case cmpnltsd:   PredCode = 5; goto CMPXXSD;
  case cmpnlesd:   PredCode = 6; goto CMPXXSD;
  case cmpordsd:   PredCode = 7; goto CMPXXSD;
  CMPXXSD: {
    Function *cmpsd =
      Intrinsic::getDeclaration(TheModule, Intrinsic::x86_sse2_cmp_sd);
    Value *Pred = ConstantInt::get(Type::getInt8Ty(Context), PredCode);
    Value *CallOps[3] = { Ops[0], Ops[1], Pred };
    Result = Builder.CreateCall(cmpsd, CallOps, CallOps+3);
    Result = Builder.CreateBitCast(Result, ResultType);
    return true;
  }
  case ldmxcsr: {
    Function *ldmxcsr =
      Intrinsic::getDeclaration(TheModule, Intrinsic::x86_sse_ldmxcsr);
    Value *Ptr = CreateTemporary(Type::getInt32Ty(Context));
    Builder.CreateStore(Ops[0], Ptr);
    Ptr = Builder.CreateBitCast(Ptr, Type::getInt8PtrTy(Context));
    Builder.CreateCall(ldmxcsr, Ptr);
    return true;
  }
  case stmxcsr: {
    Function *stmxcsr =
      Intrinsic::getDeclaration(TheModule, Intrinsic::x86_sse_stmxcsr);
    Value *Ptr  = CreateTemporary(Type::getInt32Ty(Context));
    Value *BPtr = Builder.CreateBitCast(Ptr, Type::getInt8PtrTy(Context));
    Builder.CreateCall(stmxcsr, BPtr);

    Result = Builder.CreateLoad(Ptr);
    return true;
  }
  case palignr: {
    if (isa<ConstantInt>(Ops[2])) {

      // In the header we multiply by 8, correct that back now.
      unsigned shiftVal = (cast<ConstantInt>(Ops[2])->getZExtValue())/8;

      // If palignr is shifting the pair of input vectors less than 9 bytes,
      // emit a shuffle instruction.
      if (shiftVal <= 8) {
        const llvm::Type *IntTy = Type::getInt32Ty(Context);
        const llvm::Type *EltTy = Type::getInt8Ty(Context);
        const llvm::Type *VecTy = VectorType::get(EltTy, 8);

        Ops[1] = Builder.CreateBitCast(Ops[1], VecTy);
        Ops[0] = Builder.CreateBitCast(Ops[0], VecTy);

        SmallVector<Constant*, 8> Indices;
        for (unsigned i = 0; i != 8; ++i)
          Indices.push_back(ConstantInt::get(IntTy, shiftVal + i));

        Value* SV = ConstantVector::get(Indices);
        Result = Builder.CreateShuffleVector(Ops[1], Ops[0], SV, "palignr");
        return true;
      }

      // If palignr is shifting the pair of input vectors more than 8 but less
      // than 16 bytes, emit a logical right shift of the destination.
      if (shiftVal < 16) {
        // MMX has these as 1 x i64 vectors for some odd optimization reasons.
        const llvm::Type *EltTy = Type::getInt64Ty(Context);
        const llvm::Type *VecTy = VectorType::get(EltTy, 1);

        Ops[0] = Builder.CreateBitCast(Ops[0], VecTy, "cast");
        Ops[1] = ConstantInt::get(VecTy, (shiftVal-8) * 8);

        // create i32 constant
        Function *F = Intrinsic::getDeclaration(TheModule,
                                                Intrinsic::x86_mmx_psrl_q);
        Result = Builder.CreateCall(F, &Ops[0], &Ops[0] + 2, "palignr");
        return true;
      }

      // If palignr is shifting the pair of vectors more than 32 bytes,
      // emit zero.
      Result = Constant::getNullValue(ResultType);
      return true;
    } else {
      error_at(gimple_location(stmt), "mask must be an immediate");
      Result = Ops[0];
      return true;
    }
  }
  case palignr128: {
    if (isa<ConstantInt>(Ops[2])) {

      // In the header we multiply by 8, correct that back now.
      unsigned shiftVal = (cast<ConstantInt>(Ops[2])->getZExtValue())/8;

      // If palignr is shifting the pair of input vectors less than 17 bytes,
      // emit a shuffle instruction.
      if (shiftVal <= 16) {
        const llvm::Type *IntTy = Type::getInt32Ty(Context);
        const llvm::Type *EltTy = Type::getInt8Ty(Context);
        const llvm::Type *VecTy = VectorType::get(EltTy, 16);

        Ops[1] = Builder.CreateBitCast(Ops[1], VecTy);
        Ops[0] = Builder.CreateBitCast(Ops[0], VecTy);

        llvm::SmallVector<Constant*, 16> Indices;
        for (unsigned i = 0; i != 16; ++i)
          Indices.push_back(ConstantInt::get(IntTy, shiftVal + i));

        Value* SV = ConstantVector::get(Indices);
        Result = Builder.CreateShuffleVector(Ops[1], Ops[0], SV, "palignr");
        return true;
      }

      // If palignr is shifting the pair of input vectors more than 16 but less
      // than 32 bytes, emit a logical right shift of the destination.
      if (shiftVal < 32) {
        const llvm::Type *EltTy = Type::getInt64Ty(Context);
        const llvm::Type *VecTy = VectorType::get(EltTy, 2);
        const llvm::Type *IntTy = Type::getInt32Ty(Context);

        Ops[0] = Builder.CreateBitCast(Ops[0], VecTy, "cast");
        Ops[1] = ConstantInt::get(IntTy, (shiftVal-16) * 8);

        // create i32 constant
        llvm::Function *F = Intrinsic::getDeclaration(TheModule,
                                                  Intrinsic::x86_sse2_psrl_dq);
        Result = Builder.CreateCall(F, &Ops[0], &Ops[0] + 2, "palignr");
        return true;
      }

      // If palignr is shifting the pair of vectors more than 32 bytes, emit zero.
      Result = Constant::getNullValue(ResultType);
      return true;
    } else {
      error_at(gimple_location(stmt), "mask must be an immediate");
      Result = Ops[0];
      return true;
    }
  }
  case movntdq:
  case movntdq256:
  case movnti:
  case movntpd:
  case movntpd256:
  case movntps:
  case movntps256:
  case movntq:
  case movntsd:
  case movntss: {
    MDNode *Node = MDNode::get(Context, Builder.getInt32(1));

    // Convert the type of the pointer to a pointer to the stored type.
    unsigned AS = cast<PointerType>(Ops[0]->getType())->getAddressSpace();
    Value *Ptr = Builder.CreateBitCast(Ops[0],
                                       PointerType::get(Ops[1]->getType(), AS),
                                       "cast");

    StoreInst *SI = Builder.CreateStore(Ops[1], Ptr);
    SI->setMetadata(TheModule->getMDKindID("nontemporal"), Node);
    SI->setAlignment(16);
    return true;
  }
  case sqrtpd:
  case sqrtpd256:
  case sqrtps:
  case sqrtps256:
  case sqrtsd:
  case sqrtss:
  // No need for a Newton-Raphson step - sqrtps is already accurate.
  case sqrtps_nr:
  case sqrtps_nr256: {
    const Type *Ty = Ops[0]->getType();
    Function *sqrt = Intrinsic::getDeclaration(TheModule, Intrinsic::sqrt, &Ty,
                                               1);
    Result = Builder.CreateCall(sqrt, Ops[0]);
    return true;
  }
  }
  DieAbjectly("Builtin not implemented!", stmt);
  return false;
}

/* These are defined in i386.c */
#define MAX_CLASSES 4
extern "C" enum machine_mode type_natural_mode(tree, CUMULATIVE_ARGS *);
extern "C" int examine_argument(enum machine_mode, const_tree, int, int*, int*);
extern "C" int classify_argument(enum machine_mode, const_tree,
                               enum x86_64_reg_class classes[MAX_CLASSES], int);

/* Target hook for llvm-abi.h. It returns true if an aggregate of the
   specified type should be passed in memory. This is only called for
   x86-64. */
static bool llvm_x86_64_should_pass_aggregate_in_memory(tree TreeType,
                                                        enum machine_mode Mode){
  int IntRegs, SSERegs;
  /* If examine_argument return 0, then it's passed byval in memory.*/
  int ret = examine_argument(Mode, TreeType, 0, &IntRegs, &SSERegs);
  if (ret==0)
    return true;
  if (ret==1 && IntRegs==0 && SSERegs==0)   // zero-sized struct
    return true;
  return false;
}

/* Returns true if all elements of the type are integer types. */
static bool llvm_x86_is_all_integer_types(const Type *Ty) {
  for (Type::subtype_iterator I = Ty->subtype_begin(), E = Ty->subtype_end();
       I != E; ++I) {
    const Type *STy = I->get();
    if (!STy->isIntOrIntVectorTy() && !STy->isPointerTy())
      return false;
  }
  return true;
}

/* Target hook for llvm-abi.h. It returns true if an aggregate of the
   specified type should be passed in a number of registers of mixed types.
   It also returns a vector of types that correspond to the registers used
   for parameter passing. This is only called for x86-32. */
bool
llvm_x86_32_should_pass_aggregate_in_mixed_regs(tree TreeType, const Type *Ty,
                                                std::vector<const Type*> &Elts){
  // If this is a small fixed size type, investigate it.
  HOST_WIDE_INT SrcSize = int_size_in_bytes(TreeType);
  if (SrcSize <= 0 || SrcSize > 16)
    return false;

  // X86-32 passes aggregates on the stack.  If this is an extremely simple
  // aggregate whose elements would be passed the same if passed as scalars,
  // pass them that way in order to promote SROA on the caller and callee side.
  // Note that we can't support passing all structs this way.  For example,
  // {i16, i16} should be passed in on 32-bit unit, which is not how "i16, i16"
  // would be passed as stand-alone arguments.
  const StructType *STy = dyn_cast<StructType>(Ty);
  if (!STy || STy->isPacked()) return false;

  for (unsigned i = 0, e = STy->getNumElements(); i != e; ++i) {
    const Type *EltTy = STy->getElementType(i);
    // 32 and 64-bit integers are fine, as are float and double.  Long double
    // (which can be picked as the type for a union of 16 bytes) is not fine,
    // as loads and stores of it get only 10 bytes.
    if (EltTy == Type::getInt32Ty(Context) ||
        EltTy == Type::getInt64Ty(Context) ||
        EltTy == Type::getFloatTy(Context) ||
        EltTy == Type::getDoubleTy(Context) ||
        EltTy->isPointerTy()) {
      Elts.push_back(EltTy);
      continue;
    }

    // TODO: Vectors are also ok to pass if they don't require extra alignment.
    // TODO: We can also pass structs like {i8, i32}.

    Elts.clear();
    return false;
  }

  return true;
}

/* It returns true if an aggregate of the specified type should be passed as a
   first class aggregate. */
bool llvm_x86_should_pass_aggregate_as_fca(tree type, const Type *Ty) {
  if (TREE_CODE(type) != COMPLEX_TYPE)
    return false;
  const StructType *STy = dyn_cast<StructType>(Ty);
  if (!STy || STy->isPacked()) return false;

  // FIXME: Currently codegen isn't lowering most _Complex types in a way that
  // makes it ABI compatible for x86-64. Same for _Complex char and _Complex
  // short in 32-bit.
  const Type *EltTy = STy->getElementType(0);
  return !((TARGET_64BIT && (EltTy->isIntegerTy() ||
                             EltTy == Type::getFloatTy(Context) ||
                             EltTy == Type::getDoubleTy(Context))) ||
           EltTy->isIntegerTy(16) || EltTy->isIntegerTy(8));
}

/* Target hook for llvm-abi.h. It returns true if an aggregate of the
   specified type should be passed in memory. */
bool llvm_x86_should_pass_aggregate_in_memory(tree TreeType, const Type *Ty) {
  if (llvm_x86_should_pass_aggregate_as_fca(TreeType, Ty))
    return false;

  enum machine_mode Mode = type_natural_mode(TreeType, NULL);
  HOST_WIDE_INT Bytes =
    (Mode == BLKmode) ? int_size_in_bytes(TreeType) : (int) GET_MODE_SIZE(Mode);

  // Zero sized array, struct, or class, not passed in memory.
  if (Bytes == 0)
    return false;

  if (!TARGET_64BIT) {
    std::vector<const Type*> Elts;
    return !llvm_x86_32_should_pass_aggregate_in_mixed_regs(TreeType, Ty, Elts);
  }
  return llvm_x86_64_should_pass_aggregate_in_memory(TreeType, Mode);
}

/* count_num_registers_uses - Return the number of GPRs and XMMs parameter
   register used so far.  Caller is responsible for initializing outputs. */
static void count_num_registers_uses(std::vector<const Type*> &ScalarElts,
                                     unsigned &NumGPRs, unsigned &NumXMMs) {
  for (unsigned i = 0, e = ScalarElts.size(); i != e; ++i) {
    const Type *Ty = ScalarElts[i];
    if (const VectorType *VTy = dyn_cast<VectorType>(Ty)) {
      if (!TARGET_MACHO)
        continue;
      if (VTy->getNumElements() == 1)
        // v1i64 is passed in GPRs on Darwin.
        ++NumGPRs;
      else
        // All other vector scalar values are passed in XMM registers.
        ++NumXMMs;
    } else if (Ty->isIntegerTy() || Ty->isPointerTy()) {
      ++NumGPRs;
    } else if (Ty==Type::getVoidTy(Context)) {
      // Padding bytes that are not passed anywhere
      ;
    } else {
      // Floating point scalar argument.
      assert(Ty->isFloatingPointTy() && Ty->isPrimitiveType() &&
             "Expecting a floating point primitive type!");
      if (Ty->getTypeID() == Type::FloatTyID
          || Ty->getTypeID() == Type::DoubleTyID)
        ++NumXMMs;
    }
  }
}

/* Target hook for llvm-abi.h. This is called when an aggregate is being passed
   in registers. If there are only enough available parameter registers to pass
   part of the aggregate, return true. That means the aggregate should instead
   be passed in memory. */
bool
llvm_x86_64_aggregate_partially_passed_in_regs(std::vector<const Type*> &Elts,
                                         std::vector<const Type*> &ScalarElts,
                                         bool isShadowReturn) {
  // Counting number of GPRs and XMMs used so far. According to AMD64 ABI
  // document: "If there are no registers available for any eightbyte of an
  // argument, the whole  argument is passed on the stack." X86-64 uses 6
  // integer
  // For example, if two GPRs are required but only one is available, then
  // both parts will be in memory.
  // FIXME: This is a temporary solution. To be removed when llvm has first
  // class aggregate values.
  unsigned NumGPRs = isShadowReturn ? 1 : 0;
  unsigned NumXMMs = 0;
  count_num_registers_uses(ScalarElts, NumGPRs, NumXMMs);

  unsigned NumGPRsNeeded = 0;
  unsigned NumXMMsNeeded = 0;
  count_num_registers_uses(Elts, NumGPRsNeeded, NumXMMsNeeded);

  bool GPRsSatisfied = true;
  if (NumGPRsNeeded) {
    if (NumGPRs < 6) {
      if ((NumGPRs + NumGPRsNeeded) > 6)
        // Only partially satisfied.
        return true;
    } else
      GPRsSatisfied = false;
  }

  bool XMMsSatisfied = true;
  if (NumXMMsNeeded) {
    if (NumXMMs < 8) {
      if ((NumXMMs + NumXMMsNeeded) > 8)
        // Only partially satisfied.
        return true;
    } else
      XMMsSatisfied = false;
  }

  return !GPRsSatisfied || !XMMsSatisfied;
}

/* Target hook for llvm-abi.h. It returns true if an aggregate of the
   specified type should be passed in a number of registers of mixed types.
   It also returns a vector of types that correspond to the registers used
   for parameter passing. This is only called for x86-64. */
bool
llvm_x86_64_should_pass_aggregate_in_mixed_regs(tree TreeType, const Type *Ty,
                                                std::vector<const Type*> &Elts){
  if (llvm_x86_should_pass_aggregate_as_fca(TreeType, Ty))
    return false;

  enum x86_64_reg_class Class[MAX_CLASSES];
  enum machine_mode Mode = type_natural_mode(TreeType, NULL);
  bool totallyEmpty = true;
  HOST_WIDE_INT Bytes =
    (Mode == BLKmode) ? int_size_in_bytes(TreeType) : (int) GET_MODE_SIZE(Mode);
  int NumClasses = classify_argument(Mode, TreeType, Class, 0);
  if (!NumClasses)
    return false;

  if (NumClasses == 1 && Class[0] == X86_64_INTEGERSI_CLASS)
    // This will fit in one i32 register.
    return false;

  for (int i = 0; i < NumClasses; ++i) {
    switch (Class[i]) {
    case X86_64_INTEGER_CLASS:
    case X86_64_INTEGERSI_CLASS:
      Elts.push_back(Type::getInt64Ty(Context));
      totallyEmpty = false;
      Bytes -= 8;
      break;
    case X86_64_SSE_CLASS:
      totallyEmpty = false;
      // If it's a SSE class argument, then one of the followings are possible:
      // 1. 1 x SSE, size is 8: 1 x Double.
      // 2. 1 x SSE, size is 4: 1 x Float.
      // 3. 1 x SSE + 1 x SSEUP, size is 16: 1 x <4 x i32>, <4 x f32>,
      //                                         <2 x i64>, or <2 x f64>.
      // 4. 1 x SSE + 1 x SSESF, size is 12: 1 x Double, 1 x Float.
      // 5. 2 x SSE, size is 16: 2 x Double.
      if ((NumClasses-i) == 1) {
        if (Bytes == 8) {
          Elts.push_back(Type::getDoubleTy(Context));
          Bytes -= 8;
        } else if (Bytes == 4) {
          Elts.push_back (Type::getFloatTy(Context));
          Bytes -= 4;
        } else
          assert(0 && "Not yet handled!");
      } else if ((NumClasses-i) == 2) {
        if (Class[i+1] == X86_64_SSEUP_CLASS) {
          const Type *Ty = ConvertType(TreeType);
          if (const StructType *STy = dyn_cast<StructType>(Ty))
            // Look pass the struct wrapper.
            if (STy->getNumElements() == 1)
              Ty = STy->getElementType(0);
          if (const VectorType *VTy = dyn_cast<VectorType>(Ty)) {
            if (VTy->getNumElements() == 2) {
              if (VTy->getElementType()->isIntegerTy()) {
                Elts.push_back(VectorType::get(Type::getInt64Ty(Context), 2));
              } else {
                Elts.push_back(VectorType::get(Type::getDoubleTy(Context), 2));
              }
              Bytes -= 8;
            } else {
              assert(VTy->getNumElements() == 4);
              if (VTy->getElementType()->isIntegerTy()) {
                Elts.push_back(VectorType::get(Type::getInt32Ty(Context), 4));
              } else {
                Elts.push_back(VectorType::get(Type::getFloatTy(Context), 4));
              }
              Bytes -= 4;
            }
          } else if (llvm_x86_is_all_integer_types(Ty)) {
            Elts.push_back(VectorType::get(Type::getInt32Ty(Context), 4));
            Bytes -= 4;
          } else {
            Elts.push_back(VectorType::get(Type::getFloatTy(Context), 4));
            Bytes -= 4;
          }
        } else if (Class[i+1] == X86_64_SSESF_CLASS) {
          assert(Bytes == 12 && "Not yet handled!");
          Elts.push_back(Type::getDoubleTy(Context));
          Elts.push_back(Type::getFloatTy(Context));
          Bytes -= 12;
        } else if (Class[i+1] == X86_64_SSE_CLASS) {
          Elts.push_back(Type::getDoubleTy(Context));
          Elts.push_back(Type::getDoubleTy(Context));
          Bytes -= 16;
        } else if (Class[i+1] == X86_64_SSEDF_CLASS && Bytes == 16) {
          Elts.push_back(VectorType::get(Type::getFloatTy(Context), 2));
          Elts.push_back(Type::getDoubleTy(Context));
        } else if (Class[i+1] == X86_64_INTEGER_CLASS) {
          Elts.push_back(VectorType::get(Type::getFloatTy(Context), 2));
          Elts.push_back(Type::getInt64Ty(Context));
        } else if (Class[i+1] == X86_64_NO_CLASS) {
          // padding bytes, don't pass
          Elts.push_back(Type::getDoubleTy(Context));
          Elts.push_back(Type::getVoidTy(Context));
          Bytes -= 16;
        } else
          assert(0 && "Not yet handled!");
        ++i; // Already handled the next one.
      } else
        assert(0 && "Not yet handled!");
      break;
    case X86_64_SSESF_CLASS:
      totallyEmpty = false;
      Elts.push_back(Type::getFloatTy(Context));
      Bytes -= 4;
      break;
    case X86_64_SSEDF_CLASS:
      totallyEmpty = false;
      Elts.push_back(Type::getDoubleTy(Context));
      Bytes -= 8;
      break;
    case X86_64_X87_CLASS:
    case X86_64_X87UP_CLASS:
    case X86_64_COMPLEX_X87_CLASS:
      return false;
    case X86_64_NO_CLASS:
      // Padding bytes that are not passed (unless the entire object consists
      // of padding)
      Elts.push_back(Type::getVoidTy(Context));
      Bytes -= 8;
      break;
    default: assert(0 && "Unexpected register class!");
    }
  }

  return !totallyEmpty;
}

/* On Darwin x86-32, vectors which are not MMX nor SSE should be passed as
   integers.  On Darwin x86-64, such vectors bigger than 128 bits should be
   passed in memory (byval). */
bool llvm_x86_should_pass_vector_in_integer_regs(tree type) {
  if (!TARGET_MACHO)
    return false;
  if (TREE_CODE(type) == VECTOR_TYPE &&
      TYPE_SIZE(type) &&
      TREE_CODE(TYPE_SIZE(type))==INTEGER_CST) {
    if (TREE_INT_CST_LOW(TYPE_SIZE(type))==64 && TARGET_MMX)
      return false;
    if (TREE_INT_CST_LOW(TYPE_SIZE(type))==128 && TARGET_SSE)
      return false;
    if (TARGET_64BIT && TREE_INT_CST_LOW(TYPE_SIZE(type)) > 128)
      return false;
  }
  return true;
}

/* On Darwin x86-64, vectors which are bigger than 128 bits should be passed
   byval (in memory).  */
bool llvm_x86_should_pass_vector_using_byval_attr(tree type) {
  if (!TARGET_MACHO)
    return false;
  if (!TARGET_64BIT)
    return false;
  if (TREE_CODE(type) == VECTOR_TYPE &&
      TYPE_SIZE(type) &&
      TREE_CODE(TYPE_SIZE(type))==INTEGER_CST) {
    if (TREE_INT_CST_LOW(TYPE_SIZE(type))<=128)
      return false;
  }
  return true;
}

/* The MMX vector v1i64 is returned in EAX and EDX on Darwin.  Communicate
    this by returning i64 here.  Likewise, (generic) vectors such as v2i16
    are returned in EAX.
   On Darwin x86-64, v1i64 is returned in RAX and other MMX vectors are
    returned in XMM0.  Judging from comments, this would not be right for
    Win64.  Don't know about Linux.  */
tree llvm_x86_should_return_vector_as_scalar(tree type, bool isBuiltin) {
  if (TARGET_MACHO &&
      !isBuiltin &&
      TREE_CODE(type) == VECTOR_TYPE &&
      TYPE_SIZE(type) &&
      TREE_CODE(TYPE_SIZE(type))==INTEGER_CST) {
    if (TREE_INT_CST_LOW(TYPE_SIZE(type))==64 &&
        TYPE_VECTOR_SUBPARTS(type)==1)
      return uint64_type_node;
    if (TARGET_64BIT && TREE_INT_CST_LOW(TYPE_SIZE(type))==64)
      return double_type_node;
    if (TREE_INT_CST_LOW(TYPE_SIZE(type))==32)
      return uint32_type_node;
  }
  return 0;
}

/* MMX vectors are returned in XMM0 on x86-64 Darwin.  The easiest way to
   communicate this is pretend they're doubles.
   Judging from comments, this would not be right for Win64.  Don't know
   about Linux.  */
tree llvm_x86_should_return_selt_struct_as_scalar(tree type) {
  tree retType = isSingleElementStructOrArray(type, true, false);
  if (!retType || !TARGET_64BIT || !TARGET_MACHO)
    return retType;
  if (TREE_CODE(retType) == VECTOR_TYPE &&
      TYPE_SIZE(retType) &&
      TREE_CODE(TYPE_SIZE(retType))==INTEGER_CST &&
      TREE_INT_CST_LOW(TYPE_SIZE(retType))==64)
    return double_type_node;
  return retType;
}

/* MMX vectors v2i32, v4i16, v8i8, v2f32 are returned using sret on Darwin
   32-bit.  Vectors bigger than 128 are returned using sret.  */
bool llvm_x86_should_return_vector_as_shadow(tree type, bool isBuiltin) {
  if (TARGET_MACHO &&
    !isBuiltin &&
    !TARGET_64BIT &&
    TREE_CODE(type) == VECTOR_TYPE &&
    TYPE_SIZE(type) &&
    TREE_CODE(TYPE_SIZE(type))==INTEGER_CST) {
    if (TREE_INT_CST_LOW(TYPE_SIZE(type))==64 &&
       TYPE_VECTOR_SUBPARTS(type)>1)
      return true;
  }
  if (TREE_INT_CST_LOW(TYPE_SIZE(type))>128)
    return true;
  return false;
}

// llvm_x86_should_not_return_complex_in_memory -  Return true if TYPE
// should be returned using multiple value return instruction.
bool llvm_x86_should_not_return_complex_in_memory(tree type) {

  if (!TARGET_64BIT)
    return false;

  if (TREE_CODE(type) == COMPLEX_TYPE &&
      TREE_INT_CST_LOW(TYPE_SIZE_UNIT(type)) == 32)
    return true;

  return false;
}

// llvm_suitable_multiple_ret_value_type - Return TRUE if return value
// of type TY should be returned using multiple value return instruction.
static bool llvm_suitable_multiple_ret_value_type(const Type *Ty,
                                                  tree TreeType) {

  if (!TARGET_64BIT)
    return false;

  const StructType *STy = dyn_cast<StructType>(Ty);
  if (!STy)
    return false;

  if (llvm_x86_should_not_return_complex_in_memory(TreeType))
    return true;

  // Let gcc specific routine answer the question.
  enum x86_64_reg_class Class[MAX_CLASSES];
  enum machine_mode Mode = type_natural_mode(TreeType, NULL);
  int NumClasses = classify_argument(Mode, TreeType, Class, 0);
  if (NumClasses == 0)
    return false;

  if (NumClasses == 1 &&
      (Class[0] == X86_64_INTEGERSI_CLASS || Class[0] == X86_64_INTEGER_CLASS))
    // This will fit in one i64 register.
    return false;

  if (NumClasses == 2 &&
      (Class[0] == X86_64_NO_CLASS || Class[1] == X86_64_NO_CLASS))
    // One word is padding which is not passed at all; treat this as returning
    // the scalar type of the other word.
    return false;

  // Otherwise, use of multiple value return is OK.
  return true;
}

// llvm_x86_scalar_type_for_struct_return - Return LLVM type if TYPE
// can be returned as a scalar, otherwise return NULL.
const Type *llvm_x86_scalar_type_for_struct_return(tree type, unsigned *Offset) {
  *Offset = 0;
  const Type *Ty = ConvertType(type);
  unsigned Size = getTargetData().getTypeAllocSize(Ty);
  if (Size == 0)
    return Type::getVoidTy(Context);
  else if (Size == 1)
    return Type::getInt8Ty(Context);
  else if (Size == 2)
    return Type::getInt16Ty(Context);
  else if (Size <= 4)
    return Type::getInt32Ty(Context);

  // Check if Ty should be returned using multiple value return instruction.
  if (llvm_suitable_multiple_ret_value_type(Ty, type))
    return NULL;

  if (TARGET_64BIT) {
    // This logic relies on llvm_suitable_multiple_ret_value_type to have
    // removed anything not expected here.
    enum x86_64_reg_class Class[MAX_CLASSES];
    enum machine_mode Mode = type_natural_mode(type, NULL);
    int NumClasses = classify_argument(Mode, type, Class, 0);
    if (NumClasses == 0)
      return Type::getInt64Ty(Context);

    if (NumClasses == 1) {
      if (Class[0] == X86_64_INTEGERSI_CLASS ||
          Class[0] == X86_64_INTEGER_CLASS) {
        // one int register
        HOST_WIDE_INT Bytes =
          (Mode == BLKmode) ? int_size_in_bytes(type) :
                              (int) GET_MODE_SIZE(Mode);
        if (Bytes>4)
          return Type::getInt64Ty(Context);
        else if (Bytes>2)
          return Type::getInt32Ty(Context);
        else if (Bytes>1)
          return Type::getInt16Ty(Context);
        else
          return Type::getInt8Ty(Context);
      }
      assert(0 && "Unexpected type!");
    }
    if (NumClasses == 2) {
      if (Class[1] == X86_64_NO_CLASS) {
        if (Class[0] == X86_64_INTEGER_CLASS ||
            Class[0] == X86_64_NO_CLASS ||
            Class[0] == X86_64_INTEGERSI_CLASS)
          return Type::getInt64Ty(Context);
        else if (Class[0] == X86_64_SSE_CLASS || Class[0] == X86_64_SSEDF_CLASS)
          return Type::getDoubleTy(Context);
        else if (Class[0] == X86_64_SSESF_CLASS)
          return Type::getFloatTy(Context);
        assert(0 && "Unexpected type!");
      }
      if (Class[0] == X86_64_NO_CLASS) {
        *Offset = 8;
        if (Class[1] == X86_64_INTEGERSI_CLASS ||
            Class[1] == X86_64_INTEGER_CLASS)
          return Type::getInt64Ty(Context);
        else if (Class[1] == X86_64_SSE_CLASS || Class[1] == X86_64_SSEDF_CLASS)
          return Type::getDoubleTy(Context);
        else if (Class[1] == X86_64_SSESF_CLASS)
          return Type::getFloatTy(Context);
        assert(0 && "Unexpected type!");
      }
      assert(0 && "Unexpected type!");
    }
    assert(0 && "Unexpected type!");
  } else {
    if (Size <= 8)
      return Type::getInt64Ty(Context);
    else if (Size <= 16)
      return IntegerType::get(Context, 128);
    else if (Size <= 32)
      return IntegerType::get(Context, 256);
  }
  return NULL;
}

/// llvm_x86_64_get_multiple_return_reg_classes - Find register classes used
/// to return Ty. It is expected that Ty requires multiple return values.
/// This routine uses GCC implementation to find required register classes.
/// The original implementation of this routine is based on
/// llvm_x86_64_should_pass_aggregate_in_mixed_regs code.
void
llvm_x86_64_get_multiple_return_reg_classes(tree TreeType, const Type * /*Ty*/,
                                            std::vector<const Type*> &Elts) {
  enum x86_64_reg_class Class[MAX_CLASSES];
  enum machine_mode Mode = type_natural_mode(TreeType, NULL);
  HOST_WIDE_INT Bytes =
    (Mode == BLKmode) ? int_size_in_bytes(TreeType) : (int) GET_MODE_SIZE(Mode);
  int NumClasses = classify_argument(Mode, TreeType, Class, 0);
  if (!NumClasses)
     assert(0 && "This type does not need multiple return registers!");

  if (NumClasses == 1 && Class[0] == X86_64_INTEGERSI_CLASS)
    // This will fit in one i32 register.
     assert(0 && "This type does not need multiple return registers!");

  if (NumClasses == 1 && Class[0] == X86_64_INTEGER_CLASS)
     assert(0 && "This type does not need multiple return registers!");

  // classify_argument uses a single X86_64_NO_CLASS as a special case for
  // empty structs. Recognize it and don't add any return values in that
  // case.
  if (NumClasses == 1 && Class[0] == X86_64_NO_CLASS)
     return;

  for (int i = 0; i < NumClasses; ++i) {
    switch (Class[i]) {
    case X86_64_INTEGER_CLASS:
    case X86_64_INTEGERSI_CLASS:
      Elts.push_back(Type::getInt64Ty(Context));
      Bytes -= 8;
      break;
    case X86_64_SSE_CLASS:
      // If it's a SSE class argument, then one of the followings are possible:
      // 1. 1 x SSE, size is 8: 1 x Double.
      // 2. 1 x SSE, size is 4: 1 x Float.
      // 3. 1 x SSE + 1 x SSEUP, size is 16: 1 x <4 x i32>, <4 x f32>,
      //                                         <2 x i64>, or <2 x f64>.
      // 4. 1 x SSE + 1 x SSESF, size is 12: 1 x Double, 1 x Float.
      // 5. 2 x SSE, size is 16: 2 x Double.
      // 6. 1 x SSE, 1 x NO:  Second is padding, pass as double.
      if ((NumClasses-i) == 1) {
        if (Bytes == 8) {
          Elts.push_back(Type::getDoubleTy(Context));
          Bytes -= 8;
        } else if (Bytes == 4) {
          Elts.push_back(Type::getFloatTy(Context));
          Bytes -= 4;
        } else
          assert(0 && "Not yet handled!");
      } else if ((NumClasses-i) == 2) {
        if (Class[i+1] == X86_64_SSEUP_CLASS) {
          const Type *Ty = ConvertType(TreeType);
          if (const StructType *STy = dyn_cast<StructType>(Ty))
            // Look pass the struct wrapper.
            if (STy->getNumElements() == 1)
              Ty = STy->getElementType(0);
          if (const VectorType *VTy = dyn_cast<VectorType>(Ty)) {
            if (VTy->getNumElements() == 2) {
              if (VTy->getElementType()->isIntegerTy())
                Elts.push_back(VectorType::get(Type::getInt64Ty(Context), 2));
              else
                Elts.push_back(VectorType::get(Type::getDoubleTy(Context), 2));
              Bytes -= 8;
            } else {
              assert(VTy->getNumElements() == 4);
              if (VTy->getElementType()->isIntegerTy())
                Elts.push_back(VectorType::get(Type::getInt32Ty(Context), 4));
              else
                Elts.push_back(VectorType::get(Type::getFloatTy(Context), 4));
              Bytes -= 4;
            }
          } else if (llvm_x86_is_all_integer_types(Ty)) {
            Elts.push_back(VectorType::get(Type::getInt32Ty(Context), 4));
            Bytes -= 4;
          } else {
            Elts.push_back(VectorType::get(Type::getFloatTy(Context), 4));
            Bytes -= 4;
          }
        } else if (Class[i+1] == X86_64_SSESF_CLASS) {
          assert(Bytes == 12 && "Not yet handled!");
          Elts.push_back(Type::getDoubleTy(Context));
          Elts.push_back(Type::getFloatTy(Context));
          Bytes -= 12;
        } else if (Class[i+1] == X86_64_SSE_CLASS) {
          Elts.push_back(Type::getDoubleTy(Context));
          Elts.push_back(Type::getDoubleTy(Context));
          Bytes -= 16;
        } else if (Class[i+1] == X86_64_SSEDF_CLASS && Bytes == 16) {
          Elts.push_back(VectorType::get(Type::getFloatTy(Context), 2));
          Elts.push_back(Type::getDoubleTy(Context));
        } else if (Class[i+1] == X86_64_INTEGER_CLASS) {
          Elts.push_back(VectorType::get(Type::getFloatTy(Context), 2));
          Elts.push_back(Type::getInt64Ty(Context));
        } else if (Class[i+1] == X86_64_NO_CLASS) {
          Elts.push_back(Type::getDoubleTy(Context));
          Bytes -= 16;
        } else {
          assert(0 && "Not yet handled!");
        }
        ++i; // Already handled the next one.
      } else
        assert(0 && "Not yet handled!");
      break;
    case X86_64_SSESF_CLASS:
      Elts.push_back(Type::getFloatTy(Context));
      Bytes -= 4;
      break;
    case X86_64_SSEDF_CLASS:
      Elts.push_back(Type::getDoubleTy(Context));
      Bytes -= 8;
      break;
    case X86_64_X87_CLASS:
    case X86_64_X87UP_CLASS:
    case X86_64_COMPLEX_X87_CLASS:
      Elts.push_back(Type::getX86_FP80Ty(Context));
      break;
    case X86_64_NO_CLASS:
      // padding bytes.
      Elts.push_back(Type::getInt64Ty(Context));
      break;
    default: assert(0 && "Unexpected register class!");
    }
  }
}

// Return LLVM Type if TYPE can be returned as an aggregate,
// otherwise return NULL.
const Type *llvm_x86_aggr_type_for_struct_return(tree type) {
  const Type *Ty = ConvertType(type);
  if (!llvm_suitable_multiple_ret_value_type(Ty, type))
    return NULL;

  const StructType *STy = cast<StructType>(Ty);
  std::vector<const Type *> ElementTypes;

  // Special handling for _Complex.
  if (llvm_x86_should_not_return_complex_in_memory(type)) {
    ElementTypes.push_back(Type::getX86_FP80Ty(Context));
    ElementTypes.push_back(Type::getX86_FP80Ty(Context));
    return StructType::get(Context, ElementTypes, STy->isPacked());
  }

  std::vector<const Type*> GCCElts;
  llvm_x86_64_get_multiple_return_reg_classes(type, Ty, GCCElts);
  return StructType::get(Context, GCCElts, false);
}

// llvm_x86_extract_mrv_array_element - Helper function that help extract
// an array element from multiple return value.
//
// Here, SRC is returning multiple values. DEST's DESTFIELNO field is an array.
// Extract SRCFIELDNO's ELEMENO value and store it in DEST's FIELDNO field's
// ELEMENTNO.
//
static void llvm_x86_extract_mrv_array_element(Value *Src, Value *Dest,
                                               unsigned SrcFieldNo,
                                               unsigned SrcElemNo,
                                               unsigned DestFieldNo,
                                               unsigned DestElemNo,
                                               LLVMBuilder &Builder,
                                               bool isVolatile) {
  Value *EVI = Builder.CreateExtractValue(Src, SrcFieldNo, "mrv_gr");
  const StructType *STy = cast<StructType>(Src->getType());
  llvm::Value *Idxs[3];
  Idxs[0] = ConstantInt::get(llvm::Type::getInt32Ty(Context), 0);
  Idxs[1] = ConstantInt::get(llvm::Type::getInt32Ty(Context), DestFieldNo);
  Idxs[2] = ConstantInt::get(llvm::Type::getInt32Ty(Context), DestElemNo);
  Value *GEP = Builder.CreateGEP(Dest, Idxs, Idxs+3, "mrv_gep");
  if (STy->getElementType(SrcFieldNo)->isVectorTy()) {
    Value *ElemIndex = ConstantInt::get(Type::getInt32Ty(Context), SrcElemNo);
    Value *EVIElem = Builder.CreateExtractElement(EVI, ElemIndex, "mrv");
    Builder.CreateStore(EVIElem, GEP, isVolatile);
  } else {
    Builder.CreateStore(EVI, GEP, isVolatile);
  }
}

// llvm_x86_extract_multiple_return_value - Extract multiple values returned
// by SRC and store them in DEST. It is expected thaty SRC and
// DEST types are StructType, but they may not match.
void llvm_x86_extract_multiple_return_value(Value *Src, Value *Dest,
                                            bool isVolatile,
                                            LLVMBuilder &Builder) {

  const StructType *STy = cast<StructType>(Src->getType());
  unsigned NumElements = STy->getNumElements();

  const PointerType *PTy = cast<PointerType>(Dest->getType());
  const StructType *DestTy = cast<StructType>(PTy->getElementType());

  unsigned SNO = 0;
  unsigned DNO = 0;

  if (DestTy->getNumElements() == 3
      && DestTy->getElementType(0)->getTypeID() == Type::FloatTyID
      && DestTy->getElementType(1)->getTypeID() == Type::FloatTyID
      && DestTy->getElementType(2)->getTypeID() == Type::FloatTyID) {
    // DestTy is { float, float, float }
    // STy is { <4 x float>, float > }

    Value *EVI = Builder.CreateExtractValue(Src, 0, "mrv_gr");

    Value *E0Index = ConstantInt::get(Type::getInt32Ty(Context), 0);
    Value *EVI0 = Builder.CreateExtractElement(EVI, E0Index, "mrv.v");
    Value *GEP0 = Builder.CreateStructGEP(Dest, 0, "mrv_gep");
    Builder.CreateStore(EVI0, GEP0, isVolatile);

    Value *E1Index = ConstantInt::get(Type::getInt32Ty(Context), 1);
    Value *EVI1 = Builder.CreateExtractElement(EVI, E1Index, "mrv.v");
    Value *GEP1 = Builder.CreateStructGEP(Dest, 1, "mrv_gep");
    Builder.CreateStore(EVI1, GEP1, isVolatile);

    Value *GEP2 = Builder.CreateStructGEP(Dest, 2, "mrv_gep");
    Value *EVI2 = Builder.CreateExtractValue(Src, 1, "mrv_gr");
    Builder.CreateStore(EVI2, GEP2, isVolatile);
    return;
  }

  while (SNO < NumElements) {

    const Type *DestElemType = DestTy->getElementType(DNO);

    // Directly access first class values using getresult.
    if (DestElemType->isSingleValueType()) {
      Value *GEP = Builder.CreateStructGEP(Dest, DNO, "mrv_gep");
      Value *EVI = Builder.CreateExtractValue(Src, SNO, "mrv_gr");
      Builder.CreateStore(EVI, GEP, isVolatile);
      ++DNO; ++SNO;
      continue;
    }

    // Special treatement for _Complex.
    if (DestElemType->isStructTy()) {
      llvm::Value *Idxs[3];
      Idxs[0] = ConstantInt::get(llvm::Type::getInt32Ty(Context), 0);
      Idxs[1] = ConstantInt::get(llvm::Type::getInt32Ty(Context), DNO);

      Idxs[2] = ConstantInt::get(llvm::Type::getInt32Ty(Context), 0);
      Value *GEP = Builder.CreateGEP(Dest, Idxs, Idxs+3, "mrv_gep");
      Value *EVI = Builder.CreateExtractValue(Src, 0, "mrv_gr");
      Builder.CreateStore(EVI, GEP, isVolatile);
      ++SNO;

      Idxs[2] = ConstantInt::get(llvm::Type::getInt32Ty(Context), 1);
      GEP = Builder.CreateGEP(Dest, Idxs, Idxs+3, "mrv_gep");
      EVI = Builder.CreateExtractValue(Src, 1, "mrv_gr");
      Builder.CreateStore(EVI, GEP, isVolatile);
      ++DNO; ++SNO;
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
        if (SElemTy->getElementType()->getTypeID() == Type::FloatTyID
            && Size == 4)
          // Ignore last two <4 x float> elements.
          Size = 2;
      }
      while (i < Size) {
        llvm_x86_extract_mrv_array_element(Src, Dest, SNO, i++,
                                           DNO, DElemNo++,
                                           Builder, isVolatile);
      }
      // Consumed this src field. Try next one.
      ++SNO;
    }
    // Finished building current dest field.
    ++DNO;
  }
}

/// llvm_x86_should_pass_aggregate_in_integer_regs - x86-32 is same as the
/// default.  x86-64 detects the case where a type is 16 bytes long but
/// only 8 of them are passed, the rest being padding (*size is set to 8
/// to identify this case).  It also pads out the size to that of a full
/// register.  This means we'll be loading bytes off the end of the object
/// in some cases.  That's what gcc does, so it must be OK, right?  Right?
bool llvm_x86_should_pass_aggregate_in_integer_regs(tree type, unsigned *size,
                                                    bool *DontCheckAlignment) {
  *size = 0;
  if (TARGET_64BIT) {
    enum x86_64_reg_class Class[MAX_CLASSES];
    enum machine_mode Mode = type_natural_mode(type, NULL);
    int NumClasses = classify_argument(Mode, type, Class, 0);
    *DontCheckAlignment= true;
    if (NumClasses == 1 && (Class[0] == X86_64_INTEGER_CLASS ||
                            Class[0] == X86_64_INTEGERSI_CLASS)) {
      // one int register
      HOST_WIDE_INT Bytes =
        (Mode == BLKmode) ? int_size_in_bytes(type) : (int) GET_MODE_SIZE(Mode);
      if (Bytes>4)
        *size = 8;
      else if (Bytes>2)
        *size = 4;
      else
        *size = Bytes;
      return true;
    }
    if (NumClasses == 2 && (Class[0] == X86_64_INTEGERSI_CLASS ||
                            Class[0] == X86_64_INTEGER_CLASS)) {
      if (Class[1] == X86_64_INTEGER_CLASS) {
        // 16 byte object, 2 int registers
        *size = 16;
        return true;
      }
      // IntegerSI can occur only as element 0.
      if (Class[1] == X86_64_NO_CLASS) {
        // 16 byte object, only 1st register has information
        *size = 8;
        return true;
      }
    }
    return false;
  }
  else
    return !isSingleElementStructOrArray(type, false, true);
}
