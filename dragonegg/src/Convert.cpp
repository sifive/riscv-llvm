//===------------- Convert.cpp - Converting gimple to LLVM IR -------------===//
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
// This is the code that converts GCC AST nodes into LLVM code.
//===----------------------------------------------------------------------===//

// Plugin headers
#include "dragonegg/ABI.h"
#include "dragonegg/Aliasing.h"
#include "dragonegg/ConstantConversion.h"
#include "dragonegg/Debug.h"
#include "dragonegg/TypeConversion.h"

// LLVM headers
#include "llvm/MDBuilder.h"
#include "llvm/Module.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/Debug.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"

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

#include "diagnostic.h"
#include "except.h"
#include "flags.h"
#if (GCC_MINOR > 6)
#include "gimple-pretty-print.h"
#endif
#include "langhooks.h"
#include "output.h"
#include "rtl.h"
#include "target.h" // For targetm.
#include "tm_p.h"
#include "toplev.h"
#include "tree-flow.h"
#include "tree-pass.h"

#if (GCC_MINOR < 6)
extern enum machine_mode reg_raw_mode[FIRST_PSEUDO_REGISTER];
#else
// TODO: Submit a GCC patch to install "regs.h" as a plugin header.
struct target_regs {
  unsigned char x_hard_regno_nregs[FIRST_PSEUDO_REGISTER][MAX_MACHINE_MODE];
  enum machine_mode x_reg_raw_mode[FIRST_PSEUDO_REGISTER];
};

extern struct target_regs default_target_regs;

#define reg_raw_mode (default_target_regs.x_reg_raw_mode)
#endif

#if (GCC_MINOR == 6)
extern void debug_gimple_stmt(union gimple_statement_d *);
#endif

#ifndef ENABLE_BUILD_WITH_CXX
} // extern "C"
#endif

// Trees header.
#include "dragonegg/Trees.h"

static LLVMContext &Context = getGlobalContext();

STATISTIC(NumBasicBlocks, "Number of basic blocks converted");
STATISTIC(NumStatements,  "Number of gimple statements converted");

/// getPointerAlignment - Return the alignment in bytes of exp, a pointer valued
/// expression, or 1 if the alignment is not known.
static unsigned int getPointerAlignment(tree exp) {
  assert(isa<ACCESS_TYPE>(TREE_TYPE (exp)) && "Expected a pointer type!");
  unsigned int align =
#if (GCC_MINOR < 7)
    get_pointer_alignment(exp, BIGGEST_ALIGNMENT);
#else
    get_pointer_alignment(exp);
#endif
  return align >= 8 ? align / 8 : 1;
}

/// getSSAPlaceholder - A fake value associated with an SSA name when the name
/// is used before being defined (this can occur because basic blocks are not
/// output in dominator order).  Replaced with the correct value when the SSA
/// name's definition is encountered.
static Value *GetSSAPlaceholder(Type *Ty) {
  // Cannot use a constant, since there is no way to distinguish a fake value
  // from a real value.  So use an instruction with no parent.  This needs to
  // be an instruction that can return a struct type, since the SSA name might
  // be a complex number.  It could be a PHINode, except that the GCC phi node
  // conversion logic also constructs phi nodes with no parent.  A SelectInst
  // would work, but a LoadInst seemed neater.
  return new LoadInst(UndefValue::get(Ty->getPointerTo()), NULL);
}

/// isSSAPlaceholder - Whether this is a fake value being used as a placeholder
/// for the definition of an SSA name.
static bool isSSAPlaceholder(Value *V) {
  LoadInst *LI = dyn_cast<LoadInst>(V);
  return LI && !LI->getParent();
}

/// NameValue - Try to name the given value after the given GCC tree node.  If
/// the GCC tree node has no sensible name then it does nothing.  If the value
/// already has a name then it is not changed.
static void NameValue(Value *V, tree t) {
  if (!V->hasName()) {
    const std::string &Name = getDescriptiveName(t);
    if (!Name.empty())
      V->setName(Name);
  }
}

/// SelectFPName - Helper for choosing a name depending on whether a floating
/// point type is float, double or long double.  Returns an empty string for
/// other types, such as the x86 128 bit floating point type.
static StringRef SelectFPName(tree type, StringRef FloatName,
                              StringRef DoubleName, StringRef LongDoubleName) {
  assert(isa<REAL_TYPE>(type) && "Expected a floating point type!");
  if (TYPE_MODE(type) == TYPE_MODE(float_type_node))
    return FloatName;
  if (TYPE_MODE(type) == TYPE_MODE(double_type_node))
    return DoubleName;
  if (TYPE_MODE(type) == TYPE_MODE(long_double_type_node))
    return LongDoubleName;
  return StringRef();
}

/// DisplaceLocationByUnits - Move a memory location by a fixed number of units.
/// This uses an "inbounds" getelementptr, so the displacement should remain
/// inside the original object.
MemRef DisplaceLocationByUnits(MemRef Loc, int32_t Offset,
                               LLVMBuilder &Builder) {
  // Convert to a byte pointer and displace by the offset.
  unsigned AddrSpace = Loc.Ptr->getType()->getPointerAddressSpace();
  Type *UnitPtrTy = GetUnitPointerType(Context, AddrSpace);
  Value *Ptr = Builder.CreateBitCast(Loc.Ptr, UnitPtrTy);
  Ptr = Builder.CreateConstInBoundsGEP1_32(Ptr, Offset,
                                           flag_verbose_asm ? "dsplc" : "");
  Ptr = Builder.CreateBitCast(Ptr, Loc.Ptr->getType());
  uint32_t Align = MinAlign(Loc.getAlignment(), Offset);
  return MemRef(Ptr, Align, Loc.Volatile);
}

/// LoadFromLocation - Load a value of the given type from a memory location.
static LoadInst *LoadFromLocation(MemRef Loc, Type *Ty, MDNode *AliasTag,
                                  LLVMBuilder &Builder) {
  unsigned AddrSpace = Loc.Ptr->getType()->getPointerAddressSpace();
  Value *Ptr = Builder.CreateBitCast(Loc.Ptr, Ty->getPointerTo(AddrSpace));
  LoadInst *LI = Builder.CreateAlignedLoad(Ptr, Loc.getAlignment(),
                                           Loc.Volatile);
  if (AliasTag)
    LI->setMetadata(LLVMContext::MD_tbaa, AliasTag);
  return LI;
}

/// StoreToLocation - Store a value to the given memory location.
static StoreInst *StoreToLocation(Value *V, MemRef Loc, MDNode *AliasTag,
                                  LLVMBuilder &Builder) {
  Type *Ty = V->getType();
  unsigned AddrSpace = Loc.Ptr->getType()->getPointerAddressSpace();
  Value *Ptr = Builder.CreateBitCast(Loc.Ptr, Ty->getPointerTo(AddrSpace));
  StoreInst *SI = Builder.CreateAlignedStore(V, Ptr,  Loc.getAlignment(),
                                             Loc.Volatile);
  if (AliasTag)
    SI->setMetadata(LLVMContext::MD_tbaa, AliasTag);
  return SI;
}

/// Mem2Reg - Convert a value of in-memory type (that given by ConvertType)
/// to in-register type (that given by getRegType).  TODO: Eliminate these
/// methods: "memory" values should never be held in registers.  Currently
/// this is mainly used for marshalling function parameters and return values,
/// but that should be completely independent of the reg vs mem value logic.
static Value *Mem2Reg(Value *V, tree type, LLVMBuilder &Builder) {
  Type *MemTy = V->getType();
  Type *RegTy = getRegType(type);
  assert(MemTy == ConvertType(type) && "Not of memory type!");

  if (MemTy == RegTy)
    return V;

  if (RegTy->isIntegerTy()) {
    assert(MemTy->isIntegerTy() && "Type mismatch!");
    return Builder.CreateIntCast(V, RegTy, /*isSigned*/!TYPE_UNSIGNED(type));
  }

  if (RegTy->isPointerTy()) {
    assert(MemTy->isPointerTy() && "Type mismatch!");
    return Builder.CreateBitCast(V, RegTy);
  }

  if (RegTy->isStructTy()) {
    assert(isa<COMPLEX_TYPE>(type) && "Expected a complex type!");
    assert(MemTy->isStructTy() && "Type mismatch!");
    Value *RealPart = Builder.CreateExtractValue(V, 0);
    Value *ImagPart = Builder.CreateExtractValue(V, 1);
    RealPart = Mem2Reg(RealPart, TREE_TYPE(type), Builder);
    ImagPart = Mem2Reg(ImagPart, TREE_TYPE(type), Builder);
    V = UndefValue::get(RegTy);
    V = Builder.CreateInsertValue(V, RealPart, 0);
    V = Builder.CreateInsertValue(V, ImagPart, 1);
    return V;
  }

  if (RegTy->isVectorTy()) {
    assert(isa<VECTOR_TYPE>(type) && "Expected a vector type!");
    assert(MemTy->isVectorTy() && "Type mismatch!");
    Value *Res = UndefValue::get(RegTy);
    unsigned NumElts = (unsigned)TYPE_VECTOR_SUBPARTS(type);
    for (unsigned i = 0; i != NumElts; ++i) {
      Value *Idx = Builder.getInt32(i);
      Value *Val = Builder.CreateExtractElement(V, Idx);
      Val = Mem2Reg(Val, TREE_TYPE(type), Builder);
      Res = Builder.CreateInsertElement(Res, Val, Idx);
    }
    return Res;
  }

  debug_tree(type);
  llvm_unreachable("Don't know how to turn this into a register!");
}

/// Reg2Mem - Convert a value of in-register type (that given by getRegType)
/// to in-memory type (that given by ConvertType).  TODO: Eliminate this
/// method: "memory" values should never be held in registers.  Currently
/// this is mainly used for marshalling function parameters and return values,
/// but that should be completely independent of the reg vs mem value logic.
static Value *Reg2Mem(Value *V, tree type, LLVMBuilder &Builder) {
  Type *RegTy = V->getType();
  Type *MemTy = ConvertType(type);
  assert(RegTy == getRegType(type) && "Not of register type!");

  if (RegTy == MemTy)
    return V;

  if (MemTy->isIntegerTy()) {
    assert(RegTy->isIntegerTy() && "Type mismatch!");
    return Builder.CreateIntCast(V, MemTy, /*isSigned*/!TYPE_UNSIGNED(type));
  }

  if (MemTy->isPointerTy()) {
    assert(RegTy->isPointerTy() && "Type mismatch!");
    return Builder.CreateBitCast(V, MemTy);
  }

  if (MemTy->isStructTy()) {
    assert(isa<COMPLEX_TYPE>(type) && "Expected a complex type!");
    assert(RegTy->isStructTy() && "Type mismatch!");
    Value *RealPart = Builder.CreateExtractValue(V, 0);
    Value *ImagPart = Builder.CreateExtractValue(V, 1);
    RealPart = Reg2Mem(RealPart, TREE_TYPE(type), Builder);
    ImagPart = Reg2Mem(ImagPart, TREE_TYPE(type), Builder);
    Value *Z = UndefValue::get(MemTy);
    Z = Builder.CreateInsertValue(Z, RealPart, 0);
    Z = Builder.CreateInsertValue(Z, ImagPart, 1);
    return Z;
  }

  if (MemTy->isVectorTy()) {
    assert(isa<VECTOR_TYPE>(type) && "Expected a vector type!");
    assert(RegTy->isVectorTy() && "Type mismatch!");
    Value *Res = UndefValue::get(MemTy);
    unsigned NumElts = (unsigned)TYPE_VECTOR_SUBPARTS(type);
    for (unsigned i = 0; i != NumElts; ++i) {
      Value *Idx = Builder.getInt32(i);
      Value *Val = Builder.CreateExtractElement(V, Idx);
      Val = Reg2Mem(Val, TREE_TYPE(type), Builder);
      Res = Builder.CreateInsertElement(Res, Val, Idx);
    }
    return Res;
  }

  debug_tree(type);
  llvm_unreachable("Don't know how to turn this into memory!");
}

/// describeTypeRange - Return metadata describing the set of possible values
/// that an in-memory variable of the given GCC type can take on.
static MDNode *describeTypeRange(tree type) {
  if (!isa<INTEGRAL_TYPE>(type)) return 0; // Only discrete types have ranges.

  // The range of possible values is TYPE_MIN_VALUE .. TYPE_MAX_VALUE.
  tree min = TYPE_MIN_VALUE(type);
  assert(min && isa<INTEGER_CST>(min) && "Min not a constant!");
  tree max = TYPE_MAX_VALUE(type);
  assert(max && isa<INTEGER_CST>(max) && "Max not a constant!");

  unsigned BitWidth = GET_MODE_BITSIZE(TYPE_MODE(type));

  APInt Lo = getAPIntValue(min, BitWidth);
  APInt Hi = getAPIntValue(max, BitWidth);

  // Unlike GCC's, LLVM ranges do not include the upper end point.
  ++Hi;

  MDBuilder MDHelper(Context);
  return MDHelper.createRange(Lo, Hi);
}

/// isDirectMemoryAccessSafe - Whether directly storing/loading a value of the
/// given register type generates the correct in-memory representation for the
/// type.  Eg, if a 32 bit wide integer type has only one bit of precision then
/// the register type is i1 and the in-memory type i32.  Storing an i1 directly
/// to memory would not properly set up all 32 in-memory bits, thus this method
/// would return false.
static bool isDirectMemoryAccessSafe(Type *RegTy, tree type) {
  assert(RegTy == getRegType(type) && "Wrong register type!");

  switch (TREE_CODE(type)) {
  default:
    debug_tree(type);
    llvm_unreachable("Unknown register type!");

  case BOOLEAN_TYPE:
  case ENUMERAL_TYPE:
  case INTEGER_TYPE:
    assert(RegTy->isIntegerTy() && "Expected an integer type!");
    return RegTy->getIntegerBitWidth() == GET_MODE_BITSIZE(TYPE_MODE(type));

  case COMPLEX_TYPE:
  case VECTOR_TYPE: {
    assert((!isa<COMPLEX_TYPE>(type) || RegTy->isStructTy()) &&
           "Expected a struct type!");
    assert((!isa<VECTOR_TYPE>(type) || RegTy->isVectorTy()) &&
           "Expected a vector type!");
    tree elt_type = main_type(type);
    Type *EltRegTy = getRegType(elt_type);
    // Check that fields are safe to access directly.
    if (!isDirectMemoryAccessSafe(EltRegTy, elt_type))
      return false;
    // Check that the field positions agree with GCC.
    unsigned StrideBits = GET_MODE_BITSIZE(TYPE_MODE(elt_type));
    return getDataLayout().getTypeAllocSizeInBits(EltRegTy) == StrideBits;
  }

  case OFFSET_TYPE:
    assert(RegTy->isIntegerTy() && "Expected an integer type!");
    return true;

  case POINTER_TYPE:
  case REFERENCE_TYPE:
    assert(RegTy->isPointerTy() && "Expected a pointer type!");
    return true;

  case REAL_TYPE:
    assert(RegTy->isFloatingPointTy() && "Expected a floating point type!");
    // NOTE: This might be wrong for floats with precision less than their alloc
    // size on big-endian machines.
    return true;
  }
}

/// LoadRegisterFromMemory - Loads a value of the given scalar GCC type from
/// the memory location pointed to by Loc.  Takes care of adjusting for any
/// differences between in-memory and in-register types (the returned value
/// is of in-register type, as returned by getRegType).
static Value *LoadRegisterFromMemory(MemRef Loc, tree type, MDNode *AliasTag,
                                     LLVMBuilder &Builder) {
  // NOTE: Needs to be kept in sync with getRegType.
  Type *RegTy = getRegType(type);

  // If loading the register type directly out of memory gives the right result,
  // then just do that.
  if (isDirectMemoryAccessSafe(RegTy, type)) {
    LoadInst *LI = LoadFromLocation(Loc, RegTy, AliasTag, Builder);
    MDNode *Range = describeTypeRange(type);
    if (Range)
      LI->setMetadata(LLVMContext::MD_range, Range);
    return LI;
  }

  // There is a discrepancy between the in-register type and the in-memory type.
  switch (TREE_CODE(type)) {
  default:
    debug_tree(type);
    llvm_unreachable("Unexpected type mismatch!");

  case BOOLEAN_TYPE:
  case ENUMERAL_TYPE:
  case INTEGER_TYPE: {
    // For integral types, load an integer with size equal to the mode size,
    // then truncate down to the precision.  For example, when extracting a bool
    // this probably first loads out an i8 or i32 which is then truncated to i1.
    // This roundabout approach means we get the right result on both little and
    // big endian machines.
    unsigned Size = GET_MODE_BITSIZE(TYPE_MODE(type));
    Type *MemTy = IntegerType::get(Context, Size);
    LoadInst *LI = LoadFromLocation(Loc, MemTy, AliasTag, Builder);
    MDNode *Range = describeTypeRange(type);
    if (Range)
      LI->setMetadata(LLVMContext::MD_range, Range);
    return Builder.CreateTruncOrBitCast(LI, RegTy);
  }

  case COMPLEX_TYPE: {
    // Load the complex number component by component.
    tree elt_type = main_type(type);
    unsigned Stride = GET_MODE_SIZE(TYPE_MODE(elt_type));
    Value *RealPart = LoadRegisterFromMemory(Loc, elt_type, AliasTag, Builder);
    Loc = DisplaceLocationByUnits(Loc, Stride, Builder);
    Value *ImagPart = LoadRegisterFromMemory(Loc, elt_type, AliasTag, Builder);
    Value *Res = UndefValue::get(RegTy);
    Res = Builder.CreateInsertValue(Res, RealPart, 0);
    Res = Builder.CreateInsertValue(Res, ImagPart, 1);
    return Res;
  }

  case VECTOR_TYPE: {
    tree elt_type = main_type(type);
    Type *EltRegTy = getRegType(elt_type);
    unsigned NumElts = TYPE_VECTOR_SUBPARTS(type);
    unsigned Size = GET_MODE_BITSIZE(TYPE_MODE(elt_type));
    // If, say, the register type is a vector of i1 but memory is laid out as a
    // vector of i32 then load an i32 vector out and truncate to a vector of i1.
    if (EltRegTy->isIntegerTy() && EltRegTy->getIntegerBitWidth() != Size) {
      // See if changing the element type to an integer with size equal to the
      // mode size gives a vector type that corresponds to the in-memory layout.
      Type *MemTy = IntegerType::get(Context, Size);
      if (getDataLayout().getTypeAllocSizeInBits(MemTy) == Size) {
        // It does!  Load out the memory as a vector of that type then truncate
        // to the register size.
        Type *MemVecTy = VectorType::get(MemTy, NumElts);
        LoadInst *LI = LoadFromLocation(Loc, MemVecTy, AliasTag, Builder);
        return Builder.CreateTruncOrBitCast(LI, RegTy);
      }
    }
    // Otherwise, load the vector component by component.
    Value *Res = UndefValue::get(RegTy);
    unsigned Stride = GET_MODE_SIZE(TYPE_MODE(elt_type));
    for (unsigned i = 0; i != NumElts; ++i) {
      Value *Idx = Builder.getInt32(i);
      Value *Elt = LoadRegisterFromMemory(Loc, elt_type, AliasTag, Builder);
      Res = Builder.CreateInsertElement(Res, Elt, Idx);
      if (i + 1 != NumElts)
        Loc = DisplaceLocationByUnits(Loc, Stride, Builder);
    }
    return Res;
  }
  }
}

/// StoreRegisterToMemory - Stores the given value to the memory pointed to by
/// Loc.  Takes care of adjusting for any differences between the value's type
/// (which is the in-register type given by getRegType) and the in-memory type.
static void StoreRegisterToMemory(Value *V, MemRef Loc, tree type,
                                  MDNode *AliasTag, LLVMBuilder &Builder) {
  // NOTE: Needs to be kept in sync with getRegType.
  assert(V->getType() == getRegType(type) && "Not of register type!");

  // If storing the register directly to memory gives the right result, then
  // just do that.
  if (isDirectMemoryAccessSafe(V->getType(), type)) {
    StoreToLocation(V, Loc, AliasTag, Builder);
    return;
  }

  // There is a discrepancy between the in-register type and the in-memory type.
  switch (TREE_CODE(type)) {
  default:
    debug_tree(type);
    llvm_unreachable("Unexpected type mismatch!");

  case BOOLEAN_TYPE:
  case ENUMERAL_TYPE:
  case INTEGER_TYPE: {
    // For integral types extend to an integer with size equal to the mode size.
    // For example, when inserting a bool this probably extends it to an i8 or
    // to an i32.  This approach means we get the right result on both little
    // and big endian machines.
    unsigned Size = GET_MODE_BITSIZE(TYPE_MODE(type));
    Type *MemTy = IntegerType::get(Context, Size);
    V = Builder.CreateIntCast(V, MemTy, /*isSigned*/!TYPE_UNSIGNED(type));
    StoreToLocation(V, Loc, AliasTag, Builder);
    break;
  }

  case COMPLEX_TYPE: {
    // Store the complex number component by component.
    tree elt_type = main_type(type);
    unsigned Stride = GET_MODE_SIZE(TYPE_MODE(elt_type));
    Value *RealPart = Builder.CreateExtractValue(V, 0);
    Value *ImagPart = Builder.CreateExtractValue(V, 1);
    StoreRegisterToMemory(RealPart, Loc, elt_type, AliasTag, Builder);
    Loc = DisplaceLocationByUnits(Loc, Stride, Builder);
    StoreRegisterToMemory(ImagPart, Loc, elt_type, AliasTag, Builder);
    break;
  }

  case VECTOR_TYPE: {
    tree elt_type = main_type(type);
    Type *EltRegTy = getRegType(elt_type);
    unsigned NumElts = TYPE_VECTOR_SUBPARTS(type);
    unsigned Size = GET_MODE_BITSIZE(TYPE_MODE(elt_type));
    // If, say, the register type is a vector of i1 but memory is laid out as a
    // vector of i32 then extend the i1 vector to an i32 vector and store that.
    if (EltRegTy->isIntegerTy() && EltRegTy->getIntegerBitWidth() != Size) {
      // See if changing the element type to an integer with size equal to the
      // mode size gives a vector type that corresponds to the in-memory layout.
      Type *MemTy = IntegerType::get(Context, Size);
      if (getDataLayout().getTypeAllocSizeInBits(MemTy) == Size) {
        // It does!  Extend the register value to a vector of that type then
        // store it to memory.
        Type *MemVecTy = VectorType::get(MemTy, NumElts);
        V = Builder.CreateIntCast(V, MemVecTy,
                                  /*isSigned*/!TYPE_UNSIGNED(elt_type));
        StoreToLocation(V, Loc, AliasTag, Builder);
        break;
      }
    }
    // Otherwise, store the vector component by component.
    unsigned Stride = GET_MODE_SIZE(TYPE_MODE(elt_type));
    for (unsigned i = 0; i != NumElts; ++i) {
      Value *Idx = Builder.getInt32(i);
      Value *Elt = Builder.CreateExtractElement(V, Idx);
      StoreRegisterToMemory(Elt, Loc, elt_type, AliasTag, Builder);
      if (i + 1 != NumElts)
        Loc = DisplaceLocationByUnits(Loc, Stride, Builder);
    }
    break;
  }
  }
}


//===----------------------------------------------------------------------===//
//                         ... High-Level Methods ...
//===----------------------------------------------------------------------===//

/// TheTreeToLLVM - Keep track of the current function being compiled.
TreeToLLVM *TheTreeToLLVM = 0;

const DataLayout &getDataLayout() {
  return *TheTarget->getDataLayout();
}

/// EmitDebugInfo - Return true if debug info is to be emitted for current
/// function.
bool TreeToLLVM::EmitDebugInfo() {
  if (TheDebugInfo && !DECL_IGNORED_P(getFUNCTION_DECL()))
    return true;
  return false;
}

TreeToLLVM::TreeToLLVM(tree fndecl) :
    DL(getDataLayout()), Builder(Context, *TheFolder) {
  FnDecl = fndecl;
  AllocaInsertionPoint = 0;
  Fn = 0;
  ReturnBB = 0;
  ReturnOffset = 0;

  if (EmitDebugInfo()) {
    expanded_location Location = expand_location(DECL_SOURCE_LOCATION (fndecl));

    if (Location.file) {
      TheDebugInfo->setLocationFile(Location.file);
      TheDebugInfo->setLocationLine(Location.line);
    } else {
      TheDebugInfo->setLocationFile("");
      TheDebugInfo->setLocationLine(0);
    }
  }

  assert(TheTreeToLLVM == 0 && "Reentering function creation?");
  TheTreeToLLVM = this;
}

TreeToLLVM::~TreeToLLVM() {
  TheTreeToLLVM = 0;
}

//===----------------------------------------------------------------------===//
//                         ... Local declarations ...
//===----------------------------------------------------------------------===//

/// isLocalDecl - Whether this declaration is local to the current function.
static bool isLocalDecl(tree decl) {
  if (isa<CONST_DECL>(decl)) return false;
  assert(HAS_RTL_P(decl) && "Expected a declaration with RTL!");
  return
    // GCC bug workaround: RESULT_DECL may not have DECL_CONTEXT set in thunks.
    (!DECL_CONTEXT(decl) && isa<RESULT_DECL>(decl)) ||
    // Usual case.
    (DECL_CONTEXT(decl) == current_function_decl &&
     !DECL_EXTERNAL(decl) && // External variables are not local.
     !TREE_STATIC(decl) && // Static variables not considered local.
     !isa<FUNCTION_DECL>(decl)); // Nested functions not considered local.
}

/// set_decl_local - Remember the LLVM value for a GCC declaration.
Value *TreeToLLVM::set_decl_local(tree decl, Value *V) {
  if (!isLocalDecl(decl))
    return SET_DECL_LLVM(decl, V);
  if (V != NULL)
    return LocalDecls[decl] = V;
  LocalDecls.erase(decl);
  return NULL;
}

/// get_decl_local - Retrieve the LLVM value for a GCC declaration, or NULL.
Value *TreeToLLVM::get_decl_local(tree decl) {
  if (!isLocalDecl(decl))
    return get_decl_llvm(decl);
  DenseMap<tree, AssertingVH<Value> >::iterator I = LocalDecls.find(decl);
  if (I != LocalDecls.end())
    return I->second;
  return NULL;
}

/// make_decl_local - Return the LLVM value for a GCC declaration if it exists.
/// Otherwise creates and returns an appropriate value.
Value *TreeToLLVM::make_decl_local(tree decl) {
  if (!isLocalDecl(decl))
    return make_decl_llvm(decl);

  DenseMap<tree, AssertingVH<Value> >::iterator I = LocalDecls.find(decl);
  if (I != LocalDecls.end())
    return I->second;

  switch (TREE_CODE(decl)) {
  default:
    debug_tree(decl);
    llvm_unreachable("Unhandled local declaration!");

  case RESULT_DECL:
  case VAR_DECL:
    EmitAutomaticVariableDecl(decl);
    I = LocalDecls.find(decl);
    assert(I != LocalDecls.end() && "Not a local variable?");
    return I->second;
  }
}

/// make_definition_local - Ensure that the body or initial value of the given
/// GCC declaration will be output, and return a declaration for it.
Value *TreeToLLVM::make_definition_local(tree decl) {
  if (!isLocalDecl(decl))
    return make_definition_llvm(decl);
  return make_decl_local(decl);
}

/// llvm_store_scalar_argument - Store scalar argument ARGVAL of type
/// LLVMTY at location LOC.
static void llvm_store_scalar_argument(Value *Loc, Value *ArgVal,
                                       llvm::Type *LLVMTy,
                                       unsigned RealSize,
                                       LLVMBuilder &Builder) {
  if (RealSize) {
    // Not clear what this is supposed to do on big endian machines...
    assert(!BYTES_BIG_ENDIAN && "Unsupported case - please report");
    // Do byte wise store because actual argument type does not match LLVMTy.
    assert(ArgVal->getType()->isIntegerTy() && "Expected an integer value!");
    Type *StoreType = IntegerType::get(Context, RealSize * 8);
    Loc = Builder.CreateBitCast(Loc, StoreType->getPointerTo());
    if (ArgVal->getType()->getPrimitiveSizeInBits() >=
        StoreType->getPrimitiveSizeInBits())
      ArgVal = Builder.CreateTrunc(ArgVal, StoreType);
    else
      ArgVal = Builder.CreateZExt(ArgVal, StoreType);
    Builder.CreateStore(ArgVal, Loc);
  } else {
    // This cast only involves pointers, therefore BitCast.
    Loc = Builder.CreateBitCast(Loc, LLVMTy->getPointerTo());
    // FIXME: Pass down the alignment so we can do better than using 1 here.
    Builder.CreateAlignedStore(ArgVal, Loc, 1);
  }
}

#ifndef LLVM_STORE_SCALAR_ARGUMENT
#define LLVM_STORE_SCALAR_ARGUMENT(LOC,ARG,TYPE,SIZE,BUILDER)   \
  llvm_store_scalar_argument((LOC),(ARG),(TYPE),(SIZE),(BUILDER))
#endif

// This is true for types whose alignment when passed on the stack is less
// than the alignment of the type.
#define LLVM_BYVAL_ALIGNMENT_TOO_SMALL(T) \
   (LLVM_BYVAL_ALIGNMENT(T) && LLVM_BYVAL_ALIGNMENT(T) < TYPE_ALIGN_UNIT(T))

namespace {
  /// FunctionPrologArgumentConversion - This helper class is driven by the ABI
  /// definition for this target to figure out how to retrieve arguments from
  /// the stack/regs coming into a function and store them into an appropriate
  /// alloca for the argument.
  struct FunctionPrologArgumentConversion : public DefaultABIClient {
    tree FunctionDecl;
    Function::arg_iterator &AI;
    LLVMBuilder Builder;
    std::vector<Value*> LocStack;
    std::vector<std::string> NameStack;
    CallingConv::ID &CallingConv;
    unsigned Offset;
    bool isShadowRet;
    FunctionPrologArgumentConversion(tree FnDecl,
                                     Function::arg_iterator &ai,
                                     const LLVMBuilder &B, CallingConv::ID &CC)
      : FunctionDecl(FnDecl), AI(ai), Builder(B), CallingConv(CC), Offset(0),
        isShadowRet(false) {}

    /// getCallingConv - This provides the desired CallingConv for the function.
    CallingConv::ID getCallingConv(void) { return CallingConv; }

    void HandlePad(llvm::Type * /*LLVMTy*/) {
      ++AI;
    }

    bool isShadowReturn() const {
      return isShadowRet;
    }
    void setName(const std::string &Name) {
      NameStack.push_back(Name);
    }
    void setLocation(Value *Loc) {
      LocStack.push_back(Loc);
    }
    void clear() {
      assert(NameStack.size() == 1 && LocStack.size() == 1 && "Imbalance!");
      NameStack.clear();
      LocStack.clear();
    }

    void HandleAggregateShadowResult(PointerType * /*PtrArgTy*/,
                                     bool /*RetPtr*/) {
      // If the function returns a structure by value, we transform the function
      // to take a pointer to the result as the first argument of the function
      // instead.
      assert(AI != Builder.GetInsertBlock()->getParent()->arg_end() &&
             "No explicit return value?");
      AI->setName("agg.result");

      isShadowRet = true;
      tree ResultDecl = DECL_RESULT(FunctionDecl);
      tree RetTy = TREE_TYPE(TREE_TYPE(FunctionDecl));
      if (TREE_CODE(RetTy) == TREE_CODE(TREE_TYPE(ResultDecl))) {
        TheTreeToLLVM->set_decl_local(ResultDecl, AI);
        ++AI;
        return;
      }

      // Otherwise, this must be something returned with NRVO.
      assert(isa<REFERENCE_TYPE>(TREE_TYPE(ResultDecl)) &&
             "Not type match and not passing by reference?");
      // Create an alloca for the ResultDecl.
      Value *Tmp = TheTreeToLLVM->CreateTemporary(AI->getType());
      Builder.CreateStore(AI, Tmp);

      TheTreeToLLVM->set_decl_local(ResultDecl, Tmp);
      if (TheDebugInfo && !DECL_IGNORED_P(FunctionDecl)) {
        TheDebugInfo->EmitDeclare(ResultDecl,
                                  dwarf::DW_TAG_return_variable,
                                  "agg.result", RetTy, Tmp,
                                  Builder);
      }
      ++AI;
    }

    void HandleScalarShadowResult(PointerType * /*PtrArgTy*/,
                                  bool /*RetPtr*/) {
      assert(AI != Builder.GetInsertBlock()->getParent()->arg_end() &&
             "No explicit return value?");
      AI->setName("scalar.result");
      isShadowRet = true;
      TheTreeToLLVM->set_decl_local(DECL_RESULT(FunctionDecl), AI);
      ++AI;
    }

    void HandleScalarArgument(llvm::Type *LLVMTy, tree /*type*/,
                              unsigned RealSize = 0) {
      Value *ArgVal = AI;
      if (ArgVal->getType() != LLVMTy) {
        if (ArgVal->getType()->isPointerTy() && LLVMTy->isPointerTy()) {
          // If this is GCC being sloppy about pointer types, insert a bitcast.
          // See PR1083 for an example.
          ArgVal = Builder.CreateBitCast(ArgVal, LLVMTy);
        } else if (ArgVal->getType()->isDoubleTy()) {
          // If this is a K&R float parameter, it got promoted to double. Insert
          // the truncation to float now.
          ArgVal = Builder.CreateFPTrunc(ArgVal, LLVMTy, NameStack.back());
        } else {
          // If this is just a mismatch between integer types, this is due
          // to K&R prototypes, where the forward proto defines the arg as int
          // and the actual impls is a short or char.
          assert(ArgVal->getType()->isIntegerTy(32) && LLVMTy->isIntegerTy() &&
                 "Lowerings don't match?");
          ArgVal = Builder.CreateTrunc(ArgVal, LLVMTy,NameStack.back());
        }
      }
      assert(!LocStack.empty());
      Value *Loc = LocStack.back();
      LLVM_STORE_SCALAR_ARGUMENT(Loc,ArgVal,LLVMTy,RealSize,Builder);
      AI->setName(NameStack.back());
      ++AI;
    }

    void HandleByValArgument(llvm::Type * /*LLVMTy*/, tree type) {
      if (LLVM_BYVAL_ALIGNMENT_TOO_SMALL(type)) {
        // Incoming object on stack is insufficiently aligned for the type.
        // Make a correctly aligned copy.
        assert(!LocStack.empty());
        Value *Loc = LocStack.back();
        // We cannot use field-by-field copy here; x86 long double is 16
        // bytes, but only 10 are copied.  If the object is really a union
        // we might need the other bytes.  We must also be careful to use
        // the smaller alignment.
        Type *SBP = Type::getInt8PtrTy(Context);
        Type *IntPtr = getDataLayout().getIntPtrType(Context, 0);
        Value *Ops[5] = {
          Builder.CreateCast(Instruction::BitCast, Loc, SBP),
          Builder.CreateCast(Instruction::BitCast, AI, SBP),
          ConstantInt::get(IntPtr,
                           TREE_INT_CST_LOW(TYPE_SIZE_UNIT(type))),
          Builder.getInt32(LLVM_BYVAL_ALIGNMENT(type)),
          Builder.getFalse()
        };
        Type *ArgTypes[3] = {SBP, SBP, IntPtr };
        Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                     Intrinsic::memcpy,
                                                     ArgTypes), Ops);

        AI->setName(NameStack.back());
      }
      ++AI;
    }

    void HandleFCAArgument(llvm::Type * /*LLVMTy*/, tree /*type*/) {
      // Store the FCA argument into alloca.
      assert(!LocStack.empty());
      Value *Loc = LocStack.back();
      Builder.CreateStore(AI, Loc);
      AI->setName(NameStack.back());
      ++AI;
    }

    void HandleAggregateResultAsScalar(Type * /*ScalarTy*/,
                                       unsigned Off = 0) {
      this->Offset = Off;
    }

    void EnterField(unsigned FieldNo, llvm::Type *StructTy) {
      NameStack.push_back(NameStack.back()+"."+utostr(FieldNo));

      Value *Loc = LocStack.back();
      // This cast only involves pointers, therefore BitCast.
      Loc = Builder.CreateBitCast(Loc, StructTy->getPointerTo());

      Loc = Builder.CreateStructGEP(Loc, FieldNo,
                                    flag_verbose_asm ? "ntr" : "");
      LocStack.push_back(Loc);
    }
    void ExitField() {
      NameStack.pop_back();
      LocStack.pop_back();
    }
  };
}

// isPassedByVal - Return true if an aggregate of the specified type will be
// passed in memory byval.
static bool isPassedByVal(tree type, Type *Ty,
                          std::vector<Type*> &ScalarArgs,
                          bool isShadowRet, CallingConv::ID CC) {
  (void)type; (void)Ty; (void)ScalarArgs; (void)isShadowRet;
  (void)CC; // Not used by all ABI macros.
  if (LLVM_SHOULD_PASS_AGGREGATE_USING_BYVAL_ATTR(type, Ty))
    return true;

  std::vector<Type*> Args;
  if (LLVM_SHOULD_PASS_AGGREGATE_IN_MIXED_REGS(type, Ty, CC, Args) &&
      LLVM_AGGREGATE_PARTIALLY_PASSED_IN_REGS(Args, ScalarArgs, isShadowRet,
                                              CC))
    // We want to pass the whole aggregate in registers but only some of the
    // registers are available.
    return true;
  return false;
}

void TreeToLLVM::StartFunctionBody() {
  // TODO: Add support for dropping the leading '\1' in order to support
  //   unsigned bswap(unsigned) __asm__("llvm.bswap");
  // This would also require adjustments in make_decl_llvm.

  // Determine the FunctionType and calling convention for this function.
  tree static_chain = cfun->static_chain_decl;
  FunctionType *FTy;
  CallingConv::ID CallingConv;
  AttrListPtr PAL;

  // If this is a K&R-style function: with a type that takes no arguments but
  // with arguments none the less, then calculate the LLVM type from the list
  // of arguments.
  if (flag_functions_from_args || (TYPE_ARG_TYPES(TREE_TYPE(FnDecl)) == 0 &&
                                   DECL_ARGUMENTS(FnDecl))) {
    SmallVector<tree, 8> Args;
    for (tree Arg = DECL_ARGUMENTS(FnDecl); Arg; Arg = TREE_CHAIN(Arg))
      Args.push_back(Arg);
    FTy = ConvertArgListToFnType(TREE_TYPE(FnDecl), Args, static_chain,
                                 !flag_functions_from_args, CallingConv, PAL);
  } else {
    // Otherwise, just get the type from the function itself.
    FTy = ConvertFunctionType(TREE_TYPE(FnDecl), FnDecl, static_chain,
                              CallingConv, PAL);
  }

  // If we've already seen this function and created a prototype, and if the
  // proto has the right LLVM type, just use it.
  if (DECL_LOCAL_SET_P(FnDecl) &&
      cast<PointerType>(DECL_LOCAL(FnDecl)->getType())->getElementType()==FTy) {
    Fn = cast<Function>(DECL_LOCAL(FnDecl));
    assert(Fn->getCallingConv() == CallingConv &&
           "Calling convention disagreement between prototype and impl!");
    // The visibility can be changed from the last time we've seen this
    // function. Set to current.
    handleVisibility(FnDecl, Fn);
  } else {
    std::string Name = getLLVMAssemblerName(FnDecl);
    Function *FnEntry = TheModule->getFunction(Name);
    if (FnEntry) {
      assert(FnEntry->getName() == Name && "Same entry, different name?");
      assert((FnEntry->isDeclaration() ||
              FnEntry->getLinkage() == Function::AvailableExternallyLinkage) &&
             "Multiple fns with same name and neither are external!");
      FnEntry->setName("");  // Clear name to avoid conflicts.
      assert(FnEntry->getCallingConv() == CallingConv &&
             "Calling convention disagreement between prototype and impl!");
    }

    // Otherwise, either it exists with the wrong type or it doesn't exist.  In
    // either case create a new function.
    Fn = Function::Create(FTy, Function::ExternalLinkage, Name, TheModule);
    assert(Fn->getName() == Name && "Preexisting fn with the same name!");
    Fn->setCallingConv(CallingConv);
    Fn->setAttributes(PAL);

    // If a previous proto existed with the wrong type, replace any uses of it
    // with the actual function and delete the proto.
    if (FnEntry) {
      FnEntry->replaceAllUsesWith
        (TheFolder->CreateBitCast(Fn, FnEntry->getType()));
      changeLLVMConstant(FnEntry, Fn);
      FnEntry->eraseFromParent();
    }
    SET_DECL_LOCAL(FnDecl, Fn);
  }

  // The function should not already have a body.
  assert(Fn->empty() && "Function expanded multiple times!");

  // Compute the linkage that the function should get.
  if (false) {//FIXME DECL_LLVM_PRIVATE(FnDecl)) {
    Fn->setLinkage(Function::PrivateLinkage);
  } else if (false) {//FIXME DECL_LLVM_LINKER_PRIVATE(FnDecl)) {
    Fn->setLinkage(Function::LinkerPrivateLinkage);
  } else if (!TREE_PUBLIC(FnDecl) /*|| lang_hooks.llvm_is_in_anon(subr)*/) {
    Fn->setLinkage(Function::InternalLinkage);
  } else if (DECL_COMDAT(FnDecl)) {
    Fn->setLinkage(Function::getLinkOnceLinkage(flag_odr));
  } else if (DECL_WEAK(FnDecl)) {
    // The user may have explicitly asked for weak linkage - ignore flag_odr.
    Fn->setLinkage(Function::WeakAnyLinkage);
  } else if (DECL_ONE_ONLY(FnDecl)) {
    Fn->setLinkage(Function::getWeakLinkage(flag_odr));
  } else if (DECL_EXTERNAL(FnDecl)) {
    Fn->setLinkage(Function::AvailableExternallyLinkage);
  }

#ifdef TARGET_ADJUST_LLVM_LINKAGE
  TARGET_ADJUST_LLVM_LINKAGE(Fn,FnDecl);
#endif /* TARGET_ADJUST_LLVM_LINKAGE */

  Fn->setUnnamedAddr(!TREE_ADDRESSABLE(FnDecl));

  // Handle visibility style
  handleVisibility(FnDecl, Fn);

  // Register constructors and destructors.
  if (DECL_STATIC_CONSTRUCTOR(FnDecl))
    register_ctor_dtor(Fn, DECL_INIT_PRIORITY(FnDecl), true);
  if (DECL_STATIC_DESTRUCTOR(FnDecl))
    register_ctor_dtor(Fn, DECL_FINI_PRIORITY(FnDecl), false);

  // Handle attribute "aligned".
  if (DECL_ALIGN(FnDecl) != FUNCTION_BOUNDARY)
    Fn->setAlignment(DECL_ALIGN(FnDecl) / 8);

  // Handle functions in specified sections.
  if (DECL_SECTION_NAME(FnDecl))
    Fn->setSection(TREE_STRING_POINTER(DECL_SECTION_NAME(FnDecl)));

  // Handle used Functions
  if (lookup_attribute ("used", DECL_ATTRIBUTES (FnDecl)))
    AttributeUsedGlobals.insert(Fn);

  // Handle noinline Functions
  if (lookup_attribute ("noinline", DECL_ATTRIBUTES (FnDecl)))
    Fn->addFnAttr(Attributes::NoInline);

  // Handle always_inline attribute
  if (lookup_attribute ("always_inline", DECL_ATTRIBUTES (FnDecl)))
    Fn->addFnAttr(Attributes::AlwaysInline);

  // Pass inline keyword to optimizer.
  if (DECL_DECLARED_INLINE_P(FnDecl))
    Fn->addFnAttr(Attributes::InlineHint);

  if (optimize_size)
    Fn->addFnAttr(Attributes::OptimizeForSize);

  // Handle stack smashing protection.
  if (flag_stack_protect == 1)
    Fn->addFnAttr(Attributes::StackProtect);
  else if (flag_stack_protect == 2)
    Fn->addFnAttr(Attributes::StackProtectReq);

  // Handle naked attribute
  if (lookup_attribute ("naked", DECL_ATTRIBUTES (FnDecl)))
    Fn->addFnAttr(Attributes::Naked);

  // Handle annotate attributes
  if (DECL_ATTRIBUTES(FnDecl))
    AddAnnotateAttrsToGlobal(Fn, FnDecl);

  // Mark the function "nounwind" if not doing exception handling.
  if (!flag_exceptions)
    Fn->setDoesNotThrow();

 if (flag_unwind_tables)
   Fn->setHasUWTable();

  // Create a new basic block for the function.
  BasicBlock *EntryBlock = BasicBlock::Create(Context, "entry", Fn);
  BasicBlocks[ENTRY_BLOCK_PTR] = EntryBlock;
  Builder.SetInsertPoint(EntryBlock);

  if (EmitDebugInfo())
    TheDebugInfo->EmitFunctionStart(FnDecl, Fn);

  // Loop over all of the arguments to the function, setting Argument names and
  // creating argument alloca's for the PARM_DECLs in case their address is
  // exposed.
  Function::arg_iterator AI = Fn->arg_begin();

  // Rename and alloca'ify real arguments.
  FunctionPrologArgumentConversion Client(FnDecl, AI, Builder, CallingConv);
  DefaultABI ABIConverter(Client);

  // Handle the DECL_RESULT.
  ABIConverter.HandleReturnType(TREE_TYPE(TREE_TYPE(FnDecl)), FnDecl,
                                DECL_BUILT_IN(FnDecl));
  // Remember this for use by FinishFunctionBody.
  TheTreeToLLVM->ReturnOffset = Client.Offset;

  // Prepend the static chain (if any) to the list of arguments.
  tree Args = static_chain ? static_chain : DECL_ARGUMENTS(FnDecl);

  // Scalar arguments processed so far.
  std::vector<Type*> ScalarArgs;
  while (Args) {
    const char *Name = "unnamed_arg";
    if (DECL_NAME(Args)) Name = IDENTIFIER_POINTER(DECL_NAME(Args));

    Type *ArgTy = ConvertType(TREE_TYPE(Args));
    bool isInvRef = isPassedByInvisibleReference(TREE_TYPE(Args));
    if (isInvRef ||
        (ArgTy->isVectorTy() &&
         LLVM_SHOULD_PASS_VECTOR_USING_BYVAL_ATTR(TREE_TYPE(Args)) &&
         !LLVM_BYVAL_ALIGNMENT_TOO_SMALL(TREE_TYPE(Args))) ||
        (!ArgTy->isSingleValueType() &&
         isPassedByVal(TREE_TYPE(Args), ArgTy, ScalarArgs,
                       Client.isShadowReturn(), CallingConv) &&
         !LLVM_BYVAL_ALIGNMENT_TOO_SMALL(TREE_TYPE(Args)))) {
      // If the value is passed by 'invisible reference' or 'byval reference',
      // the l-value for the argument IS the argument itself.  But for byval
      // arguments whose alignment as an argument is less than the normal
      // alignment of the type (examples are x86-32 aggregates containing long
      // double and large x86-64 vectors), we need to make the copy.
      AI->setName(Name);
      SET_DECL_LOCAL(Args, AI);
      if (!isInvRef && EmitDebugInfo())
        TheDebugInfo->EmitDeclare(Args, dwarf::DW_TAG_arg_variable,
                                  Name, TREE_TYPE(Args),
                                  AI, Builder);
      ABIConverter.HandleArgument(TREE_TYPE(Args), ScalarArgs);
    } else {
      // Otherwise, we create an alloca to hold the argument value and provide
      // an l-value.  On entry to the function, we copy formal argument values
      // into the alloca.
      Value *Tmp = CreateTemporary(ArgTy, TYPE_ALIGN_UNIT(TREE_TYPE(Args)));
      Tmp->setName(std::string(Name)+"_addr");
      SET_DECL_LOCAL(Args, Tmp);
      if (EmitDebugInfo()) {
        TheDebugInfo->EmitDeclare(Args, dwarf::DW_TAG_arg_variable,
                                  Name, TREE_TYPE(Args), Tmp,
                                  Builder);
      }

      // Emit annotate intrinsic if arg has annotate attr
      if (DECL_ATTRIBUTES(Args))
        EmitAnnotateIntrinsic(Tmp, Args);

      // Emit gcroot intrinsic if arg has attribute
      if (isa<ACCESS_TYPE>(TREE_TYPE(Args))
          && lookup_attribute ("gcroot", TYPE_ATTRIBUTES(TREE_TYPE(Args))))
        EmitTypeGcroot(Tmp);

      Client.setName(Name);
      Client.setLocation(Tmp);
      ABIConverter.HandleArgument(TREE_TYPE(Args), ScalarArgs);
      Client.clear();
    }

    Args = Args == static_chain ? DECL_ARGUMENTS(FnDecl) : TREE_CHAIN(Args);
  }

  // Loading the value of a PARM_DECL at this point yields its initial value.
  // Remember this for use when materializing the reads implied by SSA default
  // definitions.
  SSAInsertionPoint = Builder.Insert(CastInst::Create(Instruction::BitCast,
                              Constant::getNullValue(Type::getInt32Ty(Context)),
                              Type::getInt32Ty(Context)), "ssa point");

  // If this function has nested functions, we should handle a potential
  // nonlocal_goto_save_area.
  if (cfun->nonlocal_goto_save_area) {
    // Not supported yet.
  }

  if (EmitDebugInfo())
    TheDebugInfo->EmitStopPoint(Builder.GetInsertBlock(), Builder);

  // Ensure that local variables are output in the order that they were declared
  // rather than in the order we come across them. This is only done to make the
  // IR more readable and is not needed for correctness.
  EmitVariablesInScope(DECL_INITIAL(FnDecl));
}

/// EmitVariablesInScope - Output a declaration for every variable in the
/// given scope.
void TreeToLLVM::EmitVariablesInScope(tree scope) {
  for (tree t = BLOCK_VARS(scope); t; t = TREE_CHAIN (t))
    if (isa<VAR_DECL>(t))
      // If this is just the rotten husk of a variable that the gimplifier
      // eliminated all uses of, but is preserving for debug info, ignore it.
      if (!DECL_HAS_VALUE_EXPR_P(t))
        make_decl_local(t);
  // Declare variables in contained scopes.
  for (tree t = BLOCK_SUBBLOCKS (scope); t ; t = BLOCK_CHAIN (t))
    EmitVariablesInScope(t);
}

/// DefineSSAName - Use the given value as the definition of the given SSA name.
/// Returns the provided value as a convenience.
Value *TreeToLLVM::DefineSSAName(tree reg, Value *Val) {
  assert(isa<SSA_NAME>(reg) && "Not an SSA name!");
  if (Value *ExistingValue = SSANames[reg]) {
    if (Val != ExistingValue) {
      assert(isSSAPlaceholder(ExistingValue) && "Multiply defined SSA name!");
      // Replace the placeholder with the value everywhere.  This also updates
      // the map entry, because it is a TrackingVH.
      ExistingValue->replaceAllUsesWith(Val);
      delete ExistingValue;
    }
    return Val;
  }
  return SSANames[reg] = Val;
}

typedef SmallVector<std::pair<BasicBlock*, unsigned>, 8> PredVector;
typedef SmallVector<std::pair<BasicBlock*, tree>, 8> TreeVector;
typedef SmallVector<std::pair<BasicBlock*, Value*>, 8> ValueVector;

/// PopulatePhiNodes - Populate generated phi nodes with their operands.
void TreeToLLVM::PopulatePhiNodes() {
  PredVector Predecessors;
  TreeVector IncomingValues;
  ValueVector PhiArguments;

  for (unsigned Idx = 0, EIdx = (unsigned)PendingPhis.size(); Idx < EIdx;
       ++Idx) {
    // The phi node to process.
    PhiRecord &P = PendingPhis[Idx];

    // Extract the incoming value for each predecessor from the GCC phi node.
    for (unsigned i = 0, e = gimple_phi_num_args(P.gcc_phi); i != e; ++i) {
      // The incoming GCC basic block.
      basic_block bb = gimple_phi_arg_edge(P.gcc_phi, i)->src;

      // The corresponding LLVM basic block.
      DenseMap<basic_block, BasicBlock*>::iterator BI = BasicBlocks.find(bb);
      assert(BI != BasicBlocks.end() && "GCC basic block not output?");

      // The incoming GCC expression.
      tree val = gimple_phi_arg(P.gcc_phi, i)->def;

      // Associate it with the LLVM basic block.
      IncomingValues.push_back(std::make_pair(BI->second, val));

      // Several LLVM basic blocks may be generated when emitting one GCC basic
      // block.  The additional blocks always occur immediately after the main
      // basic block, and can be identified by the fact that they are nameless.
      // Associate the incoming expression with all of them, since any of them
      // may occur as a predecessor of the LLVM basic block containing the phi.
      Function::iterator FI(BI->second), FE = Fn->end();
      for (++FI; FI != FE && !FI->hasName(); ++FI) {
        assert(FI->getSinglePredecessor() == IncomingValues.back().first &&
               "Anonymous block does not continue predecessor!");
        IncomingValues.push_back(std::make_pair(FI, val));
      }
    }

    // Sort the incoming values by basic block to help speed up queries.
    std::sort(IncomingValues.begin(), IncomingValues.end());

    // Get the LLVM predecessors for the basic block containing the phi node,
    // and remember their positions in the list of predecessors (this is used
    // to avoid adding phi operands in a non-deterministic order).
    Predecessors.reserve(gimple_phi_num_args(P.gcc_phi)); // At least this many.
    BasicBlock *PhiBB = P.PHI->getParent();
    unsigned Index = 0;
    for (pred_iterator PI = pred_begin(PhiBB), PE = pred_end(PhiBB); PI != PE;
         ++PI, ++Index)
      Predecessors.push_back(std::make_pair(*PI, Index));

    if (Predecessors.empty()) {
      // FIXME: If this happens then GCC has a control flow edge where LLVM has
      // none - something has gone wrong.  For the moment be laid back about it
      // because the fact we don't yet wire up exception handling code means it
      // happens all the time in Ada and C++.
      P.PHI->replaceAllUsesWith(UndefValue::get(P.PHI->getType()));
      P.PHI->eraseFromParent();
      IncomingValues.clear();
      continue;
    }

    // Sort the predecessors by basic block.  In GCC, each predecessor occurs
    // exactly once.  However in LLVM a predecessor can occur several times,
    // and then every copy of the predecessor must be associated with exactly
    // the same incoming value in the phi node.  Sorting the predecessors groups
    // multiple occurrences together, making this easy to handle.
    std::sort(Predecessors.begin(), Predecessors.end());

    // Now iterate over the predecessors, setting phi operands as we go.
    TreeVector::iterator VI = IncomingValues.begin(), VE = IncomingValues.end();
    PredVector::iterator PI = Predecessors.begin(), PE = Predecessors.end();
    PhiArguments.resize((unsigned)Predecessors.size());
    while (PI != PE) {
      // The predecessor basic block.
      BasicBlock *BB = PI->first;

      // Find the incoming value for this predecessor.
      while (VI != VE && VI->first != BB) ++VI;
      assert(VI != VE && "No value for predecessor!");
      Value *Val = EmitRegister(VI->second);

      // Need to bitcast to the right type (useless_type_conversion_p).  Place
      // the bitcast at the end of the predecessor, before the terminator.
      if (Val->getType() != P.PHI->getType())
        Val = new BitCastInst(Val, P.PHI->getType(), "", BB->getTerminator());

      // Add the phi node arguments for all occurrences of this predecessor.
      do {
        // Place the argument at the position given by PI->second, which is the
        // original position before sorting of the predecessor in the pred list.
        // Since the predecessors were sorted non-deterministically (by pointer
        // value), this ensures that the same bitcode is produced on any run.
        PhiArguments[PI++->second] = std::make_pair(BB, Val);
      } while (PI != PE && PI->first == BB);
    }

    // Add the operands to the phi node.
    for (ValueVector::iterator I = PhiArguments.begin(), E = PhiArguments.end();
         I != E; ++I)
      P.PHI->addIncoming(I->second, I->first);

    IncomingValues.clear();
    PhiArguments.clear();
    Predecessors.clear();
  }

  PendingPhis.clear();
}

Function *TreeToLLVM::FinishFunctionBody() {
  if (ReturnBB) {
    // Insert the return block at the end of the function.
    BeginBlock(ReturnBB);

    SmallVector <Value *, 4> RetVals;

    // If the function returns a value, get it into a register and return it now.
    if (!Fn->getReturnType()->isVoidTy()) {
      tree TreeRetVal = DECL_RESULT(FnDecl);
      LValue ResultLV = EmitLV(TreeRetVal);
      assert(!ResultLV.isBitfield() && "Bitfields not allowed here!");

      if (!isa<AGGREGATE_TYPE>(TREE_TYPE(TreeRetVal)) &&
          !isa<COMPLEX_TYPE>(TREE_TYPE(TreeRetVal))) {
        // If the DECL_RESULT is a scalar type, just load out the return value
        // and return it.
        LoadInst *Load = Builder.CreateAlignedLoad(ResultLV.Ptr,
                                                   ResultLV.getAlignment());
        RetVals.push_back(Builder.CreateBitCast(Load, Fn->getReturnType()));
      } else {
        uint64_t ResultSize =
          getDataLayout().getTypeAllocSize(ConvertType(TREE_TYPE(TreeRetVal)));
        uint64_t ReturnSize =
          getDataLayout().getTypeAllocSize(Fn->getReturnType());

        // The load does not necessarily start at the beginning of the aggregate
        // (x86-64).
        if (ReturnOffset >= ResultSize) {
          // Also catches the case of an empty return value.
          RetVals.push_back(UndefValue::get(Fn->getReturnType()));
        } else {
          // Advance to the point we want to load from.
          if (ReturnOffset) {
            ResultLV.Ptr =
              Builder.CreateBitCast(ResultLV.Ptr, Type::getInt8PtrTy(Context));
            ResultLV.Ptr =
              Builder.CreateGEP(ResultLV.Ptr,
                                ConstantInt::get(DL.getIntPtrType(Context, 0),
                                                 ReturnOffset),
                                flag_verbose_asm ? "rtvl" : "");
            ResultLV.setAlignment(MinAlign(ResultLV.getAlignment(), ReturnOffset));
            ResultSize -= ReturnOffset;
          }

          // A place to build up the function return value.
          MemRef ReturnLoc = CreateTempLoc(Fn->getReturnType());

          // Copy out DECL_RESULT while being careful to not overrun the source or
          // destination buffers.
          uint64_t OctetsToCopy = std::min(ResultSize, ReturnSize);
          EmitMemCpy(ReturnLoc.Ptr, ResultLV.Ptr, Builder.getInt64(OctetsToCopy),
                     std::min(ReturnLoc.getAlignment(), ResultLV.getAlignment()));

          if (StructType *STy = dyn_cast<StructType>(Fn->getReturnType())) {
            llvm::Value *Idxs[2];
            Idxs[0] = Builder.getInt32(0);
            bool Packed = STy->isPacked();
            for (unsigned ri = 0; ri < STy->getNumElements(); ++ri) {
              Idxs[1] = Builder.getInt32(ri);
              Value *GEP = Builder.CreateGEP(ReturnLoc.Ptr, Idxs,
                                             flag_verbose_asm ? "mrv_gep" : "");
              Value *E = Builder.CreateAlignedLoad(GEP, /*Align*/Packed,
                                                   flag_verbose_asm ? "mrv":"");
              RetVals.push_back(E);
            }
            // If the return type specifies an empty struct then return one.
            if (RetVals.empty())
              RetVals.push_back(UndefValue::get(Fn->getReturnType()));
          } else {
            // Otherwise, this aggregate result must be something that is returned
            // in a scalar register for this target.  We must bit convert the
            // aggregate to the specified scalar type, which we do by casting the
            // pointer and loading.
            RetVals.push_back(Builder.CreateLoad(ReturnLoc.Ptr, "retval"));
          }
        }
      }
    }
    if (RetVals.empty())
      Builder.CreateRetVoid();
    else if (RetVals.size() == 1 && RetVals[0]->getType() == Fn->getReturnType()){
      Builder.CreateRet(RetVals[0]);
    } else {
      assert(Fn->getReturnType()->isAggregateType() && "Return type mismatch!");
      Builder.CreateAggregateRet(RetVals.data(), (unsigned)RetVals.size());
    }
  } else { // !ReturnBB
    BasicBlock *CurBB = Builder.GetInsertBlock();
    if (CurBB->getTerminator() == 0) {
      if (CurBB->getName().empty() && CurBB->begin() == CurBB->end()) {
        // If the previous block has no label and is empty, remove it: it is a
        // post-terminator block.
        CurBB->eraseFromParent();
        Builder.SetInsertPoint(&Fn->getBasicBlockList().back());
      } else {
        // The previous block may contain code but no terminator if it finished
        // with an unsupported GCC builtin.
        Builder.CreateUnreachable();
      }
    }
  }


  // Populate phi nodes with their operands now that all ssa names have been
  // defined and all basic blocks output.
  PopulatePhiNodes();

  // Now that phi nodes have been output, emit pending exception handling code.
  EmitLandingPads();
  EmitFailureBlocks();

  if (ReturnBB) {
    // FIXME: This should be output just before the return call generated above.
    // But because EmitFunctionEnd pops the region stack, that means that if the
    // call to PopulatePhiNodes (for example) generates complicated debug info,
    // then the debug info logic barfs.  Testcases showing this are 20011126-2.c
    // or pr42221.c from the gcc testsuite compiled with -g -O3.
    if (EmitDebugInfo()) {
      TheDebugInfo->EmitStopPoint(ReturnBB, Builder);
      TheDebugInfo->EmitFunctionEnd(true);
    }
  }

#ifdef NDEBUG
  // When processing broken code it can be awkward to ensure that every SSA name
  // that was used has a definition.  So in this case we play it cool and create
  // an artificial definition for such SSA names.  The choice of definition does
  // not matter because the compiler is going to exit with an error anyway.
  if (errorcount || sorrycount)
#else
  // When checks are enabled, complain if an SSA name was used but not defined.
#endif
    for (DenseMap<tree,TrackingVH<Value> >::const_iterator I = SSANames.begin(),
         E = SSANames.end(); I != E; ++I) {
      Value *NameDef = I->second;
      // If this is not a placeholder then the SSA name was defined.
      if (!isSSAPlaceholder(NameDef))
        continue;

      // If an error occurred then replace the placeholder with undef.  Thanks
      // to this we can just bail out on errors, without having to worry about
      // whether we defined every SSA name.
      if (errorcount || sorrycount) {
        NameDef->replaceAllUsesWith(UndefValue::get(NameDef->getType()));
        delete NameDef;
      } else {
        debug_tree(I->first);
        llvm_unreachable("SSA name never defined!");
      }
    }

  return Fn;
}

/// getBasicBlock - Find or create the LLVM basic block corresponding to BB.
BasicBlock *TreeToLLVM::getBasicBlock(basic_block bb) {
  // If we already associated an LLVM basic block with BB, then return it.
  DenseMap<basic_block, BasicBlock*>::iterator I = BasicBlocks.find(bb);
  if (I != BasicBlocks.end())
    return I->second;

  // Otherwise, create a new LLVM basic block.
  BasicBlock *BB = BasicBlock::Create(Context);

  // All basic blocks that directly correspond to GCC basic blocks (those
  // created here) must have a name.  All artificial basic blocks produced
  // while generating code must be nameless.  That way, artificial blocks
  // can be easily identified.

  // Give the basic block a name.  If the user specified -fverbose-asm then
  // use the same naming scheme as GCC.
  if (flag_verbose_asm) {
    // If BB contains labels, name the LLVM basic block after the first label.
    gimple stmt = first_stmt(bb);
    if (stmt && gimple_code(stmt) == GIMPLE_LABEL) {
      tree label = gimple_label_label(stmt);
      const std::string &LabelName = getDescriptiveName(label);
      if (!LabelName.empty())
        BB->setName("<" + LabelName + ">");
    } else {
      // When there is no label, use the same name scheme as the GCC tree dumps.
      Twine Index(bb->index);
      BB->setName("<bb " + Index + ">");
    }
  } else {
    Twine Index(bb->index);
    BB->setName(Index);
  }

  return BasicBlocks[bb] = BB;
}

/// getLabelDeclBlock - Lazily get and create a basic block for the specified
/// label.
BasicBlock *TreeToLLVM::getLabelDeclBlock(tree LabelDecl) {
  assert(isa<LABEL_DECL>(LabelDecl) && "Isn't a label!?");
  if (DECL_LOCAL_SET_P(LabelDecl))
    return cast<BasicBlock>(DECL_LOCAL(LabelDecl));

  basic_block bb = label_to_block(LabelDecl);
  if (!bb) {
    sorry("address of a non-local label");
    bb = ENTRY_BLOCK_PTR; // Do not crash.
  }

  BasicBlock *BB = getBasicBlock(bb);
  SET_DECL_LOCAL(LabelDecl, BB);
  return BB;
}

void TreeToLLVM::EmitBasicBlock(basic_block bb) {
  location_t saved_loc = input_location;
  ++NumBasicBlocks;

  // Avoid outputting a pointless branch at the end of the entry block.
  if (bb != ENTRY_BLOCK_PTR)
    BeginBlock(getBasicBlock(bb));

  // Create an LLVM phi node for each GCC phi and define the associated ssa name
  // using it.  Do not populate with operands at this point since some ssa names
  // the phi uses may not have been defined yet - phis are special this way.
  for (gimple_stmt_iterator gsi = gsi_start_phis(bb); !gsi_end_p(gsi);
       gsi_next(&gsi)) {
    gimple gcc_phi = gsi_stmt(gsi);
    // Skip virtual operands.
    if (!is_gimple_reg(gimple_phi_result(gcc_phi)))
      continue;

    // Create the LLVM phi node.
    Type *Ty = getRegType(TREE_TYPE(gimple_phi_result(gcc_phi)));
    PHINode *PHI = Builder.CreatePHI(Ty, gimple_phi_num_args(gcc_phi));

    // The phi defines the associated ssa name.
    tree name = gimple_phi_result(gcc_phi);
    assert(isa<SSA_NAME>(name) && "PHI result not an SSA name!");
    if (flag_verbose_asm)
      NameValue(PHI, name);
    DefineSSAName(name, PHI);

    // The phi operands will be populated later - remember the phi node.
    PhiRecord P = { gcc_phi, PHI };
    PendingPhis.push_back(P);
  }

  // Render statements.
  for (gimple_stmt_iterator gsi = gsi_start_bb(bb); !gsi_end_p(gsi);
       gsi_next(&gsi)) {
    gimple stmt = gsi_stmt(gsi);
    input_location = gimple_location(stmt);
    ++NumStatements;

    if (EmitDebugInfo()) {
      if (gimple_has_location(stmt)) {
        TheDebugInfo->setLocationFile(gimple_filename(stmt));
        TheDebugInfo->setLocationLine(gimple_lineno(stmt));
      } else {
        TheDebugInfo->setLocationFile("");
        TheDebugInfo->setLocationLine(0);
      }
      TheDebugInfo->EmitStopPoint(Builder.GetInsertBlock(), Builder);
    }

    switch (gimple_code(stmt)) {
    default:
      debug_gimple_stmt(stmt);
      llvm_unreachable("Unhandled GIMPLE statement during LLVM emission!");

    case GIMPLE_ASM:
      RenderGIMPLE_ASM(stmt);
      break;

    case GIMPLE_ASSIGN:
      RenderGIMPLE_ASSIGN(stmt);
      break;

    case GIMPLE_CALL:
       RenderGIMPLE_CALL(stmt);
       break;

    case GIMPLE_COND:
      RenderGIMPLE_COND(stmt);
      break;

    case GIMPLE_DEBUG:
      // TODO: Output debug info rather than just discarding it.
      break;

    case GIMPLE_EH_DISPATCH:
      RenderGIMPLE_EH_DISPATCH(stmt);
      break;

    case GIMPLE_GOTO:
      RenderGIMPLE_GOTO(stmt);
      break;

    case GIMPLE_LABEL:
    case GIMPLE_NOP:
    case GIMPLE_PREDICT:
      break;

    case GIMPLE_RESX:
      RenderGIMPLE_RESX(stmt);
      break;

    case GIMPLE_RETURN:
      RenderGIMPLE_RETURN(stmt);
      break;

    case GIMPLE_SWITCH:
      RenderGIMPLE_SWITCH(stmt);
      break;
    }
  }

  if (EmitDebugInfo()) {
    TheDebugInfo->setLocationFile("");
    TheDebugInfo->setLocationLine(0);
    TheDebugInfo->EmitStopPoint(Builder.GetInsertBlock(), Builder);
  }

  // Add a branch to the fallthru block.
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->flags & EDGE_FALLTHRU) {
      input_location = e->goto_locus;
      // TODO: set the debug info location.
      Builder.CreateBr(getBasicBlock(e->dest));
      break;
    }

  input_location = saved_loc;
}

Function *TreeToLLVM::EmitFunction() {
  // Set up parameters and prepare for return, for the function.
  StartFunctionBody();

  // Output the basic blocks.
  basic_block bb;
  FOR_EACH_BB(bb)
    EmitBasicBlock(bb);

  // Wrap things up.
  return FinishFunctionBody();
}

/// EmitAggregate - Store the specified tree node into the location given by
/// DestLoc.
void TreeToLLVM::EmitAggregate(tree exp, const MemRef &DestLoc) {
  assert(isa<AGGREGATE_TYPE>(TREE_TYPE(exp)) && "Expected an aggregate type!");
  if (isa<CONSTRUCTOR>(exp)) {
    EmitCONSTRUCTOR(exp, &DestLoc);
    return;
  }
  LValue LV = EmitLV(exp);
  assert(!LV.isBitfield() && "Bitfields containing aggregates not supported!");
  EmitAggregateCopy(DestLoc, MemRef(LV.Ptr, LV.getAlignment(),
                                    TREE_THIS_VOLATILE(exp)), TREE_TYPE(exp));
}

/// get_constant_alignment - Return the alignment of constant EXP in bits.
///
static unsigned int
get_constant_alignment (tree exp)
{
  unsigned int align = TYPE_ALIGN (TREE_TYPE (exp));
#ifdef CONSTANT_ALIGNMENT
  align = CONSTANT_ALIGNMENT (exp, align);
#endif
  return align;
}

/// EmitLV - Convert the specified l-value tree node to LLVM code, returning
/// the address of the result.
LValue TreeToLLVM::EmitLV(tree exp) {
  LValue LV;

  switch (TREE_CODE(exp)) {
  default:
    debug_tree(exp);
    llvm_unreachable("Unhandled lvalue expression!");

  case PARM_DECL:
  case VAR_DECL:
  case FUNCTION_DECL:
  case CONST_DECL:
  case RESULT_DECL:
    LV = EmitLV_DECL(exp);
    break;
  case ARRAY_RANGE_REF:
  case ARRAY_REF:
    LV = EmitLV_ARRAY_REF(exp);
    break;
  case COMPONENT_REF:
    LV = EmitLV_COMPONENT_REF(exp);
    break;
  case BIT_FIELD_REF:
    LV = EmitLV_BIT_FIELD_REF(exp);
    break;
  case REALPART_EXPR:
    LV = EmitLV_XXXXPART_EXPR(exp, 0);
    break;
  case IMAGPART_EXPR:
    LV = EmitLV_XXXXPART_EXPR(exp, 1);
    break;
  case SSA_NAME:
    LV = EmitLV_SSA_NAME(exp);
    break;
#if (GCC_MINOR > 5)
  case MEM_REF:
    LV = EmitLV_MEM_REF(exp);
    break;
#endif
  case TARGET_MEM_REF:
    LV = EmitLV_TARGET_MEM_REF(exp);
    break;

  // Constants.
  case LABEL_DECL: {
    LV = LValue(AddressOfLABEL_DECL(exp), 1);
    break;
  }
  case COMPLEX_CST:
  case INTEGER_CST:
  case REAL_CST:
  case STRING_CST:
  case VECTOR_CST: {
    Value *Ptr = AddressOf(exp);
    LV = LValue(Ptr, get_constant_alignment(exp) / 8);
    break;
  }

  // Type Conversion.
  case VIEW_CONVERT_EXPR:
    LV = EmitLV_VIEW_CONVERT_EXPR(exp);
    break;

  // Trivial Cases.
  case WITH_SIZE_EXPR:
    LV = EmitLV_WITH_SIZE_EXPR(exp);
    break;
  case INDIRECT_REF:
    LV = EmitLV_INDIRECT_REF(exp);
    break;
#if (GCC_MINOR < 6)
  case MISALIGNED_INDIRECT_REF:
    LV = EmitLV_MISALIGNED_INDIRECT_REF(exp);
    break;
#endif
  }

  // Check that the type of the lvalue is indeed that of a pointer to the tree
  // node.  This may not hold for bitfields because the type of a bitfield need
  // not match the type of the value being loaded out of it.  Since LLVM has no
  // void* type, don't insist that void* be converted to a specific LLVM type.
  assert((LV.isBitfield() || isa<VOID_TYPE>(TREE_TYPE(exp)) ||
          LV.Ptr->getType() == ConvertType(TREE_TYPE(exp))->getPointerTo()) &&
         "LValue has wrong type!");

  return LV;
}

//===----------------------------------------------------------------------===//
//                         ... Utility Functions ...
//===----------------------------------------------------------------------===//

/// CastToAnyType - Cast the specified value to the specified type making no
/// assumptions about the types of the arguments. This creates an inferred cast.
Value *TreeToLLVM::CastToAnyType(Value *V, bool VisSigned,
                                 Type* DestTy, bool DestIsSigned) {
  Type *SrcTy = V->getType();

  // Eliminate useless casts of a type to itself.
  if (SrcTy == DestTy)
    return V;

  // Check whether the cast needs to be done in two steps, for example a pointer
  // to float cast requires converting the pointer to an integer before casting
  // to the float.
  if (!CastInst::isCastable(SrcTy, DestTy)) {
    unsigned SrcBits = SrcTy->getScalarSizeInBits();
    unsigned DestBits = DestTy->getScalarSizeInBits();
    if (SrcBits && !isa<IntegerType>(SrcTy)) {
      Type *IntTy = IntegerType::get(Context, SrcBits);
      V = Builder.CreateBitCast(V, IntTy);
      return CastToAnyType(V, VisSigned, DestTy, DestIsSigned);
    }
    if (DestBits && !isa<IntegerType>(DestTy)) {
      Type *IntTy = IntegerType::get(Context, DestBits);
      V = CastToAnyType(V, VisSigned, IntTy, DestIsSigned);
      return Builder.CreateBitCast(V, DestTy);
    }
    llvm_unreachable("Unable to cast between these types!");
  }

  // The types are different so we must cast. Use getCastOpcode to create an
  // inferred cast opcode.
  Instruction::CastOps opc =
    CastInst::getCastOpcode(V, VisSigned, DestTy, DestIsSigned);

  // Generate the cast and return it.
  return Builder.CreateCast(opc, V, DestTy);
}

/// CastFromSameSizeInteger - Cast an integer (or vector of integer) value to
/// the given scalar (resp. vector of scalar) type of the same bitwidth.
Value *TreeToLLVM::CastFromSameSizeInteger(Value *V, Type *Ty) {
  Type *OrigTy = V->getType();
  Type *OrigEltTy = OrigTy->getScalarType();
  (void)OrigEltTy;
  assert(OrigEltTy->isIntegerTy() && "Expected an integer type!");
  Type *EltTy = Ty->getScalarType();
  if (EltTy->isIntegerTy()) {
    // Already an integer/vector of integer - nothing to do.
    assert(OrigTy == Ty && "Integer type not same size!");
    return V;
  }
  if (EltTy->isPointerTy()) {
    // A pointer/vector of pointer - use inttoptr.
    assert(OrigEltTy->getPrimitiveSizeInBits() ==
           DL.getPointerSizeInBits(cast<PointerType>(EltTy)->getAddressSpace())
           && "Pointer type not same size!");
    return Builder.CreateIntToPtr(V, Ty);
  }
  // Everything else.
  assert(Ty->isFPOrFPVectorTy() && "Expected a floating point type!");
  return Builder.CreateBitCast(V, Ty); // Will catch any size mismatch.
}

/// CastToSameSizeInteger - Cast the specified scalar (or vector of scalar)
/// value to an integer (resp. vector of integer) of the same bit width.
Value *TreeToLLVM::CastToSameSizeInteger(Value *V) {
  Type *OrigTy = V->getType();
  Type *OrigEltTy = OrigTy->getScalarType();
  if (OrigEltTy->isIntegerTy())
    // Already an integer/vector of integer - nothing to do.
    return V;
  if (OrigEltTy->isPointerTy()) {
    // A pointer/vector of pointer - form a (vector of) pointer sized integers.
    Type *NewTy = DL.getIntPtrType(OrigTy);
    return Builder.CreatePtrToInt(V, NewTy);
  }
  // Everything else.
  assert(OrigTy->isFPOrFPVectorTy() && "Expected a floating point type!");
  unsigned VecElts = isa<VectorType>(OrigTy) ?
    cast<VectorType>(OrigTy)->getNumElements() : 0;
  unsigned BitWidth = OrigEltTy->getPrimitiveSizeInBits();
  Type *NewEltTy = IntegerType::get(Context, BitWidth);
  Type *NewTy = VecElts ? VectorType::get(NewEltTy, VecElts) : NewEltTy;
  return Builder.CreateBitCast(V, NewTy);
}

/// that the value and type are floating point.
Value *TreeToLLVM::CastToFPType(Value *V, Type* Ty) {
  unsigned SrcBits = V->getType()->getPrimitiveSizeInBits();
  unsigned DstBits = Ty->getPrimitiveSizeInBits();
  if (SrcBits == DstBits)
    return V;
  Instruction::CastOps opcode = (SrcBits > DstBits ?
      Instruction::FPTrunc : Instruction::FPExt);
  return Builder.CreateCast(opcode, V, Ty);
}

/// CreateAnyAdd - Add two LLVM scalar values with the given GCC type.  Does not
/// support complex numbers.  The type is used to set overflow flags.
Value *TreeToLLVM::CreateAnyAdd(Value *LHS, Value *RHS, tree type) {
  if (isa<FLOAT_TYPE>(type))
    return Builder.CreateFAdd(LHS, RHS);
  return Builder.CreateAdd(LHS, RHS, "", hasNUW(type), hasNSW(type));
}

/// CreateAnyMul - Multiply two LLVM scalar values with the given GCC type.
/// Does not support complex numbers.  The type is used to set overflow flags.
Value *TreeToLLVM::CreateAnyMul(Value *LHS, Value *RHS, tree type) {
  if (isa<FLOAT_TYPE>(type))
    return Builder.CreateFMul(LHS, RHS);
  return Builder.CreateMul(LHS, RHS, "", hasNUW(type), hasNSW(type));
}

/// CreateAnyNeg - Negate an LLVM scalar value with the given GCC type.  Does
/// not support complex numbers.  The type is used to set overflow flags.
Value *TreeToLLVM::CreateAnyNeg(Value *V, tree type) {
  if (isa<FLOAT_TYPE>(type))
    return Builder.CreateFNeg(V);
  return Builder.CreateNeg(V, "", hasNUW(type), hasNSW(type));
}

/// CreateAnySub - Subtract two LLVM scalar values with the given GCC type.
/// Does not support complex numbers.  The type is used to set overflow flags.
Value *TreeToLLVM::CreateAnySub(Value *LHS, Value *RHS, tree type) {
  if (isa<FLOAT_TYPE>(type))
    return Builder.CreateFSub(LHS, RHS);
  return Builder.CreateSub(CastToSameSizeInteger(LHS),
                           CastToSameSizeInteger(RHS), "",
                           hasNUW(type), hasNSW(type));
}

/// CreateTemporary - Create a new alloca instruction of the specified type,
/// inserting it into the entry block and returning it.  The resulting
/// instruction's type is a pointer to the specified type.
AllocaInst *TreeToLLVM::CreateTemporary(Type *Ty, unsigned align) {
  if (AllocaInsertionPoint == 0) {
    // Create a dummy instruction in the entry block as a marker to insert new
    // alloc instructions before.  It doesn't matter what this instruction is,
    // it is dead.  This allows us to insert allocas in order without having to
    // scan for an insertion point. Use BitCast for int -> int
    AllocaInsertionPoint = CastInst::Create(Instruction::BitCast,
      Constant::getNullValue(Type::getInt32Ty(Context)),
      Type::getInt32Ty(Context), "alloca point");
    // Insert it as the first instruction in the entry block.
    Fn->begin()->getInstList().insert(Fn->begin()->begin(),
                                      AllocaInsertionPoint);
  }
  return new AllocaInst(Ty, 0, align, "", AllocaInsertionPoint);
}

/// CreateTempLoc - Like CreateTemporary, but returns a MemRef.
MemRef TreeToLLVM::CreateTempLoc(Type *Ty) {
  AllocaInst *AI = CreateTemporary(Ty);
  // MemRefs do not allow alignment 0.
  if (!AI->getAlignment())
    AI->setAlignment(DL.getPrefTypeAlignment(Ty));
  return MemRef(AI, AI->getAlignment(), false);
}

/// BeginBlock - Add the specified basic block to the end of the function.  If
/// the previous block falls through into it, add an explicit branch.
void TreeToLLVM::BeginBlock(BasicBlock *BB) {
  BasicBlock *CurBB = Builder.GetInsertBlock();
  // If the previous block falls through to BB, add an explicit branch.
  if (CurBB->getTerminator() == 0) {
    // If the previous block has no label and is empty, remove it: it is a
    // post-terminator block.
    if (CurBB->getName().empty() && CurBB->begin() == CurBB->end())
      CurBB->eraseFromParent();
    else
      // Otherwise, fall through to this block.
      Builder.CreateBr(BB);
  }

  // Add this block.
  Fn->getBasicBlockList().push_back(BB);
  Builder.SetInsertPoint(BB);  // It is now the current block.
}

static const unsigned TooCostly = 8;

/// CostOfAccessingAllElements - Return a number representing the cost of doing
/// an element by element copy of the specified type.  If it is clear that the
/// type should not be copied this way, for example because it has a bazillion
/// elements or contains fields of variable size, then TooCostly (or larger) is
/// returned.
static unsigned CostOfAccessingAllElements(tree type) {
  // If the type is incomplete, enormous or of variable size then don't copy it.
  if (!isInt64(TYPE_SIZE(type), true))
    return TooCostly;

  // A scalar copy has a cost of 1.
  if (!isa<AGGREGATE_TYPE>(type))
    return 1;

  // The cost of a record type is the sum of the costs of its fields.
  if (isa<RECORD_TYPE>(type)) {
    Type *Ty = ConvertType(type);
    unsigned TotalCost = 0;
    for (tree Field = TYPE_FIELDS(type); Field; Field = TREE_CHAIN(Field)) {
      if (!isa<FIELD_DECL>(Field)) continue;
      // If the field has no size, for example because it is a C-style variable
      // length array, then just give up.
      if (!DECL_SIZE(Field))
        return TooCostly;
      // Ignore fields of size zero.  This way, we don't give up just because
      // there is a size zero field that is not represented in the LLVM type.
      if (integer_zerop(DECL_SIZE(Field)))
        continue;
      // Bitfields are too hard - give up.
      if (isBitfield(Field))
        return TooCostly;
      // If there is no corresponding LLVM field then something funky is going
      // on - just give up.
      if (GetFieldIndex(Field, Ty) == INT_MAX)
        return TooCostly;
      TotalCost += CostOfAccessingAllElements(TREE_TYPE(Field));
      if (TotalCost >= TooCostly)
        return TooCostly;
    }
    return TotalCost;
  }

  // For array types, multiply the array length by the component cost.
  if (isa<ARRAY_TYPE>(type)) {
    // If this is an array with a funky component type then just give up.
    if (!isSizeCompatible(TREE_TYPE(type)))
      return TooCostly;
    uint64_t ArrayLength = ArrayLengthOf(type);
    if (ArrayLength >= TooCostly)
      return TooCostly;
    unsigned ComponentCost = CostOfAccessingAllElements(TREE_TYPE(type));
    if (ComponentCost >= TooCostly)
      return TooCostly;
    return ArrayLength * ComponentCost;
  }

  // Other types are not supported.
  return TooCostly;
}

/// CopyElementByElement - Recursively traverse the potentially aggregate
/// src/dest ptrs, copying all of the elements.  Helper for EmitAggregateCopy.
void TreeToLLVM::CopyElementByElement(MemRef DestLoc, MemRef SrcLoc,
                                      tree type) {
  if (!isa<AGGREGATE_TYPE>(type)) {
    // Copy scalar.
    MDNode *AliasTag = describeAliasSet(type);
    StoreRegisterToMemory(LoadRegisterFromMemory(SrcLoc, type, AliasTag,
                                                 Builder),
                          DestLoc, type, AliasTag, Builder);
    return;
  }

  if (isa<RECORD_TYPE>(type)) {
    // Ensure the source and destination are pointers to the record type.
    Type *Ty = ConvertType(type);
    DestLoc.Ptr = Builder.CreateBitCast(DestLoc.Ptr, Ty->getPointerTo());
    SrcLoc.Ptr = Builder.CreateBitCast(SrcLoc.Ptr, Ty->getPointerTo());

    // Copy each field in turn.
    for (tree Field = TYPE_FIELDS(type); Field; Field = TREE_CHAIN(Field)) {
      if (!isa<FIELD_DECL>(Field)) continue;
      // Ignore fields of size zero.
      if (integer_zerop(DECL_SIZE(Field)))
        continue;
      // Get the address of the field.
      int FieldIdx = GetFieldIndex(Field, Ty);
      assert(FieldIdx != INT_MAX && "Should not be copying if no LLVM field!");
      Value *DestFieldPtr = Builder.CreateStructGEP(DestLoc.Ptr, FieldIdx,
                                                    flag_verbose_asm ? "df":"");
      Value *SrcFieldPtr = Builder.CreateStructGEP(SrcLoc.Ptr, FieldIdx,
                                                   flag_verbose_asm ? "sf":"");

      // Compute the field's alignment.
      unsigned DestFieldAlign = DestLoc.getAlignment();
      unsigned SrcFieldAlign = SrcLoc.getAlignment();
      if (FieldIdx) {
        DestFieldAlign = MinAlign(DestFieldAlign, getFieldAlignment(Field));
        SrcFieldAlign = MinAlign(SrcFieldAlign, getFieldAlignment(Field));
      }

      // Copy the field.
      MemRef DestFieldLoc(DestFieldPtr, DestFieldAlign, DestLoc.Volatile);
      MemRef SrcFieldLoc(SrcFieldPtr, SrcFieldAlign, SrcLoc.Volatile);
      CopyElementByElement(DestFieldLoc, SrcFieldLoc, TREE_TYPE(Field));
    }
    return;
  }

  assert(isa<ARRAY_TYPE>(type) && "Expected an array!");

  // Turn the source and destination into pointers to the component type.
  Type *CompType = ConvertType(TREE_TYPE(type));
  DestLoc.Ptr = Builder.CreateBitCast(DestLoc.Ptr, CompType->getPointerTo());
  SrcLoc.Ptr = Builder.CreateBitCast(SrcLoc.Ptr, CompType->getPointerTo());

  // Copy each component in turn.
  unsigned ComponentBytes = getDataLayout().getTypeAllocSize(CompType);
  unsigned ArrayLength = ArrayLengthOf(type);
  for (unsigned i = 0; i != ArrayLength; ++i) {
    // Get the address of the component.
    Value *DestCompPtr = DestLoc.Ptr, *SrcCompPtr = SrcLoc.Ptr;
    if (i) {
      DestCompPtr = Builder.CreateConstInBoundsGEP1_32(DestCompPtr, i,
                                                  flag_verbose_asm ? "da" : "");
      SrcCompPtr = Builder.CreateConstInBoundsGEP1_32(SrcCompPtr, i,
                                                  flag_verbose_asm ? "sa" : "");
    }

    // Compute the component's alignment.
    unsigned DestCompAlign = DestLoc.getAlignment();
    unsigned SrcCompAlign = SrcLoc.getAlignment();
    if (i) {
      DestCompAlign = MinAlign(DestCompAlign, i * ComponentBytes);
      SrcCompAlign = MinAlign(SrcCompAlign, i * ComponentBytes);
    }

    // Copy the component.
    MemRef DestCompLoc(DestCompPtr, DestCompAlign, DestLoc.Volatile);
    MemRef SrcCompLoc(SrcCompPtr, SrcCompAlign, SrcLoc.Volatile);
    CopyElementByElement(DestCompLoc, SrcCompLoc, TREE_TYPE(type));
  }
}

#ifndef TARGET_DRAGONEGG_MEMCPY_COST
#define TARGET_DRAGONEGG_MEMCPY_COST 5
#endif

/// EmitAggregateCopy - Copy the elements from SrcLoc to DestLoc, using the
/// GCC type specified by GCCType to know which elements to copy.
void TreeToLLVM::EmitAggregateCopy(MemRef DestLoc, MemRef SrcLoc, tree type) {
  if (DestLoc.Ptr == SrcLoc.Ptr && !DestLoc.Volatile && !SrcLoc.Volatile)
    return;  // noop copy.

  // If the type is small, copy element by element instead of using memcpy.
  unsigned Cost = CostOfAccessingAllElements(type);
  if (Cost < TooCostly && Cost < TARGET_DRAGONEGG_MEMCPY_COST) {
    CopyElementByElement(DestLoc, SrcLoc, type);
    return;
  }

  Value *TypeSize = EmitRegister(TYPE_SIZE_UNIT(type));
  EmitMemCpy(DestLoc.Ptr, SrcLoc.Ptr, TypeSize,
             std::min(DestLoc.getAlignment(), SrcLoc.getAlignment()));
}

/// ZeroElementByElement - Recursively traverse the potentially aggregate
/// DestLoc, zero'ing all of the elements.  Helper for EmitAggregateZero.
void TreeToLLVM::ZeroElementByElement(MemRef DestLoc, tree type) {
  if (!isa<AGGREGATE_TYPE>(type)) {
    // Zero scalar.
    StoreRegisterToMemory(Constant::getNullValue(getRegType(type)), DestLoc,
                          type, describeAliasSet(type), Builder);
    return;
  }

  if (isa<RECORD_TYPE>(type)) {
    // Ensure the pointer is to the record type.
    Type *Ty = ConvertType(type);
    DestLoc.Ptr = Builder.CreateBitCast(DestLoc.Ptr, Ty->getPointerTo());

    // Zero each field in turn.
    for (tree Field = TYPE_FIELDS(type); Field; Field = TREE_CHAIN(Field)) {
      if (!isa<FIELD_DECL>(Field)) continue;
      // Ignore fields of size zero.
      if (integer_zerop(DECL_SIZE(Field)))
        continue;
      // Get the address of the field.
      int FieldIdx = GetFieldIndex(Field, Ty);
      assert(FieldIdx != INT_MAX && "Should not be zeroing if no LLVM field!");
      Value *FieldPtr = Builder.CreateStructGEP(DestLoc.Ptr, FieldIdx,
                                                flag_verbose_asm ? "zf" : "");

      // Compute the field's alignment.
      unsigned FieldAlign = DestLoc.getAlignment();
      if (FieldIdx)
        FieldAlign = MinAlign(FieldAlign, getFieldAlignment(Field));

      // Zero the field.
      MemRef FieldLoc(FieldPtr, FieldAlign, DestLoc.Volatile);
      ZeroElementByElement(FieldLoc, TREE_TYPE(Field));
    }
    return;
  }

  assert(isa<ARRAY_TYPE>(type) && "Expected an array!");

  // Turn the pointer into a pointer to the component type.
  Type *CompType = ConvertType(TREE_TYPE(type));
  DestLoc.Ptr = Builder.CreateBitCast(DestLoc.Ptr, CompType->getPointerTo());

  // Zero each component in turn.
  unsigned ComponentBytes = getDataLayout().getTypeAllocSize(CompType);
  unsigned ArrayLength = ArrayLengthOf(type);
  for (unsigned i = 0; i != ArrayLength; ++i) {
    // Get the address of the component.
    Value *CompPtr = DestLoc.Ptr;
    if (i)
      CompPtr = Builder.CreateConstInBoundsGEP1_32(CompPtr, i,
                                                   flag_verbose_asm ? "za":"");

    // Compute the component's alignment.
    unsigned CompAlign = DestLoc.getAlignment();
    if (i)
      CompAlign = MinAlign(CompAlign, i * ComponentBytes);

    // Zero the component.
    MemRef CompLoc(CompPtr, CompAlign, DestLoc.Volatile);
    ZeroElementByElement(CompLoc, TREE_TYPE(type));
  }
}

#ifndef TARGET_DRAGONEGG_MEMSET_COST
#define TARGET_DRAGONEGG_MEMSET_COST 5
#endif

/// EmitAggregateZero - Zero the elements of DestLoc.
void TreeToLLVM::EmitAggregateZero(MemRef DestLoc, tree type) {
  // If the type is small, zero element by element instead of using memset.
  unsigned Cost = CostOfAccessingAllElements(type);
  if (Cost < TooCostly && Cost < TARGET_DRAGONEGG_MEMSET_COST) {
    ZeroElementByElement(DestLoc, type);
    return;
  }

  EmitMemSet(DestLoc.Ptr, Builder.getInt8(0),
             EmitRegister(TYPE_SIZE_UNIT(type)), DestLoc.getAlignment());
}

Value *TreeToLLVM::EmitMemCpy(Value *DestPtr, Value *SrcPtr, Value *Size,
                              unsigned Align) {

  Type *SBP = Type::getInt8PtrTy(Context);
  Type *IntPtr = DL.getIntPtrType(DestPtr->getType());
  Value *Ops[5] = {
    Builder.CreateBitCast(DestPtr, SBP),
    Builder.CreateBitCast(SrcPtr, SBP),
    Builder.CreateIntCast(Size, IntPtr, /*isSigned*/true),
    Builder.getInt32(Align),
    Builder.getFalse()
  };
  Type *ArgTypes[3] = { SBP, SBP, IntPtr };

  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Intrinsic::memcpy,
                                               ArgTypes), Ops);
  return Ops[0];
}

Value *TreeToLLVM::EmitMemMove(Value *DestPtr, Value *SrcPtr, Value *Size,
                               unsigned Align) {
  Type *SBP = Type::getInt8PtrTy(Context);
  Type *IntPtr = DL.getIntPtrType(DestPtr->getType());
  Value *Ops[5] = {
    Builder.CreateBitCast(DestPtr, SBP),
    Builder.CreateBitCast(SrcPtr, SBP),
    Builder.CreateIntCast(Size, IntPtr, /*isSigned*/true),
    Builder.getInt32(Align),
    Builder.getFalse()
  };
  Type *ArgTypes[3] = { SBP, SBP, IntPtr };

  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Intrinsic::memmove,
                                               ArgTypes), Ops);
  return Ops[0];
}

Value *TreeToLLVM::EmitMemSet(Value *DestPtr, Value *SrcVal, Value *Size,
                              unsigned Align) {
  Type *SBP = Type::getInt8PtrTy(Context);
  Type *IntPtr = DL.getIntPtrType(DestPtr->getType());
  Value *Ops[5] = {
    Builder.CreateBitCast(DestPtr, SBP),
    Builder.CreateIntCast(SrcVal, Type::getInt8Ty(Context), /*isSigned*/true),
    Builder.CreateIntCast(Size, IntPtr, /*isSigned*/true),
    Builder.getInt32(Align),
    Builder.getFalse()
  };
  Type *ArgTypes[2] = { SBP, IntPtr };

  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Intrinsic::memset,
                                               ArgTypes), Ops);
  return Ops[0];
}


// Emits code to do something for a type attribute
void TreeToLLVM::EmitTypeGcroot(Value *V) {
  // GC intrinsics can only be used in functions which specify a collector.
  Fn->setGC("shadow-stack");

  Function *gcrootFun = Intrinsic::getDeclaration(TheModule,
                                                  Intrinsic::gcroot);

  // The idea is that it's a pointer to type "Value"
  // which is opaque* but the routine expects i8** and i8*.
  PointerType *Ty = Type::getInt8PtrTy(Context);
  V = Builder.CreateBitCast(V, Ty->getPointerTo());

  Value *Ops[2] = {
    V,
    ConstantPointerNull::get(Ty)
  };

  Builder.CreateCall(gcrootFun, Ops);
}

// Emits annotate intrinsic if the decl has the annotate attribute set.
void TreeToLLVM::EmitAnnotateIntrinsic(Value *V, tree decl) {

  // Handle annotate attribute on global.
  tree annotateAttr = lookup_attribute("annotate", DECL_ATTRIBUTES (decl));

  if (!annotateAttr)
    return;

  Function *annotateFun = Intrinsic::getDeclaration(TheModule,
                                                    Intrinsic::var_annotation);

  // Get file and line number
  Constant *lineNo =
    ConstantInt::get(Type::getInt32Ty(Context), DECL_SOURCE_LINE(decl));
  Constant *file = ConvertMetadataStringToGV(DECL_SOURCE_FILE(decl));
  Type *SBP = Type::getInt8PtrTy(Context);
  file = TheFolder->CreateBitCast(file, SBP);

  // There may be multiple annotate attributes. Pass return of lookup_attr
  //  to successive lookups.
  while (annotateAttr) {

    // Each annotate attribute is a tree list.
    // Get value of list which is our linked list of args.
    tree args = TREE_VALUE(annotateAttr);

    // Each annotate attribute may have multiple args.
    // Treat each arg as if it were a separate annotate attribute.
    for (tree a = args; a; a = TREE_CHAIN(a)) {
      // Each element of the arg list is a tree list, so get value
      tree val = TREE_VALUE(a);

      // Assert its a string, and then get that string.
      assert(isa<STRING_CST>(val) &&
             "Annotate attribute arg should always be a string");
      Constant *strGV = AddressOf(val);
      Value *Ops[4] = {
        Builder.CreateBitCast(V, SBP),
        Builder.CreateBitCast(strGV, SBP),
        file,
        lineNo
      };

      Builder.CreateCall(annotateFun, Ops);
    }

    // Get next annotate attribute.
    annotateAttr = TREE_CHAIN(annotateAttr);
    if (annotateAttr)
      annotateAttr = lookup_attribute("annotate", annotateAttr);
  }
}

//===----------------------------------------------------------------------===//
//                  ... Basic Lists and Binding Scopes ...
//===----------------------------------------------------------------------===//

/// EmitAutomaticVariableDecl - Emit the function-local decl to the current
/// function and set DECL_LOCAL for the decl to the right pointer.
void TreeToLLVM::EmitAutomaticVariableDecl(tree decl) {
  // If this is just the rotten husk of a variable that the gimplifier
  // eliminated all uses of, but is preserving for debug info, ignore it.
  if (isa<VAR_DECL>(decl) && DECL_HAS_VALUE_EXPR_P(decl))
    return;

  tree type = TREE_TYPE(decl);
  Type *Ty;  // Type to allocate
  Value *Size = 0; // Amount to alloca (null for 1)

  if (DECL_SIZE(decl) == 0) {    // Variable with incomplete type.
    if (DECL_INITIAL(decl) == 0)
      return; // Error message was already done; now avoid a crash.
    debug_tree(decl);
    llvm_unreachable("Initializer will decide the size of this array?");
  } else if (isa<INTEGER_CST>(DECL_SIZE_UNIT(decl))) {
    // Variable of fixed size that goes on the stack.
    Ty = ConvertType(type);
  } else {
    // Compute the variable's size in bytes.
    Size = EmitRegister(DECL_SIZE_UNIT(decl));
    Ty = Type::getInt8Ty(Context);
  }

  unsigned Alignment = DECL_ALIGN(decl) / 8; // Alignment in octets.
  assert(Alignment != 0 && "Local variable with unknown alignment!");

  // If this is the alignment we would have given the variable anyway then don't
  // use an explicit alignment, making the IR look more portable.
  if (Alignment == getDataLayout().getABITypeAlignment(Ty))
    Alignment = 0;

  // Insert an alloca for this variable.
  AllocaInst *AI;
  if (!Size) {                           // Fixed size alloca -> entry block.
    AI = CreateTemporary(Ty);
  } else {
    AI = Builder.CreateAlloca(Ty, Size);
  }
  NameValue(AI, decl);

  AI->setAlignment(Alignment);

  SET_DECL_LOCAL(decl, AI);

  // Handle annotate attributes
  if (DECL_ATTRIBUTES(decl))
    EmitAnnotateIntrinsic(AI, decl);

  // Handle gcroot attribute
  if (isa<ACCESS_TYPE>(TREE_TYPE (decl))
      && lookup_attribute("gcroot", TYPE_ATTRIBUTES(TREE_TYPE (decl))))
    {
      // We should null out local variables so that a stack crawl
      // before initialization doesn't get garbage results to follow.
      Type *T = cast<PointerType>(AI->getType())->getElementType();
      EmitTypeGcroot(AI);
      Builder.CreateStore(Constant::getNullValue(T), AI);
    }

  if (EmitDebugInfo()) {
    if (DECL_NAME(decl)) {
      TheDebugInfo->EmitDeclare(decl, dwarf::DW_TAG_auto_variable,
                                AI->getName(), TREE_TYPE(decl), AI, Builder);
    } else if (isa<RESULT_DECL>(decl)) {
      TheDebugInfo->EmitDeclare(decl, dwarf::DW_TAG_return_variable,
                                AI->getName(), TREE_TYPE(decl), AI, Builder);
    }
  }
}


//===----------------------------------------------------------------------===//
//                           ... Control Flow ...
//===----------------------------------------------------------------------===//

/// ConvertTypeInfo - Convert an exception handling type info into a pointer to
/// the associated runtime type info object.
static Constant *ConvertTypeInfo(tree type) {
  // TODO: Once pass_ipa_free_lang is made a default pass, remove the call to
  // lookup_type_for_runtime below.
  if (isa<TYPE>(type))
    type = lookup_type_for_runtime (type);
  STRIP_NOPS(type);
  if (isa<ADDR_EXPR>(type))
    type = TREE_OPERAND(type, 0);
  return AddressOf(type);
}

/// getExceptionPtr - Return the local holding the exception pointer for the
/// given exception handling region, creating it if necessary.
AllocaInst *TreeToLLVM::getExceptionPtr(int RegionNo) {
  assert(RegionNo >= 0 && "Invalid exception handling region!");

  if ((unsigned)RegionNo >= ExceptionPtrs.size())
    ExceptionPtrs.resize(RegionNo + 1, 0);

  AllocaInst *&ExceptionPtr = ExceptionPtrs[RegionNo];

  if (!ExceptionPtr) {
    ExceptionPtr = CreateTemporary(Type::getInt8PtrTy(Context));
    ExceptionPtr->setName("exc_tmp");
  }

  return ExceptionPtr;
}

/// getExceptionFilter - Return the local holding the filter value for the
/// given exception handling region, creating it if necessary.
AllocaInst *TreeToLLVM::getExceptionFilter(int RegionNo) {
  assert(RegionNo >= 0 && "Invalid exception handling region!");

  if ((unsigned)RegionNo >= ExceptionFilters.size())
    ExceptionFilters.resize(RegionNo + 1, 0);

  AllocaInst *&ExceptionFilter = ExceptionFilters[RegionNo];

  if (!ExceptionFilter) {
    ExceptionFilter = CreateTemporary(Type::getInt32Ty(Context));
    ExceptionFilter->setName("filt_tmp");
  }

  return ExceptionFilter;
}

/// getFailureBlock - Return the basic block containing the failure code for
/// the given exception handling region, creating it if necessary.
BasicBlock *TreeToLLVM::getFailureBlock(int RegionNo) {
  assert(RegionNo >= 0 && "Invalid exception handling region!");

  if ((unsigned)RegionNo >= FailureBlocks.size())
    FailureBlocks.resize(RegionNo + 1, 0);

  BasicBlock *&FailureBlock = FailureBlocks[RegionNo];

  if (!FailureBlock)
    FailureBlock = BasicBlock::Create(Context, "fail");

  return FailureBlock;
}

/// EmitLandingPads - Emit EH landing pads.
void TreeToLLVM::EmitLandingPads() {
  // If there are no invokes then there is nothing to do.
  if (NormalInvokes.empty())
    return;

  // If a GCC post landing pad is shared by several exception handling regions,
  // or if there is a normal edge to it, then create LLVM landing pads for each
  // eh region.  The landing pad instruction will then go in the LLVM landing
  // pad, which then branches to the GCC post landing pad.
  for (unsigned LPadNo = 1; LPadNo < NormalInvokes.size(); ++LPadNo) {
    // Get the list of invokes for this GCC landing pad.
    SmallVector<InvokeInst *, 8> &InvokesForPad = NormalInvokes[LPadNo];

    if (InvokesForPad.empty())
      continue;

    // All of the invokes unwind to the GCC post landing pad.
    BasicBlock *PostPad = InvokesForPad[0]->getUnwindDest();

    // If the number of invokes is equal to the number of predecessors of the
    // post landing pad then it follows that no other GCC landing pad has any
    // invokes that unwind to this post landing pad, and also that no normal
    // edges land at this post pad.  In this case there is no need to create
    // an LLVM specific landing pad.
    if ((unsigned)std::distance(pred_begin(PostPad), pred_end(PostPad)) ==
        InvokesForPad.size())
      continue;

    // Create the LLVM landing pad right before the GCC post landing pad.
    BasicBlock *LPad = BasicBlock::Create(Context, "lpad", Fn, PostPad);

    // Redirect invoke unwind edges from the GCC post landing pad to LPad.
    for (unsigned i = 0, e = InvokesForPad.size(); i < e; ++i)
      InvokesForPad[i]->setSuccessor(1, LPad);

    // If there are any PHI nodes in PostPad, we need to update them to merge
    // incoming values from LPad instead.
    pred_iterator PB = pred_begin(LPad), PE = pred_end(LPad);
    for (BasicBlock::iterator II = PostPad->begin(); isa<PHINode>(II);) {
      PHINode *PN = cast<PHINode>(II++);

      // Check to see if all of the values coming in via invoke unwind edges are
      // the same.  If so, we don't need to create a new PHI node.
      Value *InVal = PN->getIncomingValueForBlock(*PB);
      for (pred_iterator PI = PB; PI != PE; ++PI) {
        if (PI != PB && InVal != PN->getIncomingValueForBlock(*PI)) {
          InVal = 0;
          break;
        }
      }

      if (InVal == 0) {
        // Different unwind edges have different values.  Create a new PHI node
        // in LPad.
        PHINode *NewPN = PHINode::Create(PN->getType(), std::distance(PB, PE),
                                         PN->getName()+".lpad", LPad);
        // Add an entry for each unwind edge, using the value from the old PHI.
        for (pred_iterator PI = PB; PI != PE; ++PI)
          NewPN->addIncoming(PN->getIncomingValueForBlock(*PI), *PI);

        // Now use this new PHI as the common incoming value for LPad in PN.
        InVal = NewPN;
      }

      // Revector exactly one entry in the PHI node to come from LPad and
      // delete the entries that came from the invoke unwind edges.
      for (pred_iterator PI = PB; PI != PE; ++PI)
        PN->removeIncomingValue(*PI);
      PN->addIncoming(InVal, LPad);
    }

    // Add a fallthrough from LPad to the original landing pad.
    BranchInst::Create(PostPad, LPad);
  }

  // Create the landing pad instruction for each exception handling region at
  // the start of the corresponding landing pad.  At this point each exception
  // handling region has its own landing pad, which is only reachable via the
  // unwind edges of the region's invokes.
  Type *UnwindDataTy = StructType::get(Builder.getInt8PtrTy(),
                                       Builder.getInt32Ty(), NULL);
  for (unsigned LPadNo = 1; LPadNo < NormalInvokes.size(); ++LPadNo) {
    // Get the list of invokes for this GCC landing pad.
    SmallVector<InvokeInst *, 8> &InvokesForPad = NormalInvokes[LPadNo];

    if (InvokesForPad.empty())
      continue;

    // All of the invokes unwind to the the landing pad.
    BasicBlock *LPad = InvokesForPad[0]->getUnwindDest();

    // The exception handling region this landing pad is for.
    eh_region region = get_eh_region_from_lp_number(LPadNo);
    assert(region->index > 0 && "Invalid landing pad region!");
    unsigned RegionNo = region->index;

    // Insert instructions at the start of the landing pad, but after any phis.
    Builder.SetInsertPoint(LPad, LPad->getFirstNonPHI());

    // Create the landingpad instruction without any clauses.  Clauses are added
    // below.
    tree personality = DECL_FUNCTION_PERSONALITY(FnDecl);
    if (!personality) {
      assert(function_needs_eh_personality(cfun) == eh_personality_any &&
             "No exception handling personality!");
      personality = lang_hooks.eh_personality();
    }
    LandingPadInst *LPadInst = Builder.CreateLandingPad(UnwindDataTy,
                                                        DECL_LLVM(personality),
                                                        0, "exc");

    // Store the exception pointer if made use of elsewhere.
    if (RegionNo < ExceptionPtrs.size() && ExceptionPtrs[RegionNo]) {
      Value *ExcPtr = Builder.CreateExtractValue(LPadInst, 0, "exc_ptr");
      Builder.CreateStore(ExcPtr, ExceptionPtrs[RegionNo]);
    }

    // Store the selector value if made use of elsewhere.
    if (RegionNo < ExceptionFilters.size() && ExceptionFilters[RegionNo]) {
      Value *Filter = Builder.CreateExtractValue(LPadInst, 1, "filter");
      Builder.CreateStore(Filter, ExceptionFilters[RegionNo]);
    }

    // Add clauses to the landing pad instruction.
    bool AllCaught = false; // Did we see a catch-all or no-throw?
    SmallSet<Constant *, 8> AlreadyCaught; // Typeinfos known caught already.
    for (; region && !AllCaught; region = region->outer)
      switch (region->type) {
      case ERT_ALLOWED_EXCEPTIONS: {
        // Filter.  Compute the list of type infos.
        AllCaught = true;
        std::vector<Constant*> TypeInfos;
        for (tree type = region->u.allowed.type_list; type;
             type = TREE_CHAIN(type)) {
          Constant *TypeInfo = ConvertTypeInfo(TREE_VALUE(type));
          // No point in letting a typeinfo through if we know it can't reach
          // the filter in the first place.
          if (AlreadyCaught.count(TypeInfo))
            continue;
          TypeInfo = TheFolder->CreateBitCast(TypeInfo, Builder.getInt8PtrTy());
          TypeInfos.push_back(TypeInfo);
          AllCaught = false;
        }

        // Add the list of typeinfos as a filter clause.
        ArrayType *FilterTy = ArrayType::get(Builder.getInt8PtrTy(),
                                             TypeInfos.size());
        LPadInst->addClause(ConstantArray::get(FilterTy, TypeInfos));
        break;
      }
      case ERT_CLEANUP:
        LPadInst->setCleanup(true);
        break;
      case ERT_MUST_NOT_THROW: {
        // Same as a zero-length filter: add an empty filter clause.
        ArrayType *FilterTy = ArrayType::get(Builder.getInt8PtrTy(), 0);
        LPadInst->addClause(ConstantArray::get(FilterTy,
                                               ArrayRef<Constant*>()));
        AllCaught = true;
        break;
      }
      case ERT_TRY:
        // Catches.
        for (eh_catch c = region->u.eh_try.first_catch; c ; c = c->next_catch)
          if (!c->type_list) {
            // Catch-all - add a null pointer as a catch clause.
            LPadInst->addClause(Constant::getNullValue(Builder.getInt8PtrTy()));
            AllCaught = true;
            break;
          } else {
            // Add the type infos.
            for (tree type = c->type_list; type; type = TREE_CHAIN(type)) {
              Constant *TypeInfo = ConvertTypeInfo(TREE_VALUE(type));
              // No point in trying to catch a typeinfo that was already caught.
              if (!AlreadyCaught.insert(TypeInfo))
                continue;
              LPadInst->addClause(TypeInfo);
            }
          }
        break;
      }
  }

  NormalInvokes.clear();
}

/// EmitFailureBlocks - Emit the blocks containing failure code executed when
/// an exception is thrown in a must-not-throw region.
void TreeToLLVM::EmitFailureBlocks() {
  for (unsigned RegionNo = 1; RegionNo < FailureBlocks.size(); ++RegionNo) {
    BasicBlock *FailureBlock = FailureBlocks[RegionNo];

    if (!FailureBlock)
      continue;

    eh_region region = get_eh_region_from_number(RegionNo);
    assert(region->type == ERT_MUST_NOT_THROW && "Unexpected region type!");

    // Check whether all predecessors are invokes or not.  Nothing exotic can
    // occur here, only direct branches and unwinding via an invoke.
    bool hasBranchPred = false;
    bool hasInvokePred = false;
    for (pred_iterator I = pred_begin(FailureBlock), E = pred_end(FailureBlock);
         I != E && (!hasInvokePred || !hasBranchPred); ++I) {
      TerminatorInst *T = (*I)->getTerminator();
      if (isa<InvokeInst>(T)) {
        assert(FailureBlock != T->getSuccessor(0) && "Expected unwind target!");
        hasInvokePred = true;
      } else {
        assert(isa<BranchInst>(T) && "Wrong kind of failure predecessor!");
        hasBranchPred = true;
      }
    }
    assert((hasBranchPred || hasInvokePred) && "No predecessors!");

    // Determine the landing pad that invokes will unwind to.  If there are no
    // invokes, then there is no landing pad.
    BasicBlock *LandingPad = NULL;
    if (hasInvokePred) {
      // If all predecessors are invokes, then the failure block can be used as
      // the landing pad.  Otherwise, create a landing pad.
      if (hasBranchPred)
        LandingPad = BasicBlock::Create(Context, "pad");
      else
        LandingPad = FailureBlock;
    }

    if (LandingPad) {
      BeginBlock(LandingPad);

      // Generate a landingpad instruction with an empty (i.e. catch-all) filter
      // clause.
      Type *UnwindDataTy = StructType::get(Builder.getInt8PtrTy(),
                                           Builder.getInt32Ty(), NULL);
      tree personality = DECL_FUNCTION_PERSONALITY(FnDecl);
      assert(personality && "No-throw region but no personality function!");
      LandingPadInst *LPadInst =
        Builder.CreateLandingPad(UnwindDataTy, DECL_LLVM(personality), 1,
                                 "exc");
      ArrayType *FilterTy = ArrayType::get(Builder.getInt8PtrTy(), 0);
      LPadInst->addClause(ConstantArray::get(FilterTy, ArrayRef<Constant*>()));

      if (LandingPad != FailureBlock) {
        // Make sure all invokes unwind to the new landing pad.
        for (pred_iterator I = pred_begin(FailureBlock),
             E = pred_end(FailureBlock); I != E; ) {
          TerminatorInst *T = (*I++)->getTerminator();
          if (isa<InvokeInst>(T))
            T->setSuccessor(1, LandingPad);
        }

        // Branch to the failure block at the end of the landing pad.
        Builder.CreateBr(FailureBlock);
      }
    }

    if (LandingPad != FailureBlock)
      BeginBlock(FailureBlock);

    // Determine the failure function to call.
    Value *FailFunc = DECL_LLVM(region->u.must_not_throw.failure_decl);

    // Make sure it has the right type.
    FunctionType *FTy = FunctionType::get(Type::getVoidTy(Context), false);
    FailFunc = Builder.CreateBitCast(FailFunc, FTy->getPointerTo());

    // Spank the user for being naughty.
    // TODO: Set the correct debug location.
    CallInst *FailCall = Builder.CreateCall(FailFunc);

    // This is always fatal.
    FailCall->setDoesNotReturn();
    FailCall->setDoesNotThrow();
    Builder.CreateUnreachable();
  }
}


//===----------------------------------------------------------------------===//
//                           ... Expressions ...
//===----------------------------------------------------------------------===//

static bool canEmitRegisterVariable(tree exp) {
  // Only variables can be marked as 'register'.
  if (!isa<VAR_DECL>(exp) || !DECL_REGISTER(exp))
    return false;

  // We can emit inline assembler for access to global register variables.
  if (TREE_STATIC(exp) || DECL_EXTERNAL(exp) || TREE_PUBLIC(exp))
    return true;

  // Emit inline asm if this is local variable with assembler name on it.
  if (DECL_ASSEMBLER_NAME_SET_P(exp))
    return true;

  // Otherwise - it's normal automatic variable.
  return false;
}

/// EmitLoadOfLValue - When an l-value expression is used in a context that
/// requires an r-value, this method emits the lvalue computation, then loads
/// the result.
Value *TreeToLLVM::EmitLoadOfLValue(tree exp) {
  if (canEmitRegisterVariable(exp))
    // If this is a register variable, EmitLV can't handle it (there is no
    // l-value of a register variable).  Emit an inline asm node that copies the
    // value out of the specified register.
    return EmitReadOfRegisterVariable(exp);

  LValue LV = EmitLV(exp);
  LV.Volatile = TREE_THIS_VOLATILE(exp);
  // TODO: Arrange for Volatile to already be set in the LValue.
  unsigned Alignment = LV.getAlignment();

  if (!LV.isBitfield()) {
    // Scalar value: emit a load.
    return LoadRegisterFromMemory(LV, TREE_TYPE(exp), describeAliasSet(exp),
                                  Builder);
  } else {
    // This is a bitfield reference.
    Type *Ty = getRegType(TREE_TYPE(exp));
    if (!LV.BitSize)
      return Constant::getNullValue(Ty);

    // Load the minimum number of bytes that covers the field.
    unsigned LoadSizeInBits = LV.BitStart + LV.BitSize;
    LoadSizeInBits = RoundUpToAlignment(LoadSizeInBits, BITS_PER_UNIT);
    Type *LoadType = IntegerType::get(Context, LoadSizeInBits);

    // Load the bits.
    Value *Ptr = Builder.CreateBitCast(LV.Ptr, LoadType->getPointerTo());
    Value *Val = Builder.CreateAlignedLoad(Ptr, Alignment, LV.Volatile);

    // Mask the bits out by shifting left first, then shifting right.  The
    // optimizers will turn this into an "and" in the unsigned case.

    // Shift the sign bit of the bitfield to the sign bit position in the loaded
    // type.  This zaps any extra bits occurring after the end of the bitfield.
    unsigned FirstBitInVal = BYTES_BIG_ENDIAN ?
      LoadSizeInBits - LV.BitStart - LV.BitSize : LV.BitStart;
    if (FirstBitInVal + LV.BitSize != LoadSizeInBits) {
      Value *ShAmt = ConstantInt::get(LoadType, LoadSizeInBits -
                                      (FirstBitInVal + LV.BitSize));
      Val = Builder.CreateShl(Val, ShAmt);
    }
    // Shift the first bit of the bitfield to be bit zero.  This zaps any extra
    // bits that occurred before the start of the bitfield.  In the signed case
    // this also duplicates the sign bit, giving a sign extended value.
    bool isSigned = !TYPE_UNSIGNED(TREE_TYPE(exp));
    Value *ShAmt = ConstantInt::get(LoadType, LoadSizeInBits - LV.BitSize);
    Val = isSigned ?
      Builder.CreateAShr(Val, ShAmt) : Builder.CreateLShr(Val, ShAmt);

    // Get the bits as a value of the correct type.
    // FIXME: This assumes the result is an integer.
    return Builder.CreateIntCast(Val, Ty, isSigned);
  }
}

Value *TreeToLLVM::EmitADDR_EXPR(tree exp) {
  LValue LV = EmitLV(TREE_OPERAND(exp, 0));
  assert((!LV.isBitfield() || LV.BitStart == 0) &&
         "It is illegal to take the address of a bitfield!");
  // Perform a cast here if necessary.  For example, GCC sometimes forms an
  // ADDR_EXPR where the operand is an array, and the ADDR_EXPR type is a
  // pointer to the first element.
  return Builder.CreateBitCast(LV.Ptr, getRegType(TREE_TYPE(exp)));
}

#if (GCC_MINOR < 7)
Value *TreeToLLVM::EmitCondExpr(tree exp) {
  return TriviallyTypeConvert(EmitReg_CondExpr(TREE_OPERAND(exp, 0),
                                               TREE_OPERAND(exp, 1),
                                               TREE_OPERAND(exp, 2)),
                              getRegType(TREE_TYPE(exp)));
}
#endif

Value *TreeToLLVM::EmitOBJ_TYPE_REF(tree exp) {
  return Builder.CreateBitCast(EmitRegister(OBJ_TYPE_REF_EXPR(exp)),
                               getRegType(TREE_TYPE(exp)));
}

/// EmitCONSTRUCTOR - emit the constructor into the location specified by
/// DestLoc.
Value *TreeToLLVM::EmitCONSTRUCTOR(tree exp, const MemRef *DestLoc) {
  tree type = TREE_TYPE(exp);
  Type *Ty = ConvertType(type);
  if (VectorType *VTy = dyn_cast<VectorType>(Ty)) {
    assert(DestLoc == 0 && "Dest location for vector value?");
    std::vector<Value *> BuildVecOps;
    BuildVecOps.reserve(VTy->getNumElements());

    // Insert all of the elements here.
    unsigned HOST_WIDE_INT idx;
    tree value;
    FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (exp), idx, value) {
      Value *Elt = EmitRegister(value);

      if (VectorType *EltTy = dyn_cast<VectorType>(Elt->getType())) {
        // GCC allows vectors to be built up from vectors.  Extract all of the
        // vector elements and add them to the list of build vector operands.
        for (unsigned i = 0, e = EltTy->getNumElements(); i != e; ++i) {
          Value *Index = Builder.getInt32(i);
          BuildVecOps.push_back(Builder.CreateExtractElement(Elt, Index));
        }
      } else {
        assert(Elt->getType() == VTy->getElementType() &&
               "Unexpected type for vector constructor!");
        BuildVecOps.push_back(Elt);
      }
    }

    // Insert zero for any unspecified values.
    while (BuildVecOps.size() < VTy->getNumElements())
      BuildVecOps.push_back(Constant::getNullValue(VTy->getElementType()));
    assert(BuildVecOps.size() == VTy->getNumElements() &&
           "Vector constructor specified too many values!");

    return BuildVector(BuildVecOps);
  }

  assert(isa<AGGREGATE_TYPE>(type) && "Constructor for scalar type??");

  // Start out with the value zero'd out.
  EmitAggregateZero(*DestLoc, type);

  VEC(constructor_elt, gc) *elt = CONSTRUCTOR_ELTS(exp);
  switch (TREE_CODE(TREE_TYPE(exp))) {
  case ARRAY_TYPE:
  case RECORD_TYPE:
  default:
    if (!elt || !VEC_length(constructor_elt, elt))
      return 0;
    debug_tree(exp);
    llvm_unreachable("We don't handle elements yet!");
  case QUAL_UNION_TYPE:
  case UNION_TYPE:
    // Store each element of the constructor into the corresponding field of
    // DEST.
    if (!elt || VEC_empty(constructor_elt, elt)) return 0;  // no elements
    assert(VEC_length(constructor_elt, elt) == 1
           && "Union CONSTRUCTOR should have one element!");
    tree tree_purpose = VEC_index(constructor_elt, elt, 0)->index;
    tree tree_value   = VEC_index(constructor_elt, elt, 0)->value;
    if (!tree_purpose)
      return 0;  // Not actually initialized?

    if (isa<AGGREGATE_TYPE>(TREE_TYPE(tree_purpose))) {
      EmitAggregate(tree_value, *DestLoc);
    } else {
      // Scalar value.  Evaluate to a register, then do the store.
      Value *V = EmitRegister(tree_value);
      StoreRegisterToMemory(V, *DestLoc, TREE_TYPE(tree_purpose), 0, Builder);
    }
    break;
  }
  return 0;
}

/// llvm_load_scalar_argument - Load value located at LOC.
static Value *llvm_load_scalar_argument(Value *L,
                                        llvm::Type *LLVMTy,
                                        unsigned RealSize,
                                        LLVMBuilder &Builder) {
  if (!RealSize)
    return UndefValue::get(LLVMTy);

  // Not clear what this is supposed to do on big endian machines...
  assert(!BYTES_BIG_ENDIAN && "Unsupported case - please report");
  assert(LLVMTy->isIntegerTy() && "Expected an integer value!");
  Type *LoadType = IntegerType::get(Context, RealSize * 8);
  L = Builder.CreateBitCast(L, LoadType->getPointerTo());
  Value *Val = Builder.CreateLoad(L);
  if (LoadType->getPrimitiveSizeInBits() >= LLVMTy->getPrimitiveSizeInBits())
    Val = Builder.CreateTrunc(Val, LLVMTy);
  else
    Val = Builder.CreateZExt(Val, LLVMTy);
  return Val;
}

#ifndef LLVM_LOAD_SCALAR_ARGUMENT
#define LLVM_LOAD_SCALAR_ARGUMENT(LOC,TY,SIZE,BUILDER) \
  llvm_load_scalar_argument((LOC),(TY),(SIZE),(BUILDER))
#endif

namespace {
  /// FunctionCallArgumentConversion - This helper class is driven by the ABI
  /// definition for this target to figure out how to pass arguments into the
  /// stack/regs for a function call.
  struct FunctionCallArgumentConversion : public DefaultABIClient {
    SmallVector<Value*, 16> &CallOperands;
    SmallVector<Value*, 2> LocStack;
    FunctionType *FTy;
    const MemRef *DestLoc;
    LLVMBuilder &Builder;
    Value *TheValue;
    MemRef RetBuf;
    CallingConv::ID &CallingConv;
    unsigned Offset;
    bool isShadowRet;
    bool isAggrRet;
    bool useReturnSlot;

    FunctionCallArgumentConversion(SmallVector<Value*, 16> &ops,
                                   FunctionType *FnTy,
                                   const MemRef *destloc,
                                   bool ReturnSlotOpt,
                                   LLVMBuilder &b,
                                   CallingConv::ID &CC)
      : CallOperands(ops), FTy(FnTy), DestLoc(destloc), Builder(b),
        CallingConv(CC), Offset(0), isShadowRet(false), isAggrRet(false),
        useReturnSlot(ReturnSlotOpt) { }

    /// getCallingConv - This provides the desired CallingConv for the function.
    CallingConv::ID getCallingConv(void) { return CallingConv; }

    // Push the address of an argument.
    void pushAddress(Value *Loc) {
      assert(Loc && "Invalid location!");
      LocStack.push_back(Loc);
    }

    // Push the value of an argument.
    void pushValue(Value *V) {
      assert(LocStack.empty() && "Value only allowed at top level!");
      LocStack.push_back(NULL);
      TheValue = V;
    }

    // Get the address of the current location.
    Value *getAddress(void) {
      assert(!LocStack.empty());
      Value *&Loc = LocStack.back();
      if (!Loc) {
        // A value.  Store to a temporary, and return the temporary's address.
        // Any future access to this argument will reuse the same address.
        Loc = TheTreeToLLVM->CreateTemporary(TheValue->getType());
        Builder.CreateStore(TheValue, Loc);
      }
      return Loc;
    }

    // Get the value of the current location (of type Ty).
    Value *getValue(Type *Ty) {
      assert(!LocStack.empty());
      Value *Loc = LocStack.back();
      if (Loc) {
        // An address.  Convert to the right type and load the value out.
        Loc = Builder.CreateBitCast(Loc, Ty->getPointerTo());
        // FIXME: Pass alignment information down rather than just using 1 here.
        return Builder.CreateAlignedLoad(Loc, 1, "val");
      } else {
        // A value - just return it.
        assert(TheValue->getType() == Ty && "Value not of expected type!");
        return TheValue;
      }
    }

    void clear() {
      assert(LocStack.size() == 1 && "Imbalance!");
      LocStack.clear();
    }

    bool isShadowReturn() const { return isShadowRet; }
    bool isAggrReturn() { return isAggrRet; }

    // EmitShadowResult - If the return result was redirected to a buffer,
    // emit it now.
    Value *EmitShadowResult(tree type, const MemRef *DstLoc) {
      if (!RetBuf.Ptr)
        return 0;

      if (DstLoc) {
        // Copy out the aggregate return value now.
        assert(ConvertType(type) ==
               cast<PointerType>(RetBuf.Ptr->getType())->getElementType() &&
               "Inconsistent result types!");
        TheTreeToLLVM->EmitAggregateCopy(*DstLoc, RetBuf, type);
        return 0;
      } else {
        // Read out the scalar return value now.
        return Builder.CreateLoad(RetBuf.Ptr, "result");
      }
    }

    /// HandleScalarResult - This callback is invoked if the function returns a
    /// simple scalar result value.
    void HandleScalarResult(Type * /*RetTy*/) {
      // There is nothing to do here if we return a scalar or void.
      assert(DestLoc == 0 &&
             "Call returns a scalar but caller expects aggregate!");
    }

    /// HandleAggregateResultAsScalar - This callback is invoked if the function
    /// returns an aggregate value by bit converting it to the specified scalar
    /// type and returning that.
    void HandleAggregateResultAsScalar(Type * /*ScalarTy*/,
                                       unsigned Off = 0) {
      this->Offset = Off;
    }

    /// HandleAggregateResultAsAggregate - This callback is invoked if the
    /// function returns an aggregate value using multiple return values.
    void HandleAggregateResultAsAggregate(Type * /*AggrTy*/) {
      // There is nothing to do here.
      isAggrRet = true;
    }

    /// HandleAggregateShadowResult - This callback is invoked if the function
    /// returns an aggregate value by using a "shadow" first parameter.  If
    /// RetPtr is set to true, the pointer argument itself is returned from the
    /// function.
    void HandleAggregateShadowResult(PointerType *PtrArgTy, bool /*RetPtr*/) {
      // We need to pass memory to write the return value into.
      // FIXME: alignment and volatility are being ignored!
      assert(!DestLoc || PtrArgTy == DestLoc->Ptr->getType());

      if (DestLoc == 0) {
        // The result is unused, but still needs to be stored somewhere.
        Value *Buf = TheTreeToLLVM->CreateTemporary(PtrArgTy->getElementType());
        CallOperands.push_back(Buf);
      } else if (useReturnSlot) {
        // Letting the call write directly to the final destination is safe and
        // may be required.  Do not use a buffer.
        CallOperands.push_back(DestLoc->Ptr);
      } else {
        // Letting the call write directly to the final destination may not be
        // safe (eg: if DestLoc aliases a parameter) and is not required - pass
        // a buffer and copy it to DestLoc after the call.
        RetBuf = TheTreeToLLVM->CreateTempLoc(PtrArgTy->getElementType());
        CallOperands.push_back(RetBuf.Ptr);
      }

      // Note the use of a shadow argument.
      isShadowRet = true;
    }

    void HandlePad(llvm::Type *LLVMTy) {
      CallOperands.push_back(UndefValue::get(LLVMTy));
    }

    /// HandleScalarShadowResult - This callback is invoked if the function
    /// returns a scalar value by using a "shadow" first parameter, which is a
    /// pointer to the scalar, of type PtrArgTy.  If RetPtr is set to true,
    /// the pointer argument itself is returned from the function.
    void HandleScalarShadowResult(PointerType *PtrArgTy,
                                  bool /*RetPtr*/) {
      assert(DestLoc == 0 &&
             "Call returns a scalar but caller expects aggregate!");
      // Create a buffer to hold the result.  The result will be loaded out of
      // it after the call.
      RetBuf = TheTreeToLLVM->CreateTempLoc(PtrArgTy->getElementType());
      CallOperands.push_back(RetBuf.Ptr);

      // Note the use of a shadow argument.
      isShadowRet = true;
    }

    /// HandleScalarArgument - This is the primary callback that specifies an
    /// LLVM argument to pass.  It is only used for first class types.
    void HandleScalarArgument(llvm::Type *LLVMTy, tree type,
                              unsigned RealSize = 0) {
      Value *Loc = NULL;
      if (RealSize) {
        Value *L = getAddress();
        Loc = LLVM_LOAD_SCALAR_ARGUMENT(L,LLVMTy,RealSize,Builder);
      } else
        Loc = getValue(LLVMTy);

      // Perform any implicit type conversions.
      if (CallOperands.size() < FTy->getNumParams()) {
        Type *CalledTy= FTy->getParamType(CallOperands.size());
        if (Loc->getType() != CalledTy) {
          if (type) {
            bool isSigned = !TYPE_UNSIGNED(type);
            Loc = TheTreeToLLVM->CastToAnyType(Loc, isSigned, CalledTy, false);
          } else {
            // Only trivial type conversions should get here.
            Loc = Builder.CreateBitCast(Loc, CalledTy);
          }
        }
      }

      CallOperands.push_back(Loc);
    }

    /// HandleByInvisibleReferenceArgument - This callback is invoked if a
    /// pointer (of type PtrTy) to the argument is passed rather than the
    /// argument itself.
    void HandleByInvisibleReferenceArgument(llvm::Type *PtrTy,
                                            tree /*type*/) {
      Value *Loc = getAddress();
      Loc = Builder.CreateBitCast(Loc, PtrTy);
      CallOperands.push_back(Loc);
    }

    /// HandleByValArgument - This callback is invoked if the aggregate function
    /// argument is passed by value. It is lowered to a parameter passed by
    /// reference with an additional parameter attribute "ByVal".
    void HandleByValArgument(llvm::Type *LLVMTy, tree /*type*/) {
      Value *Loc = getAddress();
      assert(LLVMTy->getPointerTo() == Loc->getType());
      (void)LLVMTy; // Otherwise unused if asserts off - avoid compiler warning.
      CallOperands.push_back(Loc);
    }

    /// HandleFCAArgument - This callback is invoked if the aggregate function
    /// argument is passed as a first class aggregate.
    void HandleFCAArgument(llvm::Type *LLVMTy, tree /*type*/) {
      Value *Loc = getAddress();
      assert(LLVMTy->getPointerTo() == Loc->getType());
      (void)LLVMTy; // Otherwise unused if asserts off - avoid compiler warning.
      CallOperands.push_back(Builder.CreateLoad(Loc));
    }

    /// EnterField - Called when we're about the enter the field of a struct
    /// or union.  FieldNo is the number of the element we are entering in the
    /// LLVM Struct, StructTy is the LLVM type of the struct we are entering.
    void EnterField(unsigned FieldNo, llvm::Type *StructTy) {
      Value *Loc = getAddress();
      Loc = Builder.CreateBitCast(Loc, StructTy->getPointerTo());
      pushAddress(Builder.CreateStructGEP(Loc, FieldNo,
                                          flag_verbose_asm ? "elt" : ""));
    }
    void ExitField() {
      assert(!LocStack.empty());
      LocStack.pop_back();
    }
  };
}

/// EmitCallOf - Emit a call to the specified callee with the operands specified
/// in the GIMPLE_CALL 'stmt'. If the result of the call is a scalar, return the
/// result, otherwise store it in DestLoc.
Value *TreeToLLVM::EmitCallOf(Value *Callee, gimple stmt, const MemRef *DestLoc,
                              const AttrListPtr &InPAL) {
  BasicBlock *LandingPad = 0; // Non-zero indicates an invoke.
  int LPadNo = 0;

  AttrListPtr PAL = InPAL;
  if (PAL.isEmpty() && isa<Function>(Callee))
    PAL = cast<Function>(Callee)->getAttributes();

  // Work out whether to use an invoke or an ordinary call.
  if (!stmt_could_throw_p(stmt)) {
    // This call does not throw - mark it 'nounwind'.
    Attributes NoUnwind = Attributes::get(Callee->getContext(),
                                          Attributes::NoUnwind);
    PAL = PAL.addAttr(Callee->getContext(), ~0, NoUnwind);
  }

  if (!PAL.getFnAttributes().hasAttribute(Attributes::NoUnwind)) {
    // This call may throw.  Determine if we need to generate
    // an invoke rather than a simple call.
    LPadNo = lookup_stmt_eh_lp(stmt);

    if (LPadNo > 0) {
      // The call is in an exception handling region with a landing pad.
      // Generate an invoke, with the GCC landing pad as the unwind destination.
      // The destination may change to an LLVM only landing pad,  which precedes
      // the GCC one, after phi nodes have been populated (doing things this way
      // simplifies the generation of phi nodes).
      eh_landing_pad lp = get_eh_landing_pad_from_number(LPadNo);
      assert(lp && "Post landing pad not found!");
      LandingPad = getLabelDeclBlock(lp->post_landing_pad);
    } else if (LPadNo < 0) {
      eh_region region = get_eh_region_from_lp_number(LPadNo);
      // The call is in a must-not-throw region.  Generate an invoke that causes
      // the region's failure code to be run if an exception is thrown.
      assert(region->type == ERT_MUST_NOT_THROW && "Unexpected region type!");

      // Unwind to the block containing the failure code.
      LandingPad = getFailureBlock(region->index);
    }
  }

  tree fndecl = gimple_call_fndecl(stmt);
  tree fntype = fndecl ?
    TREE_TYPE(fndecl) : TREE_TYPE (TREE_TYPE(gimple_call_fn(stmt)));

  // Determine the calling convention.
  CallingConv::ID CallingConvention = CallingConv::C;
#ifdef TARGET_ADJUST_LLVM_CC
  TARGET_ADJUST_LLVM_CC(CallingConvention, fntype);
#endif

  SmallVector<Value*, 16> CallOperands;
  PointerType *PFTy = cast<PointerType>(Callee->getType());
  FunctionType *FTy = cast<FunctionType>(PFTy->getElementType());
  FunctionCallArgumentConversion Client(CallOperands, FTy, DestLoc,
                                        gimple_call_return_slot_opt_p(stmt),
                                        Builder, CallingConvention);
  DefaultABI ABIConverter(Client);

  // Handle the result, including struct returns.
  ABIConverter.HandleReturnType(gimple_call_return_type(stmt),
                                fndecl ? fndecl : fntype,
                                fndecl ? DECL_BUILT_IN(fndecl) : false);

  // Pass the static chain, if any, as the first parameter.
  if (gimple_call_chain(stmt))
    CallOperands.push_back(EmitMemory(gimple_call_chain(stmt)));

  // Loop over the arguments, expanding them and adding them to the op list.
  std::vector<Type*> ScalarArgs;
  for (unsigned i = 0, e = gimple_call_num_args(stmt); i != e; ++i) {
    tree arg = gimple_call_arg(stmt, i);
    tree type = TREE_TYPE(arg);
    Type *ArgTy = ConvertType(type);

    // Push the argument.
    if (ArgTy->isSingleValueType()) {
      // A scalar - push the value.
      Client.pushValue(EmitMemory(arg));
    } else if (LLVM_SHOULD_PASS_AGGREGATE_AS_FCA(type, ArgTy)) {
      if (isa<AGGREGATE_TYPE>(type)) {
        // Pass the aggregate as a first class value.
        LValue ArgVal = EmitLV(arg);
        Client.pushValue(Builder.CreateLoad(ArgVal.Ptr));
      } else {
        // Already first class (eg: a complex number) - push the value.
        Client.pushValue(EmitMemory(arg));
      }
    } else {
      if (isa<AGGREGATE_TYPE>(type)) {
        // An aggregate - push the address.
        LValue ArgVal = EmitLV(arg);
        assert(!ArgVal.isBitfield() && "Bitfields are first-class types!");
        Client.pushAddress(ArgVal.Ptr);
      } else {
        // A first class value (eg: a complex number).  Push the address of a
        // temporary copy.
        MemRef Copy = CreateTempLoc(ArgTy);
        StoreRegisterToMemory(EmitRegister(arg), Copy, type, 0, Builder);
        Client.pushAddress(Copy.Ptr);
      }
    }

    AttrBuilder AttrBuilder;

    unsigned OldSize = CallOperands.size();

    ABIConverter.HandleArgument(type, ScalarArgs, &AttrBuilder);

    if (AttrBuilder.hasAttributes()) {
      // If the argument is split into multiple scalars, assign the
      // attributes to all scalars of the aggregate.
      for (unsigned j = OldSize + 1; j <= CallOperands.size(); ++j)
        PAL = PAL.addAttr(Context, j, Attributes::get(Context, AttrBuilder));
    }

    Client.clear();
  }

  // If the caller and callee disagree about a parameter type but the difference
  // is trivial, correct the type used by the caller.
  for (unsigned i = 0, e = std::min((unsigned)CallOperands.size(),
                                    FTy->getNumParams());
       i != e; ++i) {
    Type *ExpectedTy = FTy->getParamType(i);
    Type *ActualTy = CallOperands[i]->getType();
    if (ActualTy == ExpectedTy)
      continue;
    assert(isa<PointerType>(ActualTy) && isa<PointerType>(ExpectedTy) &&
           "Type difference is not trivial!");
    CallOperands[i] = Builder.CreateBitCast(CallOperands[i], ExpectedTy);
  }

  // Unlike LLVM, GCC does not require that call statements provide a value for
  // every function argument (it passes rubbish for arguments with no value).
  // To get the same effect we pass 'undef' for any unspecified arguments.
  if (CallOperands.size() < FTy->getNumParams())
    for (unsigned i = CallOperands.size(), e = FTy->getNumParams(); i != e; ++i)
      CallOperands.push_back(UndefValue::get(FTy->getParamType(i)));

  Value *Call;
  if (!LandingPad) {
    Call = Builder.CreateCall(Callee, CallOperands);
    cast<CallInst>(Call)->setCallingConv(CallingConvention);
    cast<CallInst>(Call)->setAttributes(PAL);
  } else {
    BasicBlock *NextBlock = BasicBlock::Create(Context);
    Call = Builder.CreateInvoke(Callee, NextBlock, LandingPad, CallOperands);
    cast<InvokeInst>(Call)->setCallingConv(CallingConvention);
    cast<InvokeInst>(Call)->setAttributes(PAL);

    if (LPadNo > 0) {
      // The invoke's destination may change to an LLVM only landing pad, which
      // precedes the GCC one, after phi nodes have been populated (doing things
      // this way simplifies the generation of phi nodes).  Record the invoke as
      // well as the GCC exception handling region.
      if ((unsigned)LPadNo >= NormalInvokes.size())
        NormalInvokes.resize(LPadNo + 1);
      NormalInvokes[LPadNo].push_back(cast<InvokeInst>(Call));
    }

    BeginBlock(NextBlock);
  }

  // If the call statement has void type then either the callee does not return
  // a result, or it does but the result should be discarded.
  if (isa<VOID_TYPE>(gimple_call_return_type(stmt)))
    return 0;

  if (Client.isShadowReturn())
    return Client.EmitShadowResult(gimple_call_return_type(stmt), DestLoc);

  if (Client.isAggrReturn()) {
    MemRef Target;
    if (DestLoc)
      Target = *DestLoc;
    else
      // Destination is a first class value (eg: a complex number).  Extract to
      // a temporary then load the value out later.
      Target = CreateTempLoc(ConvertType(gimple_call_return_type(stmt)));

    if (DL.getTypeAllocSize(Call->getType()) <=
        DL.getTypeAllocSize(cast<PointerType>(Target.Ptr->getType())
                                             ->getElementType())) {
      Value *Dest = Builder.CreateBitCast(Target.Ptr,
                                          Call->getType()->getPointerTo());
      LLVM_EXTRACT_MULTIPLE_RETURN_VALUE(Call, Dest, Target.Volatile,
                                         Builder);
    } else {
      // The call will return an aggregate value in registers, but
      // those registers are bigger than Target.  Allocate a
      // temporary to match the registers, store the registers there,
      // cast the temporary into the correct (smaller) type, and using
      // the correct type, copy the value into Target.  Assume the
      // optimizer will delete the temporary and clean this up.
      AllocaInst *biggerTmp = CreateTemporary(Call->getType());
      LLVM_EXTRACT_MULTIPLE_RETURN_VALUE(Call,biggerTmp,/*Volatile=*/false,
                                       Builder);
      EmitAggregateCopy(Target,
                        MemRef(Builder.CreateBitCast(biggerTmp,Call->getType()->
                                                     getPointerTo()),
                               Target.getAlignment(), Target.Volatile),
                        gimple_call_return_type(stmt));
    }

    return DestLoc ? 0 : Builder.CreateLoad(Target.Ptr);
  }

  if (!DestLoc) {
    Type *RetTy = ConvertType(gimple_call_return_type(stmt));
    if (Call->getType() == RetTy)
      return Call;   // Normal scalar return.

    // May be something as simple as a float being returned as an integer, or
    // something trickier like a complex int type { i32, i32 } being returned
    // as an i64.
    if (Call->getType()->canLosslesslyBitCastTo(RetTy))
      return Builder.CreateBitCast(Call, RetTy); // Simple case.
    // Probably a scalar to complex conversion.
    assert(DL.getTypeAllocSize(Call->getType()) == DL.getTypeAllocSize(RetTy) &&
           "Size mismatch in scalar to scalar conversion!");
    Value *Tmp = CreateTemporary(Call->getType());
    Builder.CreateStore(Call, Tmp);
    return Builder.CreateLoad(Builder.CreateBitCast(Tmp,RetTy->getPointerTo()));
  }

  // If the caller expects an aggregate, we have a situation where the ABI for
  // the current target specifies that the aggregate be returned in scalar
  // registers even though it is an aggregate.  We must bitconvert the scalar
  // to the destination aggregate type.  We do this by casting the DestLoc
  // pointer and storing into it.  The store does not necessarily start at the
  // beginning of the aggregate (x86-64).
  Value *Ptr = DestLoc->Ptr;
  unsigned Align = DestLoc->getAlignment();
  // AggTy - The type of the aggregate being stored to.
  Type *AggTy = cast<PointerType>(Ptr->getType())->getElementType();
  // MaxStoreSize - The maximum number of bytes we can store without overflowing
  // the aggregate.
  int64_t MaxStoreSize = DL.getTypeAllocSize(AggTy);
  if (Client.Offset) {
    Ptr = Builder.CreateBitCast(Ptr, Type::getInt8PtrTy(Context));
    Ptr = Builder.CreateGEP(Ptr,
                            ConstantInt::get(DL.getIntPtrType(Ptr->getType()),
                                             Client.Offset),
                            flag_verbose_asm ? "ro" : "");
    Align = MinAlign(Align, Client.Offset);
    MaxStoreSize -= Client.Offset;
  }
  assert(MaxStoreSize > 0 && "Storing off end of aggregate?");
  Value *Val = Call;
  // Check whether storing the scalar directly would overflow the aggregate.
  if (DL.getTypeStoreSize(Call->getType()) > (uint64_t)MaxStoreSize) {
    // Chop down the size of the scalar to the maximum number of bytes that can
    // be stored without overflowing the destination.
    // TODO: Check whether this works correctly on big-endian machines.
    // Store the scalar to a temporary.
    Value *Tmp = CreateTemporary(Call->getType());
    Builder.CreateStore(Call, Tmp);
    // Load the desired number of bytes back out again as an integer of the
    // appropriate size.
    Type *SmallTy = IntegerType::get(Context, MaxStoreSize*8);
    Tmp = Builder.CreateBitCast(Tmp, PointerType::getUnqual(SmallTy));
    Val = Builder.CreateLoad(Tmp);
    // Store the integer rather than the call result to the aggregate.
  }
  Ptr = Builder.CreateBitCast(Ptr, PointerType::getUnqual(Val->getType()));
  Builder.CreateAlignedStore(Val, Ptr, Align, DestLoc->Volatile);
  return 0;
}

/// EmitSimpleCall - Emit a call to the function with the given name and return
/// type, passing the provided arguments (which should all be gimple registers
/// or local constants of register type).  No marshalling is done: the arguments
/// are directly passed through.
CallInst *TreeToLLVM::EmitSimpleCall(StringRef CalleeName, tree ret_type,
                                     /* arguments */ ...) {
  va_list ops;
  va_start(ops, ret_type);

  // Build the list of arguments.
  std::vector<Value*> Args;
#ifdef TARGET_ADJUST_LLVM_CC
  // Build the list of GCC argument types.
  tree arg_types;
  tree *chainp = &arg_types;
#endif
  while (tree arg = va_arg(ops, tree)) {
    Args.push_back(EmitRegister(arg));
#ifdef TARGET_ADJUST_LLVM_CC
    *chainp = build_tree_list(NULL, TREE_TYPE(arg));
    chainp = &TREE_CHAIN(*chainp);
#endif
  }
#ifdef TARGET_ADJUST_LLVM_CC
  // Indicate that this function is not varargs.
  *chainp = void_list_node;
#endif
  va_end(ops);

  Type *RetTy = isa<VOID_TYPE>(ret_type) ?
    Type::getVoidTy(Context) : getRegType(ret_type);

  // The LLVM argument types.
  std::vector<Type*> ArgTys;
  ArgTys.reserve(Args.size());
  for (unsigned i = 0, e = Args.size(); i != e; ++i)
    ArgTys.push_back(Args[i]->getType());

  // Determine the calling convention.
  CallingConv::ID CC = CallingConv::C;
#ifdef TARGET_ADJUST_LLVM_CC
  // Query the target for the calling convention to use.
  tree fntype = build_function_type(ret_type, arg_types);
  TARGET_ADJUST_LLVM_CC(CC, fntype);
#endif

  // Get the function declaration for the callee.
  FunctionType *FTy = FunctionType::get(RetTy, ArgTys, /*isVarArg*/false);
  Constant *Func = TheModule->getOrInsertFunction(CalleeName, FTy);

  // If the function already existed with the wrong prototype then don't try to
  // muck with its calling convention.  Otherwise, set the calling convention.
  if (Function *F = dyn_cast<Function>(Func))
    F->setCallingConv(CC);

  // Finally, call the function.
  CallInst *CI = Builder.CreateCall(Func, Args);
  CI->setCallingConv(CC);
  return CI;
}


//===----------------------------------------------------------------------===//
//               ... Inline Assembly and Register Variables ...
//===----------------------------------------------------------------------===//

// LLVM_GET_REG_NAME - Default to use GCC's register names.  Targets may
// override this to use different names for some registers.  The REG_NAME is
// the name before it was decoded; it may be null in some contexts.
#ifndef LLVM_GET_REG_NAME
#define LLVM_GET_REG_NAME(REG_NAME, REG_NUM) reg_names[REG_NUM]
#endif

// LLVM_CANONICAL_ADDRESS_CONSTRAINTS - GCC defines the "p" constraint to
// allow a valid memory address, but targets differ widely on what is allowed
// as an address.  This macro is a string containing the canonical constraint
// characters that are conservatively valid addresses.  Default to allowing an
// address in a register, since that works for many targets.
#ifndef LLVM_CANONICAL_ADDRESS_CONSTRAINTS
#define LLVM_CANONICAL_ADDRESS_CONSTRAINTS "r"
#endif

/// Reads from register variables are handled by emitting an inline asm node
/// that copies the value out of the specified register.
Value *TreeToLLVM::EmitReadOfRegisterVariable(tree decl) {
  Type *MemTy = ConvertType(TREE_TYPE(decl));
  Type *RegTy = getRegType(TREE_TYPE(decl));

  // If there was an error, return something bogus.
  if (ValidateRegisterVariable(decl))
    return UndefValue::get(RegTy);

  // Turn this into a 'tmp = call Ty asm "", "={reg}"()'.
  FunctionType *FTy = FunctionType::get(MemTy, std::vector<Type*>(),
                                        false);

  const char *Name = extractRegisterName(decl);
  Name = LLVM_GET_REG_NAME(Name, decode_reg_name(Name));

  InlineAsm *IA = InlineAsm::get(FTy, "", "={"+std::string(Name)+"}", true);
  CallInst *Call = Builder.CreateCall(IA);
  Call->setDoesNotThrow();

  // Convert the call result to in-register type.
  return Mem2Reg(Call, TREE_TYPE(decl), Builder);
}

/// Stores to register variables are handled by emitting an inline asm node
/// that copies the value into the specified register.
void TreeToLLVM::EmitModifyOfRegisterVariable(tree decl, Value *RHS) {
  // If there was an error, bail out.
  if (ValidateRegisterVariable(decl))
    return;

  // Convert to in-memory type.
  RHS = Reg2Mem(RHS, TREE_TYPE(decl), Builder);

  // Turn this into a 'call void asm sideeffect "", "{reg}"(Ty %RHS)'.
  std::vector<Type*> ArgTys;
  ArgTys.push_back(RHS->getType());
  FunctionType *FTy = FunctionType::get(Type::getVoidTy(Context), ArgTys,
                                        false);

  const char *Name = extractRegisterName(decl);
  Name = LLVM_GET_REG_NAME(Name, decode_reg_name(Name));

  InlineAsm *IA = InlineAsm::get(FTy, "", "{"+std::string(Name)+"}", true);
  CallInst *Call = Builder.CreateCall(IA, RHS);
  Call->setDoesNotThrow();
}

/// ConvertInlineAsmStr - Convert the specified inline asm string to an LLVM
/// InlineAsm string.  The GNU style inline asm template string has the
/// following format:
///   %N (for N a digit) means print operand N in usual manner.
///   %=  means a unique number for the inline asm.
///   %lN means require operand N to be a CODE_LABEL or LABEL_REF
///       and print the label name with no punctuation.
///   %cN means require operand N to be a constant
///       and print the constant expression with no punctuation.
///   %aN means expect operand N to be a memory address
///       (not a memory reference!) and print a reference to that address.
///   %nN means expect operand N to be a constant and print a constant
///       expression for minus the value of the operand, with no other
///       punctuation.
/// Other %xN expressions are turned into LLVM ${N:x} operands.
///
static std::string ConvertInlineAsmStr(gimple stmt, unsigned NumOperands) {
  const char *AsmStr = gimple_asm_string(stmt);

  // gimple_asm_input_p - This flag is set if this is a non-extended ASM,
  // which means that the asm string should not be interpreted, other than
  // to escape $'s.
  if (gimple_asm_input_p(stmt)) {
    const char *InStr = AsmStr;
    std::string Result;
    while (1) {
      switch (*InStr++) {
      case 0: return Result;                 // End of string.
      default: Result += InStr[-1]; break;   // Normal character.
      case '$': Result += "$$"; break;       // Escape '$' characters.
      }
    }
  }

  std::string Result;
  while (1) {
    switch (*AsmStr++) {
    case 0: return Result;                 // End of string.
    default: Result += AsmStr[-1]; break;  // Normal character.
    case '$': Result += "$$"; break;       // Escape '$' characters.
#ifdef ASSEMBLER_DIALECT
    // Note that we can't escape to ${, because that is the syntax for vars.
    case '{': Result += "$("; break;       // Escape '{' character.
    case '}': Result += "$)"; break;       // Escape '}' character.
    case '|': Result += "$|"; break;       // Escape '|' character.
#endif
    case '%':                              // GCC escape character.
      char EscapedChar = *AsmStr++;
      if (EscapedChar == '%') {            // Escaped '%' character
        Result += '%';
      } else if (EscapedChar == '=') {     // Unique ID for the asm instance.
        Result += "${:uid}";
      }
#ifdef LLVM_ASM_EXTENSIONS
      LLVM_ASM_EXTENSIONS(EscapedChar, AsmStr, Result)
#endif
      else if (ISALPHA(EscapedChar)) {
        // % followed by a letter and some digits. This outputs an operand in a
        // special way depending on the letter.  We turn this into LLVM ${N:o}
        // syntax.
        char *EndPtr;
        unsigned long OpNum = strtoul(AsmStr, &EndPtr, 10);

        if (AsmStr == EndPtr) {
          error("operand number missing after %%-letter");
          return Result;
        } else if (OpNum >= NumOperands) {
          error("operand number out of range");
          return Result;
        }
        Result += "${" + utostr(OpNum) + ":" + EscapedChar + "}";
        AsmStr = EndPtr;
      } else if (ISDIGIT(EscapedChar)) {
        char *EndPtr;
        unsigned long OpNum = strtoul(AsmStr-1, &EndPtr, 10);
        AsmStr = EndPtr;
        Result += "$" + utostr(OpNum);
#ifdef PRINT_OPERAND_PUNCT_VALID_P
      } else if (PRINT_OPERAND_PUNCT_VALID_P((unsigned char)EscapedChar)) {
        Result += "${:";
        Result += EscapedChar;
        Result += "}";
#endif
      } else {
        output_operand_lossage("invalid %%-code");
      }
      break;
    }
  }
}

/// isOperandMentioned - Return true if the given operand is explicitly
/// mentioned in the asm string.  For example if passed operand 1 then
/// this routine checks that the asm string does not contain "%1".
static bool isOperandMentioned(gimple stmt, unsigned OpNum) {
  // If this is a non-extended ASM then the contents of the asm string are not
  // to be interpreted.
  if (gimple_asm_input_p(stmt))
    return false;
  // Search for a non-escaped '%' character followed by OpNum.
  for (const char *AsmStr = gimple_asm_string(stmt); *AsmStr; ++AsmStr) {
    if (*AsmStr != '%')
      // Not a '%', move on to next character.
      continue;
    char Next = AsmStr[1];
    // If this is "%%" then the '%' is escaped - skip both '%' characters.
    if (Next == '%') {
      ++AsmStr;
      continue;
    }
    // Whitespace is not allowed between the '%' and the number, so check that
    // the next character is a digit.
    if (!ISDIGIT(Next))
      continue;
    char *EndPtr;
    // If this is an explicit reference to OpNum then we are done.
    if (OpNum == strtoul(AsmStr+1, &EndPtr, 10))
      return true;
    // Otherwise, skip over the number and keep scanning.
    AsmStr = EndPtr - 1;
  }
  return false;
}

/// CanonicalizeConstraint - If we can canonicalize the constraint into
/// something simpler, do so now.  This turns register classes with a single
/// register into the register itself, expands builtin constraints to multiple
/// alternatives, etc.
static std::string CanonicalizeConstraint(const char *Constraint) {
  std::string Result;

  // Skip over modifier characters.
  bool DoneModifiers = false;
  while (!DoneModifiers) {
    switch (*Constraint) {
    default: DoneModifiers = true; break;
    case '=':
      llvm_unreachable("Should be after '='s");
    case '+':
      llvm_unreachable("'+' should already be expanded");
    case '*':
    case '?':
    case '!':
      ++Constraint;
      break;
    case '&':     // Pass earlyclobber to LLVM.
    case '%':     // Pass commutative to LLVM.
      Result += *Constraint++;
      break;
    case '#':  // No constraint letters left.
      return Result;
    }
  }

  // If this constraint is multiple letters add a parsing helper prefix.
  if (CONSTRAINT_LEN(*Constraint, Constraint) > 1)
    Result += "^";

  while (*Constraint) {
    char ConstraintChar = *Constraint++;

    // 'g' is just short-hand for 'imr'.
    if (ConstraintChar == 'g') {
      Result += "imr";
      continue;
    }

    // Translate 'p' to a target-specific set of constraints that
    // conservatively allow a valid memory address.  For inline assembly there
    // is no way to know the mode of the data being addressed, so this is only
    // a rough approximation of how GCC handles this constraint.
    if (ConstraintChar == 'p') {
      Result += LLVM_CANONICAL_ADDRESS_CONSTRAINTS;
      continue;
    }

    // See if this is a regclass constraint.
    unsigned RegClass;
    if (ConstraintChar == 'r')
      // REG_CLASS_FROM_CONSTRAINT doesn't support 'r' for some reason.
      RegClass = GENERAL_REGS;
    else
      RegClass = REG_CLASS_FROM_CONSTRAINT(Constraint[-1], Constraint-1);

    if (RegClass == NO_REGS) {  // not a reg class.
      Result += ConstraintChar;
      continue;
    }

    // Look to see if the specified regclass has exactly one member, and if so,
    // what it is.  Cache this information in AnalyzedRegClasses once computed.
    static std::map<unsigned, int> AnalyzedRegClasses;

    std::map<unsigned, int>::iterator I =
      AnalyzedRegClasses.lower_bound(RegClass);

    int RegMember;
    if (I != AnalyzedRegClasses.end() && I->first == RegClass) {
      // We've already computed this, reuse value.
      RegMember = I->second;
    } else {
      // Otherwise, scan the regclass, looking for exactly one member.
      RegMember = -1;  // -1 => not a single-register class.
      for (unsigned j = 0; j != FIRST_PSEUDO_REGISTER; ++j)
        if (TEST_HARD_REG_BIT(reg_class_contents[RegClass], j)) {
          if (RegMember == -1) {
            RegMember = j;
          } else {
            RegMember = -1;
            break;
          }
        }
      // Remember this answer for the next query of this regclass.
      AnalyzedRegClasses.insert(I, std::make_pair(RegClass, RegMember));
    }

    // If we found a single register register class, return the register.
    if (RegMember != -1) {
      Result += '{';
      Result += LLVM_GET_REG_NAME(0, RegMember);
      Result += '}';
    } else {
      Result += ConstraintChar;
    }
  }

  return Result;
}

/// See if operand "exp" can use the indicated Constraint (which is
/// terminated by a null or a comma).
/// Returns:  -1=no, 0=yes but auxiliary instructions needed, 1=yes and free
static int MatchWeight(const char *Constraint, tree Operand) {
  const char *p = Constraint;
  int RetVal = 0;
  // Look for hard register operand.  This matches only a constraint of a
  // register class that includes that hard register, and it matches that
  // perfectly, so we never return 0 in this case.
  if (isa<VAR_DECL>(Operand) && DECL_HARD_REGISTER(Operand)) {
    int RegNum = decode_reg_name(extractRegisterName(Operand));
    RetVal = -1;
    if (RegNum >= 0) {
      do {
        unsigned RegClass;
        if (*p == 'r')
          RegClass = GENERAL_REGS;
        else
          RegClass = REG_CLASS_FROM_CONSTRAINT(*p, p);
        if (RegClass != NO_REGS &&
            TEST_HARD_REG_BIT(reg_class_contents[RegClass], RegNum)) {
          RetVal = 1;
          break;
        }
        ++p;
      } while (*p != ',' && *p != 0);
    }
  }
  // Look for integer constant operand.  This cannot match "m", and "i" is
  // better than "r".  FIXME target-dependent immediate letters are not handled
  // yet; in general they require looking at the value.
  if (isa<INTEGER_CST>(Operand)) {
    do {
      RetVal = -1;
      if (*p == 'i' || *p == 'n') {     // integer constant
        RetVal = 1;
        break;
      }
      if (*p != 'm' && *p != 'o' && *p != 'V')    // not memory
        RetVal = 0;
      ++p;
    } while (*p != ',' && *p != 0);
  }
  /// TEMPORARY.  This has the effect that alternative 0 is always chosen,
  /// except in the cases handled above.
  return RetVal;
}

/// ChooseConstraintTuple: we know each of the NumInputs+NumOutputs strings
/// in Constraints[] is a comma-separated list of NumChoices different
/// constraints.  Look through the operands and constraint possibilities
/// and pick a tuple where all the operands match.  Replace the strings
/// in Constraints[] with the shorter strings from that tuple (malloc'ed,
/// caller is responsible for cleaning it up).  Later processing can alter what
/// Constraints points to, so to make sure we delete everything, the addresses
/// of everything we allocated also are returned in StringStorage.
/// Casting back and forth from char* to const char* is Ugly, but we have to
/// interface with C code that expects const char*.
///
/// gcc's algorithm for picking "the best" tuple is quite complicated, and
/// is performed after things like SROA, not before.  At the moment we are
/// just trying to pick one that will work.  This may get refined.
static void ChooseConstraintTuple(gimple stmt, const char **Constraints,
                                  unsigned NumChoices,
                                  BumpPtrAllocator &StringStorage) {
  unsigned NumInputs = gimple_asm_ninputs(stmt);
  unsigned NumOutputs = gimple_asm_noutputs(stmt);

  int MaxWeight = -1;
  unsigned int CommasToSkip = 0;
  int *Weights = (int *)alloca(NumChoices * sizeof(int));
  // RunningConstraints is pointers into the Constraints strings which
  // are incremented as we go to point to the beginning of each
  // comma-separated alternative.
  const char** RunningConstraints =
    (const char**)alloca((NumInputs+NumOutputs)*sizeof(const char*));
  memcpy(RunningConstraints, Constraints,
         (NumInputs+NumOutputs) * sizeof(const char*));
  // The entire point of this loop is to compute CommasToSkip.
  for (unsigned i = 0; i != NumChoices; ++i) {
    Weights[i] = 0;
    for (unsigned j = 0; j != NumOutputs; ++j) {
      tree Output = gimple_asm_output_op(stmt, j);
      if (i==0)
        RunningConstraints[j]++;    // skip leading =
      const char* p = RunningConstraints[j];
      while (*p=='*' || *p=='&' || *p=='%')   // skip modifiers
        p++;
      if (Weights[i] != -1) {
        int w = MatchWeight(p, TREE_VALUE(Output));
        // Nonmatch means the entire tuple doesn't match.  However, we
        // keep scanning to set up RunningConstraints correctly for the
        // next tuple.
        if (w < 0)
          Weights[i] = -1;
        else
          Weights[i] += w;
      }
      while (*p!=0 && *p!=',')
        p++;
      if (*p!=0) {
        p++;      // skip comma
        while (*p=='*' || *p=='&' || *p=='%')
          p++;    // skip modifiers
      }
      RunningConstraints[j] = p;
    }
    for (unsigned j = 0; j != NumInputs; ++j) {
      tree Input = gimple_asm_input_op(stmt, j);
      const char* p = RunningConstraints[NumOutputs + j];
      if (Weights[i] != -1) {
        int w = MatchWeight(p, TREE_VALUE(Input));
        if (w < 0)
          Weights[i] = -1;    // As above.
        else
          Weights[i] += w;
      }
      while (*p!=0 && *p!=',')
        p++;
      if (*p!=0)
        p++;
      RunningConstraints[NumOutputs + j] = p;
    }
    if (Weights[i]>MaxWeight) {
      CommasToSkip = i;
      MaxWeight = Weights[i];
    }
  }
  // We have picked an alternative (the CommasToSkip'th one).
  // Change Constraints to point to malloc'd copies of the appropriate
  // constraints picked out of the original strings.
  for (unsigned int i=0; i<NumInputs+NumOutputs; i++) {
    assert(*(RunningConstraints[i])==0);   // sanity check
    const char* start = Constraints[i];
    if (i<NumOutputs)
      start++;          // skip '=' or '+'
    const char* end = start;
    while (*end != ',' && *end != 0)
      end++;
    for (unsigned int j=0; j<CommasToSkip; j++) {
      start = end+1;
      end = start;
      while (*end != ',' && *end != 0)
        end++;
    }
    // String we want is at start..end-1 inclusive.
    // For outputs, copy the leading = or +.
    char *newstring;
    if (i<NumOutputs) {
      newstring = StringStorage.Allocate<char>(end-start+1+1);
      newstring[0] = *(Constraints[i]);
      strncpy(newstring+1, start, end-start);
      newstring[end-start+1] = 0;
    } else {
      newstring = StringStorage.Allocate<char>(end-start+1);
      strncpy(newstring, start, end-start);
      newstring[end-start] = 0;
    }
    Constraints[i] = (const char *)newstring;
  }
}


//===----------------------------------------------------------------------===//
//               ... Helpers for Builtin Function Expansion ...
//===----------------------------------------------------------------------===//

Value *TreeToLLVM::BuildVector(const std::vector<Value*> &Ops) {
  assert((Ops.size() & (Ops.size()-1)) == 0 &&
         "Not a power-of-two sized vector!");
  bool AllConstants = true;
  for (unsigned i = 0, e = Ops.size(); i != e && AllConstants; ++i)
    AllConstants &= isa<Constant>(Ops[i]);

  // If this is a constant vector, create a ConstantVector.
  if (AllConstants) {
    SmallVector<Constant*, 16> CstOps;
    for (unsigned i = 0, e = Ops.size(); i != e; ++i)
      CstOps.push_back(cast<Constant>(Ops[i]));
    return ConstantVector::get(CstOps);
  }

  // Otherwise, insertelement the values to build the vector.
  Value *Result =
    UndefValue::get(VectorType::get(Ops[0]->getType(), Ops.size()));

  for (unsigned i = 0, e = Ops.size(); i != e; ++i)
    Result = Builder.CreateInsertElement(Result, Ops[i], Builder.getInt32(i));

  return Result;
}

/// BuildVector - This varargs function builds a literal vector ({} syntax) with
/// the specified null-terminated list of elements.  The elements must be all
/// the same element type and there must be a power of two of them.
Value *TreeToLLVM::BuildVector(Value *Elt, ...) {
  std::vector<Value*> Ops;
  va_list VA;
  va_start(VA, Elt);

  Ops.push_back(Elt);
  while (Value *Arg = va_arg(VA, Value *))
    Ops.push_back(Arg);
  va_end(VA);

  return BuildVector(Ops);
}

/// BuildVectorShuffle - Given two vectors and a variable length list of int
/// constants, create a shuffle of the elements of the inputs, where each dest
/// is specified by the indexes.  The int constant list must be as long as the
/// number of elements in the input vector.
///
/// Undef values may be specified by passing in -1 as the result value.
///
Value *TreeToLLVM::BuildVectorShuffle(Value *InVec1, Value *InVec2, ...) {
  assert(InVec1->getType()->isVectorTy() &&
         InVec1->getType() == InVec2->getType() && "Invalid shuffle!");
  unsigned NumElements = cast<VectorType>(InVec1->getType())->getNumElements();

  // Get all the indexes from varargs.
  SmallVector<Constant*, 16> Idxs;
  va_list VA;
  va_start(VA, InVec2);
  for (unsigned i = 0; i != NumElements; ++i) {
    int idx = va_arg(VA, int);
    if (idx == -1)
      Idxs.push_back(UndefValue::get(Type::getInt32Ty(Context)));
    else {
      assert((unsigned)idx < 2*NumElements && "Element index out of range!");
      Idxs.push_back(Builder.getInt32(idx));
    }
  }
  va_end(VA);

  // Turn this into the appropriate shuffle operation.
  return Builder.CreateShuffleVector(InVec1, InVec2,
                                     ConstantVector::get(Idxs));
}

//===----------------------------------------------------------------------===//
//                     ... Builtin Function Expansion ...
//===----------------------------------------------------------------------===//

/// EmitFrontendExpandedBuiltinCall - We allow the target to do some amount
/// of lowering.  This allows us to avoid having intrinsics for operations that
/// directly correspond to LLVM constructs.
///
/// This method returns true if the builtin is handled, otherwise false.
///
bool TreeToLLVM::EmitFrontendExpandedBuiltinCall(gimple stmt, tree fndecl,
                                                 const MemRef *DestLoc,
                                                 Value *&Result) {
#ifdef LLVM_TARGET_INTRINSIC_LOWER
  // Get the result type and operand line in an easy to consume format.
  Type *ResultType = ConvertType(TREE_TYPE(TREE_TYPE(fndecl)));
  std::vector<Value*> Operands;
  for (unsigned i = 0, e = gimple_call_num_args(stmt); i != e; ++i) {
    tree OpVal = gimple_call_arg(stmt, i);
    if (isa<AGGREGATE_TYPE>(TREE_TYPE(OpVal))) {
      MemRef OpLoc = CreateTempLoc(ConvertType(TREE_TYPE(OpVal)));
      EmitAggregate(OpVal, OpLoc);
      Operands.push_back(Builder.CreateLoad(OpLoc.Ptr));
    } else {
      Operands.push_back(EmitMemory(OpVal));
    }
  }

  return LLVM_TARGET_INTRINSIC_LOWER(stmt, fndecl, DestLoc, Result, ResultType,
                                     Operands);
#else
  // Avoid compiler warnings about unused parameters.
  (void)stmt; (void)fndecl; (void)DestLoc; (void)Result;
  return false;
#endif
}

/// TargetBuiltinCache - A cache of builtin intrinsics indexed by the GCC
/// builtin number.
static std::vector<Constant*> TargetBuiltinCache;

Value *TreeToLLVM::BuildBinaryAtomic(gimple stmt, AtomicRMWInst::BinOp Kind,
                                     unsigned PostOp) {
  tree return_type = gimple_call_return_type(stmt);
  Type *ResultTy = ConvertType(return_type);
  Value* C[2] = {
    EmitMemory(gimple_call_arg(stmt, 0)),
    EmitMemory(gimple_call_arg(stmt, 1))
  };
  Type* Ty[2];
  Ty[0] = ResultTy;
  Ty[1] = ResultTy->getPointerTo();
  C[0] = Builder.CreateBitCast(C[0], Ty[1]);
  C[1] = Builder.CreateIntCast(C[1], Ty[0],
                               /*isSigned*/!TYPE_UNSIGNED(return_type),
                               "cast");
  Value *Result = Builder.CreateAtomicRMW(Kind, C[0], C[1],
                                          SequentiallyConsistent);
  if (PostOp)
    Result = Builder.CreateBinOp(Instruction::BinaryOps(PostOp), Result, C[1]);

  Result = Builder.CreateIntToPtr(Result, ResultTy);
  return Result;
}

Value *
TreeToLLVM::BuildCmpAndSwapAtomic(gimple stmt, unsigned Bits, bool isBool) {
  tree ptr = gimple_call_arg(stmt, 0);
  tree old_val = gimple_call_arg(stmt, 1);
  tree new_val = gimple_call_arg(stmt, 2);

  // The type loaded from/stored to memory.
  Type *MemTy = IntegerType::get(Context, Bits);
  Type *MemPtrTy = MemTy->getPointerTo();

  Value *Ptr = Builder.CreateBitCast(EmitRegister(ptr), MemPtrTy);
  Value *Old_Val = CastToAnyType(EmitRegister(old_val),
                                 !TYPE_UNSIGNED(TREE_TYPE(old_val)), MemTy,
                                 !TYPE_UNSIGNED(TREE_TYPE(old_val)));
  Value *New_Val = CastToAnyType(EmitRegister(new_val),
                                 !TYPE_UNSIGNED(TREE_TYPE(new_val)), MemTy,
                                 !TYPE_UNSIGNED(TREE_TYPE(new_val)));

  Value *C[3] = { Ptr, Old_Val, New_Val };
  Value *Result = Builder.CreateAtomicCmpXchg(C[0], C[1], C[2],
                                              SequentiallyConsistent);

  if (isBool)
    Result = Builder.CreateICmpEQ(Result, Old_Val);
  tree return_type = gimple_call_return_type(stmt);
  Result = CastToAnyType(Result, !TYPE_UNSIGNED(return_type),
                         getRegType(return_type), !TYPE_UNSIGNED(return_type));
  return Reg2Mem(Result, return_type, Builder);
}

/// EmitBuiltinCall - stmt is a call to fndecl, a builtin function.  Try to emit
/// the call in a special way, setting Result to the scalar result if necessary.
/// If we can't handle the builtin, return false, otherwise return true.
bool TreeToLLVM::EmitBuiltinCall(gimple stmt, tree fndecl,
                                 const MemRef *DestLoc, Value *&Result) {
  if (DECL_BUILT_IN_CLASS(fndecl) == BUILT_IN_MD) {
    unsigned FnCode = DECL_FUNCTION_CODE(fndecl);
    if (TargetBuiltinCache.size() <= FnCode)
      TargetBuiltinCache.resize(FnCode+1);

    // If we haven't converted this intrinsic over yet, do so now.
    if (TargetBuiltinCache[FnCode] == 0) {
      const char *TargetPrefix = "";
#ifdef LLVM_TARGET_INTRINSIC_PREFIX
      TargetPrefix = LLVM_TARGET_INTRINSIC_PREFIX;
#endif
      // If the backend has some special code to lower, go ahead and try to
      // do that first.
      if (EmitFrontendExpandedBuiltinCall(stmt, fndecl, DestLoc, Result))
        return true;

      // If this builtin directly corresponds to an LLVM intrinsic, get the
      // IntrinsicID now.
      const char *BuiltinName = IDENTIFIER_POINTER(DECL_NAME(fndecl));
      Intrinsic::ID IntrinsicID =
        Intrinsic::getIntrinsicForGCCBuiltin(TargetPrefix, BuiltinName);
      if (IntrinsicID == Intrinsic::not_intrinsic) {
        error("unsupported target builtin %<%s%> used", BuiltinName);
        Type *ResTy = ConvertType(gimple_call_return_type(stmt));
        if (ResTy->isSingleValueType())
          Result = UndefValue::get(ResTy);
        return true;
      }

      // Finally, map the intrinsic ID back to a name.
      TargetBuiltinCache[FnCode] =
        Intrinsic::getDeclaration(TheModule, IntrinsicID);
    }

    Result = EmitCallOf(TargetBuiltinCache[FnCode], stmt, DestLoc,
                        AttrListPtr());
    return true;
  }

  enum built_in_function fcode = DECL_FUNCTION_CODE(fndecl);
  switch (fcode) {
  default: return false;
  // Varargs builtins.
  case BUILT_IN_VA_START:       return EmitBuiltinVAStart(stmt);
  case BUILT_IN_VA_END:         return EmitBuiltinVAEnd(stmt);
  case BUILT_IN_VA_COPY:        return EmitBuiltinVACopy(stmt);

  case BUILT_IN_ADJUST_TRAMPOLINE:
    return EmitBuiltinAdjustTrampoline(stmt, Result);
  case BUILT_IN_ALLOCA:         return EmitBuiltinAlloca(stmt, Result);
#if (GCC_MINOR > 6)
  case BUILT_IN_ALLOCA_WITH_ALIGN:
                                return EmitBuiltinAllocaWithAlign(stmt, Result);
#endif
  case BUILT_IN_BZERO:          return EmitBuiltinBZero(stmt, Result);
  case BUILT_IN_CONSTANT_P:     return EmitBuiltinConstantP(stmt, Result);
  case BUILT_IN_EXPECT:         return EmitBuiltinExpect(stmt, Result);
  case BUILT_IN_EXTEND_POINTER: return EmitBuiltinExtendPointer(stmt, Result);
  case BUILT_IN_EXTRACT_RETURN_ADDR:
   return EmitBuiltinExtractReturnAddr(stmt, Result);
  case BUILT_IN_FRAME_ADDRESS:  return EmitBuiltinReturnAddr(stmt, Result,true);
  case BUILT_IN_FROB_RETURN_ADDR:
   return EmitBuiltinFrobReturnAddr(stmt, Result);
  case BUILT_IN_INIT_TRAMPOLINE:
    return EmitBuiltinInitTrampoline(stmt, Result);
  case BUILT_IN_MEMCPY:         return EmitBuiltinMemCopy(stmt, Result,
                                                          false, false);
  case BUILT_IN_MEMCPY_CHK:     return EmitBuiltinMemCopy(stmt, Result,
                                                          false, true);
  case BUILT_IN_MEMMOVE:        return EmitBuiltinMemCopy(stmt, Result,
                                                          true, false);
  case BUILT_IN_MEMMOVE_CHK:    return EmitBuiltinMemCopy(stmt, Result,
                                                          true, true);
  case BUILT_IN_MEMSET:         return EmitBuiltinMemSet(stmt, Result, false);
  case BUILT_IN_MEMSET_CHK:     return EmitBuiltinMemSet(stmt, Result, true);
  case BUILT_IN_PREFETCH:       return EmitBuiltinPrefetch(stmt);
  case BUILT_IN_RETURN_ADDRESS:
    return EmitBuiltinReturnAddr(stmt, Result,false);
  case BUILT_IN_STACK_RESTORE:  return EmitBuiltinStackRestore(stmt);
  case BUILT_IN_STACK_SAVE:     return EmitBuiltinStackSave(stmt, Result);
  case BUILT_IN_UNREACHABLE:    return EmitBuiltinUnreachable();

  // Exception handling builtins.
  case BUILT_IN_EH_COPY_VALUES:
    return EmitBuiltinEHCopyValues(stmt);
  case BUILT_IN_EH_FILTER:
    return EmitBuiltinEHFilter(stmt, Result);
  case BUILT_IN_EH_POINTER:
    return EmitBuiltinEHPointer(stmt, Result);

  // Builtins used by the exception handling runtime.
  case BUILT_IN_DWARF_CFA:
    return EmitBuiltinDwarfCFA(stmt, Result);
#ifdef DWARF2_UNWIND_INFO
  case BUILT_IN_DWARF_SP_COLUMN:
    return EmitBuiltinDwarfSPColumn(stmt, Result);
  case BUILT_IN_INIT_DWARF_REG_SIZES:
    return EmitBuiltinInitDwarfRegSizes(stmt, Result);
#endif
  case BUILT_IN_EH_RETURN:
    return EmitBuiltinEHReturn(stmt, Result);
#ifdef EH_RETURN_DATA_REGNO
  case BUILT_IN_EH_RETURN_DATA_REGNO:
    return EmitBuiltinEHReturnDataRegno(stmt, Result);
#endif
  case BUILT_IN_UNWIND_INIT:
    return EmitBuiltinUnwindInit(stmt, Result);

  case BUILT_IN_OBJECT_SIZE: {
    if (!validate_gimple_arglist(stmt, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE)) {
      error("Invalid builtin_object_size argument types");
      return false;
    }
    tree ObjSizeTree = gimple_call_arg(stmt, 1);
    STRIP_NOPS (ObjSizeTree);
    if (!isa<INTEGER_CST>(ObjSizeTree)
        || tree_int_cst_sgn (ObjSizeTree) < 0
        || compare_tree_int (ObjSizeTree, 3) > 0) {
      error("Invalid second builtin_object_size argument");
      return false;
    }

    // LLVM doesn't handle type 1 or type 3. Deal with that here.
    Value *Tmp = EmitMemory(gimple_call_arg(stmt, 1));

    ConstantInt *CI = cast<ConstantInt>(Tmp);

    // Clear the bottom bit since we only handle whole objects and shift to turn
    // the second bit into our boolean.
    uint64_t val = (CI->getZExtValue() & 0x2) >> 1;

    Value *NewTy = ConstantInt::get(Tmp->getType(), val);

    Value* Args[] = {
      EmitMemory(gimple_call_arg(stmt, 0)),
      NewTy
    };

    // Grab the current return type.
    Type* Ty = ConvertType(gimple_call_return_type(stmt));

    // Manually coerce the arg to the correct pointer type.
    Args[0] = Builder.CreateBitCast(Args[0], Type::getInt8PtrTy(Context));
    Args[1] = Builder.CreateIntCast(Args[1], Type::getInt1Ty(Context),
                                    /*isSigned*/false);

    Result = Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                          Intrinsic::objectsize,
                                                          Ty),
                                Args);
    return true;
  }
  // Unary bit counting intrinsics.
  // NOTE: do not merge these case statements.  That will cause the memoized
  // Function* to be incorrectly shared across the different typed functions.
  case BUILT_IN_CLZ:       // These GCC builtins always return int.
  case BUILT_IN_CLZL:
  case BUILT_IN_CLZLL:
    Result = EmitBuiltinBitCountIntrinsic(stmt, Intrinsic::ctlz);
    return true;
  case BUILT_IN_CTZ:       // These GCC builtins always return int.
  case BUILT_IN_CTZL:
  case BUILT_IN_CTZLL:
    Result = EmitBuiltinBitCountIntrinsic(stmt, Intrinsic::cttz);
    return true;
  case BUILT_IN_PARITYLL:
  case BUILT_IN_PARITYL:
  case BUILT_IN_PARITY: {
    Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
    EmitBuiltinUnaryOp(Amt, Result, Intrinsic::ctpop);
    Result = Builder.CreateBinOp(Instruction::And, Result,
                                 ConstantInt::get(Result->getType(), 1));
    tree return_type = gimple_call_return_type(stmt);
    Type *DestTy = ConvertType(return_type);
    Result = Builder.CreateIntCast(Result, DestTy,
                                   /*isSigned*/!TYPE_UNSIGNED(return_type),
                                   "cast");
    return true;
  }
  case BUILT_IN_POPCOUNT:  // These GCC builtins always return int.
  case BUILT_IN_POPCOUNTL:
  case BUILT_IN_POPCOUNTLL: {
    Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
    EmitBuiltinUnaryOp(Amt, Result, Intrinsic::ctpop);
    tree return_type = gimple_call_return_type(stmt);
    Type *DestTy = ConvertType(return_type);
    Result = Builder.CreateIntCast(Result, DestTy,
                                   /*isSigned*/!TYPE_UNSIGNED(return_type),
                                   "cast");
    return true;
  }
  case BUILT_IN_BSWAP32:
  case BUILT_IN_BSWAP64: {
    Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
    EmitBuiltinUnaryOp(Amt, Result, Intrinsic::bswap);
    tree return_type = gimple_call_return_type(stmt);
    Type *DestTy = ConvertType(return_type);
    Result = Builder.CreateIntCast(Result, DestTy,
                                   /*isSigned*/!TYPE_UNSIGNED(return_type),
                                   "cast");
    return true;
  }

  case BUILT_IN_SQRT:
  case BUILT_IN_SQRTF:
  case BUILT_IN_SQRTL:
    // The result of sqrt(negative) is implementation-defined, but follows
    // IEEE754 in most current implementations. llvm.sqrt, which has undefined
    // behavior for such inputs, is an inappropriate substitute.
    break;
  case BUILT_IN_POWI:
  case BUILT_IN_POWIF:
  case BUILT_IN_POWIL:
    Result = EmitBuiltinPOWI(stmt);
    return true;
  case BUILT_IN_POW:
  case BUILT_IN_POWF:
  case BUILT_IN_POWL:
    // If errno math has been disabled, expand these to llvm.pow calls.
    if (!flag_errno_math) {
      Result = EmitBuiltinPOW(stmt);
      return true;
    }
    break;
  case BUILT_IN_LOG:
  case BUILT_IN_LOGF:
  case BUILT_IN_LOGL:
    // If errno math has been disabled, expand these to llvm.log calls.
    if (!flag_errno_math) {
      Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
      EmitBuiltinUnaryOp(Amt, Result, Intrinsic::log);
      Result = CastToFPType(Result, ConvertType(gimple_call_return_type(stmt)));
      return true;
    }
    break;
  case BUILT_IN_LOG2:
  case BUILT_IN_LOG2F:
  case BUILT_IN_LOG2L:
    // If errno math has been disabled, expand these to llvm.log2 calls.
    if (!flag_errno_math) {
      Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
      EmitBuiltinUnaryOp(Amt, Result, Intrinsic::log2);
      Result = CastToFPType(Result, ConvertType(gimple_call_return_type(stmt)));
      return true;
    }
    break;
  case BUILT_IN_LOG10:
  case BUILT_IN_LOG10F:
  case BUILT_IN_LOG10L:
    // If errno math has been disabled, expand these to llvm.log10 calls.
    if (!flag_errno_math) {
      Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
      EmitBuiltinUnaryOp(Amt, Result, Intrinsic::log10);
      Result = CastToFPType(Result, ConvertType(gimple_call_return_type(stmt)));
      return true;
    }
    break;
  case BUILT_IN_EXP:
  case BUILT_IN_EXPF:
  case BUILT_IN_EXPL:
    // If errno math has been disabled, expand these to llvm.exp calls.
    if (!flag_errno_math) {
      Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
      EmitBuiltinUnaryOp(Amt, Result, Intrinsic::exp);
      Result = CastToFPType(Result, ConvertType(gimple_call_return_type(stmt)));
      return true;
    }
    break;
  case BUILT_IN_EXP2:
  case BUILT_IN_EXP2F:
  case BUILT_IN_EXP2L:
    // If errno math has been disabled, expand these to llvm.exp2 calls.
    if (!flag_errno_math) {
      Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
      EmitBuiltinUnaryOp(Amt, Result, Intrinsic::exp2);
      Result = CastToFPType(Result, ConvertType(gimple_call_return_type(stmt)));
      return true;
    }
    break;
  case BUILT_IN_FFS:  // These GCC builtins always return int.
  case BUILT_IN_FFSL:
  case BUILT_IN_FFSLL: {      // FFS(X) -> (x == 0 ? 0 : CTTZ(x)+1)
    // The argument and return type of cttz should match the argument type of
    // the ffs, but should ignore the return type of ffs.
    Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
    Result = Builder.CreateCall2(Intrinsic::getDeclaration(TheModule,
                                                           Intrinsic::cttz,
                                                           Amt->getType()),
                                 Amt, Builder.getTrue());
    Result = Builder.CreateAdd(Result, ConstantInt::get(Result->getType(), 1));
    Result = Builder.CreateIntCast(Result,
                                   ConvertType(gimple_call_return_type(stmt)),
                                   /*isSigned*/false);
    Value *Cond =
      Builder.CreateICmpEQ(Amt,
                           Constant::getNullValue(Amt->getType()));
    Result = Builder.CreateSelect(Cond,
                           Constant::getNullValue(Result->getType()),
                                  Result);
    return true;
  }
  case BUILT_IN_LCEIL:
  case BUILT_IN_LCEILF:
  case BUILT_IN_LCEILL:
  case BUILT_IN_LLCEIL:
  case BUILT_IN_LLCEILF:
  case BUILT_IN_LLCEILL:
    Result = EmitBuiltinLCEIL(stmt);
    return true;
  case BUILT_IN_LFLOOR:
  case BUILT_IN_LFLOORF:
  case BUILT_IN_LFLOORL:
  case BUILT_IN_LLFLOOR:
  case BUILT_IN_LLFLOORF:
  case BUILT_IN_LLFLOORL:
    Result = EmitBuiltinLFLOOR(stmt);
    return true;
  case BUILT_IN_CEXPI:
  case BUILT_IN_CEXPIF:
  case BUILT_IN_CEXPIL:
    Result = EmitBuiltinCEXPI(stmt);
    return true;
//TODO  case BUILT_IN_FLT_ROUNDS: {
//TODO    Result =
//TODO      Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
//TODO                                                   Intrinsic::flt_rounds));
//TODO    Result = Builder.CreateBitCast(Result, ConvertType(gimple_call_return_type(stmt)));
//TODO    return true;
//TODO  }
  case BUILT_IN_TRAP:
    Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Intrinsic::trap));
    // Emit an explicit unreachable instruction.
    Builder.CreateUnreachable();
    BeginBlock(BasicBlock::Create(Context));
    return true;

//TODO  // Convert annotation built-in to llvm.annotation intrinsic.
//TODO  case BUILT_IN_ANNOTATION: {
//TODO
//TODO    // Get file and line number
//TODO    location_t locus = gimple_location(stmt);
//TODO    Constant *lineNo = ConstantInt::get(Type::getInt32Ty, LOCATION_LINE(locus));
//TODO    Constant *file = ConvertMetadataStringToGV(LOCATION_FILE(locus));
//TODO    Type *SBP= Type::getInt8PtrTy(Context);
//TODO    file = TheFolder->CreateBitCast(file, SBP);
//TODO
//TODO    // Get arguments.
//TODO    tree arglist = CALL_EXPR_ARGS(stmt);
//TODO    Value *ExprVal = EmitMemory(gimple_call_arg(stmt, 0));
//TODO    Type *Ty = ExprVal->getType();
//TODO    Value *StrVal = EmitMemory(gimple_call_arg(stmt, 1));
//TODO
//TODO    SmallVector<Value *, 4> Args;
//TODO    Args.push_back(ExprVal);
//TODO    Args.push_back(StrVal);
//TODO    Args.push_back(file);
//TODO    Args.push_back(lineNo);
//TODO
//TODO    assert(Ty && "llvm.annotation arg type may not be null");
//TODO    Result = Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
//TODO                                                          Intrinsic::annotation,
//TODO                                                          &Ty,
//TODO                                                          1),
//TODO                                Args.begin(), Args.end());
//TODO    return true;
//TODO  }

#if (GCC_MINOR < 7)
  case BUILT_IN_SYNCHRONIZE:
#else
  case BUILT_IN_SYNC_SYNCHRONIZE:
#endif
    // We assume like gcc appears to, that this only applies to cached memory.
    Builder.CreateFence(llvm::SequentiallyConsistent);
    return true;
#if defined(TARGET_ALPHA) || defined(TARGET_386) || defined(TARGET_POWERPC) \
    || defined(TARGET_ARM)
    // gcc uses many names for the sync intrinsics
    // The type of the first argument is not reliable for choosing the
    // right llvm function; if the original type is not volatile, gcc has
    // helpfully changed it to "volatile void *" at this point.  The
    // original type can be recovered from the function type in most cases.
    // For lock_release and bool_compare_and_swap even that is not good
    // enough, we have to key off the opcode.
    // Note that Intrinsic::getDeclaration expects the type list in reversed
    // order, while CreateCall expects the parameter list in normal order.
#if (GCC_MINOR < 7)
  case BUILT_IN_BOOL_COMPARE_AND_SWAP_1:
#else
  case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_1:
#endif
    Result = BuildCmpAndSwapAtomic(stmt, BITS_PER_UNIT, true);
    return true;
#if (GCC_MINOR < 7)
  case BUILT_IN_BOOL_COMPARE_AND_SWAP_2:
#else
  case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_2:
#endif
    Result = BuildCmpAndSwapAtomic(stmt, 2*BITS_PER_UNIT, true);
    return true;
#if (GCC_MINOR < 7)
  case BUILT_IN_BOOL_COMPARE_AND_SWAP_4:
#else
  case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_4:
#endif
    Result = BuildCmpAndSwapAtomic(stmt, 4*BITS_PER_UNIT, true);
    return true;
#if (GCC_MINOR < 7)
  case BUILT_IN_BOOL_COMPARE_AND_SWAP_8:
#else
  case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
    Result = BuildCmpAndSwapAtomic(stmt, 8*BITS_PER_UNIT, true);
    return true;

    // Fall through.
#if (GCC_MINOR < 7)
  case BUILT_IN_VAL_COMPARE_AND_SWAP_1:
#else
  case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_1:
#endif
    Result = BuildCmpAndSwapAtomic(stmt, BITS_PER_UNIT, false);
    return true;
#if (GCC_MINOR < 7)
  case BUILT_IN_VAL_COMPARE_AND_SWAP_2:
#else
  case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_2:
#endif
    Result = BuildCmpAndSwapAtomic(stmt, 2*BITS_PER_UNIT, false);
    return true;
#if (GCC_MINOR < 7)
  case BUILT_IN_VAL_COMPARE_AND_SWAP_4:
#else
  case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_4:
#endif
    Result = BuildCmpAndSwapAtomic(stmt, 4*BITS_PER_UNIT, false);
    return true;
#if (GCC_MINOR < 7)
  case BUILT_IN_VAL_COMPARE_AND_SWAP_8:
#else
  case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
    Result = BuildCmpAndSwapAtomic(stmt, 8*BITS_PER_UNIT, false);
    return true;

#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_ADD_8:
#else
  case BUILT_IN_SYNC_FETCH_AND_ADD_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_ADD_1:
  case BUILT_IN_FETCH_AND_ADD_2:
  case BUILT_IN_FETCH_AND_ADD_4: {
#else
  case BUILT_IN_SYNC_FETCH_AND_ADD_1:
  case BUILT_IN_SYNC_FETCH_AND_ADD_2:
  case BUILT_IN_SYNC_FETCH_AND_ADD_4: {
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::Add);
    return true;
  }
#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_SUB_8:
#else
  case BUILT_IN_SYNC_FETCH_AND_SUB_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_SUB_1:
  case BUILT_IN_FETCH_AND_SUB_2:
  case BUILT_IN_FETCH_AND_SUB_4: {
#else
  case BUILT_IN_SYNC_FETCH_AND_SUB_1:
  case BUILT_IN_SYNC_FETCH_AND_SUB_2:
  case BUILT_IN_SYNC_FETCH_AND_SUB_4: {
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::Sub);
    return true;
  }
#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_OR_8:
#else
  case BUILT_IN_SYNC_FETCH_AND_OR_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_OR_1:
  case BUILT_IN_FETCH_AND_OR_2:
  case BUILT_IN_FETCH_AND_OR_4: {
#else
  case BUILT_IN_SYNC_FETCH_AND_OR_1:
  case BUILT_IN_SYNC_FETCH_AND_OR_2:
  case BUILT_IN_SYNC_FETCH_AND_OR_4: {
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::Or);
    return true;
  }
#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_AND_8:
#else
  case BUILT_IN_SYNC_FETCH_AND_AND_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_AND_1:
  case BUILT_IN_FETCH_AND_AND_2:
  case BUILT_IN_FETCH_AND_AND_4: {
#else
  case BUILT_IN_SYNC_FETCH_AND_AND_1:
  case BUILT_IN_SYNC_FETCH_AND_AND_2:
  case BUILT_IN_SYNC_FETCH_AND_AND_4: {
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::And);
    return true;
  }
#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_XOR_8:
#else
  case BUILT_IN_SYNC_FETCH_AND_XOR_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_XOR_1:
  case BUILT_IN_FETCH_AND_XOR_2:
  case BUILT_IN_FETCH_AND_XOR_4: {
#else
  case BUILT_IN_SYNC_FETCH_AND_XOR_1:
  case BUILT_IN_SYNC_FETCH_AND_XOR_2:
  case BUILT_IN_SYNC_FETCH_AND_XOR_4: {
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::Xor);
    return true;
  }
#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_NAND_8:
#else
  case BUILT_IN_SYNC_FETCH_AND_NAND_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_FETCH_AND_NAND_1:
  case BUILT_IN_FETCH_AND_NAND_2:
  case BUILT_IN_FETCH_AND_NAND_4: {
#else
  case BUILT_IN_SYNC_FETCH_AND_NAND_1:
  case BUILT_IN_SYNC_FETCH_AND_NAND_2:
  case BUILT_IN_SYNC_FETCH_AND_NAND_4: {
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::Nand);
    return true;
  }
#if (GCC_MINOR < 7)
  case BUILT_IN_LOCK_TEST_AND_SET_8:
#else
  case BUILT_IN_SYNC_LOCK_TEST_AND_SET_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_LOCK_TEST_AND_SET_1:
  case BUILT_IN_LOCK_TEST_AND_SET_2:
  case BUILT_IN_LOCK_TEST_AND_SET_4: {
#else
  case BUILT_IN_SYNC_LOCK_TEST_AND_SET_1:
  case BUILT_IN_SYNC_LOCK_TEST_AND_SET_2:
  case BUILT_IN_SYNC_LOCK_TEST_AND_SET_4: {
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::Xchg);
    return true;
  }

#if (GCC_MINOR < 7)
  case BUILT_IN_ADD_AND_FETCH_8:
#else
  case BUILT_IN_SYNC_ADD_AND_FETCH_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_ADD_AND_FETCH_1:
  case BUILT_IN_ADD_AND_FETCH_2:
  case BUILT_IN_ADD_AND_FETCH_4:
#else
  case BUILT_IN_SYNC_ADD_AND_FETCH_1:
  case BUILT_IN_SYNC_ADD_AND_FETCH_2:
  case BUILT_IN_SYNC_ADD_AND_FETCH_4:
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::Add, Instruction::Add);
    return true;
#if (GCC_MINOR < 7)
  case BUILT_IN_SUB_AND_FETCH_8:
#else
  case BUILT_IN_SYNC_SUB_AND_FETCH_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_SUB_AND_FETCH_1:
  case BUILT_IN_SUB_AND_FETCH_2:
  case BUILT_IN_SUB_AND_FETCH_4:
#else
  case BUILT_IN_SYNC_SUB_AND_FETCH_1:
  case BUILT_IN_SYNC_SUB_AND_FETCH_2:
  case BUILT_IN_SYNC_SUB_AND_FETCH_4:
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::Sub, Instruction::Sub);
    return true;
#if (GCC_MINOR < 7)
  case BUILT_IN_OR_AND_FETCH_8:
#else
  case BUILT_IN_SYNC_OR_AND_FETCH_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_OR_AND_FETCH_1:
  case BUILT_IN_OR_AND_FETCH_2:
  case BUILT_IN_OR_AND_FETCH_4:
#else
  case BUILT_IN_SYNC_OR_AND_FETCH_1:
  case BUILT_IN_SYNC_OR_AND_FETCH_2:
  case BUILT_IN_SYNC_OR_AND_FETCH_4:
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::Or, Instruction::Or);
    return true;
#if (GCC_MINOR < 7)
  case BUILT_IN_AND_AND_FETCH_8:
#else
  case BUILT_IN_SYNC_AND_AND_FETCH_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_AND_AND_FETCH_1:
  case BUILT_IN_AND_AND_FETCH_2:
  case BUILT_IN_AND_AND_FETCH_4:
#else
  case BUILT_IN_SYNC_AND_AND_FETCH_1:
  case BUILT_IN_SYNC_AND_AND_FETCH_2:
  case BUILT_IN_SYNC_AND_AND_FETCH_4:
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::And, Instruction::And);
    return true;
#if (GCC_MINOR < 7)
  case BUILT_IN_XOR_AND_FETCH_8:
#else
  case BUILT_IN_SYNC_XOR_AND_FETCH_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_XOR_AND_FETCH_1:
  case BUILT_IN_XOR_AND_FETCH_2:
  case BUILT_IN_XOR_AND_FETCH_4:
#else
  case BUILT_IN_SYNC_XOR_AND_FETCH_1:
  case BUILT_IN_SYNC_XOR_AND_FETCH_2:
  case BUILT_IN_SYNC_XOR_AND_FETCH_4:
#endif
    Result = BuildBinaryAtomic(stmt, AtomicRMWInst::Xor, Instruction::Xor);
    return true;
#if (GCC_MINOR < 7)
  case BUILT_IN_NAND_AND_FETCH_8:
#else
  case BUILT_IN_SYNC_NAND_AND_FETCH_8:
#endif
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
#if (GCC_MINOR < 7)
  case BUILT_IN_NAND_AND_FETCH_1:
  case BUILT_IN_NAND_AND_FETCH_2:
  case BUILT_IN_NAND_AND_FETCH_4: {
#else
  case BUILT_IN_SYNC_NAND_AND_FETCH_1:
  case BUILT_IN_SYNC_NAND_AND_FETCH_2:
  case BUILT_IN_SYNC_NAND_AND_FETCH_4: {
#endif
    tree return_type = gimple_call_return_type(stmt);
    Type *ResultTy = ConvertType(return_type);
    Value* C[2] = {
      EmitMemory(gimple_call_arg(stmt, 0)),
      EmitMemory(gimple_call_arg(stmt, 1))
    };
    C[0] = Builder.CreateBitCast(C[0], ResultTy->getPointerTo());
    C[1] = Builder.CreateIntCast(C[1], ResultTy,
                                 /*isSigned*/!TYPE_UNSIGNED(return_type),
                                 "cast");
    Result = Builder.CreateAtomicRMW(AtomicRMWInst::Nand, C[0], C[1],
                                     SequentiallyConsistent);

    Result = Builder.CreateAnd(Builder.CreateNot(Result), C[1]);
    Result = Builder.CreateIntToPtr(Result, ResultTy);
    return true;
  }

#if (GCC_MINOR < 7)
  case BUILT_IN_LOCK_RELEASE_1:
  case BUILT_IN_LOCK_RELEASE_2:
  case BUILT_IN_LOCK_RELEASE_4:
  case BUILT_IN_LOCK_RELEASE_8:
  case BUILT_IN_LOCK_RELEASE_16: {
#else
  case BUILT_IN_SYNC_LOCK_RELEASE_1:
  case BUILT_IN_SYNC_LOCK_RELEASE_2:
  case BUILT_IN_SYNC_LOCK_RELEASE_4:
  case BUILT_IN_SYNC_LOCK_RELEASE_8:
  case BUILT_IN_SYNC_LOCK_RELEASE_16: {
#endif
    // This is effectively a volatile store of 0, and has no return value.
    // The argument has typically been coerced to "volatile void*"; the
    // only way to find the size of the operation is from the builtin
    // opcode.
    // FIXME: This is wrong; it works to some extent on x86 if the optimizer
    // doesn't get too clever, and is horribly broken anywhere else.  It needs
    // to use "store atomic [...] release".
    Type *Ty;
    switch(DECL_FUNCTION_CODE(fndecl)) {
#if (GCC_MINOR < 7)
      case BUILT_IN_LOCK_RELEASE_16:    // not handled; should use SSE on x86
#else
      case BUILT_IN_SYNC_LOCK_RELEASE_16:    // not handled; should use SSE on x86
#endif
      default:
        llvm_unreachable("Not handled; should use SSE on x86!");
#if (GCC_MINOR < 7)
      case BUILT_IN_LOCK_RELEASE_1:
#else
      case BUILT_IN_SYNC_LOCK_RELEASE_1:
#endif
        Ty = Type::getInt8Ty(Context); break;
#if (GCC_MINOR < 7)
      case BUILT_IN_LOCK_RELEASE_2:
#else
      case BUILT_IN_SYNC_LOCK_RELEASE_2:
#endif
        Ty = Type::getInt16Ty(Context); break;
#if (GCC_MINOR < 7)
      case BUILT_IN_LOCK_RELEASE_4:
#else
      case BUILT_IN_SYNC_LOCK_RELEASE_4:
#endif
        Ty = Type::getInt32Ty(Context); break;
#if (GCC_MINOR < 7)
      case BUILT_IN_LOCK_RELEASE_8:
#else
      case BUILT_IN_SYNC_LOCK_RELEASE_8:
#endif
        Ty = Type::getInt64Ty(Context); break;
    }
    Value *Ptr = EmitMemory(gimple_call_arg(stmt, 0));
    Ptr = Builder.CreateBitCast(Ptr, Ty->getPointerTo());
    Builder.CreateStore(Constant::getNullValue(Ty), Ptr, true);
    Result = 0;
    return true;
  }

#endif //FIXME: these break the build for backends that haven't implemented them


#if 1  // FIXME: Should handle these GCC extensions eventually.
  case BUILT_IN_LONGJMP: {
    if (validate_gimple_arglist(stmt, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE)) {
      tree value = gimple_call_arg(stmt, 1);

      if (!isa<INTEGER_CST>(value) ||
          cast<ConstantInt>(EmitMemory(value))->getValue() != 1) {
        error ("%<__builtin_longjmp%> second argument must be 1");
        return false;
      }
    }
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    Value *Buf = Emit(TREE_VALUE(arglist), 0);
    Buf = Builder.CreateBitCast(Buf, Type::getInt8Ty(Context)->getPointerTo());
    Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                 Intrinsic::eh_sjlj_longjmp),
                      Buf);
    Result = 0;
    return true;
#endif
    // Fall-through
  }
  case BUILT_IN_APPLY_ARGS:
  case BUILT_IN_APPLY:
  case BUILT_IN_RETURN:
  case BUILT_IN_SAVEREGS:
#if (GCC_MINOR < 6)
  case BUILT_IN_ARGS_INFO:
#endif
  case BUILT_IN_NEXT_ARG:
  case BUILT_IN_CLASSIFY_TYPE:
  case BUILT_IN_AGGREGATE_INCOMING_ADDRESS:
  case BUILT_IN_SETJMP_SETUP:
  case BUILT_IN_SETJMP_DISPATCHER:
  case BUILT_IN_SETJMP_RECEIVER:
  case BUILT_IN_UPDATE_SETJMP_BUF:

    // FIXME: HACK: Just ignore these.
    {
      Type *Ty = ConvertType(gimple_call_return_type(stmt));
      if (!Ty->isVoidTy())
        Result = Constant::getNullValue(Ty);
      return true;
    }
#endif  // FIXME: Should handle these GCC extensions eventually.
  }
  return false;
}

bool TreeToLLVM::EmitBuiltinUnaryOp(Value *InVal, Value *&Result,
                                    Intrinsic::ID Id) {
  // The intrinsic might be overloaded in which case the argument is of
  // varying type. Make sure that we specify the actual type for "iAny"
  // by passing it as the 3rd and 4th parameters. This isn't needed for
  // most intrinsics, but is needed for ctpop, cttz, ctlz.
  Type *Ty = InVal->getType();
  Result = Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Id, Ty),
                              InVal);
  return true;
}

Value *TreeToLLVM::EmitBuiltinBitCountIntrinsic(gimple stmt, Intrinsic::ID Id) {
  Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
  Value *Result = Builder.CreateCall2(Intrinsic::getDeclaration(TheModule, Id,
                                                                Amt->getType()),
                                      Amt, Builder.getTrue());
  tree return_type = gimple_call_return_type(stmt);
  Type *DestTy = ConvertType(return_type);
  return Builder.CreateIntCast(Result, DestTy,
                               /*isSigned*/!TYPE_UNSIGNED(return_type),
                               "cast");
}

Value *TreeToLLVM::EmitBuiltinSQRT(gimple stmt) {
  Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
  Type* Ty = Amt->getType();

  return Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                      Intrinsic::sqrt, Ty),
                            Amt);
}

Value *TreeToLLVM::EmitBuiltinPOWI(gimple stmt) {
  if (!validate_gimple_arglist(stmt, REAL_TYPE, INTEGER_TYPE, VOID_TYPE))
    return 0;

  Value *Val = EmitMemory(gimple_call_arg(stmt, 0));
  Value *Pow = EmitMemory(gimple_call_arg(stmt, 1));
  Type *Ty = Val->getType();
  Pow = Builder.CreateIntCast(Pow, Type::getInt32Ty(Context), /*isSigned*/true);

  SmallVector<Value *,2> Args;
  Args.push_back(Val);
  Args.push_back(Pow);
  return Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                      Intrinsic::powi, Ty),
                            Args);
}

Value *TreeToLLVM::EmitBuiltinPOW(gimple stmt) {
  if (!validate_gimple_arglist(stmt, REAL_TYPE, REAL_TYPE, VOID_TYPE))
    return 0;

  Value *Val = EmitMemory(gimple_call_arg(stmt, 0));
  Value *Pow = EmitMemory(gimple_call_arg(stmt, 1));
  Type *Ty = Val->getType();

  SmallVector<Value *,2> Args;
  Args.push_back(Val);
  Args.push_back(Pow);
  return Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                      Intrinsic::pow, Ty),
                            Args);
}

Value *TreeToLLVM::EmitBuiltinLCEIL(gimple stmt) {
  if (!validate_gimple_arglist(stmt, REAL_TYPE, VOID_TYPE))
    return 0;

  // Cast the result of "ceil" to the appropriate integer type.
  // First call the appropriate version of "ceil".
  tree op = gimple_call_arg(stmt, 0);
  StringRef Name = SelectFPName(TREE_TYPE(op), "ceilf", "ceil", "ceill");
  assert(!Name.empty() && "Unsupported floating point type!");
  CallInst *Call = EmitSimpleCall(Name, TREE_TYPE(op), op, NULL);
  Call->setDoesNotThrow();
  Call->setDoesNotAccessMemory();

  // Then type cast the result of the "ceil" call.
  tree type = gimple_call_return_type(stmt);
  Type *RetTy = getRegType(type);
  return TYPE_UNSIGNED(type) ? Builder.CreateFPToUI(Call, RetTy) :
    Builder.CreateFPToSI(Call, RetTy);
}

Value *TreeToLLVM::EmitBuiltinLFLOOR(gimple stmt) {
  if (!validate_gimple_arglist(stmt, REAL_TYPE, VOID_TYPE))
    return 0;

  // Cast the result of "floor" to the appropriate integer type.
  // First call the appropriate version of "floor".
  tree op = gimple_call_arg(stmt, 0);
  StringRef Name = SelectFPName(TREE_TYPE(op), "floorf", "floor", "floorl");
  assert(!Name.empty() && "Unsupported floating point type!");
  CallInst *Call = EmitSimpleCall(Name, TREE_TYPE(op), op, NULL);
  Call->setDoesNotThrow();
  Call->setDoesNotAccessMemory();

  // Then type cast the result of the "floor" call.
  tree type = gimple_call_return_type(stmt);
  Type *RetTy = getRegType(type);
  return TYPE_UNSIGNED(type) ? Builder.CreateFPToUI(Call, RetTy) :
    Builder.CreateFPToSI(Call, RetTy);
}

Value *TreeToLLVM::EmitBuiltinCEXPI(gimple stmt) {
  if (!validate_gimple_arglist(stmt, REAL_TYPE, VOID_TYPE))
    return 0;

  if (TARGET_HAS_SINCOS) {
    // exp(i*arg) = cos(arg) + i*sin(arg).  Emit a call to sincos.  First
    // determine which version of sincos to call.
    tree arg = gimple_call_arg(stmt, 0);
    tree arg_type = TREE_TYPE(arg);
    StringRef Name = SelectFPName(arg_type, "sincosf", "sincos", "sincosl");
    assert(!Name.empty() && "Unsupported floating point type!");

    // Create stack slots to store the real (cos) and imaginary (sin) parts in.
    Value *Val = EmitRegister(arg);
    Value *SinPtr = CreateTemporary(Val->getType());
    Value *CosPtr = CreateTemporary(Val->getType());

    // Get the LLVM function declaration for sincos.
    Type *ArgTys[3] =
      { Val->getType(), SinPtr->getType(), CosPtr->getType() };
    FunctionType *FTy = FunctionType::get(Type::getVoidTy(Context),
                                                ArgTys, /*isVarArg*/false);
    Constant *Func = TheModule->getOrInsertFunction(Name, FTy);

    // Determine the calling convention.
    CallingConv::ID CC = CallingConv::C;
#ifdef TARGET_ADJUST_LLVM_CC
    // Query the target for the calling convention to use.
    tree fntype = build_function_type_list(void_type_node, arg_type,
                                           TYPE_POINTER_TO(arg_type),
                                           TYPE_POINTER_TO(arg_type),
                                           NULL_TREE);
    TARGET_ADJUST_LLVM_CC(CC, fntype);
#endif

    // If the function already existed with the wrong prototype then don't try to
    // muck with its calling convention.  Otherwise, set the calling convention.
    if (Function *F = dyn_cast<Function>(Func))
      F->setCallingConv(CC);

    // Call sincos.
    Value *Args[3] = { Val, SinPtr, CosPtr };
    CallInst *CI = Builder.CreateCall(Func, Args);
    CI->setCallingConv(CC);
    CI->setDoesNotThrow();

    // Load out the real (cos) and imaginary (sin) parts.
    Value *Sin = Builder.CreateLoad(SinPtr);
    Value *Cos = Builder.CreateLoad(CosPtr);

    // Return the complex number "cos(arg) + i*sin(arg)".
    return CreateComplex(Cos, Sin);
  } else {
    // Emit a call to cexp.  First determine which version of cexp to call.
    tree arg = gimple_call_arg(stmt, 0);
    tree arg_type = TREE_TYPE(arg);
    StringRef Name = SelectFPName(arg_type, "cexpf", "cexp", "cexpl");
    assert(!Name.empty() && "Unsupported floating point type!");

    // Get the GCC and LLVM function types for cexp.
    tree cplx_type = gimple_call_return_type(stmt);
    tree fntype = build_function_type_list(cplx_type, cplx_type, NULL_TREE);
    FunctionType *FTy = cast<FunctionType>(ConvertType(fntype));

    // Get the LLVM function declaration for cexp.
    Constant *Func = TheModule->getOrInsertFunction(Name, FTy);

    // Determine the calling convention.
    CallingConv::ID CC = CallingConv::C;
#ifdef TARGET_ADJUST_LLVM_CC
    // Query the target for the calling convention to use.
    TARGET_ADJUST_LLVM_CC(CC, fntype);
#endif

    // If the function already existed with the wrong prototype then don't try to
    // muck with its calling convention.  Otherwise, set the calling convention.
    if (Function *F = dyn_cast<Function>(Func))
      F->setCallingConv(CC);

    // Form the complex number "0 + i*arg".
    Value *Arg = EmitRegister(arg);
    Value *CplxArg = CreateComplex(Constant::getNullValue(Arg->getType()), Arg);

    // Call cexp and return the result.  This is rather painful because complex
    // numbers may be passed in funky ways and we don't have a proper interface
    // for marshalling call parameters.
    SmallVector<Value*, 16> CallOperands;
    FunctionCallArgumentConversion Client(CallOperands, FTy, /*destloc*/0,
                                          /*ReturnSlotOpt*/false, Builder, CC);
    DefaultABI ABIConverter(Client);

    // Handle the result.
    ABIConverter.HandleReturnType(cplx_type, fntype, false);

    // Push the argument.
    bool PassedInMemory;
    Type *CplxTy = CplxArg->getType();
    if (LLVM_SHOULD_PASS_AGGREGATE_AS_FCA(cplx_type, CplxTy)) {
      Client.pushValue(CplxArg);
      PassedInMemory = false;
    } else {
      // Push the address of a temporary copy.
      MemRef Copy = CreateTempLoc(CplxTy);
      StoreRegisterToMemory(CplxArg, Copy, cplx_type, 0, Builder);
      Client.pushAddress(Copy.Ptr);
      PassedInMemory = true;
    }

    AttrBuilder AttrBuilder;
    std::vector<Type*> ScalarArgs;
    ABIConverter.HandleArgument(cplx_type, ScalarArgs, &AttrBuilder);
    assert(!AttrBuilder.hasAttributes() && "Got attributes but none given!");
    Client.clear();

    // Create the call.
    CallInst *CI = Builder.CreateCall(Func, CallOperands);
    CI->setCallingConv(CC);
    CI->setDoesNotThrow();
    if (!PassedInMemory)
      CI->setDoesNotAccessMemory();

    // Extract and return the result.
    if (Client.isShadowReturn())
      return Client.EmitShadowResult(cplx_type, 0);

    if (Client.isAggrReturn()) {
      // Extract to a temporary then load the value out later.
      MemRef Target = CreateTempLoc(CplxTy);

      assert(DL.getTypeAllocSize(CI->getType()) <= DL.getTypeAllocSize(CplxTy)
             && "Complex number returned in too large registers!");
      Value *Dest = Builder.CreateBitCast(Target.Ptr,
                                          CI->getType()->getPointerTo());
      LLVM_EXTRACT_MULTIPLE_RETURN_VALUE(CI, Dest, Target.Volatile, Builder);
      return Builder.CreateLoad(Target.Ptr);
    }

    if (CI->getType() == CplxTy)
      return CI;   // Normal scalar return.

    // Probably { float, float } being returned as a double.
    assert(DL.getTypeAllocSize(CI->getType()) == DL.getTypeAllocSize(CplxTy) &&
           "Size mismatch in scalar to scalar conversion!");
    Value *Tmp = CreateTemporary(CI->getType());
    Builder.CreateStore(CI, Tmp);
    Type *CplxPtrTy = CplxTy->getPointerTo();
    return Builder.CreateLoad(Builder.CreateBitCast(Tmp, CplxPtrTy));
  }
}

bool TreeToLLVM::EmitBuiltinConstantP(gimple stmt, Value *&Result) {
  Result = Constant::getNullValue(ConvertType(gimple_call_return_type(stmt)));
  return true;
}

bool TreeToLLVM::EmitBuiltinExtendPointer(gimple stmt, Value *&Result) {
  tree arg0 = gimple_call_arg(stmt, 0);
  Value *Amt = EmitMemory(arg0);
  bool AmtIsSigned = !TYPE_UNSIGNED(TREE_TYPE(arg0));
  bool ExpIsSigned = !TYPE_UNSIGNED(gimple_call_return_type(stmt));
  Result = CastToAnyType(Amt, AmtIsSigned,
                         ConvertType(gimple_call_return_type(stmt)),
                         ExpIsSigned);
  return true;
}

/// OptimizeIntoPlainBuiltIn - Return true if it's safe to lower the object
/// size checking builtin calls (e.g. __builtin___memcpy_chk into the
/// plain non-checking calls. If the size of the argument is either -1 (unknown)
/// or large enough to ensure no overflow (> len), then it's safe to do so.
static bool OptimizeIntoPlainBuiltIn(gimple stmt, Value *Len, Value *Size) {
  if (BitCastInst *SizeBC = dyn_cast<BitCastInst>(Size))
    Size = SizeBC->getOperand(0);
  ConstantInt *SizeCI = dyn_cast<ConstantInt>(Size);
  if (!SizeCI)
    return false;
  if (SizeCI->isAllOnesValue())
    // If size is -1, convert to plain memcpy, etc.
    return true;

  if (BitCastInst *LenBC = dyn_cast<BitCastInst>(Len))
    Len = LenBC->getOperand(0);
  ConstantInt *LenCI = dyn_cast<ConstantInt>(Len);
  if (!LenCI)
    return false;
  if (SizeCI->getValue().ult(LenCI->getValue())) {
    warning(0, "call to %D will always overflow destination buffer",
            gimple_call_fndecl(stmt));
    return false;
  }
  return true;
}

/// EmitBuiltinMemCopy - Emit an llvm.memcpy or llvm.memmove intrinsic,
/// depending on the value of isMemMove.
bool TreeToLLVM::EmitBuiltinMemCopy(gimple stmt, Value *&Result, bool isMemMove,
                                    bool SizeCheck) {
  if (SizeCheck) {
    if (!validate_gimple_arglist(stmt, POINTER_TYPE, POINTER_TYPE,
                          INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE))
      return false;
  } else {
    if (!validate_gimple_arglist(stmt, POINTER_TYPE, POINTER_TYPE,
                          INTEGER_TYPE, VOID_TYPE))
      return false;
  }

  tree Dst = gimple_call_arg(stmt, 0);
  tree Src = gimple_call_arg(stmt, 1);
  unsigned SrcAlign = getPointerAlignment(Src);
  unsigned DstAlign = getPointerAlignment(Dst);

  Value *DstV = EmitMemory(Dst);
  Value *SrcV = EmitMemory(Src);
  Value *Len = EmitMemory(gimple_call_arg(stmt, 2));
  if (SizeCheck) {
    tree SizeArg = gimple_call_arg(stmt, 3);
    Value *Size = EmitMemory(SizeArg);
    if (!OptimizeIntoPlainBuiltIn(stmt, Len, Size))
      return false;
  }

  Result = isMemMove ?
    EmitMemMove(DstV, SrcV, Len, std::min(SrcAlign, DstAlign)) :
    EmitMemCpy(DstV, SrcV, Len, std::min(SrcAlign, DstAlign));
  return true;
}

bool TreeToLLVM::EmitBuiltinMemSet(gimple stmt, Value *&Result, bool SizeCheck){
  if (SizeCheck) {
    if (!validate_gimple_arglist(stmt, POINTER_TYPE, INTEGER_TYPE,
                          INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE))
      return false;
  } else {
    if (!validate_gimple_arglist(stmt, POINTER_TYPE, INTEGER_TYPE,
                          INTEGER_TYPE, VOID_TYPE))
      return false;
  }

  tree Dst = gimple_call_arg(stmt, 0);
  unsigned DstAlign = getPointerAlignment(Dst);

  Value *DstV = EmitMemory(Dst);
  Value *Val = EmitMemory(gimple_call_arg(stmt, 1));
  Value *Len = EmitMemory(gimple_call_arg(stmt, 2));
  if (SizeCheck) {
    tree SizeArg = gimple_call_arg(stmt, 3);
    Value *Size = EmitMemory(SizeArg);
    if (!OptimizeIntoPlainBuiltIn(stmt, Len, Size))
      return false;
  }
  Result = EmitMemSet(DstV, Val, Len, DstAlign);
  return true;
}

bool TreeToLLVM::EmitBuiltinBZero(gimple stmt, Value *&/*Result*/) {
  if (!validate_gimple_arglist(stmt, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return false;

  tree Dst = gimple_call_arg(stmt, 0);
  unsigned DstAlign = getPointerAlignment(Dst);

  Value *DstV = EmitMemory(Dst);
  Value *Val = Constant::getNullValue(Type::getInt32Ty(Context));
  Value *Len = EmitMemory(gimple_call_arg(stmt, 1));
  EmitMemSet(DstV, Val, Len, DstAlign);
  return true;
}

bool TreeToLLVM::EmitBuiltinPrefetch(gimple stmt) {
  if (!validate_gimple_arglist(stmt, POINTER_TYPE, 0))
    return false;

  Value *Ptr = EmitMemory(gimple_call_arg(stmt, 0));
  Value *ReadWrite = 0;
  Value *Locality = 0;
  Value *Data = 0;

  if (gimple_call_num_args(stmt) > 1) { // Args 1/2 are optional
    ReadWrite = EmitMemory(gimple_call_arg(stmt, 1));
    if (!isa<ConstantInt>(ReadWrite)) {
      error("second argument to %<__builtin_prefetch%> must be a constant");
      ReadWrite = 0;
    } else if (cast<ConstantInt>(ReadWrite)->getZExtValue() > 1) {
      warning (0, "invalid second argument to %<__builtin_prefetch%>;"
               " using zero");
      ReadWrite = 0;
    } else {
      ReadWrite = TheFolder->CreateIntCast(cast<Constant>(ReadWrite),
                                           Type::getInt32Ty(Context),
                                           /*isSigned*/false);
    }

    if (gimple_call_num_args(stmt) > 2) {
      Locality = EmitMemory(gimple_call_arg(stmt, 2));
      if (!isa<ConstantInt>(Locality)) {
        error("third argument to %<__builtin_prefetch%> must be a constant");
        Locality = 0;
      } else if (cast<ConstantInt>(Locality)->getZExtValue() > 3) {
        warning(0, "invalid third argument to %<__builtin_prefetch%>; using 3");
        Locality = 0;
      } else {
        Locality = TheFolder->CreateIntCast(cast<Constant>(Locality),
                                            Type::getInt32Ty(Context),
                                            /*isSigned*/false);
      }
    }
  }

  // Default to highly local read.
  if (ReadWrite == 0)
    ReadWrite = Builder.getInt32(0);
  if (Locality == 0)
    Locality = Builder.getInt32(3);
  if (Data == 0)
    Data = Builder.getInt32(1);

  Ptr = Builder.CreateBitCast(Ptr, Type::getInt8PtrTy(Context));

  Builder.CreateCall4(Intrinsic::getDeclaration(TheModule, Intrinsic::prefetch),
                      Ptr, ReadWrite, Locality, Data);
  return true;
}

/// EmitBuiltinReturnAddr - Emit an llvm.returnaddress or llvm.frameaddress
/// instruction, depending on whether isFrame is true or not.
bool TreeToLLVM::EmitBuiltinReturnAddr(gimple stmt, Value *&Result,
                                       bool isFrame) {
  if (!validate_gimple_arglist(stmt, INTEGER_TYPE, VOID_TYPE))
    return false;

  ConstantInt *Level =
    dyn_cast<ConstantInt>(EmitMemory(gimple_call_arg(stmt, 0)));
  if (!Level) {
    if (isFrame)
      error("invalid argument to %<__builtin_frame_address%>");
    else
      error("invalid argument to %<__builtin_return_address%>");
    return false;
  }

  Intrinsic::ID IID =
    !isFrame ? Intrinsic::returnaddress : Intrinsic::frameaddress;
  Result = Builder.CreateCall(Intrinsic::getDeclaration(TheModule, IID), Level);
  Result = Builder.CreateBitCast(Result,
                                 ConvertType(gimple_call_return_type(stmt)));
  return true;
}

bool TreeToLLVM::EmitBuiltinExtractReturnAddr(gimple stmt, Value *&Result) {
  Value *Ptr = EmitMemory(gimple_call_arg(stmt, 0));

  // FIXME: Actually we should do something like this:
  //
  // Result = (Ptr & MASK_RETURN_ADDR) + RETURN_ADDR_OFFSET, if mask and
  // offset are defined. This seems to be needed for: ARM, MIPS, Sparc.
  // Unfortunately, these constants are defined as RTL expressions and
  // should be handled separately.

  Result = Builder.CreateBitCast(Ptr, Type::getInt8PtrTy(Context));

  return true;
}

bool TreeToLLVM::EmitBuiltinFrobReturnAddr(gimple stmt, Value *&Result) {
  Value *Ptr = EmitMemory(gimple_call_arg(stmt, 0));

  // FIXME: Actually we should do something like this:
  //
  // Result = Ptr - RETURN_ADDR_OFFSET, if offset is defined. This seems to be
  // needed for: MIPS, Sparc.  Unfortunately, these constants are defined
  // as RTL expressions and should be handled separately.

  Result = Builder.CreateBitCast(Ptr, Type::getInt8PtrTy(Context));

  return true;
}

bool TreeToLLVM::EmitBuiltinStackSave(gimple stmt, Value *&Result) {
  if (!validate_gimple_arglist(stmt, VOID_TYPE))
    return false;

  Result = Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                        Intrinsic::stacksave));
  return true;
}

bool TreeToLLVM::EmitBuiltinUnreachable() {
  Builder.CreateUnreachable();
  return true;
}

// Exception handling builtins.

bool TreeToLLVM::EmitBuiltinEHCopyValues(gimple stmt) {
  unsigned DstRegionNo = tree_low_cst(gimple_call_arg(stmt, 0), 0);
  unsigned SrcRegionNo = tree_low_cst(gimple_call_arg(stmt, 1), 0);
  // Copy the exception pointer.
  Value *ExcPtr = Builder.CreateLoad(getExceptionPtr(SrcRegionNo));
  Builder.CreateStore(ExcPtr, getExceptionPtr(DstRegionNo));
  // Copy the selector value.
  Value *Filter = Builder.CreateLoad(getExceptionFilter(SrcRegionNo));
  Builder.CreateStore(Filter, getExceptionFilter(DstRegionNo));
  return true;
}

bool TreeToLLVM::EmitBuiltinEHFilter(gimple stmt, Value *&Result) {
  // Lookup the local that holds the selector value for this region.
  unsigned RegionNo = tree_low_cst(gimple_call_arg(stmt, 0), 0);
  AllocaInst *Filter = getExceptionFilter(RegionNo);
  // Load the selector value out.
  Result = Builder.CreateLoad(Filter);
  // Ensure the returned value has the right integer type.
  tree type = gimple_call_return_type(stmt);
  Result = CastToAnyType(Result, /*isSigned*/true, getRegType(type),
                         /*isSigned*/!TYPE_UNSIGNED(type));
  return true;
}

bool TreeToLLVM::EmitBuiltinEHPointer(gimple stmt, Value *&Result) {
  // Lookup the local that holds the exception pointer for this region.
  unsigned RegionNo = tree_low_cst(gimple_call_arg(stmt, 0), 0);
  AllocaInst *ExcPtr = getExceptionPtr(RegionNo);
  // Load the exception pointer out.
  Result = Builder.CreateLoad(ExcPtr);
  // Ensure the returned value has the right pointer type.
  tree type = gimple_call_return_type(stmt);
  Result = Builder.CreateBitCast(Result, getRegType(type));
  return true;
}


// Builtins used by the exception handling runtime.

// On most machines, the CFA coincides with the first incoming parm.
#ifndef ARG_POINTER_CFA_OFFSET
#define ARG_POINTER_CFA_OFFSET(FNDECL) FIRST_PARM_OFFSET (FNDECL)
#endif

// The mapping from gcc register number to DWARF 2 CFA column number.  By
// default, we just provide columns for all registers.
#ifndef DWARF_FRAME_REGNUM
#define DWARF_FRAME_REGNUM(REG) DBX_REGISTER_NUMBER (REG)
#endif

// Map register numbers held in the call frame info that gcc has
// collected using DWARF_FRAME_REGNUM to those that should be output in
// .debug_frame and .eh_frame.
#ifndef DWARF2_FRAME_REG_OUT
#define DWARF2_FRAME_REG_OUT(REGNO, FOR_EH) (REGNO)
#endif

/* Registers that get partially clobbered by a call in a given mode.
   These must not be call used registers.  */
#ifndef HARD_REGNO_CALL_PART_CLOBBERED
#define HARD_REGNO_CALL_PART_CLOBBERED(REGNO, MODE) 0
#endif

bool TreeToLLVM::EmitBuiltinDwarfCFA(gimple stmt, Value *&Result) {
  if (!validate_gimple_arglist(stmt, VOID_TYPE))
    return false;

  int cfa_offset = ARG_POINTER_CFA_OFFSET(exp);

  // FIXME: is i32 always enough here?
  Result =
    Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                 Intrinsic::eh_dwarf_cfa),
                       Builder.getInt32(cfa_offset));

  return true;
}

bool TreeToLLVM::EmitBuiltinDwarfSPColumn(gimple stmt, Value *&Result) {
  if (!validate_gimple_arglist(stmt, VOID_TYPE))
    return false;

  unsigned int dwarf_regnum = DWARF_FRAME_REGNUM(STACK_POINTER_REGNUM);
  Result = ConstantInt::get(ConvertType(gimple_call_return_type(stmt)),
                            dwarf_regnum);

  return true;
}

bool TreeToLLVM::EmitBuiltinEHReturnDataRegno(gimple stmt, Value *&Result) {
#ifdef EH_RETURN_DATA_REGNO
  if (!validate_gimple_arglist(stmt, INTEGER_TYPE, VOID_TYPE))
    return false;

  tree which = gimple_call_arg(stmt, 0);
  unsigned HOST_WIDE_INT iwhich;

  if (!isa<INTEGER_CST>(which)) {
    error ("argument of %<__builtin_eh_return_regno%> must be constant");
    return false;
  }

  iwhich = tree_low_cst (which, 1);
  iwhich = EH_RETURN_DATA_REGNO (iwhich);
  if (iwhich == INVALID_REGNUM)
    return false;

  iwhich = DWARF_FRAME_REGNUM (iwhich);

  Result = ConstantInt::get(ConvertType(gimple_call_return_type(stmt)), iwhich);
#endif

  return true;
}

bool TreeToLLVM::EmitBuiltinEHReturn(gimple stmt, Value *&/*Result*/) {
  if (!validate_gimple_arglist(stmt, INTEGER_TYPE, POINTER_TYPE, VOID_TYPE))
    return false;

  Type *IntPtr = DL.getIntPtrType(Context, 0);
  Value *Offset = EmitMemory(gimple_call_arg(stmt, 0));
  Value *Handler = EmitMemory(gimple_call_arg(stmt, 1));

  Intrinsic::ID IID = IntPtr->isIntegerTy(32) ?
    Intrinsic::eh_return_i32 : Intrinsic::eh_return_i64;

  Offset = Builder.CreateIntCast(Offset, IntPtr, /*isSigned*/true);
  Handler = Builder.CreateBitCast(Handler, Type::getInt8PtrTy(Context));

  Value *Args[2] = { Offset, Handler };
  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, IID), Args);
  Builder.CreateUnreachable();
  BeginBlock(BasicBlock::Create(Context));

  return true;
}

bool TreeToLLVM::EmitBuiltinInitDwarfRegSizes(gimple stmt, Value *&/*Result*/) {
#ifdef DWARF2_UNWIND_INFO
  unsigned int i;
  bool wrote_return_column = false;
  static bool reg_modes_initialized = false;

  if (!validate_gimple_arglist(stmt, POINTER_TYPE, VOID_TYPE))
    return false;

  if (!reg_modes_initialized) {
    init_reg_modes_target();
    reg_modes_initialized = true;
  }

  Value *Addr =
    Builder.CreateBitCast(EmitMemory(gimple_call_arg(stmt, 0)),
                          Type::getInt8PtrTy(Context));
  Constant *Size, *Idx;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++) {
    int rnum = DWARF2_FRAME_REG_OUT (DWARF_FRAME_REGNUM (i), 1);

    if (rnum < DWARF_FRAME_REGISTERS) {
      enum machine_mode save_mode = reg_raw_mode[i];
      HOST_WIDE_INT size;

      if (HARD_REGNO_CALL_PART_CLOBBERED (i, save_mode))
        save_mode = choose_hard_reg_mode (i, 1, true);
      if (DWARF_FRAME_REGNUM (i) == DWARF_FRAME_RETURN_COLUMN) {
        if (save_mode == VOIDmode)
          continue;
        wrote_return_column = true;
      }
      size = GET_MODE_SIZE (save_mode);
      if (rnum < 0)
        continue;

      Size = Builder.getInt8(size);
      Idx  = Builder.getInt32(rnum);
      Builder.CreateStore(Size, Builder.CreateGEP(Addr, Idx, flag_verbose_asm ?
                                                  "rnum" : ""), false);
    }
  }

  if (!wrote_return_column) {
    Size = Builder.getInt8(GET_MODE_SIZE (Pmode));
    Idx  = Builder.getInt32(DWARF_FRAME_RETURN_COLUMN);
    Builder.CreateStore(Size, Builder.CreateGEP(Addr, Idx, flag_verbose_asm ?
                                                "rcol" : ""), false);
  }

#ifdef DWARF_ALT_FRAME_RETURN_COLUMN
  Size = Builder.getInt8(GET_MODE_SIZE (Pmode));
  Idx  = Builder.getInt32(DWARF_ALT_FRAME_RETURN_COLUMN);
  Builder.CreateStore(Size, Builder.CreateGEP(Addr, Idx, flag_verbose_asm ?
                                              "acol" : ""), false);
#endif

#endif /* DWARF2_UNWIND_INFO */

  // TODO: the RS6000 target needs extra initialization [gcc changeset 122468].

  return true;
}

bool TreeToLLVM::EmitBuiltinUnwindInit(gimple stmt, Value *&/*Result*/) {
  if (!validate_gimple_arglist(stmt, VOID_TYPE))
    return false;

  Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                               Intrinsic::eh_unwind_init));

  return true;
}

bool TreeToLLVM::EmitBuiltinStackRestore(gimple stmt) {
  if (!validate_gimple_arglist(stmt, POINTER_TYPE, VOID_TYPE))
    return false;

  Value *Ptr = EmitMemory(gimple_call_arg(stmt, 0));
  Ptr = Builder.CreateBitCast(Ptr, Type::getInt8PtrTy(Context));

  Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                               Intrinsic::stackrestore), Ptr);
  return true;
}


bool TreeToLLVM::EmitBuiltinAlloca(gimple stmt, Value *&Result) {
  if (!validate_gimple_arglist(stmt, INTEGER_TYPE, VOID_TYPE))
    return false;
  Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
  AllocaInst *Alloca = Builder.CreateAlloca(Type::getInt8Ty(Context), Amt);
  Alloca->setAlignment(BIGGEST_ALIGNMENT / 8);
  Result = Alloca;
  return true;
}

bool TreeToLLVM::EmitBuiltinAllocaWithAlign(gimple stmt, Value *&Result) {
  if (!validate_gimple_arglist(stmt, INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return false;
  Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
  uint64_t Align = getInt64(gimple_call_arg(stmt, 1), true);
  AllocaInst *Alloca = Builder.CreateAlloca(Type::getInt8Ty(Context), Amt);
  Alloca->setAlignment(Align / 8);
  Result = Alloca;
  return true;
}

bool TreeToLLVM::EmitBuiltinExpect(gimple stmt, Value *&Result) {
  tree type = gimple_call_return_type(stmt);
  if (gimple_call_num_args(stmt) < 2) {
    Result = Constant::getNullValue(ConvertType(type));
    return true;
  }
  Type *ArgTy = getRegType(type);
  Value *ExpectIntr = Intrinsic::getDeclaration(TheModule, Intrinsic::expect,
                                                ArgTy);
  Value *ArgValue = EmitRegister(gimple_call_arg(stmt, 0));
  Value *ExpectedValue = EmitRegister(gimple_call_arg(stmt, 1));
  Result = Builder.CreateCall2(ExpectIntr, ArgValue, ExpectedValue);
  Result = Reg2Mem(Result, type, Builder);
  return true;
}

bool TreeToLLVM::EmitBuiltinVAStart(gimple stmt) {
  if (gimple_call_num_args(stmt) < 2) {
    error("too few arguments to function %<va_start%>");
    return true;
  }

  tree fntype = TREE_TYPE(current_function_decl);
  if (TYPE_ARG_TYPES(fntype) == 0 ||
      (tree_last(TYPE_ARG_TYPES(fntype)) == void_type_node)) {
    error("%<va_start%> used in function with fixed args");
    return true;
  }

  Constant *va_start = Intrinsic::getDeclaration(TheModule, Intrinsic::vastart);
  Value *ArgVal = EmitMemory(gimple_call_arg(stmt, 0));
  ArgVal = Builder.CreateBitCast(ArgVal, Type::getInt8PtrTy(Context));
  Builder.CreateCall(va_start, ArgVal);
  return true;
}

bool TreeToLLVM::EmitBuiltinVAEnd(gimple stmt) {
  Value *Arg = EmitMemory(gimple_call_arg(stmt, 0));
  Arg = Builder.CreateBitCast(Arg, Type::getInt8PtrTy(Context));
  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Intrinsic::vaend),
                     Arg);
  return true;
}

bool TreeToLLVM::EmitBuiltinVACopy(gimple stmt) {
  tree Arg1T = gimple_call_arg(stmt, 0);
  tree Arg2T = gimple_call_arg(stmt, 1);

  Value *Arg1 = EmitMemory(Arg1T);   // Emit the address of the destination.
  // The second arg of llvm.va_copy is a pointer to a valist.
  Value *Arg2;
  if (!isa<AGGREGATE_TYPE>(va_list_type_node)) {
    // Emit it as a value, then store it to a temporary slot.
    Value *V2 = EmitMemory(Arg2T);
    Arg2 = CreateTemporary(V2->getType());
    Builder.CreateStore(V2, Arg2);
  } else {
    // If the target has aggregate valists, then the second argument
    // from GCC is the address of the source valist and we don't
    // need to do anything special.
    Arg2 = EmitMemory(Arg2T);
  }

  static Type *VPTy = Type::getInt8PtrTy(Context);

  // FIXME: This ignores alignment and volatility of the arguments.
  SmallVector<Value *, 2> Args;
  Args.push_back(Builder.CreateBitCast(Arg1, VPTy));
  Args.push_back(Builder.CreateBitCast(Arg2, VPTy));

  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Intrinsic::vacopy),
                     Args);
  return true;
}

bool TreeToLLVM::EmitBuiltinAdjustTrampoline(gimple stmt, Value *&Result) {
  if (!validate_gimple_arglist(stmt, POINTER_TYPE, VOID_TYPE))
    return false;

  Function *Intr = Intrinsic::getDeclaration(TheModule,
                                             Intrinsic::adjust_trampoline);
  Value *Arg = Builder.CreateBitCast(EmitRegister(gimple_call_arg(stmt, 0)),
                                     Builder.getInt8PtrTy());
  Result = Builder.CreateCall(Intr, Arg);
  return true;
}

bool TreeToLLVM::EmitBuiltinInitTrampoline(gimple stmt, Value *&/*Result*/) {
  if (!validate_gimple_arglist(stmt, POINTER_TYPE, POINTER_TYPE, POINTER_TYPE,
                         VOID_TYPE))
    return false;

  Value *Tramp = EmitRegister(gimple_call_arg(stmt, 0));
  Value *Func = EmitRegister(gimple_call_arg(stmt, 1));
  Value *Chain = EmitRegister(gimple_call_arg(stmt, 2));

  Type *VPTy = Builder.getInt8PtrTy();
  Value *Ops[3] = {
    Builder.CreateBitCast(Tramp, VPTy),
    Builder.CreateBitCast(Func, VPTy),
    Builder.CreateBitCast(Chain, VPTy)
  };

  Function *Intr = Intrinsic::getDeclaration(TheModule,
                                             Intrinsic::init_trampoline);
  Builder.CreateCall(Intr, Ops);
  return true;
}

//===----------------------------------------------------------------------===//
//                      ... Complex Math Expressions ...
//===----------------------------------------------------------------------===//

Value *TreeToLLVM::CreateComplex(Value *Real, Value *Imag) {
  assert(Real->getType() == Imag->getType() && "Component type mismatch!");
  Type *EltTy = Real->getType();
  Value *Result = UndefValue::get(StructType::get(EltTy, EltTy, NULL));
  Result = Builder.CreateInsertValue(Result, Real, 0);
  Result = Builder.CreateInsertValue(Result, Imag, 1);
  return Result;
}

void TreeToLLVM::SplitComplex(Value *Complex, Value *&Real, Value *&Imag) {
  Real = Builder.CreateExtractValue(Complex, 0);
  Imag = Builder.CreateExtractValue(Complex, 1);
}


//===----------------------------------------------------------------------===//
//                         ... L-Value Expressions ...
//===----------------------------------------------------------------------===//

Value *TreeToLLVM::EmitFieldAnnotation(Value *FieldPtr, tree FieldDecl) {
  tree AnnotateAttr = lookup_attribute("annotate", DECL_ATTRIBUTES(FieldDecl));

  Type *SBP = Type::getInt8PtrTy(Context);

  Function *An = Intrinsic::getDeclaration(TheModule,
                                           Intrinsic::ptr_annotation,
                                           SBP);

  // Get file and line number.  FIXME: Should this be for the decl or the
  // use.  Is there a location info for the use?
  Constant *LineNo = ConstantInt::get(Type::getInt32Ty(Context),
                                      DECL_SOURCE_LINE(FieldDecl));
  Constant *File = ConvertMetadataStringToGV(DECL_SOURCE_FILE(FieldDecl));

  File = TheFolder->CreateBitCast(File, SBP);

  // There may be multiple annotate attributes. Pass return of lookup_attr
  //  to successive lookups.
  while (AnnotateAttr) {
    // Each annotate attribute is a tree list.
    // Get value of list which is our linked list of args.
    tree args = TREE_VALUE(AnnotateAttr);

    // Each annotate attribute may have multiple args.
    // Treat each arg as if it were a separate annotate attribute.
    for (tree a = args; a; a = TREE_CHAIN(a)) {
      // Each element of the arg list is a tree list, so get value
      tree val = TREE_VALUE(a);

      // Assert its a string, and then get that string.
      assert(isa<STRING_CST>(val) &&
             "Annotate attribute arg should always be a string");

      Constant *strGV = AddressOf(val);

      // We can not use the IRBuilder because it will constant fold away
      // the GEP that is critical to distinguish between an annotate
      // attribute on a whole struct from one on the first element of the
      // struct.
      BitCastInst *CastFieldPtr = new BitCastInst(FieldPtr,  SBP,
                                                  FieldPtr->getName());
      Builder.Insert(CastFieldPtr);

      Value *Ops[4] = {
        CastFieldPtr, Builder.CreateBitCast(strGV, SBP),
        File,  LineNo
      };

      Type* FieldPtrType = FieldPtr->getType();
      FieldPtr = Builder.CreateCall(An, Ops);
      FieldPtr = Builder.CreateBitCast(FieldPtr, FieldPtrType);
    }

    // Get next annotate attribute.
    AnnotateAttr = TREE_CHAIN(AnnotateAttr);
    if (AnnotateAttr)
      AnnotateAttr = lookup_attribute("annotate", AnnotateAttr);
  }
  return FieldPtr;
}

LValue TreeToLLVM::EmitLV_ARRAY_REF(tree exp) {
  // The result type is an ElementTy* in the case of an ARRAY_REF, an array
  // of ElementTy in the case of ARRAY_RANGE_REF.

  tree Array = TREE_OPERAND(exp, 0);
  tree ArrayTreeType = TREE_TYPE(Array);
  tree Index = TREE_OPERAND(exp, 1);
  tree IndexType = TREE_TYPE(Index);
  tree ElementType = TREE_TYPE(ArrayTreeType);

  assert(isa<ARRAY_TYPE>(ArrayTreeType) && "Unknown ARRAY_REF!");

  Value *ArrayAddr;
  unsigned ArrayAlign;

  // First subtract the lower bound, if any, in the type of the index.
  Value *IndexVal = EmitRegister(Index);
  tree LowerBound = array_ref_low_bound(exp);
  if (!integer_zerop(LowerBound))
    IndexVal = Builder.CreateSub(IndexVal, EmitRegister(LowerBound), "",
                                 hasNUW(TREE_TYPE(Index)),
                                 hasNSW(TREE_TYPE(Index)));

  LValue ArrayAddrLV = EmitLV(Array);
  assert(!ArrayAddrLV.isBitfield() && "Arrays cannot be bitfields!");
  ArrayAddr = ArrayAddrLV.Ptr;
  ArrayAlign = ArrayAddrLV.getAlignment();

  Type *IntPtrTy = getDataLayout().getIntPtrType(ArrayAddr->getType());
  IndexVal = Builder.CreateIntCast(IndexVal, IntPtrTy,
                                   /*isSigned*/!TYPE_UNSIGNED(IndexType));

  // If we are indexing over a fixed-size type, just use a GEP.
  if (isSizeCompatible(ElementType)) {
    // Avoid any assumptions about how the array type is represented in LLVM by
    // doing the GEP on a pointer to the first array element.
    Type *EltTy = ConvertType(ElementType);
    ArrayAddr = Builder.CreateBitCast(ArrayAddr, EltTy->getPointerTo());
    StringRef GEPName = flag_verbose_asm ? "ar" : "";
    Value *Ptr = POINTER_TYPE_OVERFLOW_UNDEFINED ?
      Builder.CreateInBoundsGEP(ArrayAddr, IndexVal, GEPName) :
      Builder.CreateGEP(ArrayAddr, IndexVal, GEPName);
    unsigned Alignment = MinAlign(ArrayAlign, DL.getABITypeAlignment(EltTy));
    return LValue(Builder.CreateBitCast(Ptr,
                  PointerType::getUnqual(ConvertType(TREE_TYPE(exp)))),
                  Alignment);
  }

  // Otherwise, just do raw, low-level pointer arithmetic.  FIXME: this could be
  // much nicer in cases like:
  //   float foo(int w, float A[][w], int g) { return A[g][0]; }

  if (isa<VOID_TYPE>(TREE_TYPE(ArrayTreeType))) {
    ArrayAddr = Builder.CreateBitCast(ArrayAddr, Type::getInt8PtrTy(Context));
    StringRef GEPName = flag_verbose_asm ? "va" : "";
    ArrayAddr = POINTER_TYPE_OVERFLOW_UNDEFINED ?
      Builder.CreateInBoundsGEP(ArrayAddr, IndexVal, GEPName) :
      Builder.CreateGEP(ArrayAddr, IndexVal, GEPName);
    return LValue(ArrayAddr, 1);
  }

  // FIXME: Might also get here if the element type has constant size, but is
  // humongous.  Add support for this case.
  assert(TREE_OPERAND(exp, 3) && "Size missing for variable sized element!");
  // ScaleFactor is the size of the element type in units divided by (exactly)
  // TYPE_ALIGN_UNIT(ElementType).
  Value *ScaleFactor = Builder.CreateIntCast(EmitRegister(TREE_OPERAND(exp, 3)),
                                             IntPtrTy, /*isSigned*/false);
  assert(isPowerOf2_32(TYPE_ALIGN(ElementType)) &&
         "Alignment not a power of two!");
  assert(TYPE_ALIGN(ElementType) >= 8 && "Unit size not a multiple of 8 bits!");
  // ScaleType is chosen to correct for the division in ScaleFactor.
  Type *ScaleType = IntegerType::get(Context, TYPE_ALIGN(ElementType));
  ArrayAddr = Builder.CreateBitCast(ArrayAddr, ScaleType->getPointerTo());

  IndexVal = Builder.CreateMul(IndexVal, ScaleFactor);
  unsigned Alignment = MinAlign(ArrayAlign, TYPE_ALIGN(ElementType) / 8);
  StringRef GEPName = flag_verbose_asm ? "ra" : "";
  Value *Ptr = POINTER_TYPE_OVERFLOW_UNDEFINED ?
    Builder.CreateInBoundsGEP(ArrayAddr, IndexVal, GEPName) :
    Builder.CreateGEP(ArrayAddr, IndexVal, GEPName);
  return LValue(Builder.CreateBitCast(Ptr,
                PointerType::getUnqual(ConvertType(TREE_TYPE(exp)))),
                Alignment);
}

LValue TreeToLLVM::EmitLV_BIT_FIELD_REF(tree exp) {
  LValue Ptr = EmitLV(TREE_OPERAND(exp, 0));
  assert(!Ptr.isBitfield() && "BIT_FIELD_REF operands cannot be bitfields!");

  unsigned BitStart = (unsigned)TREE_INT_CST_LOW(TREE_OPERAND(exp, 2));
  unsigned BitSize = (unsigned)TREE_INT_CST_LOW(TREE_OPERAND(exp, 1));
  Type *ValTy = ConvertType(TREE_TYPE(exp));

  unsigned ValueSizeInBits = DL.getTypeSizeInBits(ValTy);
  assert(BitSize <= ValueSizeInBits &&
         "ValTy isn't large enough to hold the value loaded!");

  assert(ValueSizeInBits == DL.getTypeAllocSizeInBits(ValTy) &&
         "FIXME: BIT_FIELD_REF logic is broken for non-round types");

  // BIT_FIELD_REF values can have BitStart values that are quite large.  We
  // know that the thing we are loading is ValueSizeInBits large.  If BitStart
  // is larger than ValueSizeInBits, bump the pointer over to where it should
  // be.
  if (unsigned UnitOffset = BitStart / ValueSizeInBits) {
    // TODO: If Ptr.Ptr is a struct type or something, we can do much better
    // than this.  e.g. check out when compiling unwind-dw2-fde-darwin.c.
    Ptr.Ptr = Builder.CreateBitCast(Ptr.Ptr, ValTy->getPointerTo());
    Ptr.Ptr = Builder.CreateGEP(Ptr.Ptr, Builder.getInt32(UnitOffset),
                                flag_verbose_asm ? "bfr" : "");
    unsigned OctetOffset = (UnitOffset * ValueSizeInBits) / 8;
    Ptr.setAlignment(MinAlign(Ptr.getAlignment(), OctetOffset));
    BitStart -= UnitOffset*ValueSizeInBits;
  }

  // If this is referring to the whole field, return the whole thing.
  if (BitStart == 0 && BitSize == ValueSizeInBits) {
    return LValue(Builder.CreateBitCast(Ptr.Ptr, ValTy->getPointerTo()),
                  Ptr.getAlignment());
  }

  return LValue(Builder.CreateBitCast(Ptr.Ptr, ValTy->getPointerTo()),
                1, BitStart, BitSize);
}

LValue TreeToLLVM::EmitLV_COMPONENT_REF(tree exp) {
  LValue StructAddrLV = EmitLV(TREE_OPERAND(exp, 0));
  tree FieldDecl = TREE_OPERAND(exp, 1);
  unsigned LVAlign = StructAddrLV.getAlignment();

  assert(isa<RECORD_OR_UNION_TYPE>(DECL_CONTEXT(FieldDecl)));

  Type *StructTy = ConvertType(DECL_CONTEXT(FieldDecl));

  assert((!StructAddrLV.isBitfield() ||
          StructAddrLV.BitStart == 0) && "structs cannot be bitfields!");

  StructAddrLV.Ptr = Builder.CreateBitCast(StructAddrLV.Ptr,
                                           StructTy->getPointerTo());
  Type *FieldTy = ConvertType(TREE_TYPE(FieldDecl));

  // BitStart - This is the actual offset of the field from the start of the
  // struct, in bits.  For bitfields this may be on a non-byte boundary.
  uint64_t FieldBitOffset = getInt64(DECL_FIELD_BIT_OFFSET(FieldDecl), true);
  unsigned BitStart;
  Value *FieldPtr;

  // If the GCC field directly corresponds to an LLVM field, handle it.
  unsigned MemberIndex = GetFieldIndex(FieldDecl, StructTy);
  if (MemberIndex < INT_MAX) {
    assert(!TREE_OPERAND(exp, 2) && "Constant not gimple min invariant?");
    // Get a pointer to the byte in which the GCC field starts.
    FieldPtr = Builder.CreateStructGEP(StructAddrLV.Ptr, MemberIndex,
                                       flag_verbose_asm ? "cr" : "");
    // Within that byte, the bit at which the GCC field starts.
    BitStart = FieldBitOffset & 7;
  } else {
    // Offset will hold the field offset in octets.
    Value *Offset;

    if (TREE_OPERAND(exp, 2)) {
      Offset = EmitRegister(TREE_OPERAND(exp, 2));
      // At this point the offset is measured in units divided by (exactly)
      // (DECL_OFFSET_ALIGN / BITS_PER_UNIT).  Convert to octets.
      unsigned factor = DECL_OFFSET_ALIGN(FieldDecl) / 8;
      if (factor != 1)
        Offset = Builder.CreateMul(Offset,
                                   ConstantInt::get(Offset->getType(), factor));
    } else {
      assert(DECL_FIELD_OFFSET(FieldDecl) && "Field offset not available!");
      Offset = EmitRegister(DECL_FIELD_OFFSET(FieldDecl));
      // At this point the offset is measured in units.  Convert to octets.
      unsigned factor = BITS_PER_UNIT / 8;
      if (factor != 1)
        Offset = Builder.CreateMul(Offset,
                                   ConstantInt::get(Offset->getType(), factor));
    }

    // Here BitStart gives the offset of the field in bits from Offset.
    BitStart = FieldBitOffset;

    // Incorporate as much of it as possible into the pointer computation.
    unsigned ByteOffset = BitStart / 8;
    if (ByteOffset > 0) {
      Offset = Builder.CreateAdd(Offset,
        ConstantInt::get(Offset->getType(), ByteOffset));
      BitStart -= ByteOffset*8;
    }

    Type *BytePtrTy = Type::getInt8PtrTy(Context);
    FieldPtr = Builder.CreateBitCast(StructAddrLV.Ptr, BytePtrTy);
    FieldPtr = Builder.CreateInBoundsGEP(FieldPtr, Offset, flag_verbose_asm ?
                                         "rc" : "");
    FieldPtr = Builder.CreateBitCast(FieldPtr, FieldTy->getPointerTo());
  }

  // Compute the alignment of the octet containing the first bit of the field,
  // without assuming that the containing struct itself is properly aligned.
  LVAlign = MinAlign(LVAlign, getFieldAlignment(FieldDecl));

  // If the FIELD_DECL has an annotate attribute on it, emit it.
  if (lookup_attribute("annotate", DECL_ATTRIBUTES(FieldDecl)))
    FieldPtr = EmitFieldAnnotation(FieldPtr, FieldDecl);

  // Make sure we return a pointer to the right type.
  Type *EltTy = ConvertType(TREE_TYPE(exp));
  FieldPtr = Builder.CreateBitCast(FieldPtr, EltTy->getPointerTo());

  if (!isBitfield(FieldDecl)) {
    assert(BitStart == 0 && "Not a bitfield but not at a byte offset!");
    return LValue(FieldPtr, LVAlign);
  }

  assert(BitStart < 8 && "Bit offset not properly incorporated in the pointer");
  assert(DECL_SIZE(FieldDecl) && isa<INTEGER_CST>(DECL_SIZE(FieldDecl)) &&
         "Variable sized bitfield?");
  unsigned BitfieldSize = TREE_INT_CST_LOW(DECL_SIZE(FieldDecl));
  return LValue(FieldPtr, LVAlign, BitStart, BitfieldSize);
}

LValue TreeToLLVM::EmitLV_DECL(tree exp) {
  Value *Decl = DEFINITION_LOCAL(exp);
  if (Decl == 0) {
    if (errorcount || sorrycount) {
      Type *Ty = ConvertType(TREE_TYPE(exp));
      PointerType *PTy = Ty->getPointerTo();
      LValue LV(ConstantPointerNull::get(PTy), 1);
      return LV;
    }
    debug_tree(exp);
    llvm_unreachable("Referencing decl that hasn't been laid out!");
  }

  Type *Ty = ConvertType(TREE_TYPE(exp));
  // If we have "extern void foo", make the global have type {} instead of
  // type void.
  if (Ty->isVoidTy()) Ty = StructType::get(Context);
  PointerType *PTy = Ty->getPointerTo();
  unsigned Alignment = DECL_ALIGN(exp) / 8;
  if (!Alignment)
    Alignment = 1;

  return LValue(Builder.CreateBitCast(Decl, PTy), Alignment);
}

LValue TreeToLLVM::EmitLV_INDIRECT_REF(tree exp) {
  // The lvalue is just the address.
  LValue LV = LValue(EmitRegister(TREE_OPERAND(exp, 0)), expr_align(exp) / 8);
  // May need to change pointer type, for example when INDIRECT_REF is applied
  // to a void*, resulting in a non-void type.
  LV.Ptr = Builder.CreateBitCast(LV.Ptr,
                                 ConvertType(TREE_TYPE(exp))->getPointerTo());
  return LV;
}

#if (GCC_MINOR > 5)
LValue TreeToLLVM::EmitLV_MEM_REF(tree exp) {
  // The address is the first operand offset in bytes by the second.
  Value *Addr = EmitRegister(TREE_OPERAND(exp, 0));
  if (!integer_zerop(TREE_OPERAND(exp, 1))) {
    // Convert to a byte pointer and displace by the offset.
    Addr = Builder.CreateBitCast(Addr, GetUnitPointerType(Context));
    APInt Offset = getAPIntValue(TREE_OPERAND(exp, 1));
    // The address is always inside the referenced object, so "inbounds".
    Addr = Builder.CreateInBoundsGEP(Addr, ConstantInt::get(Context, Offset),
                                     flag_verbose_asm ? "mrf" : "");
  }

  // Ensure the pointer has the right type.
  Addr = Builder.CreateBitCast(Addr, getPointerToType(TREE_TYPE(exp)));

  unsigned Alignment =
#if (GCC_MINOR < 6)
    get_object_alignment(exp, TYPE_ALIGN(TREE_TYPE (exp)), BIGGEST_ALIGNMENT);
#elif (GCC_MINOR < 7)
    std::max(get_object_alignment(exp, BIGGEST_ALIGNMENT),
             TYPE_ALIGN(TREE_TYPE (exp)));
#else
    get_object_or_type_alignment(exp);
#endif
  bool Volatile = TREE_THIS_VOLATILE(exp);

  return LValue(Addr, Alignment / 8, Volatile);
}
#endif

#if (GCC_MINOR < 6)
LValue TreeToLLVM::EmitLV_MISALIGNED_INDIRECT_REF(tree exp) {
  // The lvalue is just the address.  The alignment is given by operand 1.
  unsigned Alignment = tree_low_cst(TREE_OPERAND(exp, 1), true);
  // The alignment need not be a power of two, so replace it with the largest
  // power of two that divides it.
  Alignment &= -Alignment;
  if (!Alignment) Alignment = 8;
  assert(!(Alignment & 7) && "Alignment not in octets!");
  LValue LV = LValue(EmitRegister(TREE_OPERAND(exp, 0)), Alignment / 8);
  // May need to change pointer type, for example when MISALIGNED_INDIRECT_REF
  // is applied to a void*, resulting in a non-void type.
  LV.Ptr = Builder.CreateBitCast(LV.Ptr,
                                 ConvertType(TREE_TYPE(exp))->getPointerTo());
  return LV;
}
#endif

LValue TreeToLLVM::EmitLV_VIEW_CONVERT_EXPR(tree exp) {
  // The address is the address of the operand.
  LValue LV = EmitLV(TREE_OPERAND(exp, 0));
  // The type is the type of the expression.
  LV.Ptr = Builder.CreateBitCast(LV.Ptr,
                                 ConvertType(TREE_TYPE(exp))->getPointerTo());
  return LV;
}

LValue TreeToLLVM::EmitLV_WITH_SIZE_EXPR(tree exp) {
  // The address is the address of the operand.
  return EmitLV(TREE_OPERAND(exp, 0));
}

LValue TreeToLLVM::EmitLV_XXXXPART_EXPR(tree exp, unsigned Idx) {
  LValue Ptr = EmitLV(TREE_OPERAND(exp, 0));
  assert(!Ptr.isBitfield() &&
         "REALPART_EXPR / IMAGPART_EXPR operands cannot be bitfields!");
  unsigned Alignment;
  if (Idx == 0)
    // REALPART alignment is same as the complex operand.
    Alignment = Ptr.getAlignment();
  else
    // IMAGPART alignment = MinAlign(Ptr.Alignment, sizeof field);
    Alignment = MinAlign(Ptr.getAlignment(),
                         DL.getTypeAllocSize(Ptr.Ptr->getType()));
  return LValue(Builder.CreateStructGEP(Ptr.Ptr, Idx, flag_verbose_asm ?
                                        "prtxpr" : ""), Alignment);
}

LValue TreeToLLVM::EmitLV_SSA_NAME(tree exp) {
  // TODO: Check the ssa name is being used as an rvalue, see EmitLoadOfLValue.
  Value *Temp = CreateTemporary(ConvertType(TREE_TYPE(exp)));
  Builder.CreateStore(EmitReg_SSA_NAME(exp), Temp);
  return LValue(Temp, 1);
}

LValue TreeToLLVM::EmitLV_TARGET_MEM_REF(tree exp) {
  // TODO: Take the address space into account.

  Value *Addr;
  Value *Delta = 0; // Offset from base pointer in units
#if (GCC_MINOR > 5)
  // Starting with gcc 4.6 the address is base + index * step + index2 + offset.
  Addr = EmitRegister(TMR_BASE(exp));
  if (TMR_INDEX2(exp) && !integer_zerop (TMR_INDEX2(exp)))
    Delta = EmitRegister(TMR_INDEX2(exp));
#else
  // In gcc 4.5 the address is &symbol + base + index * step + offset.
  if (TMR_SYMBOL(exp)) {
    Addr = EmitLV(TMR_SYMBOL(exp)).Ptr;
    if (TMR_BASE(exp) && !integer_zerop (TMR_BASE(exp)))
      Delta = EmitRegister(TMR_BASE(exp));
  } else {
    assert(TMR_BASE(exp) && "TARGET_MEM_REF has neither base nor symbol!");
    Addr = EmitRegister(TMR_BASE(exp));
    // The type of BASE is sizetype or a pointer type.  Convert sizetype to i8*.
    if (!isa<PointerType>(Addr->getType()))
      Addr = Builder.CreateIntToPtr(Addr, GetUnitPointerType(Context));
  }
#endif

  if (TMR_INDEX(exp)) {
    Value *Index = EmitRegister(TMR_INDEX(exp));
    if (TMR_STEP(exp) && !integer_onep (TMR_STEP(exp)))
      Index = Builder.CreateMul(Index, EmitRegisterConstant(TMR_STEP(exp)));
    Delta = Delta ? Builder.CreateAdd(Delta, Index) : Index;
  }

  if (TMR_OFFSET(exp) && !integer_zerop (TMR_OFFSET(exp))) {
    Constant *Off = ConstantInt::get(Context, getAPIntValue(TMR_OFFSET(exp)));
    Delta = Delta ? Builder.CreateAdd(Delta, Off) : Off;
  }

  if (Delta) {
    // Advance the base pointer by the given number of units.
    Addr = Builder.CreateBitCast(Addr, GetUnitPointerType(Context));
    StringRef GEPName = flag_verbose_asm ? "" : "tmrf";
    Addr = POINTER_TYPE_OVERFLOW_UNDEFINED ?
      Builder.CreateInBoundsGEP(Addr, Delta, GEPName)
      : Builder.CreateGEP(Addr, Delta, GEPName);
  }

  // The result can be of a different pointer type even if we didn't advance it.
  Addr = Builder.CreateBitCast(Addr, getPointerToType(TREE_TYPE(exp)));
  unsigned Alignment =
#if (GCC_MINOR < 6)
    get_object_alignment(exp, TYPE_ALIGN(TREE_TYPE (exp)), BIGGEST_ALIGNMENT);
#elif (GCC_MINOR < 7)
    std::max(get_object_alignment(exp, BIGGEST_ALIGNMENT),
             TYPE_ALIGN(TREE_TYPE (exp)));
#else
    get_object_or_type_alignment(exp);
#endif
  bool Volatile = TREE_THIS_VOLATILE(exp);

  return LValue(Addr, Alignment / 8, Volatile);
}

Constant *TreeToLLVM::AddressOfLABEL_DECL(tree exp) {
  return BlockAddress::get(Fn, getLabelDeclBlock(exp));
}


//===----------------------------------------------------------------------===//
//                           ... Emit helpers ...
//===----------------------------------------------------------------------===//

/// EmitMinInvariant - The given value is constant in this function.  Return the
/// corresponding LLVM value.  Only creates code in the entry block.
Value *TreeToLLVM::EmitMinInvariant(tree reg) {
  Value *V = isa<ADDR_EXPR>(reg) ?
    EmitInvariantAddress(reg) : EmitRegisterConstant(reg);
  assert(V->getType() == getRegType(TREE_TYPE(reg)) &&
         "Gimple min invariant has wrong type!");
  return V;
}

/// EmitInvariantAddress - The given address is constant in this function.
/// Return the corresponding LLVM value.  Only creates code in the entry block.
Value *TreeToLLVM::EmitInvariantAddress(tree addr) {
  assert(is_gimple_invariant_address(addr) &&
         "Expected a locally constant address!");
  assert(is_gimple_reg_type(TREE_TYPE(addr)) && "Not of register type!");

  // Any generated code goes in the entry block.
  BasicBlock *EntryBlock = Fn->begin();

  // Note the current builder position.
  BasicBlock *SavedInsertBB = Builder.GetInsertBlock();
  BasicBlock::iterator SavedInsertPoint = Builder.GetInsertPoint();

  // Pop the entry block terminator.  There may not be a terminator if we are
  // recursing or if the entry block was not yet finished.
  Instruction *Terminator = EntryBlock->getTerminator();
  assert(((SavedInsertBB != EntryBlock && Terminator) ||
          (SavedInsertPoint == EntryBlock->end() && !Terminator)) &&
         "Insertion point doesn't make sense!");
  if (Terminator)
    Terminator->removeFromParent();

  // Point the builder at the end of the entry block.
  Builder.SetInsertPoint(EntryBlock);

  // Calculate the address.
  assert(isa<ADDR_EXPR>(addr) && "Invariant address not ADDR_EXPR!");
  Value *Address = EmitADDR_EXPR(addr);

  // Restore the entry block terminator.
  if (Terminator)
    EntryBlock->getInstList().push_back(Terminator);

  // Restore the builder insertion point.
  if (SavedInsertBB != EntryBlock)
    Builder.SetInsertPoint(SavedInsertBB, SavedInsertPoint);

  assert(Address->getType() == getRegType(TREE_TYPE(addr)) &&
         "Invariant address has wrong type!");
  return Address;
}

/// EmitRegisterConstant - Convert the given global constant of register type to
/// an LLVM constant.  Creates no code, only constants.
Constant *TreeToLLVM::EmitRegisterConstant(tree reg) {
#ifndef NDEBUG
  if (!is_gimple_constant(reg)) {
    debug_tree(reg);
    llvm_unreachable("Unsupported gimple!");
  }
#endif
  assert(is_gimple_reg_type(TREE_TYPE(reg)) && "Not of register type!");

  switch (TREE_CODE(reg)) {
  default:
    debug_tree(reg);
    llvm_unreachable("Unhandled GIMPLE constant!");

  case INTEGER_CST:
    return EmitIntegerRegisterConstant(reg);
  case REAL_CST:
    return EmitRealRegisterConstant(reg);
  //case FIXED_CST: // Fixed point constant - not yet supported.
  //case STRING_CST: // Allowed by is_gimple_constant, but no known examples.
  case COMPLEX_CST:
    return EmitComplexRegisterConstant(reg);
  case VECTOR_CST:
    return EmitVectorRegisterConstant(reg);
  case CONSTRUCTOR:
    // Vector constant constructors are gimple invariant.  See GCC testcase
    // pr34856.c for an example.
    return EmitConstantVectorConstructor(reg);
  }
}

/// EncodeExpr - Write the given expression into Buffer as it would appear in
/// memory on the target (the buffer is resized to contain exactly the bytes
/// written).  Return the number of bytes written; this can also be obtained
/// by querying the buffer's size.
/// The following kinds of expressions are currently supported: INTEGER_CST,
/// REAL_CST, COMPLEX_CST, VECTOR_CST, STRING_CST.
static unsigned EncodeExpr(tree exp, SmallVectorImpl<unsigned char> &Buffer) {
  const tree type = TREE_TYPE(exp);
  unsigned SizeInBytes = (TREE_INT_CST_LOW(TYPE_SIZE(type)) + 7) / 8;
  Buffer.resize(SizeInBytes);
  unsigned BytesWritten = native_encode_expr(exp, &Buffer[0], SizeInBytes);
  assert(BytesWritten == SizeInBytes && "Failed to fully encode expression!");
  return BytesWritten;
}

/// EmitComplexRegisterConstant - Turn the given COMPLEX_CST into an LLVM
/// constant of the corresponding register type.
Constant *TreeToLLVM::EmitComplexRegisterConstant(tree reg) {
  Constant *Elts[2] = {
    EmitRegisterConstant(TREE_REALPART(reg)),
    EmitRegisterConstant(TREE_IMAGPART(reg))
  };
  return ConstantStruct::getAnon(Elts);
}

/// EmitIntegerRegisterConstant - Turn the given INTEGER_CST into an LLVM
/// constant of the corresponding register type.
Constant *TreeToLLVM::EmitIntegerRegisterConstant(tree reg) {
  ConstantInt *CI = ConstantInt::get(Context, getAPIntValue(reg));
  // The destination can be a pointer, integer or floating point type so we need
  // a generalized cast here
  Type *Ty = getRegType(TREE_TYPE(reg));
  Instruction::CastOps opcode = CastInst::getCastOpcode(CI, false, Ty,
    !TYPE_UNSIGNED(TREE_TYPE(reg)));
  return TheFolder->CreateCast(opcode, CI, Ty);
}

/// EmitRealRegisterConstant - Turn the given REAL_CST into an LLVM constant
/// of the corresponding register type.
Constant *TreeToLLVM::EmitRealRegisterConstant(tree reg) {
  // TODO: Rather than going through memory, construct the APFloat directly from
  // the real_value.  This works fine for zero, inf and nan values, but APFloat
  // has no constructor for normal numbers, i.e. constructing a normal number
  // from the exponent and significand.
  // TODO: Test implementation on a big-endian machine.

  // Encode the constant in Buffer in target format.
  SmallVector<unsigned char, 16> Buffer;
  EncodeExpr(reg, Buffer);

  // Discard any alignment padding, which we assume comes at the end.
  unsigned Precision = TYPE_PRECISION(TREE_TYPE(reg));
  assert((Precision & 7) == 0 && "Unsupported real number precision!");
  Buffer.resize(Precision / 8);

  // We are going to view the buffer as an array of APInt words.  Ensure that
  // the buffer contains a whole number of words by extending it if necessary.
  unsigned Words = (Precision + integerPartWidth - 1) / integerPartWidth;
  // On a little-endian machine extend the buffer by adding bytes to the end.
  Buffer.resize(Words * (integerPartWidth / 8));
  // On a big-endian machine extend the buffer by adding bytes to the beginning.
  if (BYTES_BIG_ENDIAN)
    std::copy_backward(Buffer.begin(), Buffer.begin() + Precision / 8,
                       Buffer.end());

  // Ensure that the least significant word comes first: we are going to make an
  // APInt, and the APInt constructor wants the least significant word first.
  integerPart *Parts = (integerPart *)&Buffer[0];
  if (BYTES_BIG_ENDIAN)
    std::reverse(Parts, Parts + Words);

  bool isPPC_FP128 = ConvertType(TREE_TYPE(reg))->isPPC_FP128Ty();
  if (isPPC_FP128) {
    // This type is actually a pair of doubles in disguise.  They turn up the
    // wrong way round here, so flip them.
    assert(FLOAT_WORDS_BIG_ENDIAN && "PPC not big endian!");
    assert(Words == 2 && Precision == 128 && "Strange size for PPC_FP128!");
    std::swap(Parts[0], Parts[1]);
  }

  // Form an APInt from the buffer, an APFloat from the APInt, and the desired
  // floating point constant from the APFloat, phew!
  const APInt &I = APInt(Precision, Words, Parts);
  return ConstantFP::get(Context, APFloat(I, !isPPC_FP128));
}

/// EmitConstantVectorConstructor - Turn the given constant CONSTRUCTOR into
/// an LLVM constant of the corresponding vector register type.
Constant *TreeToLLVM::EmitConstantVectorConstructor(tree reg) {
  // Get the constructor as an LLVM constant.
  Constant *C = ConvertInitializer(reg);
  // Load the vector register out of it.
  return ExtractRegisterFromConstant(C, TREE_TYPE(reg));
}

/// EmitVectorRegisterConstant - Turn the given VECTOR_CST into an LLVM constant
/// of the corresponding register type.
Constant *TreeToLLVM::EmitVectorRegisterConstant(tree reg) {
  // If there are no elements then immediately return the default value for a
  // small speedup.
  if (!TREE_VECTOR_CST_ELTS(reg))
    return getDefaultValue(getRegType(TREE_TYPE(reg)));

  // Convert the elements.
  SmallVector<Constant*, 16> Elts;
  for (tree elt = TREE_VECTOR_CST_ELTS(reg); elt; elt = TREE_CHAIN(elt))
    Elts.push_back(EmitRegisterConstant(TREE_VALUE(elt)));

  // If there weren't enough elements then set the rest of the vector to the
  // default value.
  if (Elts.size() < TYPE_VECTOR_SUBPARTS(TREE_TYPE(reg))) {
    Constant *Default = getDefaultValue(Elts[0]->getType());
    Elts.append(TYPE_VECTOR_SUBPARTS(TREE_TYPE(reg)) - Elts.size(), Default);
  }

  return ConstantVector::get(Elts);
}

/// VectorHighElements - Return a vector of half the length, consisting of the
/// elements of the given vector with indices in the top half.
Value *TreeToLLVM::VectorHighElements(Value *Vec) {
  VectorType *Ty = cast<VectorType>(Vec->getType());
  assert(!(Ty->getNumElements() & 1) && "Vector has odd number of elements!");
  unsigned NumElts = Ty->getNumElements() / 2;
  SmallVector<Constant*, 8> Mask;
  Mask.reserve(NumElts);
  for (unsigned i = 0; i != NumElts; ++i)
    Mask.push_back(Builder.getInt32(NumElts + i));
  return Builder.CreateShuffleVector(Vec, UndefValue::get(Ty),
                                     ConstantVector::get(Mask));
}

/// VectorLowElements - Return a vector of half the length, consisting of the
/// elements of the given vector with indices in the bottom half.
Value *TreeToLLVM::VectorLowElements(Value *Vec) {
  VectorType *Ty = cast<VectorType>(Vec->getType());
  assert(!(Ty->getNumElements() & 1) && "Vector has odd number of elements!");
  unsigned NumElts = Ty->getNumElements() / 2;
  SmallVector<Constant*, 8> Mask;
  Mask.reserve(NumElts);
  for (unsigned i = 0; i != NumElts; ++i)
    Mask.push_back(Builder.getInt32(i));
  return Builder.CreateShuffleVector(Vec, UndefValue::get(Ty),
                                     ConstantVector::get(Mask));
}


//===----------------------------------------------------------------------===//
//           ... EmitReg* - Convert register expression to LLVM...
//===----------------------------------------------------------------------===//

/// EmitMemory - Convert the specified gimple register or local constant of
/// register type to an LLVM value with in-memory type (given by ConvertType).
Value *TreeToLLVM::EmitMemory(tree reg) {
  return Reg2Mem(EmitRegister(reg), TREE_TYPE(reg), Builder);
}

/// EmitRegister - Convert the specified gimple register or local constant of
/// register type to an LLVM value.  Only creates code in the entry block.
Value *TreeToLLVM::EmitRegister(tree reg) {
  while (isa<OBJ_TYPE_REF>(reg)) reg = OBJ_TYPE_REF_EXPR(reg);
  return isa<SSA_NAME>(reg) ? EmitReg_SSA_NAME(reg) : EmitMinInvariant(reg);
}

/// EmitReg_SSA_NAME - Return the defining value of the given SSA_NAME.
/// Only creates code in the entry block.
Value *TreeToLLVM::EmitReg_SSA_NAME(tree reg) {
  assert(is_gimple_reg_type(TREE_TYPE(reg)) && "Not of register type!");

  // If we already found the definition of the SSA name, return it.
  if (Value *ExistingValue = SSANames[reg]) {
    assert(ExistingValue->getType() == getRegType(TREE_TYPE(reg)) &&
           "SSA name has wrong type!");
    if (!isSSAPlaceholder(ExistingValue))
      return ExistingValue;
  }

  // If this is not the definition of the SSA name, return a placeholder value.
  if (!SSA_NAME_IS_DEFAULT_DEF(reg)) {
    if (Value *ExistingValue = SSANames[reg])
      return ExistingValue; // The type was sanity checked above.
    return SSANames[reg] = GetSSAPlaceholder(getRegType(TREE_TYPE(reg)));
  }

  // This SSA name is the default definition for the underlying symbol.

  // The underlying symbol is an SSA variable.
  tree var = SSA_NAME_VAR(reg);
  assert(SSA_VAR_P(var) && "Not an SSA variable!");

  // If the variable is itself an ssa name, use its LLVM value.
  if (isa<SSA_NAME>(var)) {
    Value *Val = EmitReg_SSA_NAME(var);
    assert(Val->getType() == getRegType(TREE_TYPE(reg)) &&
           "SSA name has wrong type!");
    return DefineSSAName(reg, Val);
  }

  // Otherwise the symbol is a VAR_DECL, PARM_DECL or RESULT_DECL.  Since a
  // default definition is only created if the very first reference to the
  // variable in the function is a read operation, and refers to the value
  // read, it has an undefined value for VAR_DECLs (a RESULT_DECL can have
  // an initial value if the function returns a class by value).
  assert((isa<PARM_DECL>(var) || isa<RESULT_DECL>(var) ||
          isa<VAR_DECL>(var)) && "Unsupported SSA name definition!");
  if (isa<VAR_DECL>(var))
    return DefineSSAName(reg, UndefValue::get(getRegType(TREE_TYPE(reg))));

  // Read the initial value of the parameter and associate it with the ssa name.
  assert(DECL_LOCAL_IF_SET(var) != 0 && "Parameter not laid out?");

  unsigned Alignment = DECL_ALIGN(var) / 8;
  assert(Alignment != 0 && "Parameter with unknown alignment!");

  // Perform the load in the entry block, after all parameters have been set up
  // with their initial values, and before any modifications to their values.

  // Create a builder that inserts code before the SSAInsertionPoint marker.
  LLVMBuilder SSABuilder(Context, Builder.getFolder());
  SSABuilder.SetInsertPoint(SSAInsertionPoint->getParent(), SSAInsertionPoint);

  // Use it to load the parameter value.
  MemRef ParamLoc(DECL_LOCAL_IF_SET(var), Alignment, false);
  Value *Def = LoadRegisterFromMemory(ParamLoc, TREE_TYPE(reg), 0, SSABuilder);

  if (flag_verbose_asm)
    NameValue(Def, reg);
  return DefineSSAName(reg, Def);
}

// Unary expressions.
Value *TreeToLLVM::EmitReg_ABS_EXPR(tree op) {
  if (!isa<FLOAT_TYPE>(TREE_TYPE(op))) {
    Value *Op = EmitRegister(op);
    Value *OpN = Builder.CreateNeg(Op, Op->getName()+"neg");
    ICmpInst::Predicate pred = TYPE_UNSIGNED(TREE_TYPE(op)) ?
      ICmpInst::ICMP_UGE : ICmpInst::ICMP_SGE;
    Value *Cmp = Builder.CreateICmp(pred, Op,
                    Constant::getNullValue(Op->getType()), "abscond");
    return Builder.CreateSelect(Cmp, Op, OpN, Op->getName()+"abs");
  }

  if (isa<VECTOR_TYPE>(TREE_TYPE(op))) {
    // Clear the sign bits.
    Value *Op = EmitRegister(op);
    VectorType *VecTy = cast<VectorType>(Op->getType());

    // Mask = ~(1 << (Bits-1)).
    unsigned Bits = VecTy->getElementType()->getPrimitiveSizeInBits();
    Type *IntTy = IntegerType::get(Context, Bits);
    Type *IntVecTy = VectorType::get(IntTy, VecTy->getNumElements());
    APInt API = APInt::getAllOnesValue(Bits);
    API.clearBit(Bits-1);
    Constant *Mask = ConstantInt::get(IntVecTy, API);

    // Zap the sign bits.
    Op = Builder.CreateBitCast(Op, IntVecTy);
    Op = Builder.CreateAnd(Op, Mask);
    Op = Builder.CreateBitCast(Op, VecTy);
    return Op;
  }

  // Turn FP abs into fabs/fabsf.
  StringRef Name = SelectFPName(TREE_TYPE(op), "fabsf", "fabs", "fabsl");
  if (!Name.empty()) {
    CallInst *Call = EmitSimpleCall(Name, TREE_TYPE(op), op, NULL);
    Call->setDoesNotThrow();
    Call->setDoesNotAccessMemory();
    return Call;
  }

  // Otherwise clear the sign bit.
  Value *Op = EmitRegister(op);
  Type *Ty = Op->getType();

  // Mask = ~(1 << (Bits-1)).
  unsigned Bits = Ty->getPrimitiveSizeInBits();
  Type *IntTy = IntegerType::get(Context, Bits);
  APInt API = APInt::getAllOnesValue(Bits);
  API.clearBit(Bits-1);
  Constant *Mask = ConstantInt::get(IntTy, API);

  // Zap the sign bit.
  Op = Builder.CreateBitCast(Op, IntTy);
  Op = Builder.CreateAnd(Op, Mask);
  Op = Builder.CreateBitCast(Op, Ty);
  return Op;
}

Value *TreeToLLVM::EmitReg_BIT_NOT_EXPR(tree op) {
  Value *Op = EmitRegister(op);
  return Builder.CreateNot(Op, Op->getName()+"not");
}

Value *TreeToLLVM::EmitReg_CONJ_EXPR(tree op) {
  tree elt_type = TREE_TYPE(TREE_TYPE(op));
  Value *R, *I;
  SplitComplex(EmitRegister(op), R, I);

  // ~(a+ib) = a + i*-b
  I = CreateAnyNeg(I, elt_type);

  return CreateComplex(R, I);
}

Value *TreeToLLVM::EmitReg_CONVERT_EXPR(tree type, tree op) {
  return CastToAnyType(EmitRegister(op), !TYPE_UNSIGNED(TREE_TYPE(op)),
                       getRegType(type), !TYPE_UNSIGNED(type));
}

Value *TreeToLLVM::EmitReg_NEGATE_EXPR(tree op) {
  Value *V = EmitRegister(op);
  tree type = TREE_TYPE(op);

  if (isa<COMPLEX_TYPE>(type)) {
    tree elt_type = TREE_TYPE(type);
    Value *R, *I; SplitComplex(V, R, I);

    // -(a+ib) = -a + i*-b
    R = CreateAnyNeg(R, elt_type);
    I = CreateAnyNeg(I, elt_type);

    return CreateComplex(R, I);
  }

  return CreateAnyNeg(V, type);
}

Value *TreeToLLVM::EmitReg_PAREN_EXPR(tree op) {
  // TODO: Understand and correctly deal with this subtle expression.
  return EmitRegister(op);
}

Value *TreeToLLVM::EmitReg_TRUTH_NOT_EXPR(tree type, tree op) {
  Value *V = EmitRegister(op);
  if (!V->getType()->isIntegerTy(1))
    V = Builder.CreateICmpNE(V,
          Constant::getNullValue(V->getType()), "toBool");
  V = Builder.CreateNot(V, V->getName()+"not");
  return Builder.CreateIntCast(V, getRegType(type), /*isSigned*/false);
}

// Comparisons.

/// EmitCompare - Compare LHS with RHS using the appropriate comparison code.
/// The result is an i1 boolean.
Value *TreeToLLVM::EmitCompare(tree lhs, tree rhs, unsigned code) {
  Value *LHS = EmitRegister(lhs);
  Value *RHS = TriviallyTypeConvert(EmitRegister(rhs), LHS->getType());

  // Compute the LLVM opcodes corresponding to the GCC comparison.
  CmpInst::Predicate UIPred = CmpInst::BAD_ICMP_PREDICATE;
  CmpInst::Predicate SIPred = CmpInst::BAD_ICMP_PREDICATE;
  CmpInst::Predicate FPPred = CmpInst::BAD_FCMP_PREDICATE;

  switch (code) {
  default:
    llvm_unreachable("Unhandled condition code!");
  case LT_EXPR:
    UIPred = CmpInst::ICMP_ULT;
    SIPred = CmpInst::ICMP_SLT;
    FPPred = CmpInst::FCMP_OLT;
    break;
  case LE_EXPR:
    UIPred = CmpInst::ICMP_ULE;
    SIPred = CmpInst::ICMP_SLE;
    FPPred = CmpInst::FCMP_OLE;
    break;
  case GT_EXPR:
    UIPred = CmpInst::ICMP_UGT;
    SIPred = CmpInst::ICMP_SGT;
    FPPred = CmpInst::FCMP_OGT;
    break;
  case GE_EXPR:
    UIPred = CmpInst::ICMP_UGE;
    SIPred = CmpInst::ICMP_SGE;
    FPPred = CmpInst::FCMP_OGE;
    break;
  case EQ_EXPR:
    UIPred = SIPred = CmpInst::ICMP_EQ;
    FPPred = CmpInst::FCMP_OEQ;
    break;
  case NE_EXPR:
    UIPred = SIPred = CmpInst::ICMP_NE;
    FPPred = CmpInst::FCMP_UNE;
    break;
  case UNORDERED_EXPR: FPPred = CmpInst::FCMP_UNO; break;
  case ORDERED_EXPR:   FPPred = CmpInst::FCMP_ORD; break;
  case UNLT_EXPR:      FPPred = CmpInst::FCMP_ULT; break;
  case UNLE_EXPR:      FPPred = CmpInst::FCMP_ULE; break;
  case UNGT_EXPR:      FPPred = CmpInst::FCMP_UGT; break;
  case UNGE_EXPR:      FPPred = CmpInst::FCMP_UGE; break;
  case UNEQ_EXPR:      FPPred = CmpInst::FCMP_UEQ; break;
  case LTGT_EXPR:      FPPred = CmpInst::FCMP_ONE; break;
  }

  if (isa<COMPLEX_TYPE>(TREE_TYPE(lhs))) {
    Value *LHSr, *LHSi;
    SplitComplex(LHS, LHSr, LHSi);
    Value *RHSr, *RHSi;
    SplitComplex(RHS, RHSr, RHSi);

    Value *DSTr, *DSTi;
    if (LHSr->getType()->isFloatingPointTy()) {
      DSTr = Builder.CreateFCmp(FPPred, LHSr, RHSr);
      DSTi = Builder.CreateFCmp(FPPred, LHSi, RHSi);
      if (FPPred == CmpInst::FCMP_OEQ)
        return Builder.CreateAnd(DSTr, DSTi);
      assert(FPPred == CmpInst::FCMP_UNE && "Unhandled complex comparison!");
      return Builder.CreateOr(DSTr, DSTi);
    }

    assert(SIPred == UIPred && "(In)equality comparison depends on sign!");
    DSTr = Builder.CreateICmp(UIPred, LHSr, RHSr);
    DSTi = Builder.CreateICmp(UIPred, LHSi, RHSi);
    if (UIPred == CmpInst::ICMP_EQ)
      return Builder.CreateAnd(DSTr, DSTi);
    assert(UIPred == CmpInst::ICMP_NE && "Unhandled complex comparison!");
    return Builder.CreateOr(DSTr, DSTi);
  }

  if (LHS->getType()->isFPOrFPVectorTy())
    return Builder.CreateFCmp(FPPred, LHS, RHS);

  // Determine which predicate to use based on signedness.
  CmpInst::Predicate pred = TYPE_UNSIGNED(TREE_TYPE(lhs)) ? UIPred : SIPred;
  return Builder.CreateICmp(pred, LHS, RHS);
}

Value *TreeToLLVM::EmitReg_MinMaxExpr(tree op0, tree op1, unsigned UIPred,
                                      unsigned SIPred, unsigned FPPred) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  Value *Compare;
  if (isa<FLOAT_TYPE>(TREE_TYPE(op0)))
    Compare = Builder.CreateFCmp(FCmpInst::Predicate(FPPred), LHS, RHS);
  else if (TYPE_UNSIGNED(TREE_TYPE(op0)))
    Compare = Builder.CreateICmp(ICmpInst::Predicate(UIPred), LHS, RHS);
  else
    Compare = Builder.CreateICmp(ICmpInst::Predicate(SIPred), LHS, RHS);

  return Builder.CreateSelect(Compare, LHS, RHS);
}

Value *TreeToLLVM::EmitReg_ReducMinMaxExpr(tree op, unsigned UIPred,
                                           unsigned SIPred, unsigned FPPred) {
  // In the bottom half of the vector, form the max/min of the bottom and top
  // halves of the vector.  Rinse and repeat on the just computed bottom half:
  // in the bottom quarter of the vector, form the max/min of the bottom and
  // top halves of the bottom half.  Continue until only the first element of
  // the vector is computed.  For example, reduc-max <x0, x1, x2, x3> becomes
  //   v = max <x0, x1, undef, undef>, <x2, x3, undef, undef>
  //   w = max <v0, undef, undef, undef>, <v1, undef, undef, undef>
  // where v = <v0, v1, undef, undef>.  The first element of w is the max/min
  // of x0,x1,x2,x3.
  Value *Val = EmitRegister(op);
  Type *Ty = Val->getType();

  CmpInst::Predicate Pred =
    CmpInst::Predicate(isa<FLOAT_TYPE>(TREE_TYPE(op)) ?
                       FPPred : TYPE_UNSIGNED(TREE_TYPE(op)) ? UIPred : SIPred);

  unsigned Length = (unsigned)TYPE_VECTOR_SUBPARTS(TREE_TYPE(op));
  assert(Length > 1 && !(Length & (Length - 1)) && "Length not a power of 2!");
  SmallVector<Constant*, 8> Mask(Length);
  Constant *UndefIndex = UndefValue::get(Type::getInt32Ty(Context));
  for (unsigned Elts = Length >> 1; Elts; Elts >>= 1) {
    // In the extracted vectors, elements with index Elts and on are undefined.
    for (unsigned i = Elts; i != Length; ++i)
      Mask[i] = UndefIndex;
    // Extract elements [0, Elts) from Val.
    for (unsigned i = 0; i != Elts; ++i)
      Mask[i] = Builder.getInt32(i);
    Value *LHS = Builder.CreateShuffleVector(Val, UndefValue::get(Ty),
                                             ConstantVector::get(Mask));
    // Extract elements [Elts, 2*Elts) from Val.
    for (unsigned i = 0; i != Elts; ++i)
      Mask[i] = Builder.getInt32(Elts + i);
    Value *RHS = Builder.CreateShuffleVector(Val, UndefValue::get(Ty),
                                             ConstantVector::get(Mask));

    // Replace Val with the max/min of the extracted elements.
    Value *Compare = isa<FLOAT_TYPE>(TREE_TYPE(op)) ?
      Builder.CreateFCmp(Pred, LHS, RHS) : Builder.CreateICmp(Pred, LHS, RHS);
    Val = Builder.CreateSelect(Compare, LHS, RHS);

    // Repeat, using half as many elements.
  }

  return Val;
}

Value *TreeToLLVM::EmitReg_REDUC_PLUS_EXPR(tree op) {
  // In the bottom half of the vector, form the sum of the bottom and top halves
  // of the vector.  Rinse and repeat on the just computed bottom half: in the
  // bottom quarter of the vector, form the sum of the bottom and top halves of
  // the bottom half.  Continue until only the first element of the vector is
  // computed.  For example, reduc-plus <x0, x1, x2, x3> becomes
  //   v = <x0, x1, undef, undef> + <x2, x3, undef, undef>
  //   w = <v0, undef, undef, undef> + <v1, undef, undef, undef>
  // where v = <v0, v1, undef, undef>.  The first element of w is x0+x1+x2+x3.
  Value *Val = EmitRegister(op);
  Type *Ty = Val->getType();

  unsigned Length = (unsigned)TYPE_VECTOR_SUBPARTS(TREE_TYPE(op));
  assert(Length > 1 && !(Length & (Length - 1)) && "Length not a power of 2!");
  SmallVector<Constant*, 8> Mask(Length);
  Constant *UndefIndex = UndefValue::get(Type::getInt32Ty(Context));
  for (unsigned Elts = Length >> 1; Elts; Elts >>= 1) {
    // In the extracted vectors, elements with index Elts and on are undefined.
    for (unsigned i = Elts; i != Length; ++i)
      Mask[i] = UndefIndex;
    // Extract elements [0, Elts) from Val.
    for (unsigned i = 0; i != Elts; ++i)
      Mask[i] = Builder.getInt32(i);
    Value *LHS = Builder.CreateShuffleVector(Val, UndefValue::get(Ty),
                                             ConstantVector::get(Mask));
    // Extract elements [Elts, 2*Elts) from Val.
    for (unsigned i = 0; i != Elts; ++i)
      Mask[i] = Builder.getInt32(Elts + i);
    Value *RHS = Builder.CreateShuffleVector(Val, UndefValue::get(Ty),
                                             ConstantVector::get(Mask));

    // Replace Val with the sum of the extracted elements.
    // TODO: Are nsw/nuw flags valid here?
    Val = CreateAnyAdd(LHS, RHS, TREE_TYPE(TREE_TYPE(op)));

    // Repeat, using half as many elements.
  }

  return Val;
}

Value *TreeToLLVM::EmitReg_RotateOp(tree type, tree op0, tree op1,
                                    unsigned Opc1, unsigned Opc2) {
  Value *In  = EmitRegister(op0);
  Value *Amt = EmitRegister(op1);

  if (Amt->getType() != In->getType())
    Amt = Builder.CreateIntCast(Amt, In->getType(), /*isSigned*/false,
                                Amt->getName()+".cast");

  Value *TypeSize =
    ConstantInt::get(In->getType(),
                     In->getType()->getPrimitiveSizeInBits());

  // Do the two shifts.
  Value *V1 = Builder.CreateBinOp((Instruction::BinaryOps)Opc1, In, Amt);
  Value *OtherShift = Builder.CreateSub(TypeSize, Amt);
  Value *V2 = Builder.CreateBinOp((Instruction::BinaryOps)Opc2, In, OtherShift);

  // Or the two together to return them.
  Value *Merge = Builder.CreateOr(V1, V2);
  return Builder.CreateIntCast(Merge, getRegType(type), /*isSigned*/false);
}

Value *TreeToLLVM::EmitReg_ShiftOp(tree op0, tree op1, unsigned Opc) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  // Ensure that the shift amount has the same type as the shiftee.
  if (RHS->getType() != LHS->getType()) {
    if (LHS->getType()->isVectorTy() == RHS->getType()->isVectorTy()) {
      // Scalar shifted by a scalar amount, or a vector shifted by a vector
      // amount.
      assert((!LHS->getType()->isVectorTy() ||
              cast<VectorType>(LHS->getType())->getNumElements() ==
              cast<VectorType>(RHS->getType())->getNumElements()) &&
             "Vector length mismatch!");
      RHS = CastToAnyType(RHS, /*isSigned*/false, LHS->getType(),
                          /*isSigned*/false);
    } else {
      // Vector shifted by a scalar amount.  Turn the shift amount into a vector
      // with all elements equal.
      assert(LHS->getType()->isVectorTy() &&
             "Shifting a scalar by a vector amount!");
      VectorType *VecTy = cast<VectorType>(LHS->getType());
      RHS = CastToAnyType(RHS, /*isSigned*/false, VecTy->getElementType(),
                          /*isSigned*/false);
      RHS = Builder.CreateInsertElement(UndefValue::get(VecTy), RHS,
                                        Builder.getInt32(0));
      Type *MaskTy = VectorType::get(Type::getInt32Ty(Context),
                                           VecTy->getNumElements());
      RHS = Builder.CreateShuffleVector(RHS, UndefValue::get(VecTy),
                                        ConstantInt::get(MaskTy, 0));
    }
  }
  return Builder.CreateBinOp((Instruction::BinaryOps)Opc, LHS, RHS);
}

Value *TreeToLLVM::EmitReg_VecShiftOp(tree op0, tree op1, bool isLeftShift) {
  Value *LHS = EmitRegister(op0); // A vector.
  Value *Amt = EmitRegister(op1); // An integer.
  VectorType *VecTy = cast<VectorType>(LHS->getType());
  unsigned Bits = VecTy->getPrimitiveSizeInBits();

  // If the shift is by a multiple of the element size then emit a shuffle.
  if (ConstantInt *CI = dyn_cast<ConstantInt>(Amt)) {
    // The GCC docs are not clear whether the bits shifted in must be zero or if
    // they can be anything.  Since these expressions are currently only used in
    // situations which make no assumptions about the shifted in bits, we choose
    // to consider them to be undefined since this results in better code.
    unsigned ShiftAmt = (unsigned)CI->getLimitedValue(Bits);
    if (ShiftAmt >= Bits)
      // Shifting by more than the width of the vector is documented as giving
      // an undefined result.
      return UndefValue::get(VecTy);
    unsigned EltBits = VecTy->getElementType()->getPrimitiveSizeInBits();
    if (!(ShiftAmt % EltBits)) {
      // A shift by an integral number of elements.
      unsigned EltOffset = ShiftAmt / EltBits; // Shift by this many elements.
      // Shuffle the elements sideways by the appropriate number of elements.
      unsigned Length = VecTy->getNumElements();
      SmallVector<Constant*, 8> Mask;
      Mask.reserve(Length);
      if (isLeftShift) {
        // shl <4 x i32> %v, 32 ->
        // shufflevector <4 x i32> %v, <4 x i32> undef, <undef, 0, 1, 2>
        Mask.append(Length - EltOffset,
                    UndefValue::get(Type::getInt32Ty(Context)));
        for (unsigned i = 0; i != EltOffset; ++i)
          Mask.push_back(Builder.getInt32(i));
      } else {
        // shr <4 x i32> %v, 32 ->
        // shufflevector <4 x i32> %v, <4 x i32> undef, <1, 2, 3, undef>
        for (unsigned i = EltOffset; i != Length; ++i)
          Mask.push_back(Builder.getInt32(i));
        Mask.append(EltOffset, UndefValue::get(Type::getInt32Ty(Context)));
      }
      return Builder.CreateShuffleVector(LHS, UndefValue::get(VecTy),
                                         ConstantVector::get(Mask));
    }
  }

  // Turn the vector into a mighty integer of the same size.
  LHS = Builder.CreateBitCast(LHS, IntegerType::get(Context, Bits));

  // Ensure the shift amount has the same type.
  if (Amt->getType() != LHS->getType())
    Amt = Builder.CreateIntCast(Amt, LHS->getType(), /*isSigned*/false,
                                Amt->getName()+".cast");

  // Perform the shift.
  LHS = Builder.CreateBinOp(isLeftShift ? Instruction::Shl : Instruction::LShr,
                            LHS, Amt);

  // Turn the result back into a vector.
  return Builder.CreateBitCast(LHS, VecTy);
}

Value *TreeToLLVM::EmitReg_TruthOp(tree type, tree op0, tree op1, unsigned Opc){
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  // This is a truth operation like the strict &&,||,^^.  Convert to bool as
  // a test against zero
  LHS = Builder.CreateICmpNE(LHS,
                             Constant::getNullValue(LHS->getType()),
                             "toBool");
  RHS = Builder.CreateICmpNE(RHS,
                             Constant::getNullValue(RHS->getType()),
                             "toBool");

  Value *Res = Builder.CreateBinOp((Instruction::BinaryOps)Opc, LHS, RHS);
  return Builder.CreateZExt(Res, getRegType(type));
}

Value *TreeToLLVM::EmitReg_CEIL_DIV_EXPR(tree op0, tree op1) {
  // Notation: CEIL_DIV_EXPR <-> CDiv, TRUNC_DIV_EXPR <-> Div.

  // CDiv calculates LHS/RHS by rounding up to the nearest integer.  In terms
  // of Div this means if the values of LHS and RHS have opposite signs or if
  // LHS is zero, then CDiv necessarily equals Div; and
  //   LHS CDiv RHS = (LHS - Sign(RHS)) Div RHS + 1
  // otherwise.

  Type *Ty = getRegType(TREE_TYPE(op0));
  Constant *Zero = ConstantInt::get(Ty, 0);
  Constant *One = ConstantInt::get(Ty, 1);
  Constant *MinusOne = Constant::getAllOnesValue(Ty);

  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  if (!TYPE_UNSIGNED(TREE_TYPE(op0))) {
    // In the case of signed arithmetic, we calculate CDiv as follows:
    //   LHS CDiv RHS = (LHS - Sign(RHS) * Offset) Div RHS + Offset,
    // where Offset is 1 if LHS and RHS have the same sign and LHS is
    // not zero, and 0 otherwise.

    // On some machines INT_MIN Div -1 traps.  You might expect a trap for
    // INT_MIN CDiv -1 too, but this implementation will not generate one.
    // Quick quiz question: what value is returned for INT_MIN CDiv -1?

    // Determine the signs of LHS and RHS, and whether they have the same sign.
    Value *LHSIsPositive = Builder.CreateICmpSGE(LHS, Zero);
    Value *RHSIsPositive = Builder.CreateICmpSGE(RHS, Zero);
    Value *HaveSameSign = Builder.CreateICmpEQ(LHSIsPositive, RHSIsPositive);

    // Offset equals 1 if LHS and RHS have the same sign and LHS is not zero.
    Value *LHSNotZero = Builder.CreateICmpNE(LHS, Zero);
    Value *OffsetOne = Builder.CreateAnd(HaveSameSign, LHSNotZero);
    // ... otherwise it is 0.
    Value *Offset = Builder.CreateSelect(OffsetOne, One, Zero);

    // Calculate Sign(RHS) ...
    Value *SignRHS = Builder.CreateSelect(RHSIsPositive, One, MinusOne);
    // ... and Sign(RHS) * Offset
    Value *SignedOffset = Builder.CreateSExt(OffsetOne, Ty);
    SignedOffset = Builder.CreateAnd(SignRHS, SignedOffset);

    // Return CDiv = (LHS - Sign(RHS) * Offset) Div RHS + Offset.
    Value *CDiv = Builder.CreateSub(LHS, SignedOffset);
    CDiv = Builder.CreateSDiv(CDiv, RHS);
    return Builder.CreateAdd(CDiv, Offset, "cdiv");
  }

  // In the case of unsigned arithmetic, LHS and RHS necessarily have the
  // same sign, so we can use
  //   LHS CDiv RHS = (LHS - 1) Div RHS + 1
  // as long as LHS is non-zero.

  // Offset is 1 if LHS is non-zero, 0 otherwise.
  Value *LHSNotZero = Builder.CreateICmpNE(LHS, Zero);
  Value *Offset = Builder.CreateSelect(LHSNotZero, One, Zero);

  // Return CDiv = (LHS - Offset) Div RHS + Offset.
  Value *CDiv = Builder.CreateSub(LHS, Offset);
  CDiv = Builder.CreateUDiv(CDiv, RHS);
  return Builder.CreateAdd(CDiv, Offset, "cdiv");
}

Value *TreeToLLVM::EmitReg_BIT_AND_EXPR(tree op0, tree op1) {
  Value *LHS = CastToSameSizeInteger(EmitRegister(op0));
  Value *RHS = CastToSameSizeInteger(EmitRegister(op1));
  Value *Res = Builder.CreateAnd(LHS, RHS);
  return CastFromSameSizeInteger(Res, getRegType(TREE_TYPE(op0)));
}

Value *TreeToLLVM::EmitReg_BIT_IOR_EXPR(tree op0, tree op1) {
  Value *LHS = CastToSameSizeInteger(EmitRegister(op0));
  Value *RHS = CastToSameSizeInteger(EmitRegister(op1));
  Value *Res = Builder.CreateOr(LHS, RHS);
  return CastFromSameSizeInteger(Res, getRegType(TREE_TYPE(op0)));
}

Value *TreeToLLVM::EmitReg_BIT_XOR_EXPR(tree op0, tree op1) {
  Value *LHS = CastToSameSizeInteger(EmitRegister(op0));
  Value *RHS = CastToSameSizeInteger(EmitRegister(op1));
  Value *Res = Builder.CreateXor(LHS, RHS);
  return CastFromSameSizeInteger(Res, getRegType(TREE_TYPE(op0)));
}

/// EmitReg_CondExpr - Handle COND_EXPR and VEC_COND_EXPR gimple assign right-
/// hand sides.
Value *TreeToLLVM::EmitReg_CondExpr(tree op0, tree op1, tree op2) {
  // The condition is either a comparison or an SSA register.  Note that the
  // reason for accessing tree operands directly rather than taking advantage
  // of COND_EXPR_COND and friends is that the latter fail for VEC_COND_EXPR,
  // which is also handled here.
  Value *CondVal = COMPARISON_CLASS_P(op0) ?
    EmitCompare(TREE_OPERAND(op0, 0), TREE_OPERAND(op0, 1), TREE_CODE(op0)) :
    EmitRegister(op0);

  // Ensure the condition has i1 type.
  if (!CondVal->getType()->getScalarType()->isIntegerTy(1))
    CondVal = Builder.CreateICmpNE(CondVal,
                                   Constant::getNullValue(CondVal->getType()));

  // Emit the true and false values.
  Value *TrueVal = EmitRegister(op1);
  Value *FalseVal = EmitRegister(op2);
  FalseVal = TriviallyTypeConvert(FalseVal, TrueVal->getType());

  // Select the value to use based on the condition.
  return Builder.CreateSelect(CondVal, TrueVal, FalseVal);
}

Value *TreeToLLVM::EmitReg_COMPLEX_EXPR(tree op0, tree op1) {
  return CreateComplex(EmitRegister(op0), EmitRegister(op1));
}

Value *TreeToLLVM::EmitReg_FLOOR_DIV_EXPR(tree op0, tree op1) {
  // Notation: FLOOR_DIV_EXPR <-> FDiv, TRUNC_DIV_EXPR <-> Div.
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  // FDiv calculates LHS/RHS by rounding down to the nearest integer.  In terms
  // of Div this means if the values of LHS and RHS have the same sign or if LHS
  // is zero, then FDiv necessarily equals Div; and
  //   LHS FDiv RHS = (LHS + Sign(RHS)) Div RHS - 1
  // otherwise.

  if (TYPE_UNSIGNED(TREE_TYPE(op0)))
    // In the case of unsigned arithmetic, LHS and RHS necessarily have the
    // same sign, so FDiv is the same as Div.
    return Builder.CreateUDiv(LHS, RHS, "fdiv");

  Type *Ty = getRegType(TREE_TYPE(op0));
  Constant *Zero = ConstantInt::get(Ty, 0);
  Constant *One = ConstantInt::get(Ty, 1);
  Constant *MinusOne = Constant::getAllOnesValue(Ty);

  // In the case of signed arithmetic, we calculate FDiv as follows:
  //   LHS FDiv RHS = (LHS + Sign(RHS) * Offset) Div RHS - Offset,
  // where Offset is 1 if LHS and RHS have opposite signs and LHS is
  // not zero, and 0 otherwise.

  // Determine the signs of LHS and RHS, and whether they have the same sign.
  Value *LHSIsPositive = Builder.CreateICmpSGE(LHS, Zero);
  Value *RHSIsPositive = Builder.CreateICmpSGE(RHS, Zero);
  Value *SignsDiffer = Builder.CreateICmpNE(LHSIsPositive, RHSIsPositive);

  // Offset equals 1 if LHS and RHS have opposite signs and LHS is not zero.
  Value *LHSNotZero = Builder.CreateICmpNE(LHS, Zero);
  Value *OffsetOne = Builder.CreateAnd(SignsDiffer, LHSNotZero);
  // ... otherwise it is 0.
  Value *Offset = Builder.CreateSelect(OffsetOne, One, Zero);

  // Calculate Sign(RHS) ...
  Value *SignRHS = Builder.CreateSelect(RHSIsPositive, One, MinusOne);
  // ... and Sign(RHS) * Offset
  Value *SignedOffset = Builder.CreateSExt(OffsetOne, Ty);
  SignedOffset = Builder.CreateAnd(SignRHS, SignedOffset);

  // Return FDiv = (LHS + Sign(RHS) * Offset) Div RHS - Offset.
  Value *FDiv = Builder.CreateAdd(LHS, SignedOffset);
  FDiv = Builder.CreateSDiv(FDiv, RHS);
  return Builder.CreateSub(FDiv, Offset, "fdiv");
}

Value *TreeToLLVM::EmitReg_FLOOR_MOD_EXPR(tree op0, tree op1) {
  // Notation: FLOOR_MOD_EXPR <-> Mod, TRUNC_MOD_EXPR <-> Rem.

  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  // We express Mod in terms of Rem as follows: if RHS exactly divides LHS,
  // or the values of LHS and RHS have the same sign, then Mod equals Rem.
  // Otherwise Mod equals Rem + RHS.  This means that LHS Mod RHS traps iff
  // LHS Rem RHS traps.
  if (TYPE_UNSIGNED(TREE_TYPE(op0)))
    // LHS and RHS values must have the same sign if their type is unsigned.
    return Builder.CreateURem(LHS, RHS);

  Type *Ty = getRegType(TREE_TYPE(op0));
  Constant *Zero = ConstantInt::get(Ty, 0);

  // The two possible values for Mod.
  Value *Rem = Builder.CreateSRem(LHS, RHS, "rem");
  Value *RemPlusRHS = Builder.CreateAdd(Rem, RHS);

  // HaveSameSign: (LHS >= 0) == (RHS >= 0).
  Value *LHSIsPositive = Builder.CreateICmpSGE(LHS, Zero);
  Value *RHSIsPositive = Builder.CreateICmpSGE(RHS, Zero);
  Value *HaveSameSign = Builder.CreateICmpEQ(LHSIsPositive,RHSIsPositive);

  // RHS exactly divides LHS iff Rem is zero.
  Value *RemIsZero = Builder.CreateICmpEQ(Rem, Zero);

  Value *SameAsRem = Builder.CreateOr(HaveSameSign, RemIsZero);
  return Builder.CreateSelect(SameAsRem, Rem, RemPlusRHS, "mod");
}

Value *TreeToLLVM::EmitReg_MINUS_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  tree type = TREE_TYPE(op0);

  if (isa<COMPLEX_TYPE>(type)) {
    tree elt_type = TREE_TYPE(type);
    Value *LHSr, *LHSi; SplitComplex(LHS, LHSr, LHSi);
    Value *RHSr, *RHSi; SplitComplex(RHS, RHSr, RHSi);

    // (a+ib) - (c+id) = (a-c) + i(b-d)
    LHSr = CreateAnySub(LHSr, RHSr, elt_type);
    LHSi = CreateAnySub(LHSi, RHSi, elt_type);

    return CreateComplex(LHSr, LHSi);
  }

  return CreateAnySub(LHS, RHS, type);
}

Value *TreeToLLVM::EmitReg_MULT_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  tree type = TREE_TYPE(op0);

  if (isa<COMPLEX_TYPE>(type)) {
    tree elt_type = TREE_TYPE(type);
    Value *LHSr, *LHSi; SplitComplex(LHS, LHSr, LHSi);
    Value *RHSr, *RHSi; SplitComplex(RHS, RHSr, RHSi);
    Value *DSTr, *DSTi;

    // (a+ib) * (c+id) = (ac-bd) + i(ad+cb)
    if (isa<REAL_TYPE>(elt_type)) {
      Value *Tmp1 = Builder.CreateFMul(LHSr, RHSr); // a*c
      Value *Tmp2 = Builder.CreateFMul(LHSi, RHSi); // b*d
      DSTr = Builder.CreateFSub(Tmp1, Tmp2);        // ac-bd

      Value *Tmp3 = Builder.CreateFMul(LHSr, RHSi); // a*d
      Value *Tmp4 = Builder.CreateFMul(RHSr, LHSi); // c*b
      DSTi = Builder.CreateFAdd(Tmp3, Tmp4);        // ad+cb
    } else {
      // If overflow does not wrap in the element type then it is tempting to
      // use NSW operations here.  However that would be wrong since overflow
      // of an intermediate value calculated here does not necessarily imply
      // that the final result overflows.
      Value *Tmp1 = Builder.CreateMul(LHSr, RHSr); // a*c
      Value *Tmp2 = Builder.CreateMul(LHSi, RHSi); // b*d
      DSTr = Builder.CreateSub(Tmp1, Tmp2);        // ac-bd

      Value *Tmp3 = Builder.CreateMul(LHSr, RHSi); // a*d
      Value *Tmp4 = Builder.CreateMul(RHSr, LHSi); // c*b
      DSTi = Builder.CreateAdd(Tmp3, Tmp4);        // ad+cb
    }

    return CreateComplex(DSTr, DSTi);
  }

  return CreateAnyMul(LHS, RHS, type);
}

Value *TreeToLLVM::EmitReg_PLUS_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  tree type = TREE_TYPE(op0);

  if (isa<COMPLEX_TYPE>(type)) {
    tree elt_type = TREE_TYPE(type);
    Value *LHSr, *LHSi; SplitComplex(LHS, LHSr, LHSi);
    Value *RHSr, *RHSi; SplitComplex(RHS, RHSr, RHSi);

    // (a+ib) + (c+id) = (a+c) + i(b+d)
    LHSr = CreateAnyAdd(LHSr, RHSr, elt_type);
    LHSi = CreateAnyAdd(LHSi, RHSi, elt_type);

    return CreateComplex(LHSr, LHSi);
  }

  return CreateAnyAdd(LHS, RHS, type);
}

Value *TreeToLLVM::EmitReg_POINTER_PLUS_EXPR(tree op0, tree op1) {
  Value *Ptr = EmitRegister(op0); // The pointer.
  Value *Idx = EmitRegister(op1); // The offset in units.

  // Convert the pointer into an i8* and add the offset to it.
  Ptr = Builder.CreateBitCast(Ptr, GetUnitPointerType(Context));
  StringRef GEPName = flag_verbose_asm ? "pp" : "";
  return POINTER_TYPE_OVERFLOW_UNDEFINED ?
    Builder.CreateInBoundsGEP(Ptr, Idx, GEPName) :
    Builder.CreateGEP(Ptr, Idx, GEPName);
}

Value *TreeToLLVM::EmitReg_RDIV_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  tree type = TREE_TYPE(op0);

  if (isa<COMPLEX_TYPE>(type)) {
    Value *LHSr, *LHSi; SplitComplex(LHS, LHSr, LHSi);
    Value *RHSr, *RHSi; SplitComplex(RHS, RHSr, RHSi);
    Value *DSTr, *DSTi;

    // (a+ib) / (c+id) = ((ac+bd)/(cc+dd)) + i((bc-ad)/(cc+dd))
    assert (isa<REAL_TYPE>(TREE_TYPE(type)) &&
            "RDIV_EXPR not floating point!");
    Value *Tmp1 = Builder.CreateFMul(LHSr, RHSr); // a*c
    Value *Tmp2 = Builder.CreateFMul(LHSi, RHSi); // b*d
    Value *Tmp3 = Builder.CreateFAdd(Tmp1, Tmp2); // ac+bd

    Value *Tmp4 = Builder.CreateFMul(RHSr, RHSr); // c*c
    Value *Tmp5 = Builder.CreateFMul(RHSi, RHSi); // d*d
    Value *Tmp6 = Builder.CreateFAdd(Tmp4, Tmp5); // cc+dd
    DSTr = Builder.CreateFDiv(Tmp3, Tmp6);

    Value *Tmp7 = Builder.CreateFMul(LHSi, RHSr); // b*c
    Value *Tmp8 = Builder.CreateFMul(LHSr, RHSi); // a*d
    Value *Tmp9 = Builder.CreateFSub(Tmp7, Tmp8); // bc-ad
    DSTi = Builder.CreateFDiv(Tmp9, Tmp6);

    return CreateComplex(DSTr, DSTi);
  }

  assert(isa<FLOAT_TYPE>(type) && "RDIV_EXPR not floating point!");
  return Builder.CreateFDiv(LHS, RHS);
}

Value *TreeToLLVM::EmitReg_ROUND_DIV_EXPR(tree op0, tree op1) {
  // Notation: ROUND_DIV_EXPR <-> RDiv, TRUNC_DIV_EXPR <-> Div.

  // RDiv calculates LHS/RHS by rounding to the nearest integer.  Ties
  // are broken by rounding away from zero.  In terms of Div this means:
  //   LHS RDiv RHS = (LHS + (RHS Div 2)) Div RHS
  // if the values of LHS and RHS have the same sign; and
  //   LHS RDiv RHS = (LHS - (RHS Div 2)) Div RHS
  // if the values of LHS and RHS differ in sign.  The intermediate
  // expressions in these formulae can overflow, so some tweaking is
  // required to ensure correct results.  The details depend on whether
  // we are doing signed or unsigned arithmetic.

  Type *Ty = getRegType(TREE_TYPE(op0));
  Constant *Zero = ConstantInt::get(Ty, 0);
  Constant *Two = ConstantInt::get(Ty, 2);

  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  if (!TYPE_UNSIGNED(TREE_TYPE(op0))) {
    // In the case of signed arithmetic, we calculate RDiv as follows:
    //   LHS RDiv RHS = (sign) ( (|LHS| + (|RHS| UDiv 2)) UDiv |RHS| ),
    // where sign is +1 if LHS and RHS have the same sign, -1 if their
    // signs differ.  Doing the computation unsigned ensures that there
    // is no overflow.

    // On some machines INT_MIN Div -1 traps.  You might expect a trap for
    // INT_MIN RDiv -1 too, but this implementation will not generate one.
    // Quick quiz question: what value is returned for INT_MIN RDiv -1?

    // Determine the signs of LHS and RHS, and whether they have the same sign.
    Value *LHSIsPositive = Builder.CreateICmpSGE(LHS, Zero);
    Value *RHSIsPositive = Builder.CreateICmpSGE(RHS, Zero);
    Value *HaveSameSign = Builder.CreateICmpEQ(LHSIsPositive, RHSIsPositive);

    // Calculate |LHS| ...
    Value *MinusLHS = Builder.CreateNeg(LHS);
    Value *AbsLHS = Builder.CreateSelect(LHSIsPositive, LHS, MinusLHS,
                                         LHS->getName()+".abs");
    // ... and |RHS|
    Value *MinusRHS = Builder.CreateNeg(RHS);
    Value *AbsRHS = Builder.CreateSelect(RHSIsPositive, RHS, MinusRHS,
                                         RHS->getName()+".abs");

    // Calculate AbsRDiv = (|LHS| + (|RHS| UDiv 2)) UDiv |RHS|.
    Value *HalfAbsRHS = Builder.CreateUDiv(AbsRHS, Two);
    Value *Numerator = Builder.CreateAdd(AbsLHS, HalfAbsRHS);
    Value *AbsRDiv = Builder.CreateUDiv(Numerator, AbsRHS);

    // Return AbsRDiv or -AbsRDiv according to whether LHS and RHS have the
    // same sign or not.
    Value *MinusAbsRDiv = Builder.CreateNeg(AbsRDiv);
    return Builder.CreateSelect(HaveSameSign, AbsRDiv, MinusAbsRDiv, "rdiv");
  }

  // In the case of unsigned arithmetic, LHS and RHS necessarily have the
  // same sign, however overflow is a problem.  We want to use the formula
  //   LHS RDiv RHS = (LHS + (RHS Div 2)) Div RHS,
  // but if LHS + (RHS Div 2) overflows then we get the wrong result.  Since
  // the use of a conditional branch seems to be unavoidable, we choose the
  // simple solution of explicitly checking for overflow, and using
  //   LHS RDiv RHS = ((LHS + (RHS Div 2)) - RHS) Div RHS + 1
  // if it occurred.

  // Usually the numerator is LHS + (RHS Div 2); calculate this.
  Value *HalfRHS = Builder.CreateUDiv(RHS, Two);
  Value *Numerator = Builder.CreateAdd(LHS, HalfRHS);

  // Did the calculation overflow?
  Value *Overflowed = Builder.CreateICmpULT(Numerator, HalfRHS);

  // If so, use (LHS + (RHS Div 2)) - RHS for the numerator instead.
  Value *AltNumerator = Builder.CreateSub(Numerator, RHS);
  Numerator = Builder.CreateSelect(Overflowed, AltNumerator, Numerator);

  // Quotient = Numerator / RHS.
  Value *Quotient = Builder.CreateUDiv(Numerator, RHS);

  // Return Quotient unless we overflowed, in which case return Quotient + 1.
  return Builder.CreateAdd(Quotient, Builder.CreateIntCast(Overflowed, Ty,
                                                           /*isSigned*/false),
                           "rdiv");
}

Value *TreeToLLVM::EmitReg_TRUNC_DIV_EXPR(tree op0, tree op1, bool isExact) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  tree type = TREE_TYPE(op0);

  if (isa<COMPLEX_TYPE>(type)) {
    tree elt_type = TREE_TYPE(type);
    Value *LHSr, *LHSi; SplitComplex(LHS, LHSr, LHSi);
    Value *RHSr, *RHSi; SplitComplex(RHS, RHSr, RHSi);
    Value *DSTr, *DSTi;

    // (a+ib) / (c+id) = ((ac+bd)/(cc+dd)) + i((bc-ad)/(cc+dd))
    assert (LHSr->getType()->isIntegerTy() && "TRUNC_DIV_EXPR not integer!");
    // If overflow does not wrap in the element type then it is tempting to
    // use NSW operations here.  However that would be wrong since overflow
    // of an intermediate value calculated here does not necessarily imply
    // that the final result overflows.
    Value *Tmp1 = Builder.CreateMul(LHSr, RHSr); // a*c
    Value *Tmp2 = Builder.CreateMul(LHSi, RHSi); // b*d
    Value *Tmp3 = Builder.CreateAdd(Tmp1, Tmp2); // ac+bd

    Value *Tmp4 = Builder.CreateMul(RHSr, RHSr); // c*c
    Value *Tmp5 = Builder.CreateMul(RHSi, RHSi); // d*d
    Value *Tmp6 = Builder.CreateAdd(Tmp4, Tmp5); // cc+dd
    DSTr = TYPE_UNSIGNED(elt_type) ?
      Builder.CreateUDiv(Tmp3, Tmp6) : Builder.CreateSDiv(Tmp3, Tmp6);

    Value *Tmp7 = Builder.CreateMul(LHSi, RHSr); // b*c
    Value *Tmp8 = Builder.CreateMul(LHSr, RHSi); // a*d
    Value *Tmp9 = Builder.CreateSub(Tmp7, Tmp8); // bc-ad
    DSTi = TYPE_UNSIGNED(elt_type) ?
      Builder.CreateUDiv(Tmp9, Tmp6) : Builder.CreateSDiv(Tmp9, Tmp6);

    return CreateComplex(DSTr, DSTi);
  }

  assert(LHS->getType()->isIntOrIntVectorTy() && "TRUNC_DIV_EXPR not integer!");
  if (TYPE_UNSIGNED(type))
    return Builder.CreateUDiv(LHS, RHS, "", isExact);
  else
    return Builder.CreateSDiv(LHS, RHS, "", isExact);
}

Value *TreeToLLVM::EmitReg_TRUNC_MOD_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  return TYPE_UNSIGNED(TREE_TYPE(op0)) ?
    Builder.CreateURem(LHS, RHS) : Builder.CreateSRem(LHS, RHS);
}

#if (GCC_MINOR < 7)
Value *TreeToLLVM::EmitReg_VEC_EXTRACT_EVEN_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  unsigned Length = (unsigned)TYPE_VECTOR_SUBPARTS(TREE_TYPE(op0));
  SmallVector<Constant*, 16> Mask;
  Mask.reserve(Length);
  for (unsigned i = 0; i != Length; ++i)
    Mask.push_back(Builder.getInt32(2*i));
  return Builder.CreateShuffleVector(LHS, RHS, ConstantVector::get(Mask));
}
#endif

#if (GCC_MINOR < 7)
Value *TreeToLLVM::EmitReg_VEC_EXTRACT_ODD_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  unsigned Length = (unsigned)TYPE_VECTOR_SUBPARTS(TREE_TYPE(op0));
  SmallVector<Constant*, 16> Mask;
  Mask.reserve(Length);
  for (unsigned i = 0; i != Length; ++i)
    Mask.push_back(Builder.getInt32(2*i+1));
  return Builder.CreateShuffleVector(LHS, RHS, ConstantVector::get(Mask));
}
#endif

#if (GCC_MINOR < 7)
Value *TreeToLLVM::EmitReg_VEC_INTERLEAVE_HIGH_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  unsigned Length = (unsigned)TYPE_VECTOR_SUBPARTS(TREE_TYPE(op0));
  assert(!(Length & 1) && "Expected an even number of vector elements!");
  SmallVector<Constant*, 16> Mask;
  Mask.reserve(Length);
  for (unsigned i = Length/2; i != Length; ++i) {
    Mask.push_back(Builder.getInt32(i));
    Mask.push_back(Builder.getInt32(Length + i));
  }
  return Builder.CreateShuffleVector(LHS, RHS, ConstantVector::get(Mask));
}
#endif

#if (GCC_MINOR < 7)
Value *TreeToLLVM::EmitReg_VEC_INTERLEAVE_LOW_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  unsigned Length = (unsigned)TYPE_VECTOR_SUBPARTS(TREE_TYPE(op0));
  assert(!(Length & 1) && "Expected an even number of vector elements!");
  SmallVector<Constant*, 16> Mask;
  Mask.reserve(Length);
  for (unsigned i = 0, e = Length/2; i != e; ++i) {
    Mask.push_back(Builder.getInt32(i));
    Mask.push_back(Builder.getInt32(Length + i));
  }
  return Builder.CreateShuffleVector(LHS, RHS, ConstantVector::get(Mask));
}
#endif

Value *TreeToLLVM::EmitReg_VEC_PACK_TRUNC_EXPR(tree type, tree op0, tree op1) {
  // Eg: <4 x float> = VEC_PACK_TRUNC_EXPR(<2 x double>, <2 x double>).
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  // Truncate the input elements to the output element type, eg: <2 x double>
  // -> <2 x float>.
  unsigned Length = (unsigned)TYPE_VECTOR_SUBPARTS(TREE_TYPE(op0));
  Type *DestTy = VectorType::get(getRegType(TREE_TYPE(type)), Length);
  LHS = CastToAnyType(LHS, !TYPE_UNSIGNED(TREE_TYPE(TREE_TYPE(op0))), DestTy,
                      !TYPE_UNSIGNED(TREE_TYPE(type)));
  RHS = CastToAnyType(RHS, !TYPE_UNSIGNED(TREE_TYPE(TREE_TYPE(op0))), DestTy,
                      !TYPE_UNSIGNED(TREE_TYPE(type)));

  // Concatenate the truncated inputs into one vector of twice the length,
  // eg: <2 x float>, <2 x float> -> <4 x float>.
  SmallVector<Constant*, 16> Mask;
  Mask.reserve(2*Length);
  for (unsigned i = 0, e = 2*Length; i != e; ++i)
    Mask.push_back(Builder.getInt32(i));
  return Builder.CreateShuffleVector(LHS, RHS, ConstantVector::get(Mask));
}

#if (GCC_MINOR > 6)
Value *TreeToLLVM::EmitReg_VEC_PERM_EXPR(tree op0, tree op1, tree op2) {
  unsigned Length = (unsigned)TYPE_VECTOR_SUBPARTS(TREE_TYPE(op0));

  // The vectors to shuffle.
  Value *V0 = EmitRegister(op0);
  Value *V1 = EmitRegister(op1);

  // The shuffle mask.
  Value *Mask = EmitRegister(op2);

  // The GCC semantics are that mask indices off the end are wrapped back into
  // range, so reduce the mask modulo 2*Length.
  assert(!(Length & (Length - 1)) && "Vector length not a power of two!");
  Mask = Builder.CreateAnd(Mask, ConstantInt::get(Mask->getType(), 2*Length-1));

  // Convert to a vector of i32, as required by the shufflevector instruction.
  Type *MaskTy = VectorType::get(Builder.getInt32Ty(), Length);
  tree mask_elt_type = TREE_TYPE(TREE_TYPE(op2));
  Value *Mask32 = Builder.CreateIntCast(Mask, MaskTy,
                                        !TYPE_UNSIGNED(mask_elt_type));

  // Use a shufflevector instruction if this directly corresponds to one, i.e.
  // if the mask is a vector of constant integers or undef.
  if (ShuffleVectorInst::isValidOperands(V0, V1, Mask32))
    return Builder.CreateShuffleVector(V0, V1, Mask32);

  // Store the vectors to successive memory locations in a temporary.
  tree elt_type = TREE_TYPE(TREE_TYPE(op0));
  Type *EltTy = ConvertType(elt_type);
  unsigned Align = DL.getABITypeAlignment(EltTy);
  // The temporary is a struct containing the pair of input vectors.
  Type *TmpTy = StructType::get(ConvertType(TREE_TYPE(op0)),
                                ConvertType(TREE_TYPE(op1)), NULL);
  AllocaInst *Tmp = CreateTemporary(TmpTy, Align);
  // Store the first vector to the first element of the pair.
  Value *Tmp0 = Builder.CreateStructGEP(Tmp, 0, flag_verbose_asm ?
                                        "vp1s" : "");
  StoreRegisterToMemory(V0, MemRef(Tmp0, Align, /*Volatile*/false),
                        TREE_TYPE(op0), 0, Builder);
  // Store the second vector to the second element of the pair.
  Value *Tmp1 = Builder.CreateStructGEP(Tmp, 1, flag_verbose_asm ?
                                        "vp2s" : "");
  StoreRegisterToMemory(V1, MemRef(Tmp1, Align, /*Volatile*/false),
                        TREE_TYPE(op1), 0, Builder);

  // Load out the components according to the mask.
  Value *Result = UndefValue::get(V0->getType());
  Value *BaseAddr = Builder.CreateBitCast(Tmp, EltTy->getPointerTo());
  for (unsigned i = 0; i != Length; ++i) {
    // Extract from the mask the index of the element to load.
    Value *MaskIdx = Builder.getInt32(i);
    Value *Idx = Builder.CreateExtractElement(Mask, MaskIdx);
    // Advance that many elements from the start of the temporary and load it.
    Value *Ptr = Builder.CreateInBoundsGEP(BaseAddr, Idx, flag_verbose_asm ?
                                           "vpl" : "");
    Value *Elt = LoadRegisterFromMemory(MemRef(Ptr, Align, false), elt_type, 0,
                                        Builder);
    // Insert it into the result.
    Result = Builder.CreateInsertElement(Result, Elt, MaskIdx);
  }
  return Result;
}
#endif

Value *TreeToLLVM::EmitReg_VecUnpackHiExpr(tree type, tree op0) {
  // Eg: <2 x double> = VEC_UNPACK_HI_EXPR(<4 x float>)
  Value *Op = EmitRegister(op0);

  // Extract the high elements, eg: <4 x float> -> <2 x float>.
  Op = VectorHighElements(Op);

  // Extend the input elements to the output element type, eg: <2 x float>
  // -> <2 x double>.
  Type *DestTy = getRegType(type);
  return CastToAnyType(Op, !TYPE_UNSIGNED(TREE_TYPE(TREE_TYPE(op0))), DestTy,
                       !TYPE_UNSIGNED(TREE_TYPE(type)));
}

Value *TreeToLLVM::EmitReg_VecUnpackLoExpr(tree type, tree op0) {
  // Eg: <2 x double> = VEC_UNPACK_LO_EXPR(<4 x float>)
  Value *Op = EmitRegister(op0);

  // Extract the low elements, eg: <4 x float> -> <2 x float>.
  Op = VectorLowElements(Op);

  // Extend the input elements to the output element type, eg: <2 x float>
  // -> <2 x double>.
  Type *DestTy = getRegType(type);
  return CastToAnyType(Op, !TYPE_UNSIGNED(TREE_TYPE(TREE_TYPE(op0))), DestTy,
                       !TYPE_UNSIGNED(TREE_TYPE(type)));
}

Value *TreeToLLVM::EmitReg_VEC_WIDEN_MULT_HI_EXPR(tree type, tree op0,
                                                  tree op1) {
  Value *Hi0 = EmitReg_VecUnpackHiExpr(type, op0);
  Value *Hi1 = EmitReg_VecUnpackHiExpr(type, op1);
  return Builder.CreateMul(Hi0, Hi1);
}

Value *TreeToLLVM::EmitReg_VEC_WIDEN_MULT_LO_EXPR(tree type, tree op0,
                                                  tree op1) {
  Value *Lo0 = EmitReg_VecUnpackLoExpr(type, op0);
  Value *Lo1 = EmitReg_VecUnpackLoExpr(type, op1);
  return Builder.CreateMul(Lo0, Lo1);
}

Value *TreeToLLVM::EmitReg_WIDEN_MULT_EXPR(tree type, tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  Type *DestTy = getRegType(type);
  LHS = CastToAnyType(LHS, !TYPE_UNSIGNED(TREE_TYPE(op0)), DestTy,
                      !TYPE_UNSIGNED(type));
  RHS = CastToAnyType(RHS, !TYPE_UNSIGNED(TREE_TYPE(op0)), DestTy,
                      !TYPE_UNSIGNED(type));
  return Builder.CreateMul(LHS, RHS);
}


//===----------------------------------------------------------------------===//
//                        ... Exception Handling ...
//===----------------------------------------------------------------------===//



//===----------------------------------------------------------------------===//
//                  ... Render* - Convert GIMPLE to LLVM ...
//===----------------------------------------------------------------------===//

void TreeToLLVM::RenderGIMPLE_ASM(gimple stmt) {
  // A gimple asm statement consists of an asm string, a list of outputs, a list
  // of inputs, a list of clobbers, a list of labels and a "volatile" flag.
  // These correspond directly to the elements of an asm statement.  For example
  //   asm ("combine %2,%0" : "=r" (x) : "0" (x), "g" (y));
  // Here the asm string is "combine %2,%0" and can be obtained as a const char*
  // by calling gimple_asm_string.  The only output is "=r" (x).  The number of
  // outputs is given by gimple_asm_noutputs, 1 in this case, and the outputs
  // themselves can be obtained by calling gimple_asm_output_op.  This returns a
  // TREE_LIST node with an SSA name for "x" as the TREE_VALUE; the TREE_PURPOSE
  // is also a TREE_LIST with TREE_VALUE a string constant holding "=r".  There
  // are two inputs, "0" (x) and "g" (y), so gimple_asm_ninputs returns 2.  The
  // routine gimple_asm_input_op returns them in the same format as for outputs.
  // The number of clobbers is returned by gimple_asm_nclobbers, 0 in this case.
  // To get the clobbers use gimple_asm_clobber_op.  This returns a TREE_LIST
  // node with TREE_VALUE a string constant holding the clobber.  To find out if
  // the asm is volatile call gimple_asm_volatile_p, which returns true if so.
  // See below for labels (this example does not have any).

  // Note that symbolic names have been substituted before getting here.  For
  // example this
  //   asm ("cmoveq %1,%2,%[result]" : [result] "=r"(result)
  //        : "r"(test), "r"(new), "[result]"(old));
  // turns up as
  //   asm ("cmoveq %1,%2,%0" : "=r"(result) : "r"(test), "r"(new), "0"(old));

  // Note that clobbers may not turn up in the same order as in the original, eg
  //   asm volatile ("movc3 %0,%1,%2" : /* no outputs */
  //                 : "g" (from), "g" (to), "g" (count)
  //                 : "r0", "r1", "r2", "r3", "r4", "r5");
  // The clobbers turn up as "r5", "r4", "r3", "r2", "r1", "r0".

  // Here is an example of the "asm goto" construct (not yet supported by LLVM):
  //   int frob(int x) {
  //     int y;
  //     asm goto ("frob %%r5, %1; jc %l[error]; mov (%2), %%r5"
  //               : : "r"(x), "r"(&y) : "r5", "memory" : error);
  //     return y;
  //   error:
  //     return -1;
  //   }
  // The number of labels, one in this case, is returned by gimple_asm_nlabels.
  // The labels themselves are returned by gimple_asm_label_op as a TREE_LIST
  // node with TREE_PURPOSE a string constant holding the label name ("error")
  // and TREE_VALUE holding the appropriate LABEL_DECL.

  // TODO: Add support for labels.
  if (gimple_asm_nlabels(stmt) > 0) {
    sorry("'asm goto' not supported");
    return;
  }

  const unsigned NumOutputs = gimple_asm_noutputs (stmt);
  const unsigned NumInputs = gimple_asm_ninputs(stmt);
  const unsigned NumClobbers = gimple_asm_nclobbers (stmt);

  /// Constraints - The output/input constraints, concatenated together in array
  /// form instead of list form.  This way of doing things is forced on us by
  /// GCC routines like parse_output_constraint which rummage around inside the
  /// array.
  const char **Constraints =
    (const char **)alloca((NumOutputs + NumInputs) * sizeof(const char *));

  // Initialize the Constraints array.
  for (unsigned i = 0; i != NumOutputs; ++i) {
    tree Output = gimple_asm_output_op(stmt, i);
    // If there's an erroneous arg then bail out.
    if (TREE_TYPE(TREE_VALUE(Output)) == error_mark_node) return;
    // Record the output constraint.
    const char *Constraint =
      TREE_STRING_POINTER(TREE_VALUE(TREE_PURPOSE(Output)));
    Constraints[i] = Constraint;
  }
  for (unsigned i = 0; i != NumInputs; ++i) {
    tree Input = gimple_asm_input_op(stmt, i);
    // If there's an erroneous arg then bail out.
    if (TREE_TYPE(TREE_VALUE(Input)) == error_mark_node) return;
    // Record the input constraint.
    const char *Constraint =
      TREE_STRING_POINTER(TREE_VALUE(TREE_PURPOSE(Input)));
    Constraints[NumOutputs+i] = Constraint;
  }

  // Look for multiple alternative constraints: multiple alternatives separated
  // by commas.
  unsigned NumChoices = 0;    // sentinal; real value is always at least 1.
  for (unsigned i = 0; i != NumInputs; ++i) {
    tree Input = gimple_asm_input_op(stmt, i);
    unsigned NumInputChoices = 1;
    for (const char *p = TREE_STRING_POINTER(TREE_VALUE(TREE_PURPOSE(Input)));
         *p; ++p)
      if (*p == ',')
        ++NumInputChoices;
    if (NumChoices && (NumInputChoices != NumChoices)) {
      error("operand constraints for %<asm%> differ in number of alternatives");
      return;
    }
    if (NumChoices == 0)
      NumChoices = NumInputChoices;
  }
  for (unsigned i = 0; i != NumOutputs; ++i) {
    tree Output = gimple_asm_output_op(stmt, i);
    unsigned NumOutputChoices = 1;
    for (const char *p = TREE_STRING_POINTER(TREE_VALUE(TREE_PURPOSE(Output)));
         *p; ++p)
      if (*p == ',')
        ++NumOutputChoices;
    if (NumChoices && (NumOutputChoices != NumChoices)) {
      error("operand constraints for %<asm%> differ in number of alternatives");
      return;
    }
    if (NumChoices == 0)
      NumChoices = NumOutputChoices;
  }

  // If there are multiple constraint tuples, pick one.  Constraints is
  // altered to point to shorter strings (which are malloc'ed), and everything
  // below Just Works as in the NumChoices==1 case.
  BumpPtrAllocator StringStorage(256, 256);
  if (NumChoices > 1)
    ChooseConstraintTuple(stmt, Constraints, NumChoices, StringStorage);

  // HasSideEffects - Whether the LLVM inline asm should be marked as having
  // side effects.
  bool HasSideEffects = gimple_asm_volatile_p(stmt) || (NumOutputs == 0);

  // CallResultTypes - The inline asm call may return one or more results.  The
  // types of the results are recorded here along with a flag indicating whether
  // the corresponding GCC type is signed.
  SmallVector<std::pair<Type *, bool>, 4> CallResultTypes;

  // CallResultDests - Each result returned by the inline asm call is stored in
  // a memory location.  These are listed here along with a flag indicating if
  // the GCC type corresponding to the memory location is signed.  The type of
  // the memory location is allowed to differ from the type of the call result,
  // in which case the result is converted before being stored.
  SmallVector<std::pair<Value *, bool>, 4> CallResultDests;

  // CallOps - The operands pass to the inline asm call.
  SmallVector<Value*, 16> CallOps;

  // OutputLocations - For each output holds an index into CallOps (if the flag
  // is false) or into CallResultTypes (if the flag is true).  Outputs returned
  // in memory are passed to the asm as an operand and thus appear in CallOps.
  // Those returned in registers are obtained as one of the results of the asm
  // call and thus correspond to an entry in CallResultTypes.
  SmallVector<std::pair<bool, unsigned>, 4> OutputLocations;

  // SSADefinitions - If the asm defines an SSA name then the SSA name and a
  // memory location are recorded here.  The asm result defining the SSA name
  // will be stored to the memory memory location, and loaded out afterwards
  // to define the SSA name.
  SmallVector<std::pair<tree, MemRef>, 4> SSADefinitions;

  // ConstraintStr - The string of constraints in LLVM format.
  std::string ConstraintStr;

  // Process outputs.
  for (unsigned i = 0; i != NumOutputs; ++i) {
    tree Output = gimple_asm_output_op(stmt, i);
    tree Operand = TREE_VALUE(Output);

    // Parse the output constraint.
    const char *Constraint = Constraints[i];
    bool IsInOut, AllowsReg, AllowsMem;
    if (!parse_output_constraint(&Constraint, i, NumInputs, NumOutputs,
                                 &AllowsMem, &AllowsReg, &IsInOut))
      return;
    assert(Constraint[0] == '=' && "Not an output constraint?");
    assert(!IsInOut && "asm expression not gimplified?");

    std::string SimplifiedConstraint;
    // If this output register is pinned to a machine register, use that machine
    // register instead of the specified constraint.
    if (isa<VAR_DECL>(Operand) && DECL_HARD_REGISTER(Operand)) {
      const char* RegName = extractRegisterName(Operand);
      int RegNum = decode_reg_name(RegName);
      if (RegNum >= 0) {
        RegName = LLVM_GET_REG_NAME(RegName, RegNum);
        size_t RegNameLen = strlen(RegName);
        char *NewConstraint = (char*)alloca(RegNameLen+3);
        NewConstraint[0] = '{';
        memcpy(NewConstraint+1, RegName, RegNameLen);
        NewConstraint[RegNameLen+1] = '}';
        NewConstraint[RegNameLen+2] = 0;
        SimplifiedConstraint = NewConstraint;
        // This output will now be implicit; set the sideffect flag on the asm.
        HasSideEffects = true;
        // We should no longer consider mem constraints.
        AllowsMem = false;
      } else {
        // If we can simplify the constraint into something else, do so now.
        // This avoids LLVM having to know about all the (redundant) GCC
        // constraints.
        SimplifiedConstraint = CanonicalizeConstraint(Constraint+1);
      }
    } else {
      SimplifiedConstraint = CanonicalizeConstraint(Constraint+1);
    }

    LValue Dest;
    Type *DestValTy = ConvertType(TREE_TYPE(Operand));
    if (isa<SSA_NAME>(Operand)) {
      // The ASM is defining an ssa name.  Store the output to a temporary, then
      // load it out again later as the ssa name.
      MemRef TmpLoc = CreateTempLoc(DestValTy);
      SSADefinitions.push_back(std::make_pair(Operand, TmpLoc));
      Dest = LValue(TmpLoc);
    } else {
      Dest = EmitLV(Operand);
      assert(cast<PointerType>(Dest.Ptr->getType())->getElementType() ==
             DestValTy && "LValue has wrong type!");
    }

    assert(!Dest.isBitfield() && "Cannot assign into a bitfield!");
    if (!AllowsMem && DestValTy->isSingleValueType()) {// Reg dest -> asm return
      ConstraintStr += ",=";
      ConstraintStr += SimplifiedConstraint;
      bool IsSigned = !TYPE_UNSIGNED(TREE_TYPE(Operand));
      CallResultDests.push_back(std::make_pair(Dest.Ptr, IsSigned));
      CallResultTypes.push_back(std::make_pair(DestValTy, IsSigned));
      OutputLocations.push_back(std::make_pair(true, CallResultTypes.size()-1));
    } else {
      ConstraintStr += ",=*";
      ConstraintStr += SimplifiedConstraint;
      CallOps.push_back(Dest.Ptr);
      OutputLocations.push_back(std::make_pair(false, CallOps.size()-1));
    }
  }

  // Process inputs.
  for (unsigned i = 0; i != NumInputs; ++i) {
    tree Input = gimple_asm_input_op(stmt, i);
    tree Val = TREE_VALUE(Input);
    tree type = TREE_TYPE(Val);
    bool IsSigned = !TYPE_UNSIGNED(type);

    const char *Constraint = Constraints[NumOutputs+i];

    bool AllowsReg, AllowsMem;
    if (!parse_input_constraint(Constraints+NumOutputs+i, i,
                                NumInputs, NumOutputs, 0,
                                Constraints, &AllowsMem, &AllowsReg))
      return;
    bool isIndirect = false;
    if (AllowsReg || !AllowsMem) {    // Register operand.
      Type *LLVMTy = ConvertType(type);

      Value *Op = 0;
      Type *OpTy = LLVMTy;
      if (LLVMTy->isSingleValueType()) {
        if (isa<ADDR_EXPR>(Val) && isa<LABEL_DECL>(TREE_OPERAND(Val,0))) {
          // Emit the label, but do not assume it is going to be the target
          // of an indirect branch.  Having this logic here is a hack; there
          // should be a bit in the label identifying it as in an asm.
          Op = getLabelDeclBlock(TREE_OPERAND(Val, 0));
        } else if (isa<VAR_DECL>(Val) && DECL_HARD_REGISTER(Val)) {
          // GCC special cases hard registers used as inputs to asm statements.
          // Emit an inline asm node that copies the value out of the specified
          // register.
          assert(canEmitRegisterVariable(Val) && "Cannot read hard register!");
          Op = EmitReadOfRegisterVariable(Val);
        } else {
          Op = EmitMemory(Val);
        }
      } else {
        LValue LV = EmitLV(Val);
        assert(!LV.isBitfield() && "Inline asm can't have bitfield operand");

        // Small structs and unions can be treated as integers.
        uint64_t TySize = DL.getTypeSizeInBits(LLVMTy);
        if (TySize == 1 || TySize == 8 || TySize == 16 ||
            TySize == 32 || TySize == 64 || (TySize == 128 && !AllowsMem)) {
          LLVMTy = IntegerType::get(Context, (unsigned)TySize);
          Op =
            Builder.CreateLoad(Builder.CreateBitCast(LV.Ptr,
                                                     LLVMTy->getPointerTo()));
        } else {
          // Codegen only supports indirect operands with mem constraints.
          if (!AllowsMem)
            error("aggregate does not match inline asm register constraint");
          // Otherwise, emit our value as a lvalue.
          isIndirect = true;
          Op = LV.Ptr;
          OpTy = Op->getType();
        }
      }

      // If this input operand is matching an output operand, e.g. '0', check if
      // this is something that llvm supports. If the operand types are
      // different, then emit an error if 1) one of the types is not integer or
      // pointer, 2) if size of input type is larger than the output type. If
      // the size of the integer input size is smaller than the integer output
      // type, then cast it to the larger type and shift the value if the target
      // is big endian.
      if (ISDIGIT(Constraint[0])) {
        unsigned Match = (unsigned)atoi(Constraint); // Unsigned - no minus sign
        // This output might have gotten put in either CallResult or CallArg
        // depending whether it's a register or not.  Find its type.
        Type *OTy = 0;
        unsigned OutputIndex = ~0U;
        if (Match < OutputLocations.size()) {
          // Indices here known to be within range.
          OutputIndex = OutputLocations[Match].second;
          if (OutputLocations[Match].first)
            OTy = CallResultTypes[OutputIndex].first;
          else {
            OTy = CallOps[OutputIndex]->getType();
            assert(OTy->isPointerTy() && "Expected pointer type!");
            OTy = cast<PointerType>(OTy)->getElementType();
          }
        }
        if (OTy && OTy != OpTy) {
          if (!OTy->isSingleValueType() || !OpTy->isSingleValueType()) {
            error("unsupported inline asm: input constraint with a matching "
                  "output constraint of incompatible type!");
            return;
          }
          uint64_t OTyBits = DL.getTypeSizeInBits(OTy);
          uint64_t OpTyBits = DL.getTypeSizeInBits(OpTy);
          if (OTyBits == 0 || OpTyBits == 0) {
            error("unsupported inline asm: input constraint with a matching "
                  "output constraint of incompatible type!");
            return;
          } else if (OTyBits < OpTyBits) {
            // The output is smaller than the input.
            if (OutputLocations[Match].first &&
                !isOperandMentioned(stmt, Match)) {
              // The output is a register and is not explicitly mentioned in the
              // asm string.  Use the input type for the output, and arrange for
              // the result to be truncated to the original output type after
              // the asm call.
              CallResultTypes[OutputIndex] = std::make_pair(OpTy, IsSigned);
            } else if (isa<Constant>(Op) &&
                       !isOperandMentioned(stmt, NumOutputs+i)) {
              // The input is a constant that is not explicitly mentioned in the
              // asm string.  Convert to the output type like in an assignment.
              Op = CastToAnyType(Op, IsSigned, OTy,
                                 CallResultTypes[OutputIndex].second);
            } else {
              error("unsupported inline asm: input constraint with a matching "
                    "output constraint of incompatible type!");
              return;
            }
          } else if (OTyBits > OpTyBits) {
            // The input is smaller than the output.  If the input is explicitly
            // mentioned in the asm string then we cannot safely promote it, so
            // bail out.
            if (isOperandMentioned(stmt, NumOutputs + i)) {
              error("unsupported inline asm: input constraint with a matching "
                    "output constraint of incompatible type!");
              return;
            }
            Op = CastToAnyType(Op, IsSigned, OTy,
                               CallResultTypes[OutputIndex].second);
          }
        }
      }

      CallOps.push_back(Op);
    } else {                          // Memory operand.
      mark_addressable(TREE_VALUE(Input));
      isIndirect = true;
      LValue Src = EmitLV(Val);
      assert(!Src.isBitfield() && "Cannot read from a bitfield!");
      CallOps.push_back(Src.Ptr);
    }

    ConstraintStr += ',';
    if (isIndirect)
      ConstraintStr += '*';

    // If this input register is pinned to a machine register, use that machine
    // register instead of the specified constraint.
    if (isa<VAR_DECL>(Val) && DECL_HARD_REGISTER(Val)) {
      const char *RegName = extractRegisterName(Val);
      int RegNum = decode_reg_name(RegName);
      if (RegNum >= 0) {
        RegName = LLVM_GET_REG_NAME(RegName, RegNum);
        ConstraintStr += '{';
        ConstraintStr += RegName;
        ConstraintStr += '}';
        continue;
      }
    }

    // If there is a simpler form for the register constraint, use it.
    std::string Simplified = CanonicalizeConstraint(Constraint);
    ConstraintStr += Simplified;
  }

  // Process clobbers.

  // Some targets automatically clobber registers across an asm.
  tree Clobbers;
  {
    // Create input, output & clobber lists for the benefit of md_asm_clobbers.
    tree outputs = NULL_TREE;
    if (NumOutputs) {
      tree t = outputs = gimple_asm_output_op (stmt, 0);
      for (unsigned i = 1; i < NumOutputs; i++) {
        TREE_CHAIN (t) = gimple_asm_output_op (stmt, i);
        t = gimple_asm_output_op (stmt, i);
      }
    }

    tree inputs = NULL_TREE;
    if (NumInputs) {
      tree t = inputs = gimple_asm_input_op (stmt, 0);
      for (unsigned i = 1; i < NumInputs; i++) {
        TREE_CHAIN (t) = gimple_asm_input_op (stmt, i);
        t = gimple_asm_input_op (stmt, i);
      }
    }

    tree clobbers = NULL_TREE;
    if (NumClobbers) {
      tree t = clobbers = gimple_asm_clobber_op (stmt, 0);
      for (unsigned i = 1; i < NumClobbers; i++) {
        TREE_CHAIN (t) = gimple_asm_clobber_op (stmt, i);
        t = gimple_asm_clobber_op (stmt, i);
      }
    }

    Clobbers = targetm.md_asm_clobbers(outputs, inputs, clobbers);
  }

  for (; Clobbers; Clobbers = TREE_CHAIN(Clobbers)) {
    const char *RegName = TREE_STRING_POINTER(TREE_VALUE(Clobbers));
    int RegCode = decode_reg_name(RegName);

    switch (RegCode) {
    case -1:     // Nothing specified?
    case -2:     // Invalid.
      error("unknown register name %qs in %<asm%>", RegName);
      return;
    case -3:     // cc
      ConstraintStr += ",~{cc}";
      break;
    case -4:     // memory
      ConstraintStr += ",~{memory}";
      break;
    default:     // Normal register name.
      assert(RegName && "Null register name successfully decoded!");
      RegName = LLVM_GET_REG_NAME(RegName, RegCode);
      ConstraintStr += ",~{";
      ConstraintStr += RegName;
      ConstraintStr += "}";
      break;
    }
  }

  // Compute the return type to use for the asm call.
  Type *CallResultType;
  switch (CallResultTypes.size()) {
  // If there are no results then the return type is void!
  case 0: CallResultType = Type::getVoidTy(Context); break;
  // If there is one result then use the result's type as the return type.
  case 1: CallResultType = CallResultTypes[0].first; break;
  // If the asm returns multiple results then create a struct type with the
  // result types as its fields, and use it for the return type.
  default:
    SmallVector<Type*, 4> Fields((unsigned)CallResultTypes.size());
    for (unsigned i = 0, e = (unsigned)CallResultTypes.size(); i != e; ++i)
      Fields[i] = CallResultTypes[i].first;
    CallResultType = StructType::get(Context, Fields);
    break;
  }

  // Compute the types of the arguments to the asm call.
  SmallVector<Type*, 16> CallArgTypes((unsigned)CallOps.size());
  for (unsigned i = 0, e = (unsigned)CallOps.size(); i != e; ++i)
    CallArgTypes[i] = CallOps[i]->getType();

  // Get the type of the called asm "function".
  FunctionType *FTy =
    FunctionType::get(CallResultType, CallArgTypes, false);

  // Remove the leading comma if we have operands.
  if (!ConstraintStr.empty())
    ConstraintStr.erase(ConstraintStr.begin());

  // Make sure we're created a valid inline asm expression.
  if (!InlineAsm::Verify(FTy, ConstraintStr)) {
    error("Invalid or unsupported inline assembly!");
    return;
  }

  std::string NewAsmStr = ConvertInlineAsmStr(stmt, NumOutputs+NumInputs);
  Value *Asm = InlineAsm::get(FTy, NewAsmStr, ConstraintStr, HasSideEffects);
  CallInst *CV = Builder.CreateCall(Asm, CallOps,
                                    CallResultTypes.empty() ? "" : "asmtmp");
  CV->setDoesNotThrow();
  if (gimple_has_location(stmt)) {
    // Pass the location of the asm using a !srcloc metadata.
    Constant *LocationCookie = Builder.getInt64(gimple_location(stmt));
    CV->setMetadata("srcloc", MDNode::get(Context, LocationCookie));
  }

  // If the call produces a value, store it into the destination.
  for (unsigned i = 0, NumResults = (unsigned)CallResultTypes.size();
       i != NumResults; ++i) {
    Value *Val = NumResults == 1 ?
      CV : Builder.CreateExtractValue(CV, i, "asmresult");
    bool ValIsSigned = CallResultTypes[i].second;

    Value *Dest = CallResultDests[i].first;
    Type *DestTy = cast<PointerType>(Dest->getType())->getElementType();
    bool DestIsSigned = CallResultDests[i].second;
    Val = CastToAnyType(Val, ValIsSigned, DestTy, DestIsSigned);
    Builder.CreateStore(Val, Dest);
  }

  // If the call defined any ssa names, associate them with their value.
  for (unsigned i = 0, e = (unsigned)SSADefinitions.size(); i != e; ++i) {
    tree Name = SSADefinitions[i].first;
    MemRef Loc = SSADefinitions[i].second;
    Value *Val = LoadRegisterFromMemory(Loc, TREE_TYPE(Name), 0, Builder);
    DefineSSAName(Name, Val);
  }

  // Give the backend a chance to upgrade the inline asm to LLVM code.  This
  // handles some common cases that LLVM has intrinsics for, e.g. x86 bswap ->
  // llvm.bswap.
  if (const TargetLowering *TLI = TheTarget->getTargetLowering())
    TLI->ExpandInlineAsm(CV);
}

void TreeToLLVM::RenderGIMPLE_ASSIGN(gimple stmt) {
  tree lhs = gimple_assign_lhs(stmt);

#if (GCC_MINOR > 6)
  // Assigning a right-hand side with TREE_CLOBBER_P says that the left-hand
  // side is dead from this point on.  Output an llvm.lifetime.end intrinsic.
  if (get_gimple_rhs_class(gimple_expr_code(stmt)) == GIMPLE_SINGLE_RHS &&
      TREE_CLOBBER_P(gimple_assign_rhs1(stmt))) {
    // Be conservative and only output the intrinsic if the left-hand side
    // corresponds to some kind of concrete object.  Note that we generate
    // code to read from RESULT_DECLs before returning from the function, so
    // saying that a RESULT_DECL is dead means we are dead - which is why we
    // don't even consider it.
    if (isa<PARM_DECL>(lhs) || isa<VAR_DECL>(lhs)) {
      Value *LHSAddr = Builder.CreateBitCast(DECL_LOCAL(lhs),
                                             Builder.getInt8PtrTy());
      uint64_t LHSSize = isInt64(DECL_SIZE(lhs), true) ?
        getInt64(DECL_SIZE(lhs), true) / 8 : ~0UL;
      Function *EndIntr = Intrinsic::getDeclaration(TheModule,
                                                    Intrinsic::lifetime_end);
      Builder.CreateCall2(EndIntr, Builder.getInt64(LHSSize), LHSAddr);
    }
    return;
  }
#endif

  if (isa<AGGREGATE_TYPE>(TREE_TYPE(lhs))) {
    assert(get_gimple_rhs_class(gimple_expr_code(stmt)) == GIMPLE_SINGLE_RHS &&
           "Aggregate type but rhs not simple!");
    LValue LV = EmitLV(lhs);
    MemRef NewLoc(LV.Ptr, LV.getAlignment(), TREE_THIS_VOLATILE(lhs));
    EmitAggregate(gimple_assign_rhs1 (stmt), NewLoc);
    return;
  }
  WriteScalarToLHS(lhs, EmitAssignRHS(stmt));
}

void TreeToLLVM::RenderGIMPLE_CALL(gimple stmt) {
  tree lhs = gimple_call_lhs(stmt);
  if (!lhs) {
    // The returned value is not used.
    if (!isa<AGGREGATE_TYPE>(gimple_call_return_type(stmt))) {
      OutputCallRHS(stmt, 0);
      return;
    }
    // Create a temporary to hold the returned value.
    // TODO: Figure out how to avoid creating this temporary and the
    // associated useless code that stores the returned value into it.
    MemRef Loc = CreateTempLoc(ConvertType(gimple_call_return_type(stmt)));
    OutputCallRHS(stmt, &Loc);
    return;
  }

  if (isa<AGGREGATE_TYPE>(TREE_TYPE(lhs))) {
    LValue LV = EmitLV(lhs);
    MemRef NewLoc(LV.Ptr, LV.getAlignment(), TREE_THIS_VOLATILE(lhs));
    OutputCallRHS(stmt, &NewLoc);
    return;
  }
  WriteScalarToLHS(lhs, OutputCallRHS(stmt, 0));
}

void TreeToLLVM::RenderGIMPLE_COND(gimple stmt) {
  // Emit the comparison.
  Value *Cond = EmitCompare(gimple_cond_lhs(stmt), gimple_cond_rhs(stmt),
                            gimple_cond_code(stmt));

  // Extract the target basic blocks.
  edge true_edge, false_edge;
  extract_true_false_edges_from_block(gimple_bb(stmt), &true_edge, &false_edge);
  BasicBlock *IfTrue = getBasicBlock(true_edge->dest);
  BasicBlock *IfFalse = getBasicBlock(false_edge->dest);

  // Branch based on the condition.
  Builder.CreateCondBr(Cond, IfTrue, IfFalse);
}

void TreeToLLVM::RenderGIMPLE_EH_DISPATCH(gimple stmt) {
  int RegionNo = gimple_eh_dispatch_region(stmt);
  eh_region region = get_eh_region_from_number(RegionNo);

  switch (region->type) {
  default:
    llvm_unreachable("Unexpected region type!");
  case ERT_ALLOWED_EXCEPTIONS: {
    // Filter.
    BasicBlock *Dest = getLabelDeclBlock(region->u.allowed.label);

    if (!region->u.allowed.type_list) {
      // Not allowed to throw.  Branch directly to the post landing pad.
      Builder.CreateBr(Dest);
      BeginBlock(BasicBlock::Create(Context));
      break;
    }

    // The result of a filter selection will be a negative index if there is a
    // match.
    // FIXME: It looks like you have to compare against a specific value,
    // checking for any old negative number is not enough!  This should not
    // matter if the failure code branched to on a filter match is always the
    // same (as in C++), but might cause problems with other languages.
    Value *Filter = Builder.CreateLoad(getExceptionFilter(RegionNo));

    // Compare with the filter action value.
    Value *Zero = ConstantInt::get(Filter->getType(), 0);
    Value *Compare = Builder.CreateICmpSLT(Filter, Zero);

    // Branch on the compare.
    BasicBlock *NoMatchBB = BasicBlock::Create(Context);
    Builder.CreateCondBr(Compare, Dest, NoMatchBB);
    BeginBlock(NoMatchBB);
    break;
  }
  case ERT_TRY:
    // Catches.
    Value *Filter = NULL;
    SmallSet<Value *, 8> AlreadyCaught; // Typeinfos known caught.
    Function *TypeIDIntr = Intrinsic::getDeclaration(TheModule,
                                                     Intrinsic::eh_typeid_for);
    for (eh_catch c = region->u.eh_try.first_catch; c ; c = c->next_catch) {
      BasicBlock *Dest = getLabelDeclBlock(c->label);
      if (!c->type_list) {
        // Catch-all.  Branch directly to the post landing pad.
        Builder.CreateBr(Dest);
        break;
      }

      Value *Cond = NULL;
      for (tree type = c->type_list; type; type = TREE_CHAIN (type)) {
        Value *TypeInfo = ConvertTypeInfo(TREE_VALUE(type));
        // No point in trying to catch a typeinfo that was already caught.
        if (!AlreadyCaught.insert(TypeInfo))
          continue;

        TypeInfo = Builder.CreateBitCast(TypeInfo, Builder.getInt8PtrTy());

        // Call get eh type id.
        Value *TypeID = Builder.CreateCall(TypeIDIntr, TypeInfo, "typeid");

        if (!Filter)
          Filter = Builder.CreateLoad(getExceptionFilter(RegionNo));

        // Compare with the exception selector.
        Value *Compare = Builder.CreateICmpEQ(Filter, TypeID);

        Cond = Cond ? Builder.CreateOr(Cond, Compare) : Compare;
      }

      if (Cond) {
        BasicBlock *NoMatchBB = BasicBlock::Create(Context);
        Builder.CreateCondBr(Cond, Dest, NoMatchBB);
        BeginBlock(NoMatchBB);
      }
    }
    break;
  }
}

void TreeToLLVM::RenderGIMPLE_GOTO(gimple stmt) {
  tree dest = gimple_goto_dest(stmt);

  if (isa<LABEL_DECL>(dest)) {
    // Direct branch.
    Builder.CreateBr(getLabelDeclBlock(dest));
    return;
  }

  // Indirect branch.
  basic_block source = gimple_bb(stmt);
  IndirectBrInst *Br = Builder.CreateIndirectBr(EmitRegister(dest),
                                                EDGE_COUNT(source->succs));

  // Add the list of possible destinations.
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, source->succs)
    Br->addDestination(getBasicBlock(e->dest));
}

void TreeToLLVM::RenderGIMPLE_RESX(gimple stmt) {
  // Reraise an exception.  If this statement is inside an exception handling
  // region then the reraised exception may be caught by the current function,
  // in which case it can be simplified into a branch.
  int DstLPadNo = lookup_stmt_eh_lp(stmt);
  eh_region dst_rgn =
    DstLPadNo ? get_eh_region_from_lp_number(DstLPadNo) : NULL;
  eh_region src_rgn = get_eh_region_from_number(gimple_resx_region(stmt));

  if (!src_rgn) {
    // Unreachable block?
    Builder.CreateUnreachable();
    return;
  }

  if (dst_rgn) {
    if (DstLPadNo < 0) {
      // The reraise is inside a must-not-throw region.  Turn the reraise into a
      // call to the failure routine (eg: std::terminate).
      assert(dst_rgn->type == ERT_MUST_NOT_THROW && "Unexpected region type!");

      // Branch to the block containing the failure code.
      Builder.CreateBr(getFailureBlock(dst_rgn->index));
      return;
    }

    // Use the exception pointer and filter value for the source region as the
    // values for the destination region.
    Value *ExcPtr = Builder.CreateLoad(getExceptionPtr(src_rgn->index));
    Builder.CreateStore(ExcPtr, getExceptionPtr(dst_rgn->index));
    Value *Filter = Builder.CreateLoad(getExceptionFilter(src_rgn->index));
    Builder.CreateStore(Filter, getExceptionFilter(dst_rgn->index));

    // Branch to the post landing pad for the destination region.
    eh_landing_pad lp = get_eh_landing_pad_from_number(DstLPadNo);
    assert(lp && "Post landing pad not found!");
    Builder.CreateBr(getLabelDeclBlock(lp->post_landing_pad));
    return;
  }

  // Unwind the exception out of the function using a resume instruction.
  Value *ExcPtr = Builder.CreateLoad(getExceptionPtr(src_rgn->index));
  Value *Filter = Builder.CreateLoad(getExceptionFilter(src_rgn->index));
  Type *UnwindDataTy = StructType::get(Builder.getInt8PtrTy(),
                                       Builder.getInt32Ty(), NULL);
  Value *UnwindData = UndefValue::get(UnwindDataTy);
  UnwindData = Builder.CreateInsertValue(UnwindData, ExcPtr, 0, "exc_ptr");
  UnwindData = Builder.CreateInsertValue(UnwindData, Filter, 1, "filter");
  Builder.CreateResume(UnwindData);
}

void TreeToLLVM::RenderGIMPLE_RETURN(gimple stmt) {
  tree retval = gimple_return_retval(stmt);
  tree result = DECL_RESULT(current_function_decl);

  if (retval && retval != error_mark_node && retval != result) {
    // Store the return value to the function's DECL_RESULT.
    MemRef DestLoc(DECL_LOCAL(result), 1, false); // FIXME: What alignment?
    if (isa<AGGREGATE_TYPE>(TREE_TYPE(result))) {
      EmitAggregate(retval, DestLoc);
    } else {
      Value *Val = Builder.CreateBitCast(EmitRegister(retval),
                                         getRegType(TREE_TYPE(result)));
      StoreRegisterToMemory(Val, DestLoc, TREE_TYPE(result), 0, Builder);
    }
  }

  // Emit a branch to the exit label.
  if (!ReturnBB)
    // Create a new block for the return node, but don't insert it yet.
    ReturnBB = BasicBlock::Create(Context, "return");

  Builder.CreateBr(ReturnBB);
}

void TreeToLLVM::RenderGIMPLE_SWITCH(gimple stmt) {
  // Emit the condition.
  Value *Index = EmitRegister(gimple_switch_index(stmt));
  bool IndexIsSigned = !TYPE_UNSIGNED(TREE_TYPE(gimple_switch_index(stmt)));

  // Create the switch instruction.
  tree default_label = CASE_LABEL(gimple_switch_label(stmt, 0));
  SwitchInst *SI = Builder.CreateSwitch(Index, getLabelDeclBlock(default_label),
                                        gimple_switch_num_labels(stmt));

  // Add the switch cases.
  BasicBlock *IfBlock = 0; // Set if a range was output as an "if".
  for (unsigned i = 1, e = gimple_switch_num_labels(stmt); i != e; ++i) {
    tree label = gimple_switch_label(stmt, i);
    BasicBlock *Dest = getLabelDeclBlock(CASE_LABEL(label));

    // Convert the integer to the right type.
    Value *Val = EmitRegister(CASE_LOW(label));
    Val = CastToAnyType(Val, !TYPE_UNSIGNED(TREE_TYPE(CASE_LOW(label))),
                        Index->getType(), IndexIsSigned);
    ConstantInt *LowC = cast<ConstantInt>(Val);

    if (!CASE_HIGH(label)) {
      SI->addCase(LowC, Dest); // Single destination.
      continue;
    }

    // Otherwise, we have a range, like 'case 1 ... 17'.
    Val = EmitRegister(CASE_HIGH(label));
    // Make sure the case value is the same type as the switch expression
    Val = CastToAnyType(Val, !TYPE_UNSIGNED(TREE_TYPE(CASE_HIGH(label))),
                        Index->getType(), IndexIsSigned);
    ConstantInt *HighC = cast<ConstantInt>(Val);

    APInt Range = HighC->getValue() - LowC->getValue();
    if (Range.ult(APInt(Range.getBitWidth(), 64))) {
      // Add all of the necessary successors to the switch.
      APInt CurrentValue = LowC->getValue();
      while (1) {
        SI->addCase(LowC, Dest);
        if (LowC == HighC) break;  // Emitted the last one.
        CurrentValue++;
        LowC = ConstantInt::get(Context, CurrentValue);
      }
    } else {
      // The range is too big to add to the switch - emit an "if".
      if (!IfBlock) {
        IfBlock = BasicBlock::Create(Context);
        BeginBlock(IfBlock);
      }
      Value *Diff = Builder.CreateSub(Index, LowC);
      Value *Cond = Builder.CreateICmpULE(Diff,
                                          ConstantInt::get(Context, Range));
      BasicBlock *False_Block = BasicBlock::Create(Context);
      Builder.CreateCondBr(Cond, Dest, False_Block);
      BeginBlock(False_Block);
    }
  }

  if (IfBlock) {
    Builder.CreateBr(SI->getDefaultDest());
    SI->setDefaultDest(IfBlock);
  }
}


//===----------------------------------------------------------------------===//
//                          ... Render helpers ...
//===----------------------------------------------------------------------===//

/// EmitAssignRHS - Convert the RHS of a scalar GIMPLE_ASSIGN to LLVM.
Value *TreeToLLVM::EmitAssignRHS(gimple stmt) {
  // Loads from memory and other non-register expressions are handled by
  // EmitAssignSingleRHS.
  if (get_gimple_rhs_class(gimple_expr_code(stmt)) == GIMPLE_SINGLE_RHS) {
    Value *RHS = EmitAssignSingleRHS(gimple_assign_rhs1(stmt));
    assert(RHS->getType() == getRegType(TREE_TYPE(gimple_assign_rhs1(stmt))) &&
           "RHS has wrong type!");
    return RHS;
  }

  // The RHS is a register expression.  Emit it now.
  tree type = TREE_TYPE(gimple_assign_lhs(stmt));
  tree_code code = gimple_assign_rhs_code(stmt);
  tree rhs1 = gimple_assign_rhs1(stmt);
  tree rhs2 = gimple_assign_rhs2(stmt);
#if (GCC_MINOR > 6)
  tree rhs3 = gimple_assign_rhs3(stmt);
#endif

  Value *RHS = 0;
  switch (code) {
  default:
    debug_gimple_stmt(stmt);
    llvm_unreachable("Unsupported GIMPLE assignment!");

  // Unary expressions.
  case ABS_EXPR:
    RHS = EmitReg_ABS_EXPR(rhs1); break;
  case BIT_NOT_EXPR:
    RHS = EmitReg_BIT_NOT_EXPR(rhs1); break;
  case CONJ_EXPR:
    RHS = EmitReg_CONJ_EXPR(rhs1); break;
  case CONVERT_EXPR:
  case FIX_TRUNC_EXPR:
  case FLOAT_EXPR:
  case NOP_EXPR:
    RHS = EmitReg_CONVERT_EXPR(type, rhs1); break;
  case NEGATE_EXPR:
    RHS = EmitReg_NEGATE_EXPR(rhs1); break;
  case PAREN_EXPR:
    RHS = EmitReg_PAREN_EXPR(rhs1); break;
  case TRUTH_NOT_EXPR:
    RHS = EmitReg_TRUTH_NOT_EXPR(type, rhs1); break;

  // Comparisons.
  case EQ_EXPR:
  case GE_EXPR:
  case GT_EXPR:
  case LE_EXPR:
  case LT_EXPR:
  case LTGT_EXPR:
  case NE_EXPR:
  case ORDERED_EXPR:
  case UNEQ_EXPR:
  case UNGE_EXPR:
  case UNGT_EXPR:
  case UNLE_EXPR:
  case UNLT_EXPR:
  case UNORDERED_EXPR:
    // The GCC result may be of any integer type.
    RHS = Builder.CreateZExt(EmitCompare(rhs1, rhs2, code), getRegType(type));
    break;

  // Binary expressions.
  case BIT_AND_EXPR:
    RHS = EmitReg_BIT_AND_EXPR(rhs1, rhs2); break;
  case BIT_IOR_EXPR:
    RHS = EmitReg_BIT_IOR_EXPR(rhs1, rhs2); break;
  case BIT_XOR_EXPR:
    RHS = EmitReg_BIT_XOR_EXPR(rhs1, rhs2); break;
  case CEIL_DIV_EXPR:
    RHS = EmitReg_CEIL_DIV_EXPR(rhs1, rhs2); break;
  case COMPLEX_EXPR:
    RHS = EmitReg_COMPLEX_EXPR(rhs1, rhs2); break;
  case EXACT_DIV_EXPR:
    RHS = EmitReg_TRUNC_DIV_EXPR(rhs1, rhs2, /*isExact*/true); break;
  case FLOOR_DIV_EXPR:
    RHS = EmitReg_FLOOR_DIV_EXPR(rhs1, rhs2); break;
  case FLOOR_MOD_EXPR:
    RHS = EmitReg_FLOOR_MOD_EXPR(rhs1, rhs2); break;
  case LROTATE_EXPR:
    RHS = EmitReg_RotateOp(type, rhs1, rhs2, Instruction::Shl,
                           Instruction::LShr);
    break;
  case LSHIFT_EXPR:
    RHS = EmitReg_ShiftOp(rhs1, rhs2, Instruction::Shl); break;
  case MAX_EXPR:
    RHS = EmitReg_MinMaxExpr(rhs1, rhs2, ICmpInst::ICMP_UGE, ICmpInst::ICMP_SGE,
                             FCmpInst::FCMP_OGE);
    break;
  case MIN_EXPR:
    RHS = EmitReg_MinMaxExpr(rhs1, rhs2, ICmpInst::ICMP_ULE, ICmpInst::ICMP_SLE,
                             FCmpInst::FCMP_OLE);
    break;
  case MINUS_EXPR:
    RHS = EmitReg_MINUS_EXPR(rhs1, rhs2); break;
  case MULT_EXPR:
    RHS = EmitReg_MULT_EXPR(rhs1, rhs2); break;
  case PLUS_EXPR:
    RHS = EmitReg_PLUS_EXPR(rhs1, rhs2); break;
  case POINTER_PLUS_EXPR:
    RHS = EmitReg_POINTER_PLUS_EXPR(rhs1, rhs2); break;
  case RDIV_EXPR:
    RHS = EmitReg_RDIV_EXPR(rhs1, rhs2); break;
  case REDUC_MAX_EXPR:
    RHS = EmitReg_ReducMinMaxExpr(rhs1, ICmpInst::ICMP_UGE, ICmpInst::ICMP_SGE,
                                  FCmpInst::FCMP_OGE);
    break;
  case REDUC_MIN_EXPR:
    RHS = EmitReg_ReducMinMaxExpr(rhs1, ICmpInst::ICMP_ULE, ICmpInst::ICMP_SLE,
                                  FCmpInst::FCMP_OLE);
    break;
  case REDUC_PLUS_EXPR:
    RHS = EmitReg_REDUC_PLUS_EXPR(rhs1);
    break;
  case ROUND_DIV_EXPR:
    RHS = EmitReg_ROUND_DIV_EXPR(rhs1, rhs2); break;
  case RROTATE_EXPR:
    RHS = EmitReg_RotateOp(type, rhs1, rhs2, Instruction::LShr,
                           Instruction::Shl);
    break;
  case RSHIFT_EXPR:
    RHS = EmitReg_ShiftOp(rhs1, rhs2, TYPE_UNSIGNED(type) ?
                          Instruction::LShr : Instruction::AShr);
    break;
  case TRUNC_DIV_EXPR:
    RHS = EmitReg_TRUNC_DIV_EXPR(rhs1, rhs2, /*isExact*/false); break;
  case TRUNC_MOD_EXPR:
    RHS = EmitReg_TRUNC_MOD_EXPR(rhs1, rhs2); break;
  case TRUTH_AND_EXPR:
    RHS = EmitReg_TruthOp(type, rhs1, rhs2, Instruction::And); break;
  case TRUTH_OR_EXPR:
    RHS = EmitReg_TruthOp(type, rhs1, rhs2, Instruction::Or); break;
  case TRUTH_XOR_EXPR:
    RHS = EmitReg_TruthOp(type, rhs1, rhs2, Instruction::Xor); break;
#if (GCC_MINOR < 7)
  case VEC_EXTRACT_EVEN_EXPR:
    RHS = EmitReg_VEC_EXTRACT_EVEN_EXPR(rhs1, rhs2); break;
  case VEC_EXTRACT_ODD_EXPR:
    RHS = EmitReg_VEC_EXTRACT_ODD_EXPR(rhs1, rhs2); break;
  case VEC_INTERLEAVE_HIGH_EXPR:
    RHS = EmitReg_VEC_INTERLEAVE_HIGH_EXPR(rhs1, rhs2); break;
  case VEC_INTERLEAVE_LOW_EXPR:
    RHS = EmitReg_VEC_INTERLEAVE_LOW_EXPR(rhs1, rhs2); break;
#endif
  case VEC_LSHIFT_EXPR:
    RHS = EmitReg_VecShiftOp(rhs1, rhs2, /*isLeftShift*/true); break;
  case VEC_PACK_TRUNC_EXPR:
    RHS = EmitReg_VEC_PACK_TRUNC_EXPR(type, rhs1, rhs2); break;
  case VEC_RSHIFT_EXPR:
    RHS = EmitReg_VecShiftOp(rhs1, rhs2, /*isLeftShift*/false); break;
  case VEC_UNPACK_FLOAT_HI_EXPR:
  case VEC_UNPACK_HI_EXPR:
    RHS = EmitReg_VecUnpackHiExpr(type, rhs1); break;
  case VEC_UNPACK_FLOAT_LO_EXPR:
  case VEC_UNPACK_LO_EXPR:
    RHS = EmitReg_VecUnpackLoExpr(type, rhs1); break;
  case VEC_WIDEN_MULT_HI_EXPR:
    RHS = EmitReg_VEC_WIDEN_MULT_HI_EXPR(type, rhs1, rhs2); break;
  case VEC_WIDEN_MULT_LO_EXPR:
    RHS = EmitReg_VEC_WIDEN_MULT_LO_EXPR(type, rhs1, rhs2); break;
  case WIDEN_MULT_EXPR:
    RHS = EmitReg_WIDEN_MULT_EXPR(type, rhs1, rhs2); break;

  // Ternary expressions.
#if (GCC_MINOR > 6)
  case COND_EXPR:
  case VEC_COND_EXPR:
    RHS = EmitReg_CondExpr(rhs1, rhs2, rhs3); break;
  case VEC_PERM_EXPR:
    RHS = EmitReg_VEC_PERM_EXPR(rhs1, rhs2, rhs3); break;
#endif
  }

  return TriviallyTypeConvert(RHS, getRegType(type));
}

/// EmitAssignSingleRHS - Helper for EmitAssignRHS.  Handles those RHS that are
/// not register expressions.
Value *TreeToLLVM::EmitAssignSingleRHS(tree rhs) {
  assert(!isa<AGGREGATE_TYPE>(TREE_TYPE(rhs)) && "Expected a scalar type!");

  switch (TREE_CODE(rhs)) {
  // Catch-all for SSA names, constants etc.
  default: return EmitRegister(rhs);

  // Expressions (tcc_expression).
  case ADDR_EXPR:     return EmitADDR_EXPR(rhs);
#if (GCC_MINOR < 7)
  case COND_EXPR:
  case VEC_COND_EXPR: return EmitCondExpr(rhs);
#endif
  case OBJ_TYPE_REF:  return EmitOBJ_TYPE_REF(rhs);

  // Exceptional (tcc_exceptional).
  case CONSTRUCTOR:
    // Vector constant constructors are gimple invariant.
    return is_gimple_constant(rhs) ?
      EmitRegisterConstant(rhs) : EmitCONSTRUCTOR(rhs, 0);

  // References (tcc_reference).
  case ARRAY_REF:
  case ARRAY_RANGE_REF:
  case BIT_FIELD_REF:
  case COMPONENT_REF:
  case IMAGPART_EXPR:
  case INDIRECT_REF:
#if (GCC_MINOR > 5)
  case MEM_REF:
#endif
#if (GCC_MINOR < 6)
  case MISALIGNED_INDIRECT_REF:
#endif
  case REALPART_EXPR:
  case TARGET_MEM_REF:
  case VIEW_CONVERT_EXPR:
    return EmitLoadOfLValue(rhs); // Load from memory.

  // Declarations (tcc_declaration).
  case PARM_DECL:
  case RESULT_DECL:
  case VAR_DECL:
    return EmitLoadOfLValue(rhs); // Load from memory.

  // Constants (tcc_constant).
  case STRING_CST:
    return EmitLoadOfLValue(rhs); // Load from memory.
  }
}

/// OutputCallRHS - Convert the RHS of a GIMPLE_CALL.
Value *TreeToLLVM::OutputCallRHS(gimple stmt, const MemRef *DestLoc) {
  // Check for a built-in function call.  If we can lower it directly, do so
  // now.
  tree fndecl = gimple_call_fndecl(stmt);
  if (fndecl && DECL_BUILT_IN(fndecl) &&
      DECL_BUILT_IN_CLASS(fndecl) != BUILT_IN_FRONTEND) {
    Value *Res = 0;
    if (EmitBuiltinCall(stmt, fndecl, DestLoc, Res))
      return Res ? Mem2Reg(Res, gimple_call_return_type(stmt), Builder) : 0;
  }

  tree call_expr = gimple_call_fn(stmt);
  assert(TREE_TYPE (call_expr) &&
         (isa<POINTER_TYPE>(TREE_TYPE (call_expr)) ||
          isa<REFERENCE_TYPE>(TREE_TYPE (call_expr)))
         && "Not calling a function pointer?");

  tree function_type = TREE_TYPE(TREE_TYPE (call_expr));
  Value *Callee = EmitRegister(call_expr);
  CallingConv::ID CallingConv;
  AttrListPtr PAL;

  Type *Ty;
  // If this is a K&R-style function: with a type that takes no arguments but
  // with arguments none the less, then calculate the LLVM type from the list
  // of arguments.
  if (flag_functions_from_args) {
    tree *FirstArgAddr = gimple_call_num_args(stmt) > 0 ?
      gimple_call_arg_ptr(stmt, 0) : NULL;
    Ty = ConvertArgListToFnType(function_type,
                                ArrayRef<tree>(FirstArgAddr,
                                               gimple_call_num_args(stmt)),
                                gimple_call_chain(stmt),
                                !flag_functions_from_args, CallingConv, PAL);
  } else {
    Ty = ConvertFunctionType(function_type, fndecl, gimple_call_chain(stmt),
                             CallingConv, PAL);
  }

  // If this is a direct call to a function using a static chain then we need
  // to ensure the function type is the one just calculated: it has an extra
  // parameter for the chain.
  Callee = Builder.CreateBitCast(Callee, Ty->getPointerTo());

  Value *Result = EmitCallOf(Callee, stmt, DestLoc, PAL);

  // When calling a "noreturn" function output an unreachable instruction right
  // after the function to prevent LLVM from thinking that control flow will
  // fall into the subsequent block.
  if (gimple_call_flags(stmt) & ECF_NORETURN) {
    Builder.CreateUnreachable();
    BeginBlock(BasicBlock::Create(Context));
  }

  return Result ? Mem2Reg(Result, gimple_call_return_type(stmt), Builder) : 0;
}

/// WriteScalarToLHS - Store RHS, a non-aggregate value, into the given LHS.
void TreeToLLVM::WriteScalarToLHS(tree lhs, Value *RHS) {
  // May need a useless type conversion (useless_type_conversion_p).
  RHS = TriviallyTypeConvert(RHS, getRegType(TREE_TYPE(lhs)));

  // If this is the definition of an ssa name, record it in the SSANames map.
  if (isa<SSA_NAME>(lhs)) {
    if (flag_verbose_asm)
      NameValue(RHS, lhs);
    DefineSSAName(lhs, RHS);
    return;
  }

  if (canEmitRegisterVariable(lhs)) {
    // If this is a store to a register variable, EmitLV can't handle the dest
    // (there is no l-value of a register variable).  Emit an inline asm node
    // that copies the value into the specified register.
    EmitModifyOfRegisterVariable(lhs, RHS);
    return;
  }

  LValue LV = EmitLV(lhs);
  LV.Volatile = TREE_THIS_VOLATILE(lhs);
  // TODO: Arrange for Volatile to already be set in the LValue.
  if (!LV.isBitfield()) {
    // Non-bitfield, scalar value.  Just emit a store.
    StoreRegisterToMemory(RHS, LV, TREE_TYPE(lhs), describeAliasSet(lhs),
                          Builder);
    return;
  }

  // Last case, this is a store to a bitfield, so we have to emit a
  // read/modify/write sequence.
  if (!LV.BitSize)
    return;

  // Load and store the minimum number of bytes that covers the field.
  unsigned LoadSizeInBits = LV.BitStart + LV.BitSize;
  LoadSizeInBits = (unsigned)RoundUpToAlignment(LoadSizeInBits, BITS_PER_UNIT);
  Type *LoadType = IntegerType::get(Context, LoadSizeInBits);

  // Load the bits.
  Value *Ptr = Builder.CreateBitCast(LV.Ptr, LoadType->getPointerTo());
  Value *Val = Builder.CreateAlignedLoad(Ptr, LV.getAlignment(), LV.Volatile);

  // Get the right-hand side as a value of the same type.
  // FIXME: This assumes the right-hand side is an integer.
  bool isSigned = !TYPE_UNSIGNED(TREE_TYPE(lhs));
  RHS = CastToAnyType(RHS, isSigned, LoadType, isSigned);

  // Shift the right-hand side so that its bits are in the right position.
  unsigned FirstBitInVal = BYTES_BIG_ENDIAN ?
    LoadSizeInBits - LV.BitStart - LV.BitSize : LV.BitStart;
  if (FirstBitInVal) {
    Value *ShAmt = ConstantInt::get(LoadType, FirstBitInVal);
    RHS = Builder.CreateShl(RHS, ShAmt);
  }
  // Mask out any bits in the right-hand side that shouldn't be in the result.
  // The lower bits are zero already, so this only changes bits off the end.
  APInt Mask = APInt::getBitsSet(LoadSizeInBits, FirstBitInVal,
                                 FirstBitInVal + LV.BitSize);
  if (FirstBitInVal + LV.BitSize != LoadSizeInBits)
    RHS = Builder.CreateAnd(RHS, ConstantInt::get(Context, Mask));

  // Mask out those bits in the original value that are being replaced by the
  // right-hand side.
  Val = Builder.CreateAnd(Val, ConstantInt::get(Context, ~Mask));

  // Finally, merge the two together and store it.
  Val = Builder.CreateOr(Val, RHS);
  Builder.CreateAlignedStore(Val, Ptr, LV.getAlignment(), LV.Volatile);
}
