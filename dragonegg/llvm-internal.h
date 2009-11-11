/* Internal interfaces between the LLVM backend components
Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.
Contributed by Chris Lattner  (sabre@nondot.org)

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

//===----------------------------------------------------------------------===//
// This is a C++ header file that defines the internal interfaces shared among
// the llvm-*.cpp files.
//===----------------------------------------------------------------------===//

#ifndef LLVM_INTERNAL_H
#define LLVM_INTERNAL_H

// LLVM headers
#include "llvm/CallingConv.h"
#include "llvm/Intrinsics.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/IndexedMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/TargetFolder.h"
#include "llvm/Support/raw_os_ostream.h"

// System headers
#include <vector>
#include <cassert>
#include <map>
#include <string>

namespace llvm {
  class Module;
  class GlobalVariable;
  class Function;
  class GlobalValue;
  class BasicBlock;
  class Instruction;
  class AllocaInst;
  class BranchInst;
  class Value;
  class Constant;
  class ConstantInt;
  class Type;
  class FunctionType;
  class TargetMachine;
  class TargetData;
  class DebugInfo;
  template<typename> class AssertingVH;
}
using namespace llvm;

typedef IRBuilder<true, TargetFolder> LLVMBuilder;

// Global state.

/// TheModule - This is the current global module that we are compiling into.
///
extern llvm::Module *TheModule;

/// TheDebugInfo - This object is responsible for gather all debug information.
/// If it's value is NULL then no debug information should be gathered.
extern llvm::DebugInfo *TheDebugInfo;

/// TheTarget - The current target being compiled for.
///
extern llvm::TargetMachine *TheTarget;

/// TheFolder - The constant folder to use.
extern TargetFolder *TheFolder;

/// getTargetData - Return the current TargetData object from TheTarget.
const TargetData &getTargetData();

/// AttributeUsedGlobals - The list of globals that are marked attribute(used).
extern SmallSetVector<Constant *,32> AttributeUsedGlobals;

extern Constant* ConvertMetadataStringToGV(const char* str);

/// AddAnnotateAttrsToGlobal - Adds decls that have a
/// annotate attribute to a vector to be emitted later.
extern void AddAnnotateAttrsToGlobal(GlobalValue *GV, union tree_node* decl);

// Mapping between GCC declarations and LLVM values.

/// DECL_LLVM - Holds the LLVM expression for the value of a global variable or
/// function.  This value can be evaluated lazily for functions and variables
/// with static storage duration.
extern Value *make_decl_llvm(union tree_node *);
#define DECL_LLVM(NODE) make_decl_llvm(NODE)

/// SET_DECL_LLVM - Set the DECL_LLVM for NODE to LLVM. 
extern Value *set_decl_llvm(union tree_node *, Value *);
#define SET_DECL_LLVM(NODE, LLVM) set_decl_llvm(NODE, LLVM)

/// DECL_LLVM_IF_SET - The DECL_LLVM for NODE, if it is set, or NULL, if it is
/// not set.
extern Value *get_decl_llvm(union tree_node *);
#define DECL_LLVM_IF_SET(NODE) (HAS_RTL_P(NODE) ? get_decl_llvm(NODE) : NULL)

/// DECL_LLVM_SET_P - Returns nonzero if the DECL_LLVM for NODE has already
/// been set.
#define DECL_LLVM_SET_P(NODE) (DECL_LLVM_IF_SET(NODE) != NULL)

void changeLLVMConstant(Constant *Old, Constant *New);
void register_ctor_dtor(Function *, int, bool);
void readLLVMTypesStringTable();
void writeLLVMTypesStringTable();
void readLLVMValues();
void writeLLVMValues();
void clearTargetBuiltinCache();
const char* extractRegisterName(union tree_node*);
void handleVisibility(union tree_node* decl, GlobalValue *GV);
Twine getLLVMAssemblerName(union tree_node *);

struct StructTypeConversionInfo;

/// Return true if and only if field no. N from struct type T is a padding
/// element added to match llvm struct type size and gcc struct type size.
bool isPaddingElement(union tree_node*, unsigned N);

/// TypeConverter - Implement the converter from GCC types to LLVM types.
///
class TypeConverter {
  /// ConvertingStruct - If we are converting a RECORD or UNION to an LLVM type
  /// we set this flag to true.
  bool ConvertingStruct;
  
  /// PointersToReresolve - When ConvertingStruct is true, we handling of
  /// POINTER_TYPE and REFERENCE_TYPE is changed to return
  /// opaque*'s instead of recursively calling ConvertType.  When this happens,
  /// we add the POINTER_TYPE to this list.
  ///
  std::vector<tree_node*> PointersToReresolve;

  /// FieldIndexMap - Holds the mapping from a FIELD_DECL to the index of the
  /// corresponding LLVM field.
  std::map<tree_node *, unsigned int> FieldIndexMap;
public:
  TypeConverter() : ConvertingStruct(false) {}
  
  const Type *ConvertType(tree_node *type);

  /// GetFieldIndex - Returns the index of the LLVM field corresponding to
  /// this FIELD_DECL.
  unsigned int GetFieldIndex(tree_node *field_decl);

  /// GCCTypeOverlapsWithLLVMTypePadding - Return true if the specified GCC type
  /// has any data that overlaps with structure padding in the specified LLVM
  /// type.
  static bool GCCTypeOverlapsWithLLVMTypePadding(tree_node *t, const Type *Ty);
  
  
  /// ConvertFunctionType - Convert the specified FUNCTION_TYPE or METHOD_TYPE
  /// tree to an LLVM type.  This does the same thing that ConvertType does, but
  /// it also returns the function's LLVM calling convention and attributes.
  const FunctionType *ConvertFunctionType(tree_node *type,
                                          tree_node *decl,
                                          tree_node *static_chain,
                                          CallingConv::ID &CallingConv,
                                          AttrListPtr &PAL);
  
  /// ConvertArgListToFnType - Given a DECL_ARGUMENTS list on an GCC tree,
  /// return the LLVM type corresponding to the function.  This is useful for
  /// turning "T foo(...)" functions into "T foo(void)" functions.
  const FunctionType *ConvertArgListToFnType(tree_node *type,
                                             tree_node *arglist,
                                             tree_node *static_chain,
                                             CallingConv::ID &CallingConv,
                                             AttrListPtr &PAL);
  
private:
  const Type *ConvertRECORD(tree_node *type, tree_node *orig_type);
  const Type *ConvertUNION(tree_node *type, tree_node *orig_type);
  void SetFieldIndex(tree_node *field_decl, unsigned int Index);
  bool DecodeStructFields(tree_node *Field, StructTypeConversionInfo &Info);
  void DecodeStructBitField(tree_node *Field, StructTypeConversionInfo &Info);
  void SelectUnionMember(tree_node *type, StructTypeConversionInfo &Info);
};

extern TypeConverter *TheTypeConverter;

/// ConvertType - Convert the specified tree type to an LLVM type.
///
inline const Type *ConvertType(tree_node *type) {
  return TheTypeConverter->ConvertType(type);
}

/// GetFieldIndex - Given FIELD_DECL obtain its index.
///
inline unsigned int GetFieldIndex(tree_node *field_decl) {
  return TheTypeConverter->GetFieldIndex(field_decl);
}

/// getINTEGER_CSTVal - Return the specified INTEGER_CST value as a uint64_t.
///
uint64_t getINTEGER_CSTVal(tree_node *exp);

/// isInt64 - Return true if t is an INTEGER_CST that fits in a 64 bit integer.
/// If Unsigned is false, returns whether it fits in a int64_t.  If Unsigned is
/// true, returns whether the value is non-negative and fits in a uint64_t.
/// Always returns false for overflowed constants.
bool isInt64(tree_node *t, bool Unsigned);

/// getInt64 - Extract the value of an INTEGER_CST as a 64 bit integer.  If
/// Unsigned is false, the value must fit in a int64_t.  If Unsigned is true,
/// the value must be non-negative and fit in a uint64_t.  Must not be used on
/// overflowed constants.  These conditions can be checked by calling isInt64.
uint64_t getInt64(tree_node *t, bool Unsigned);

/// isPassedByInvisibleReference - Return true if the specified type should be
/// passed by 'invisible reference'. In other words, instead of passing the
/// thing by value, pass the address of a temporary.
bool isPassedByInvisibleReference(tree_node *type);

/// isSequentialCompatible - Return true if the specified gcc array or pointer
/// type and the corresponding LLVM SequentialType lay out their components
/// identically in memory, so doing a GEP accesses the right memory location.
/// We assume that objects without a known size do not.
bool isSequentialCompatible(tree_node *type);

/// OffsetIsLLVMCompatible - Return true if the given field is offset from the
/// start of the record by a constant amount which is not humongously big.
inline bool OffsetIsLLVMCompatible(tree_node *field_decl) {
  return DECL_FIELD_OFFSET(field_decl) &&
    isInt64(DECL_FIELD_OFFSET(field_decl), true);
}

/// isBitfield - Returns whether to treat the specified field as a bitfield.
bool isBitfield(tree_node *field_decl);

/// getFieldOffsetInBits - Return the bit offset of a FIELD_DECL in a structure.
inline uint64_t getFieldOffsetInBits(tree_node *field) {
  assert(OffsetIsLLVMCompatible(field) && "Offset is not constant!");
  uint64_t Result = getInt64(DECL_FIELD_BIT_OFFSET(field), true);
  Result += getInt64(DECL_FIELD_OFFSET(field), true) * BITS_PER_UNIT;
  return Result;
}

/// getDeclaredType - Get the declared type for the specified field, and
/// not the shrunk-to-fit type that GCC gives us in TREE_TYPE.
tree_node *getDeclaredType(tree_node *field_decl);

/// ValidateRegisterVariable - Check that a static "asm" variable is
/// well-formed.  If not, emit error messages and return true.  If so, return
/// false.
bool ValidateRegisterVariable(tree_node *decl);

/// MemRef - This struct holds the information needed for a memory access:
/// a pointer to the memory, its alignment and whether the access is volatile.
struct MemRef {
  Value *Ptr;
  bool Volatile;
private:
  unsigned char LogAlign;

public:
  MemRef() : Ptr(0), Volatile(false), LogAlign(0) {}
  MemRef(Value *P, uint32_t A, bool V) : Ptr(P), Volatile(V) {
    // Forbid alignment 0 along with non-power-of-2 alignment values.
    assert(isPowerOf2_32(A) && "Alignment not a power of 2!");
    LogAlign = Log2_32(A);
  }

  uint32_t getAlignment() const {
    return 1U << LogAlign;
  }
};

/// LValue - This struct represents an lvalue in the program.  In particular,
/// the Ptr member indicates the memory that the lvalue lives in.  Alignment
/// is the alignment of the memory (in bytes).If this is a bitfield reference,
/// BitStart indicates the first bit in the memory that is part of the field
/// and BitSize indicates the extent.
///
/// "LValue" is intended to be a light-weight object passed around by-value.
struct LValue {
  Value *Ptr;
  unsigned char BitStart;
  unsigned char BitSize;
private:
  unsigned char LogAlign;

public:
  LValue() : Ptr(0), BitStart(255), BitSize(255), LogAlign(0) {}
  LValue(Value *P, uint32_t A) : Ptr(P), BitStart(255), BitSize(255) {
    // Forbid alignment 0 along with non-power-of-2 alignment values.
    assert(isPowerOf2_32(A) && "Alignment not a power of 2!");
    LogAlign = Log2_32(A);
  }
  LValue(Value *P, uint32_t A, unsigned BSt, unsigned BSi)
  : Ptr(P), BitStart(BSt), BitSize(BSi) {
    assert(BitStart == BSt && BitSize == BSi &&
           "Bit values larger than 256?");
    // Forbid alignment 0 along with non-power-of-2 alignment values.
    assert(isPowerOf2_32(A) && "Alignment not a power of 2!");
    LogAlign = Log2_32(A);
  }

  uint32_t getAlignment() const {
    return 1U << LogAlign;
  }
  bool isBitfield() const { return BitStart != 255; }
};

/// PhiRecord - This struct holds the LLVM PHI node associated with a GCC phi.
struct PhiRecord {
  gimple gcc_phi;
  PHINode *PHI;
};

/// TreeToLLVM - An instance of this class is created and used to convert the
/// body of each function to LLVM.
///
class TreeToLLVM {
  // State that is initialized when the function starts.
  const TargetData &TD;
  tree_node *FnDecl;
  Function *Fn;
  BasicBlock *ReturnBB;
  BasicBlock *UnwindBB;
  unsigned ReturnOffset;

  // State that changes as the function is emitted.

  /// Builder - Instruction creator, the location to insert into is always the
  /// same as &Fn->back().
  LLVMBuilder Builder;

  // AllocaInsertionPoint - Place to insert alloca instructions.  Lazily created
  // and managed by CreateTemporary.
  Instruction *AllocaInsertionPoint;

  // SSAInsertionPoint - Place to insert reads corresponding to SSA default
  // definitions.
  Instruction *SSAInsertionPoint;

  /// BasicBlocks - Map from GCC to LLVM basic blocks.
  DenseMap<basic_block, BasicBlock*> BasicBlocks;

  /// LocalDecls - Map from local declarations to their associated LLVM values.
  DenseMap<tree, AssertingVH<> > LocalDecls;

  /// PendingPhis - Phi nodes which have not yet been populated with operands.
  SmallVector<PhiRecord, 16> PendingPhis;

  // SSANames - Map from GCC ssa names to the defining LLVM value.
  DenseMap<tree, AssertingVH<> > SSANames;

public:

  //===---------------------- Local Declarations --------------------------===//

  /// DECL_LOCAL - Like DECL_LLVM, returns the LLVM expression for the value of
  /// a variable or function.  However DECL_LOCAL can be used with declarations
  /// local to the current function as well as with global declarations.
  Value *make_decl_local(union tree_node *);
  #define DECL_LOCAL(NODE) make_decl_local(NODE)

  /// SET_DECL_LOCAL - Set the DECL_LOCAL for NODE to LLVM. 
  Value *set_decl_local(union tree_node *, Value *);
  #define SET_DECL_LOCAL(NODE, LLVM) set_decl_local(NODE, LLVM)

  /// DECL_LOCAL_IF_SET - The DECL_LOCAL for NODE, if it is set, or NULL, if it
  /// is not set.
  Value *get_decl_local(union tree_node *);
  #define DECL_LOCAL_IF_SET(NODE) (HAS_RTL_P(NODE) ? get_decl_local(NODE) : NULL)

  /// DECL_LOCAL_SET_P - Returns nonzero if the DECL_LOCAL for NODE has already
  /// been set.
  #define DECL_LOCAL_SET_P(NODE) (DECL_LOCAL_IF_SET(NODE) != NULL)


private:

  //===---------------------- Exception Handling --------------------------===//

  /// LandingPads - The landing pad for a given EH region.
  IndexedMap<BasicBlock *> LandingPads;

  /// PostPads - The post landing pad for a given EH region.
  IndexedMap<BasicBlock *> PostPads;

  /// ExceptionValue - Is the local to receive the current exception.
  Value *ExceptionValue;

  /// ExceptionSelectorValue - Is the local to receive the current exception
  /// selector.
  Value *ExceptionSelectorValue;

  /// FuncEHException - Function used to receive the exception.
  Function *FuncEHException;

  /// FuncEHSelector - Function used to receive the exception selector.
  Function *FuncEHSelector;

  /// FuncEHGetTypeID - Function used to return type id for give typeinfo.
  Function *FuncEHGetTypeID;

public:
  TreeToLLVM(tree_node *fndecl);
  ~TreeToLLVM();
  
  /// getFUNCTION_DECL - Return the FUNCTION_DECL node for the current function
  /// being compiled.
  tree_node *getFUNCTION_DECL() const { return FnDecl; }
  
  /// EmitFunction - Convert 'fndecl' to LLVM code.
  Function *EmitFunction();

  /// EmitBasicBlock - Convert the given basic block.
  void EmitBasicBlock(basic_block bb);

  /// EmitLV - Convert the specified l-value tree node to LLVM code, returning
  /// the address of the result.
  LValue EmitLV(tree_node *exp);

  void TODO(tree_node *exp = 0);

  /// CastToAnyType - Cast the specified value to the specified type regardless
  /// of the types involved. This is an inferred cast.
  Value *CastToAnyType (Value *V, bool VSigned, const Type* Ty, bool TySigned);

  /// CastToUIntType - Cast the specified value to the specified type assuming
  /// that V's type and Ty are integral types. This arbitrates between BitCast,
  /// Trunc and ZExt.
  Value *CastToUIntType(Value *V, const Type* Ty);

  /// CastToSIntType - Cast the specified value to the specified type assuming
  /// that V's type and Ty are integral types. This arbitrates between BitCast,
  /// Trunc and SExt.
  Value *CastToSIntType(Value *V, const Type* Ty);

  /// CastToFPType - Cast the specified value to the specified type assuming
  /// that V's type and Ty are floating point types. This arbitrates between
  /// BitCast, FPTrunc and FPExt.
  Value *CastToFPType(Value *V, const Type* Ty);

  /// CreateTemporary - Create a new alloca instruction of the specified type,
  /// inserting it into the entry block and returning it.  The resulting
  /// instruction's type is a pointer to the specified type.
  AllocaInst *CreateTemporary(const Type *Ty);

  /// CreateTempLoc - Like CreateTemporary, but returns a MemRef.
  MemRef CreateTempLoc(const Type *Ty);

  /// EmitAggregateCopy - Copy the elements from SrcLoc to DestLoc, using the
  /// GCC type specified by GCCType to know which elements to copy.
  void EmitAggregateCopy(MemRef DestLoc, MemRef SrcLoc, tree_node *GCCType);

private: // Helper functions.

  /// StartFunctionBody - Start the emission of 'fndecl', outputing all
  /// declarations for parameters and setting things up.
  void StartFunctionBody();
  
  /// FinishFunctionBody - Once the body of the function has been emitted, this
  /// cleans up and returns the result function.
  Function *FinishFunctionBody();

  /// PopulatePhiNodes - Populate generated phi nodes with their operands.
  void PopulatePhiNodes();

  /// getBasicBlock - Find or create the LLVM basic block corresponding to BB.
  BasicBlock *getBasicBlock(basic_block bb);

  /// getLabelDeclBlock - Lazily get and create a basic block for the specified
  /// label.
  BasicBlock *getLabelDeclBlock(tree_node *LabelDecl);

  /// EmitSSA_NAME - Return the defining value of the given SSA_NAME.
  /// Only creates code in the entry block.
  Value *EmitSSA_NAME(tree_node *reg);

  /// EmitGimpleInvariantAddress - The given address is constant in this
  /// function.  Return the corresponding LLVM value.  Only creates code in
  /// the entry block.
  Value *EmitGimpleInvariantAddress(tree_node *reg);

  /// EmitGimpleConstant - Convert the given global constant of register type to
  /// an LLVM constant.  Creates no code, only constants.
  Constant *EmitGimpleConstant(tree_node *reg);

  /// EmitGimpleMinInvariant - The given value is constant in this function.
  /// Return the corresponding LLVM value. Only creates code in the entry block.
  Value *EmitGimpleMinInvariant(tree_node *reg) {
    if (TREE_CODE(reg) == ADDR_EXPR)
      return EmitGimpleInvariantAddress(reg);
    return EmitGimpleConstant(reg);
  }

  /// EmitGimpleReg - Convert the specified gimple register or local constant of
  /// register type to an LLVM value.  Only creates code in the entry block.
  Value *EmitGimpleReg(tree_node *reg) {
    if (TREE_CODE(reg) == SSA_NAME)
      return EmitSSA_NAME(reg);
    return EmitGimpleMinInvariant(reg);
  }

  /// Emit - Convert the specified tree node to LLVM code.  If the node is an
  /// expression that fits into an LLVM scalar value, the result is returned. If
  /// the result is an aggregate, it is stored into the location specified by
  /// DestLoc.
  Value *Emit(tree_node *exp, const MemRef *DestLoc);

  /// EmitBlock - Add the specified basic block to the end of the function.  If
  /// the previous block falls through into it, add an explicit branch.
  void EmitBlock(BasicBlock *BB);
  
  /// EmitAggregateZero - Zero the elements of DestLoc.
  ///
  void EmitAggregateZero(MemRef DestLoc, tree_node *GCCType);
                         
  /// EmitMemCpy/EmitMemMove/EmitMemSet - Emit an llvm.memcpy/llvm.memmove or
  /// llvm.memset call with the specified operands.  Returns DestPtr bitcast
  /// to i8*.
  Value *EmitMemCpy(Value *DestPtr, Value *SrcPtr, Value *Size, unsigned Align);
  Value *EmitMemMove(Value *DestPtr, Value *SrcPtr, Value *Size, unsigned Align);
  Value *EmitMemSet(Value *DestPtr, Value *SrcVal, Value *Size, unsigned Align);

  /// EmitLandingPads - Emit EH landing pads.
  void EmitLandingPads();

  /// EmitPostPads - Emit EH post landing pads.
  void EmitPostPads();

  /// EmitUnwindBlock - Emit the lazily created EH unwind block.
  void EmitUnwindBlock();

private: // Helpers for exception handling.

  /// CreateExceptionValues - Create values used internally by exception
  /// handling.
  void CreateExceptionValues();

  /// getPostPad - Return the post landing pad for the given exception handling
  /// region, creating it if necessary.
  BasicBlock *getPostPad(unsigned RegionNo);

private:

  // Render* - Convert GIMPLE to LLVM.
  void RenderGIMPLE_ASM(gimple stmt);
  void RenderGIMPLE_ASSIGN(gimple stmt);
  void RenderGIMPLE_CALL(gimple stmt);
  void RenderGIMPLE_COND(gimple stmt);
  void RenderGIMPLE_GOTO(gimple stmt);
  void RenderGIMPLE_RESX(gimple stmt);
  void RenderGIMPLE_RETURN(gimple stmt);
  void RenderGIMPLE_SWITCH(gimple stmt);

  // Render helpers.
  void WriteScalarToLHS(tree lhs, Value *Scalar);

private:
  void EmitAutomaticVariableDecl(tree_node *decl);

  /// isNoopCast - Return true if a cast from V to Ty does not change any bits.
  ///
  static bool isNoopCast(Value *V, const Type *Ty);

  void HandleMultiplyDefinedGimpleTemporary(tree_node *var);
  
  /// EmitAnnotateIntrinsic - Emits call to annotate attr intrinsic
  void EmitAnnotateIntrinsic(Value *V, tree_node *decl);

  /// EmitTypeGcroot - Emits call to make type a gcroot
  void EmitTypeGcroot(Value *V, tree_node *decl);
private:

  // Emit* - These are delegates from Emit, and have the same parameter
  // characteristics.

  // Expressions.
  Value *EmitGimpleAssignRHS(gimple stmt, const MemRef *DestLoc);
  Value *EmitGimpleCallRHS(gimple stmt, const MemRef *DestLoc);
  Value *EmitLoadOfLValue(tree_node *exp, const MemRef *DestLoc);
  Value *EmitOBJ_TYPE_REF(tree_node *exp, const MemRef *DestLoc);
  Value *EmitADDR_EXPR(tree_node *exp);
  Value *EmitOBJ_TYPE_REF(tree_node *exp);
  Value *EmitCallOf(Value *Callee, gimple stmt, const MemRef *DestLoc,
                    const AttrListPtr &PAL);
  Value *EmitNOP_EXPR(tree_node *type, tree_node *op, const MemRef *DestLoc);
  Value *EmitCONVERT_EXPR(tree_node *type, tree_node *op);
  Value *EmitVIEW_CONVERT_EXPR(tree_node *exp, const MemRef *DestLoc);
  Value *EmitNEGATE_EXPR(tree_node *op);
  Value *EmitCONJ_EXPR(tree_node *op);
  Value *EmitABS_EXPR(tree_node *op);
  Value *EmitBIT_NOT_EXPR(tree_node *op);
  Value *EmitTRUTH_NOT_EXPR(tree_node *type, tree_node *op);
  Value *EmitCompare(tree_node *lhs, tree_node *rhs, tree_code code);
  Value *EmitBinOp(tree_node *type, tree_code code, tree_node *op0,
                   tree_node *op1, unsigned Opc);
  Value *EmitTruthOp(tree_node *type, tree_node *op0, tree_node *op1,
                     unsigned Opc);
  Value *EmitShiftOp(tree_node *op0, tree_node* op1, unsigned Opc);
  Value *EmitRotateOp(tree_node *type, tree_node *op0, tree_node *op1,
                      unsigned Opc1, unsigned Opc2);
  Value *EmitMinMaxExpr(tree_node *type, tree_node *op0, tree_node* op1,
                        unsigned UIPred, unsigned SIPred, unsigned Opc,
                        bool isMax);
  Value *EmitFLOOR_MOD_EXPR(tree_node *type, tree_node *op0, tree_node *op1);
  Value *EmitCEIL_DIV_EXPR(tree_node *type, tree_node *op0, tree_node *op1);
  Value *EmitFLOOR_DIV_EXPR(tree_node *type, tree_node *op0, tree_node *op1);
  Value *EmitROUND_DIV_EXPR(tree_node *type, tree_node *op0, tree_node *op1);
  Value *EmitFieldAnnotation(Value *FieldPtr, tree_node *FieldDecl);
  Value *EmitPOINTER_PLUS_EXPR(tree_node *type, tree_node *op0, tree_node *op1);
  Value *EmitXXXXPART_EXPR(tree_node *exp, unsigned Idx);
  Value *EmitPAREN_EXPR(tree_node *exp);

  // Exception Handling.
  Value *EmitEXC_PTR_EXPR(tree_node *exp);
  Value *EmitFILTER_EXPR(tree_node *exp);

  // Inline Assembly and Register Variables.
  Value *EmitReadOfRegisterVariable(tree_node *vardecl, const MemRef *DestLoc);
  void EmitModifyOfRegisterVariable(tree_node *vardecl, Value *RHS);

  // Helpers for Builtin Function Expansion.
  void EmitMemoryBarrier(bool ll, bool ls, bool sl, bool ss);
  Value *BuildVector(const std::vector<Value*> &Elts);
  Value *BuildVector(Value *Elt, ...);
  Value *BuildVectorShuffle(Value *InVec1, Value *InVec2, ...);
  Value *BuildBinaryAtomicBuiltin(gimple stmt, Intrinsic::ID id);
  Value *BuildCmpAndSwapAtomicBuiltin(gimple stmt, tree_node *type, 
                                      bool isBool);

  // Builtin Function Expansion.
  bool EmitBuiltinCall(gimple stmt, tree_node *fndecl, 
                       const MemRef *DestLoc, Value *&Result);
  bool EmitFrontendExpandedBuiltinCall(gimple stmt, tree_node *fndecl,
                                       const MemRef *DestLoc, Value *&Result);
  bool EmitBuiltinUnaryOp(Value *InVal, Value *&Result, Intrinsic::ID Id);
  Value *EmitBuiltinSQRT(gimple stmt);
  Value *EmitBuiltinPOWI(gimple stmt);
  Value *EmitBuiltinPOW(gimple stmt);

  bool EmitBuiltinConstantP(gimple stmt, Value *&Result);
  bool EmitBuiltinAlloca(gimple stmt, Value *&Result);
  bool EmitBuiltinExpect(gimple stmt, const MemRef *DestLoc, Value *&Result);
  bool EmitBuiltinExtendPointer(gimple stmt, Value *&Result);
  bool EmitBuiltinVAStart(gimple stmt);
  bool EmitBuiltinVAEnd(gimple stmt);
  bool EmitBuiltinVACopy(gimple stmt);
  bool EmitBuiltinMemCopy(gimple stmt, Value *&Result,
                          bool isMemMove, bool SizeCheck);
  bool EmitBuiltinMemSet(gimple stmt, Value *&Result, bool SizeCheck);
  bool EmitBuiltinBZero(gimple stmt, Value *&Result);
  bool EmitBuiltinPrefetch(gimple stmt);
  bool EmitBuiltinReturnAddr(gimple stmt, Value *&Result, bool isFrame);
  bool EmitBuiltinExtractReturnAddr(gimple stmt, Value *&Result);
  bool EmitBuiltinFrobReturnAddr(gimple stmt, Value *&Result);
  bool EmitBuiltinStackSave(gimple stmt, Value *&Result);
  bool EmitBuiltinStackRestore(gimple stmt);
  bool EmitBuiltinDwarfCFA(gimple stmt, Value *&Result);
  bool EmitBuiltinDwarfSPColumn(gimple stmt, Value *&Result);
  bool EmitBuiltinEHReturnDataRegno(gimple stmt, Value *&Result);
  bool EmitBuiltinEHReturn(gimple stmt, Value *&Result);
  bool EmitBuiltinInitDwarfRegSizes(gimple stmt, Value *&Result);
  bool EmitBuiltinUnwindInit(gimple stmt, Value *&Result);
  bool EmitBuiltinAdjustTrampoline(gimple stmt, Value *&Result);
  bool EmitBuiltinInitTrampoline(gimple stmt, Value *&Result);

  // Complex Math Expressions.
  Value *CreateComplex(Value *Real, Value *Imag);
  void SplitComplex(Value *Complex, Value *&Real, Value *&Imag);
  Value *EmitCOMPLEX_EXPR(tree op0, tree op1);
  Value *EmitComplexBinOp(tree_node *type, tree_code code, tree_node *op0,
                          tree_node *op1);

  // L-Value Expressions.
  LValue EmitLV_ARRAY_REF(tree_node *exp);
  LValue EmitLV_BIT_FIELD_REF(tree_node *exp);
  LValue EmitLV_COMPONENT_REF(tree_node *exp);
  LValue EmitLV_DECL(tree_node *exp);
  LValue EmitLV_EXC_PTR_EXPR(tree_node *exp);
  LValue EmitLV_FILTER_EXPR(tree_node *exp);
  LValue EmitLV_INDIRECT_REF(tree_node *exp);
  LValue EmitLV_VIEW_CONVERT_EXPR(tree_node *exp);
  LValue EmitLV_WITH_SIZE_EXPR(tree_node *exp);
  LValue EmitLV_XXXXPART_EXPR(tree_node *exp, unsigned Idx);
  LValue EmitLV_SSA_NAME(tree_node *exp);

  // Constant Expressions.
  Value *EmitINTEGER_CST(tree_node *exp);
  Value *EmitREAL_CST(tree_node *exp);
  Value *EmitCONSTRUCTOR(tree_node *exp, const MemRef *DestLoc);

  // Optional target defined builtin intrinsic expanding function.
  bool TargetIntrinsicLower(gimple stmt,
                            tree_node *fndecl,
                            const MemRef *DestLoc,
                            Value *&Result,
                            const Type *ResultType,
                            std::vector<Value*> &Ops);

public:
  // Helper for taking the address of a label.
  Constant *EmitLV_LABEL_DECL(tree_node *exp);
};

/// TreeConstantToLLVM - An instance of this class is created and used to 
/// convert tree constant values to LLVM.  This is primarily for things like
/// global variable initializers.
///
class TreeConstantToLLVM {
public:
  // Constant Expressions
  static Constant *Convert(tree_node *exp);
  static Constant *ConvertINTEGER_CST(tree_node *exp);
  static Constant *ConvertREAL_CST(tree_node *exp);
  static Constant *ConvertVECTOR_CST(tree_node *exp);
  static Constant *ConvertSTRING_CST(tree_node *exp);
  static Constant *ConvertCOMPLEX_CST(tree_node *exp);
  static Constant *ConvertNOP_EXPR(tree_node *exp);
  static Constant *ConvertCONVERT_EXPR(tree_node *exp);
  static Constant *ConvertBinOp_CST(tree_node *exp);
  static Constant *ConvertCONSTRUCTOR(tree_node *exp);
  static Constant *ConvertArrayCONSTRUCTOR(tree_node *exp);
  static Constant *ConvertRecordCONSTRUCTOR(tree_node *exp);
  static Constant *ConvertUnionCONSTRUCTOR(tree_node *exp);
  static Constant *ConvertPOINTER_PLUS_EXPR(tree_node *exp);

  // Constant Expression l-values.
  static Constant *EmitLV(tree_node *exp);
  static Constant *EmitLV_Decl(tree_node *exp);
  static Constant *EmitLV_LABEL_DECL(tree_node *exp);
  static Constant *EmitLV_COMPLEX_CST(tree_node *exp);
  static Constant *EmitLV_STRING_CST(tree_node *exp);
  static Constant *EmitLV_COMPONENT_REF(tree_node *exp);
  static Constant *EmitLV_ARRAY_REF(tree_node *exp);
  
};

#endif /* LLVM_INTERNAL_H */
