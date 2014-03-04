//=---- Internals.h - Interface between the backend components ----*- C++ -*-=//
//
// Copyright (C) 2005 to 2013  Chris Lattner, Duncan Sands et al.
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
// This file declares the internal interfaces shared among the dragonegg files.
//===----------------------------------------------------------------------===//

#ifndef DRAGONEGG_INTERNALS_H
#define DRAGONEGG_INTERNALS_H

// LLVM headers
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Analysis/TargetFolder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"

struct basic_block_def;
union gimple_statement_d;
union tree_node;

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
class TargetMachine;
class DataLayout;
template <typename> class AssertingVH;
template <typename> class TrackingVH;
}
class DebugInfo;

typedef llvm::IRBuilder<true, llvm::TargetFolder> LLVMBuilder;

// Global state.

/// TheModule - This is the current global module that we are compiling into.
///
extern llvm::Module *TheModule;

/// TheDebugInfo - This object is responsible for gather all debug information.
/// If it's value is NULL then no debug information should be gathered.
extern DebugInfo *TheDebugInfo;

/// TheTarget - The current target being compiled for.
///
extern llvm::TargetMachine *TheTarget;

/// TheFolder - The constant folder to use.
extern llvm::TargetFolder *TheFolder;

/// getDataLayout - Return the current DataLayout object from TheTarget.
const llvm::DataLayout &getDataLayout();

/// flag_default_initialize_globals - Whether global variables with no explicit
/// initial value should be zero initialized.
extern bool flag_default_initialize_globals;

/// flag_odr - Whether the language being compiled obeys the One Definition Rule
/// (i.e. if the same function is defined in multiple compilation units, all the
/// definitions are equivalent).
extern bool flag_odr;

/// flag_functions_from_args - Construct function prototypes from the argument
/// list, ignoring the function type.  This is helpful if the language front-end
/// sometimes creates functions and/or calls where the arguments do not match
/// the arguments given in the function type.
extern bool flag_functions_from_args;

/// AttributeUsedGlobals - The list of globals that are marked attribute(used).
extern llvm::SmallSetVector<llvm::Constant *, 32> AttributeUsedGlobals;

extern llvm::Constant *ConvertMetadataStringToGV(const char *str);

/// AddAnnotateAttrsToGlobal - Adds decls that have a
/// annotate attribute to a vector to be emitted later.
extern void AddAnnotateAttrsToGlobal(llvm::GlobalValue *GV, tree_node *decl);

// Mapping between GCC declarations and LLVM values.  The GCC declaration must
// satisfy HAS_RTL_P.

/// DECL_LLVM - Returns the LLVM declaration of a global variable or function.
extern llvm::Value *make_decl_llvm(tree_node *);
#define DECL_LLVM(NODE) make_decl_llvm(NODE)

/// SET_DECL_LLVM - Set the DECL_LLVM for NODE to LLVM.
extern llvm::Value *set_decl_llvm(tree_node *, llvm::Value *);
#define SET_DECL_LLVM(NODE, LLVM) set_decl_llvm(NODE, LLVM)

/// DECL_LLVM_IF_SET - The DECL_LLVM for NODE, if it is set, or NULL, if it is
/// not set.
extern llvm::Value *get_decl_llvm(tree_node *);
#define DECL_LLVM_IF_SET(NODE) (HAS_RTL_P(NODE) ? get_decl_llvm(NODE) : NULL)

/// DECL_LLVM_SET_P - Returns nonzero if the DECL_LLVM for NODE has already
/// been set.
#define DECL_LLVM_SET_P(NODE) (DECL_LLVM_IF_SET(NODE) != NULL)

/// DEFINITION_LLVM - Ensures that the body or initial value of the given GCC
/// global will be output, and returns a declaration for it.
llvm::Value *make_definition_llvm(tree_node *decl);
#define DEFINITION_LLVM(NODE) make_definition_llvm(NODE)

// Mapping between GCC declarations and non-negative integers.  The GCC
// declaration must not satisfy HAS_RTL_P.

void changeLLVMConstant(llvm::Constant *Old, llvm::Constant *New);
void register_ctor_dtor(llvm::Function *, int, bool);
const char *extractRegisterName(tree_node *);
void handleVisibility(tree_node *decl, llvm::GlobalValue *GV);

/// Return true if and only if field no. N from struct type T is a padding
/// element added to match llvm struct type size and gcc struct type size.
bool isPaddingElement(tree_node *, unsigned N);

/// getDefaultValue - Return the default value to use for a constant or global
/// that has no value specified.  For example in C like languages such variables
/// are initialized to zero, while in Ada they hold an undefined value.
inline llvm::Constant *getDefaultValue(llvm::Type *Ty) {
  return flag_default_initialize_globals ? llvm::Constant::getNullValue(Ty)
                                         : llvm::UndefValue::get(Ty);
}

/// isPassedByInvisibleReference - Return true if the specified type should be
/// passed by 'invisible reference'. In other words, instead of passing the
/// thing by value, pass the address of a temporary.
bool isPassedByInvisibleReference(tree_node *type);

/// ValidateRegisterVariable - Check that a static "asm" variable is
/// well-formed.  If not, emit error messages and return true.  If so, return
/// false.
bool ValidateRegisterVariable(tree_node *decl);

/// MemRef - This struct holds the information needed for a memory access:
/// a pointer to the memory, its alignment and whether the access is volatile.
class MemRef {
public:
  llvm::Value *Ptr;
  bool Volatile;
private:
  unsigned char LogAlign;
public:
  explicit MemRef() : Ptr(0), Volatile(false), LogAlign(0) {}
  explicit MemRef(llvm::Value *P, uint32_t A, bool V) : Ptr(P), Volatile(V) {
    setAlignment(A);
  }

  uint32_t getAlignment() const { return 1U << LogAlign; }

  void setAlignment(uint32_t A) {
    // Forbid alignment 0 along with non-power-of-2 alignment values.
    assert(llvm::isPowerOf2_32(A) && "Alignment not a power of 2!");
    LogAlign = (unsigned char) llvm::Log2_32(A);
  }
};

/// LValue - This struct represents an lvalue in the program.  In particular,
/// the Ptr member indicates the memory that the lvalue lives in.  Alignment
/// is the alignment of the memory (in bytes).If this is a bitfield reference,
/// BitStart indicates the first bit in the memory that is part of the field
/// and BitSize indicates the extent.
///
/// "LValue" is intended to be a light-weight object passed around by-value.
class LValue : public MemRef {
public:
  unsigned char BitStart;
  unsigned char BitSize;
public:
  explicit LValue() : BitStart(255), BitSize(255) {}
  explicit LValue(MemRef &M) : MemRef(M), BitStart(255), BitSize(255) {}
  LValue(llvm::Value *P, uint32_t A, bool V = false)
      : MemRef(P, A, V), BitStart(255), BitSize(255) {}
  LValue(llvm::Value *P, uint32_t A, unsigned BSt, unsigned BSi, bool V = false)
      : MemRef(P, A, V), BitStart((unsigned char) BSt),
        BitSize((unsigned char) BSi) {
    assert(BitStart == BSt && BitSize == BSi && "Bit values larger than 256?");
  }

  bool isBitfield() const { return BitStart != 255; }
};

/// PhiRecord - This struct holds the LLVM PHI node associated with a GCC phi.
struct PhiRecord {
  gimple_statement_d *gcc_phi;
  llvm::PHINode *PHI;
};

/// TreeToLLVM - An instance of this class is created and used to convert the
/// body of each function to LLVM.
///
class TreeToLLVM {
  // State that is initialized when the function starts.
  const llvm::DataLayout &DL;
  tree_node *FnDecl;
  llvm::Function *Fn;
  llvm::BasicBlock *ReturnBB;
  unsigned ReturnOffset;

  // State that changes as the function is emitted.

  /// Builder - Instruction creator, the location to insert into is always the
  /// same as &Fn->back().
  LLVMBuilder Builder;

  // AllocaInsertionPoint - Place to insert alloca instructions.  Lazily created
  // and managed by CreateTemporary.
  llvm::Instruction *AllocaInsertionPoint;

  // SSAInsertionPoint - Place to insert reads corresponding to SSA default
  // definitions.
  llvm::Instruction *SSAInsertionPoint;

  /// BasicBlocks - Map from GCC to LLVM basic blocks.
  llvm::DenseMap<basic_block_def *, llvm::BasicBlock *> BasicBlocks;

  /// LocalDecls - Map from local declarations to their associated LLVM values.
  llvm::DenseMap<tree_node *, llvm::AssertingVH<llvm::Value> > LocalDecls;

  /// PendingPhis - Phi nodes which have not yet been populated with operands.
  llvm::SmallVector<PhiRecord, 16> PendingPhis;

  // SSANames - Map from GCC ssa names to the defining LLVM value.
  llvm::DenseMap<tree_node *, llvm::TrackingVH<llvm::Value> > SSANames;

public:

  //===---------------------- Local Declarations --------------------------===//

  /// DECL_LOCAL - Like DECL_LLVM, returns the LLVM declaration of a variable or
  /// function.  However DECL_LOCAL can be used with declarations local to the
  /// current function as well as with global declarations.
  llvm::Value *make_decl_local(tree_node *);
#define DECL_LOCAL(NODE) make_decl_local(NODE)

  /// DEFINITION_LOCAL - Like DEFINITION_LLVM, ensures that the initial value or
  /// body of a variable or function will be output.  However DEFINITION_LOCAL
  /// can be used with declarations local to the current function as well as
  /// with global declarations.
  llvm::Value *make_definition_local(tree_node *);
#define DEFINITION_LOCAL(NODE) make_definition_local(NODE)

  /// SET_DECL_LOCAL - Set the DECL_LOCAL for NODE to LLVM.
  llvm::Value *set_decl_local(tree_node *, llvm::Value *);
#define SET_DECL_LOCAL(NODE, LLVM) set_decl_local(NODE, LLVM)

  /// DECL_LOCAL_IF_SET - The DECL_LOCAL for NODE, if it is set, or NULL, if it
  /// is not set.
  llvm::Value *get_decl_local(tree_node *);
#define DECL_LOCAL_IF_SET(NODE) (HAS_RTL_P(NODE) ? get_decl_local(NODE) : NULL)

/// DECL_LOCAL_SET_P - Returns nonzero if the DECL_LOCAL for NODE has already
/// been set.
#define DECL_LOCAL_SET_P(NODE) (DECL_LOCAL_IF_SET(NODE) != NULL)

private:

  //===---------------------- Exception Handling --------------------------===//

  /// NormalInvokes - Mapping from landing pad number to the set of invoke
  /// instructions that unwind to that landing pad.
  llvm::SmallVector<llvm::SmallVector<llvm::InvokeInst *, 8>, 16> NormalInvokes;

  /// ExceptionPtrs - Mapping from EH region index to the local holding the
  /// exception pointer for that region.
  llvm::SmallVector<llvm::AllocaInst *, 16> ExceptionPtrs;

  /// ExceptionFilters - Mapping from EH region index to the local holding the
  /// filter value for that region.
  llvm::SmallVector<llvm::AllocaInst *, 16> ExceptionFilters;

  /// FailureBlocks - Mapping from the index of a must-not-throw EH region to
  /// the block containing the failure code for the region (the code that is
  /// run if an exception is thrown in this region).
  llvm::SmallVector<llvm::BasicBlock *, 16> FailureBlocks;

public:
  TreeToLLVM(tree_node *fndecl);
  ~TreeToLLVM();

  /// getFUNCTION_DECL - Return the FUNCTION_DECL node for the current function
  /// being compiled.
  tree_node *getFUNCTION_DECL() const { return FnDecl; }

  /// EmitFunction - Convert 'fndecl' to LLVM code.
  llvm::Function *EmitFunction();

  /// EmitBasicBlock - Convert the given basic block.
  void EmitBasicBlock(basic_block_def *bb);

  /// EmitLV - Convert the specified l-value tree node to LLVM code, returning
  /// the address of the result.
  LValue EmitLV(tree_node *exp);

  /// CastToAnyType - Cast the specified value to the specified type regardless
  /// of the types involved. This is an inferred cast.
  llvm::Value *CastToAnyType(llvm::Value *Src, bool SrcIsSigned,
                             llvm::Type *DstTy, bool DstIsSigned);
  llvm::Constant *CastToAnyType(llvm::Constant *Src, bool SrcIsSigned,
                                llvm::Type *DstTy, bool DstIsSigned);

  /// CastFromSameSizeInteger - Cast an integer (or vector of integer) value to
  /// the given scalar (resp. vector of scalar) type of the same bitwidth.
  llvm::Value *CastFromSameSizeInteger(llvm::Value *V, llvm::Type *Ty);

  /// CastToSameSizeInteger - Cast the specified scalar (or vector of scalar)
  /// value to an integer (resp. vector of integer) of the same bit width.
  llvm::Value *CastToSameSizeInteger(llvm::Value *V);

  /// CastToFPType - Cast the specified value to the specified type assuming
  /// that V's type and Ty are floating point types. This arbitrates between
  /// BitCast, FPTrunc and FPExt.
  llvm::Value *CastToFPType(llvm::Value *V, llvm::Type *Ty);

  /// CreateAnyAdd - Add two LLVM scalar values with the given GCC type.  Does
  /// not support complex numbers.  The type is used to set overflow flags.
  llvm::Value *CreateAnyAdd(llvm::Value *LHS, llvm::Value *RHS,
                            tree_node *type);

  /// CreateAnyMul - Multiply two LLVM scalar values with the given GCC type.
  /// Does not support complex numbers.  The type is used to set overflow flags.
  llvm::Value *CreateAnyMul(llvm::Value *LHS, llvm::Value *RHS,
                            tree_node *type);

  /// CreateAnyNeg - Negate an LLVM scalar value with the given GCC type.  Does
  /// not support complex numbers.  The type is used to set overflow flags.
  llvm::Value *CreateAnyNeg(llvm::Value *V, tree_node *type);

  /// CreateAnySub - Subtract two LLVM scalar values with the given GCC type.
  /// Does not support complex numbers.
  llvm::Value *CreateAnySub(llvm::Value *LHS, llvm::Value *RHS,
                            tree_node *type);

  /// CreateTemporary - Create a new alloca instruction of the specified type,
  /// inserting it into the entry block and returning it.  The resulting
  /// instruction's type is a pointer to the specified type.
  llvm::AllocaInst *CreateTemporary(llvm::Type *Ty, unsigned align = 0);

  /// CreateTempLoc - Like CreateTemporary, but returns a MemRef.
  MemRef CreateTempLoc(llvm::Type *Ty);

  /// EmitAggregateCopy - Copy the elements from SrcLoc to DestLoc, using the
  /// GCC type specified by GCCType to know which elements to copy.
  void EmitAggregateCopy(MemRef DestLoc, MemRef SrcLoc, tree_node *GCCType);

  /// EmitAggregate - Store the specified tree node into the location given by
  /// DestLoc.
  void EmitAggregate(tree_node *exp, const MemRef &DestLoc);

private : // Helper functions.

  /// StartFunctionBody - Start the emission of 'fndecl', outputing all
  /// declarations for parameters and setting things up.
  void StartFunctionBody();

  /// FinishFunctionBody - Once the body of the function has been emitted, this
  /// cleans up and returns the result function.
  llvm::Function *FinishFunctionBody();

  /// EmitVariablesInScope - Output a declaration for every variable in the
  /// given scope.
  void EmitVariablesInScope(tree_node *scope);

  /// PopulatePhiNodes - Populate generated phi nodes with their operands.
  void PopulatePhiNodes();

  /// getBasicBlock - Find or create the LLVM basic block corresponding to BB.
  llvm::BasicBlock *getBasicBlock(basic_block_def *bb);

  /// getLabelDeclBlock - Lazily get and create a basic block for the specified
  /// label.
  llvm::BasicBlock *getLabelDeclBlock(tree_node *LabelDecl);

  /// DefineSSAName - Use the given value as the definition of the given SSA
  /// name.  Returns the provided value as a convenience.
  llvm::Value *DefineSSAName(tree_node *reg, llvm::Value *Val);

  /// BeginBlock - Add the specified basic block to the end of the function.  If
  /// the previous block falls through into it, add an explicit branch.
  void BeginBlock(llvm::BasicBlock *BB);

  /// CopyElementByElement - Recursively traverse the potentially aggregate
  /// src/dest ptrs, copying all of the elements.  Helper for EmitAggregateCopy.
  void CopyElementByElement(MemRef DestLoc, MemRef SrcLoc, tree_node *type);

  /// ZeroElementByElement - Recursively traverse the potentially aggregate
  /// DestLoc, zero'ing all of the elements.  Helper for EmitAggregateZero.
  void ZeroElementByElement(MemRef DestLoc, tree_node *type);

  /// EmitAggregateZero - Zero the elements of DestLoc.
  void EmitAggregateZero(MemRef DestLoc, tree_node *type);

  /// EmitMemCpy/EmitMemMove/EmitMemSet - Emit an llvm.memcpy/llvm.memmove or
  /// llvm.memset call with the specified operands.  Returns DestPtr bitcast
  /// to i8*.
  llvm::Value *EmitMemCpy(llvm::Value *DestPtr, llvm::Value *SrcPtr,
                          llvm::Value *Size, unsigned Align);
  llvm::Value *EmitMemMove(llvm::Value *DestPtr, llvm::Value *SrcPtr,
                           llvm::Value *Size, unsigned Align);
  llvm::Value *EmitMemSet(llvm::Value *DestPtr, llvm::Value *SrcVal,
                          llvm::Value *Size, unsigned Align);

  /// EmitLandingPads - Emit EH landing pads.
  void EmitLandingPads();

  /// EmitFailureBlocks - Emit the blocks containing failure code executed when
  /// an exception is thrown in a must-not-throw region.
  void EmitFailureBlocks();

  /// EmitDebugInfo - Return true if debug info is to be emitted for current
  /// function.
  bool EmitDebugInfo();

private : // Helpers for exception handling.

  /// getExceptionPtr - Return the local holding the exception pointer for the
  /// given exception handling region, creating it if necessary.
  llvm::AllocaInst *getExceptionPtr(int RegionNo);

  /// getExceptionFilter - Return the local holding the filter value for the
  /// given exception handling region, creating it if necessary.
  llvm::AllocaInst *getExceptionFilter(int RegionNo);

  /// getFailureBlock - Return the basic block containing the failure code for
  /// the given exception handling region, creating it if necessary.
  llvm::BasicBlock *getFailureBlock(int RegionNo);

private:
  void EmitAutomaticVariableDecl(tree_node *decl);

  /// EmitAnnotateIntrinsic - Emits call to annotate attr intrinsic
  void EmitAnnotateIntrinsic(llvm::Value *V, tree_node *decl);

  /// EmitTypeGcroot - Emits call to make type a gcroot
  void EmitTypeGcroot(llvm::Value *V);

private:

  //===------------------ Render* - Convert GIMPLE to LLVM ----------------===//

  void RenderGIMPLE_ASM(gimple_statement_d *stmt);
  void RenderGIMPLE_ASSIGN(gimple_statement_d *stmt);
  void RenderGIMPLE_CALL(gimple_statement_d *stmt);
  void RenderGIMPLE_COND(gimple_statement_d *stmt);
  void RenderGIMPLE_EH_DISPATCH(gimple_statement_d *stmt);
  void RenderGIMPLE_GOTO(gimple_statement_d *stmt);
  void RenderGIMPLE_RESX(gimple_statement_d *stmt);
  void RenderGIMPLE_RETURN(gimple_statement_d *stmt);
  void RenderGIMPLE_SWITCH(gimple_statement_d *stmt);

  // Render helpers.

  /// EmitAssignRHS - Convert the RHS of a scalar GIMPLE_ASSIGN to LLVM.
  llvm::Value *EmitAssignRHS(gimple_statement_d *stmt);

  /// EmitAssignSingleRHS - Helper for EmitAssignRHS.  Handles those RHS that
  /// are not register expressions.
  llvm::Value *EmitAssignSingleRHS(tree_node *rhs);

  /// OutputCallRHS - Convert the RHS of a GIMPLE_CALL.
  llvm::Value *OutputCallRHS(gimple_statement_d *stmt, const MemRef *DestLoc);

  /// WriteScalarToLHS - Store RHS, a non-aggregate value, into the given LHS.
  void WriteScalarToLHS(tree_node *lhs, llvm::Value *Scalar);

private:

  //===---------- EmitReg* - Convert register expression to LLVM ----------===//

  /// TriviallyTypeConvert - Convert the given value to the given type, assuming
  /// that the original and target types are LLVM register types that correspond
  /// to GCC scalar types t1 and t2 satisfying useless_type_conversion_p(t1, t2)
  /// or useless_type_conversion_p(t2, t1).
  llvm::Value *TriviallyTypeConvert(llvm::Value *V, llvm::Type *Ty) {
    // If useless_type_conversion_p(t1, t2) holds then the corresponding LLVM
    // register types are either equal or are both pointer types.
    if (V->getType() == Ty)
      return V;
    assert(V->getType()->isPointerTy() && Ty->isPointerTy() && "Not trivial!");
    return Builder.CreateBitCast(V, Ty);
  }

  /// EmitRegister - Convert the specified gimple register or local constant of
  /// register type to an LLVM value.  Only creates code in the entry block.
  llvm::Value *EmitRegister(tree_node *reg);

  /// EmitRegisterWithCast - Utility method that calls EmitRegister, then casts
  /// the returned value to the given register type.
  llvm::Value *EmitRegisterWithCast(tree_node *reg, tree_node *type);

  /// EmitReg_SSA_NAME - Return the defining value of the given SSA_NAME.
  /// Only creates code in the entry block.
  llvm::Value *EmitReg_SSA_NAME(tree_node *reg);

  // Unary expressions.
  llvm::Value *EmitReg_ABS_EXPR(tree_node *op);
  llvm::Value *EmitReg_BIT_NOT_EXPR(tree_node *op);
  llvm::Value *EmitReg_CONJ_EXPR(tree_node *op);
  llvm::Value *EmitReg_CONVERT_EXPR(tree_node *type, tree_node *op);
  llvm::Value *EmitReg_NEGATE_EXPR(tree_node *op);
  llvm::Value *EmitReg_PAREN_EXPR(tree_node *exp);
  llvm::Value *EmitReg_TRUTH_NOT_EXPR(tree_node *type, tree_node *op);

  // Comparisons.

  /// EmitCompare - Compare LHS with RHS using the appropriate comparison code.
  /// The result is an i1 boolean.
  llvm::Value *EmitCompare(tree_node *lhs, tree_node *rhs, unsigned code);

  // Binary expressions.
  llvm::Value *EmitReg_MinMaxExpr(tree_node *op0, tree_node *op1,
                                  unsigned UIPred, unsigned SIPred,
                                  unsigned Opc);
  llvm::Value *EmitReg_ReducMinMaxExpr(tree_node *op, unsigned UIPred,
                                       unsigned SIPred, unsigned Opc);
  llvm::Value *EmitReg_RotateOp(tree_node *type, tree_node *op0, tree_node *op1,
                                unsigned Opc1, unsigned Opc2);
  llvm::Value *EmitReg_ShiftOp(tree_node *op0, tree_node *op1, unsigned Opc);
  llvm::Value *EmitReg_VecShiftOp(tree_node *op0, tree_node *op1,
                                  bool isLeftShift);
  llvm::Value *EmitReg_TruthOp(tree_node *type, tree_node *op0, tree_node *op1,
                               unsigned Opc);
  llvm::Value *EmitReg_VecUnpackHiExpr(tree_node *type, tree_node *op0);
  llvm::Value *EmitReg_VecUnpackLoExpr(tree_node *type, tree_node *op0);
  llvm::Value *EmitReg_BIT_AND_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_BIT_IOR_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_BIT_XOR_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_CEIL_DIV_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_COMPLEX_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_FLOOR_DIV_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_FLOOR_MOD_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_MINUS_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_MULT_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_PLUS_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_POINTER_PLUS_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_RDIV_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_REDUC_PLUS_EXPR(tree_node *op);
  llvm::Value *EmitReg_ROUND_DIV_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_TRUNC_DIV_EXPR(tree_node *op0, tree_node *op1,
                                      bool isExact);
  llvm::Value *EmitReg_TRUNC_MOD_EXPR(tree_node *op0, tree_node *op1);
#if (GCC_MINOR < 7)
  llvm::Value *EmitReg_VEC_EXTRACT_EVEN_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_VEC_EXTRACT_ODD_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_VEC_INTERLEAVE_HIGH_EXPR(tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_VEC_INTERLEAVE_LOW_EXPR(tree_node *op0, tree_node *op1);
#endif
  llvm::Value *EmitReg_VEC_PACK_FIX_TRUNC_EXPR(tree_node *type, tree_node *op0,
                                         tree_node *op1);
  llvm::Value *
  EmitReg_VEC_PACK_TRUNC_EXPR(tree_node *type, tree_node *op0, tree_node *op1);
  llvm::Value *EmitReg_VEC_WIDEN_MULT_HI_EXPR(tree_node *type, tree_node *op0,
                                        tree_node *op1);
  llvm::Value *EmitReg_VEC_WIDEN_MULT_LO_EXPR(tree_node *type, tree_node *op0,
                                        tree_node *op1);
  llvm::Value *
  EmitReg_WIDEN_MULT_EXPR(tree_node *type, tree_node *op0, tree_node *op1);

  // Ternary expressions.
  llvm::Value *EmitReg_CondExpr(tree_node *op0, tree_node *op1, tree_node *op2);
#if (GCC_MINOR > 5)
  llvm::Value *EmitReg_FMA_EXPR(tree_node *op0, tree_node *op1, tree_node *op2);
#endif
#if (GCC_MINOR > 6)
  llvm::Value *EmitReg_VEC_PERM_EXPR(tree_node *op0, tree_node *op1,
                                     tree_node *op2);
#endif

  llvm::Value *EmitLoadOfLValue(tree_node *exp);
  llvm::Value *EmitOBJ_TYPE_REF(tree_node *exp);
  llvm::Value *EmitADDR_EXPR(tree_node *exp);
#if (GCC_MINOR < 7)
  llvm::Value *EmitCondExpr(tree_node *exp);
#endif
  llvm::Value *EmitCallOf(llvm::Value *Callee, gimple_statement_d *stmt,
                          const MemRef *DestLoc, const llvm::AttributeSet &PAL);
  llvm::CallInst *EmitSimpleCall(llvm::StringRef CalleeName,
                                 tree_node *ret_type,
                                 /* arguments */ ...) END_WITH_NULL;
  llvm::Value *EmitFieldAnnotation(llvm::Value *FieldPtr, tree_node *FieldDecl);

  // Inline Assembly and Register Variables.
  llvm::Value *EmitReadOfRegisterVariable(tree_node *vardecl);
  void EmitModifyOfRegisterVariable(tree_node *vardecl, llvm::Value *RHS);

  // Helpers for Builtin Function Expansion.
  llvm::Value *BuildVector(const std::vector<llvm::Value *> &Elts);
  llvm::Value *BuildVector(llvm::Value *Elt, ...);
  llvm::Value *BuildVectorShuffle(llvm::Value *InVec1, llvm::Value *InVec2, ...);
  llvm::Value *BuildBinaryAtomic(gimple_statement_d *stmt,
                                 llvm::AtomicRMWInst::BinOp Kind,
                                 unsigned PostOp = 0);
  llvm::Value *
  BuildCmpAndSwapAtomic(gimple_statement_d *stmt, unsigned Bits, bool isBool);

  // Builtin Function Expansion.
  bool EmitBuiltinCall(gimple_statement_d *stmt, tree_node *fndecl,
                       const MemRef *DestLoc, llvm::Value *&Result);
  bool EmitFrontendExpandedBuiltinCall(gimple_statement_d *stmt,
                                       tree_node *fndecl, const MemRef *DestLoc,
                                       llvm::Value *&Result);
  bool EmitBuiltinUnaryOp(llvm::Value *InVal, llvm::Value *&Result,
                          llvm::Intrinsic::ID Id);
  llvm::Value *
  EmitBuiltinBitCountIntrinsic(gimple_statement_d *stmt,
                               llvm::Intrinsic::ID Id);
  llvm::Value *EmitBuiltinSQRT(gimple_statement_d *stmt);
  llvm::Value *EmitBuiltinPOWI(gimple_statement_d *stmt);
  llvm::Value *EmitBuiltinPOW(gimple_statement_d *stmt);
  llvm::Value *EmitBuiltinLCEIL(gimple_statement_d *stmt);
  llvm::Value *EmitBuiltinLFLOOR(gimple_statement_d *stmt);
  llvm::Value *EmitBuiltinLROUND(gimple_statement_d *stmt);
  llvm::Value *EmitBuiltinCEXPI(gimple_statement_d *stmt);
  llvm::Value *EmitBuiltinSIGNBIT(gimple_statement_d *stmt);

  bool EmitBuiltinAdjustTrampoline(gimple_statement_d *stmt,
                                   llvm::Value *&Result);
  bool EmitBuiltinAlloca(gimple_statement_d *stmt, llvm::Value *&Result);
  bool EmitBuiltinAllocaWithAlign(gimple_statement_d *stmt,
                                  llvm::Value *&Result);
#if (GCC_MINOR > 6)
  bool EmitBuiltinAssumeAligned(gimple_statement_d *stmt, llvm::Value *&Result);
#endif
  bool EmitBuiltinBZero(gimple_statement_d *stmt, llvm::Value *&Result);
  bool EmitBuiltinConstantP(gimple_statement_d *stmt, llvm::Value *&Result);
  bool EmitBuiltinExpect(gimple_statement_d *stmt, llvm::Value *&Result);
  bool EmitBuiltinExtendPointer(gimple_statement_d *stmt, llvm::Value *&Result);
  bool EmitBuiltinExtractReturnAddr(gimple_statement_d *stmt,
                                    llvm::Value *&Result);
  bool EmitBuiltinFrobReturnAddr(gimple_statement_d *stmt,
                                 llvm::Value *&Result);
  bool EmitBuiltinInitTrampoline(gimple_statement_d *stmt, bool OnStack);
  bool EmitBuiltinMemCopy(gimple_statement_d *stmt, llvm::Value *&Result,
                          bool isMemMove, bool SizeCheck);
  bool EmitBuiltinMemSet(gimple_statement_d *stmt, llvm::Value *&Result,
                         bool SizeCheck);
  bool EmitBuiltinPrefetch(gimple_statement_d *stmt);
  bool EmitBuiltinReturnAddr(gimple_statement_d *stmt, llvm::Value *&Result,
                             bool isFrame);
  bool EmitBuiltinStackRestore(gimple_statement_d *stmt);
  bool EmitBuiltinStackSave(gimple_statement_d *stmt, llvm::Value *&Result);
  bool EmitBuiltinUnreachable();
  bool EmitBuiltinVACopy(gimple_statement_d *stmt);
  bool EmitBuiltinVAEnd(gimple_statement_d *stmt);
  bool EmitBuiltinVAStart(gimple_statement_d *stmt);

  bool EmitBuiltinEHCopyValues(gimple_statement_d *stmt);
  bool EmitBuiltinEHFilter(gimple_statement_d *stmt, llvm::Value *&Result);
  bool EmitBuiltinEHPointer(gimple_statement_d *stmt, llvm::Value *&Result);
  bool EmitBuiltinDwarfCFA(gimple_statement_d *stmt, llvm::Value *&Result);
  bool EmitBuiltinDwarfSPColumn(gimple_statement_d *stmt, llvm::Value *&Result);
  bool EmitBuiltinEHReturnDataRegno(gimple_statement_d *stmt,
                                    llvm::Value *&Result);
  bool EmitBuiltinEHReturn(gimple_statement_d *stmt, llvm::Value *&Result);
  bool EmitBuiltinInitDwarfRegSizes(gimple_statement_d *stmt,
                                    llvm::Value *&Result);
  bool EmitBuiltinUnwindInit(gimple_statement_d *stmt, llvm::Value *&Result);

  // Complex Math Expressions.
  llvm::Value *CreateComplex(llvm::Value *Real, llvm::Value *Imag);
  void SplitComplex(llvm::Value *Complex, llvm::Value *&Real,
                    llvm::Value *&Imag);

  // L-Value Expressions.
  LValue EmitLV_ARRAY_REF(tree_node *exp);
  LValue EmitLV_BIT_FIELD_REF(tree_node *exp);
  LValue EmitLV_COMPONENT_REF(tree_node *exp);
  LValue EmitLV_DECL(tree_node *exp);
  LValue EmitLV_INDIRECT_REF(tree_node *exp);
#if (GCC_MINOR > 5)
  LValue EmitLV_MEM_REF(tree_node *exp);
#endif
#if (GCC_MINOR < 6)
  LValue EmitLV_MISALIGNED_INDIRECT_REF(tree_node *exp);
#endif
  LValue EmitLV_VIEW_CONVERT_EXPR(tree_node *exp);
  LValue EmitLV_WITH_SIZE_EXPR(tree_node *exp);
  LValue EmitLV_XXXXPART_EXPR(tree_node *exp, unsigned Idx);
  LValue EmitLV_SSA_NAME(tree_node *exp);
  LValue EmitLV_TARGET_MEM_REF(tree_node *exp);

  // Constant Expressions.
  llvm::Value *EmitINTEGER_CST(tree_node *exp);
  llvm::Value *EmitREAL_CST(tree_node *exp);
  llvm::Value *EmitCONSTRUCTOR(tree_node *exp, const MemRef *DestLoc);

  // Emit helpers.

  /// EmitMinInvariant - The given value is constant in this function.  Return
  /// the corresponding LLVM value. Only creates code in the entry block.
  llvm::Value *EmitMinInvariant(tree_node *reg);

  /// EmitInvariantAddress - The given address is constant in this function.
  /// Return the corresponding LLVM value. Only creates code in the entry block.
  llvm::Value *EmitInvariantAddress(tree_node *addr);

  /// EmitRegisterConstant - Convert the given global constant of register type
  /// to an LLVM constant.  Creates no code, only constants.
  llvm::Constant *EmitRegisterConstant(tree_node *reg);

  /// EmitRegisterConstantWithCast - Utility that casts the value returned by
  /// EmitRegisterConstant to the given register type.
  llvm::Constant *EmitRegisterConstantWithCast(tree_node *reg, tree_node *type);

  /// EmitComplexRegisterConstant - Turn the given COMPLEX_CST into an LLVM
  /// constant of the corresponding register type.
  llvm::Constant *EmitComplexRegisterConstant(tree_node *reg);

  /// EmitIntegerRegisterConstant - Turn the given INTEGER_CST into an LLVM
  /// constant of the corresponding register type.
  llvm::Constant *EmitIntegerRegisterConstant(tree_node *reg);

  /// EmitRealRegisterConstant - Turn the given REAL_CST into an LLVM constant
  /// of the corresponding register type.
  llvm::Constant *EmitRealRegisterConstant(tree_node *reg);

  /// EmitConstantVectorConstructor - Turn the given constant CONSTRUCTOR into
  /// an LLVM constant of the corresponding vector register type.
  llvm::Constant *EmitConstantVectorConstructor(tree_node *reg);

  /// EmitVectorRegisterConstant - Turn the given VECTOR_CST into an LLVM
  /// constant of the corresponding register type.
  llvm::Constant *EmitVectorRegisterConstant(tree_node *reg);

  /// EmitMemory - Convert the specified gimple register or local constant of
  /// register type to an LLVM value with in-memory type (given by ConvertType).
  /// TODO: Eliminate this method, see Mem2Reg and Reg2Mem above.
  llvm::Value *EmitMemory(tree_node *reg);

  /// VectorHighElements - Return a vector of half the length, consisting of the
  /// elements of the given vector with indices in the top half.
  llvm::Value *VectorHighElements(llvm::Value *Vec);

  /// VectorLowElements - Return a vector of half the length, consisting of the
  /// elements of the given vector with indices in the bottom half.
  llvm::Value *VectorLowElements(llvm::Value *Vec);

private:
  // Optional target defined builtin intrinsic expanding function.
  bool TargetIntrinsicLower(gimple_statement_d *stmt, tree_node *fndecl,
                            const MemRef *DestLoc, llvm::Value *&Result,
                            llvm::Type *ResultType,
                            std::vector<llvm::Value *> &Ops);

public:
  // Helper for taking the address of a label.
  llvm::Constant *AddressOfLABEL_DECL(tree_node *exp);
};

#endif /* DRAGONEGG_INTERNALS_H */
