//===------------- Convert.cpp - Converting gimple to LLVM IR -------------===//
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
// This is the code that converts GCC AST nodes into LLVM code.
//===----------------------------------------------------------------------===//

// Plugin headers
#include "dragonegg/ABI.h"
#include "dragonegg/Constants.h"
#include "dragonegg/Debug.h"
#include "dragonegg/Trees.h"

// LLVM headers
#include "llvm/Module.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Host.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"

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

#include "diagnostic.h"
#include "except.h"
#include "flags.h"
#include "langhooks.h"
#include "output.h"
#include "rtl.h"
#include "tm_p.h"
#include "toplev.h"
#include "tree-flow.h"
#include "tree-pass.h"

extern int get_pointer_alignment (tree exp, unsigned int max_align);
extern enum machine_mode reg_raw_mode[FIRST_PSEUDO_REGISTER];
}

static LLVMContext &Context = getGlobalContext();

STATISTIC(NumBasicBlocks, "Number of basic blocks converted");
STATISTIC(NumStatements,  "Number of gimple statements converted");

/// getINTEGER_CSTVal - Return the specified INTEGER_CST value as a uint64_t.
///
uint64_t getINTEGER_CSTVal(tree exp) {
  unsigned HOST_WIDE_INT HI = (unsigned HOST_WIDE_INT)TREE_INT_CST_HIGH(exp);
  unsigned HOST_WIDE_INT LO = (unsigned HOST_WIDE_INT)TREE_INT_CST_LOW(exp);
  if (HOST_BITS_PER_WIDE_INT == 64) {
    return (uint64_t)LO;
  } else {
    assert(HOST_BITS_PER_WIDE_INT == 32 &&
           "Only 32- and 64-bit hosts supported!");
    return ((uint64_t)HI << 32) | (uint64_t)LO;
  }
}

/// isInt64 - Return true if t is an INTEGER_CST that fits in a 64 bit integer.
/// If Unsigned is false, returns whether it fits in a int64_t.  If Unsigned is
/// true, returns whether the value is non-negative and fits in a uint64_t.
/// Always returns false for overflowed constants.
bool isInt64(tree t, bool Unsigned) {
  if (!t)
    return false;
  if (HOST_BITS_PER_WIDE_INT == 64)
    return host_integerp(t, Unsigned) && !TREE_OVERFLOW (t);
  assert(HOST_BITS_PER_WIDE_INT == 32 &&
         "Only 32- and 64-bit hosts supported!");
  return
    (TREE_CODE (t) == INTEGER_CST && !TREE_OVERFLOW (t))
    && ((TYPE_UNSIGNED(TREE_TYPE(t)) == Unsigned) ||
        // If the constant is signed and we want an unsigned result, check
        // that the value is non-negative.  If the constant is unsigned and
        // we want a signed result, check it fits in 63 bits.
        (HOST_WIDE_INT)TREE_INT_CST_HIGH(t) >= 0);
}

/// getInt64 - Extract the value of an INTEGER_CST as a 64 bit integer.  If
/// Unsigned is false, the value must fit in a int64_t.  If Unsigned is true,
/// the value must be non-negative and fit in a uint64_t.  Must not be used on
/// overflowed constants.  These conditions can be checked by calling isInt64.
uint64_t getInt64(tree t, bool Unsigned) {
  assert(isInt64(t, Unsigned) && "invalid constant!");
  (void)Unsigned; // Otherwise unused if asserts off - avoid compiler warning.
  return getINTEGER_CSTVal(t);
}

/// getPointerAlignment - Return the alignment in bytes of exp, a pointer valued
/// expression, or 1 if the alignment is not known.
static unsigned int getPointerAlignment(tree exp) {
  assert(POINTER_TYPE_P (TREE_TYPE (exp)) && "Expected a pointer type!");
  unsigned int align = get_pointer_alignment(exp, BIGGEST_ALIGNMENT) / 8;
  return align ? align : 1;
}

/// getSSAPlaceholder - A fake value associated with an SSA name when the name
/// is used before being defined (this can occur because basic blocks are not
/// output in dominator order).  Replaced with the correct value when the SSA
/// name's definition is encountered.
static Value *GetSSAPlaceholder(const Type *Ty) {
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
/// point type is float, double or long double.
static StringRef SelectFPName(tree type, StringRef FloatName,
                              StringRef DoubleName, StringRef LongDoubleName) {
  assert(SCALAR_FLOAT_TYPE_P(type) && "Expected a floating point type!");
  if (TYPE_MODE(type) == TYPE_MODE(float_type_node))
    return FloatName;
  if (TYPE_MODE(type) == TYPE_MODE(double_type_node))
    return DoubleName;
  assert(TYPE_MODE(type) == TYPE_MODE(long_double_type_node) &&
         "Unknown floating point type!");
  return LongDoubleName;
}


//===----------------------------------------------------------------------===//
//                         ... High-Level Methods ...
//===----------------------------------------------------------------------===//

/// TheTreeToLLVM - Keep track of the current function being compiled.
TreeToLLVM *TheTreeToLLVM = 0;

const TargetData &getTargetData() {
  return *TheTarget->getTargetData();
}

/// EmitDebugInfo - Return true if debug info is to be emitted for current
/// function.
bool TreeToLLVM::EmitDebugInfo() {
  if (TheDebugInfo && !DECL_IGNORED_P(getFUNCTION_DECL()))
    return true;
  return false;
}

TreeToLLVM::TreeToLLVM(tree fndecl) :
    TD(getTargetData()), Builder(Context, *TheFolder) {
  FnDecl = fndecl;
  AllocaInsertionPoint = 0;
  Fn = 0;
  ReturnBB = 0;
  ReturnOffset = 0;
  RewindBB = 0;
  RewindTmp = 0;

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
  assert(HAS_RTL_P(decl) && "Expected a declaration with RTL!");
  return DECL_CONTEXT(decl) == current_function_decl &&
    !TREE_STATIC(decl) && // Static variables not considered local.
    TREE_CODE(decl) != FUNCTION_DECL; // Nested functions not considered local.
}

/// set_decl_local - Remember the LLVM value for a GCC declaration.
Value *TreeToLLVM::set_decl_local(tree decl, Value *V) {
  if (!isLocalDecl(decl))
    return set_decl_llvm(decl, V);
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
    DieAbjectly("Unhandled local declaration!", decl);

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
                                       const llvm::Type *LLVMTy,
                                       unsigned RealSize,
                                       LLVMBuilder &Builder) {
  if (RealSize) {
    // Not clear what this is supposed to do on big endian machines...
    assert(!BYTES_BIG_ENDIAN && "Unsupported case - please report");
    // Do byte wise store because actual argument type does not match LLVMTy.
    assert(ArgVal->getType()->isIntegerTy() && "Expected an integer value!");
    const Type *StoreType = IntegerType::get(Context, RealSize * 8);
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
    Builder.CreateStore(ArgVal, Loc);
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
    unsigned Offset;
    CallingConv::ID &CallingConv;
    bool isShadowRet;
    FunctionPrologArgumentConversion(tree FnDecl,
                                     Function::arg_iterator &ai,
                                     const LLVMBuilder &B, CallingConv::ID &CC)
      : FunctionDecl(FnDecl), AI(ai), Builder(B), Offset(0), CallingConv(CC),
        isShadowRet(false) {}

    /// getCallingConv - This provides the desired CallingConv for the function.
    CallingConv::ID& getCallingConv(void) { return CallingConv; }

    void HandlePad(const llvm::Type * /*LLVMTy*/) {
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

    void HandleAggregateShadowResult(const PointerType * /*PtrArgTy*/,
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
      assert(TREE_CODE(TREE_TYPE(ResultDecl)) == REFERENCE_TYPE &&
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

    void HandleScalarShadowResult(const PointerType * /*PtrArgTy*/,
                                  bool /*RetPtr*/) {
      assert(AI != Builder.GetInsertBlock()->getParent()->arg_end() &&
             "No explicit return value?");
      AI->setName("scalar.result");
      isShadowRet = true;
      TheTreeToLLVM->set_decl_local(DECL_RESULT(FunctionDecl), AI);
      ++AI;
    }

    void HandleScalarArgument(const llvm::Type *LLVMTy, tree /*type*/,
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
          ArgVal = Builder.CreateFPTrunc(ArgVal, LLVMTy,
                                         NameStack.back().c_str());
        } else {
          // If this is just a mismatch between integer types, this is due
          // to K&R prototypes, where the forward proto defines the arg as int
          // and the actual impls is a short or char.
          assert(ArgVal->getType()->isIntegerTy(32) && LLVMTy->isIntegerTy() &&
                 "Lowerings don't match?");
          ArgVal = Builder.CreateTrunc(ArgVal, LLVMTy,NameStack.back().c_str());
        }
      }
      assert(!LocStack.empty());
      Value *Loc = LocStack.back();
      LLVM_STORE_SCALAR_ARGUMENT(Loc,ArgVal,LLVMTy,RealSize,Builder);
      AI->setName(NameStack.back());
      ++AI;
    }

    void HandleByValArgument(const llvm::Type * /*LLVMTy*/, tree type) {
      if (LLVM_BYVAL_ALIGNMENT_TOO_SMALL(type)) {
        // Incoming object on stack is insufficiently aligned for the type.
        // Make a correctly aligned copy.
        assert(!LocStack.empty());
        Value *Loc = LocStack.back();
        // We cannot use field-by-field copy here; x86 long double is 16
        // bytes, but only 10 are copied.  If the object is really a union
        // we might need the other bytes.  We must also be careful to use
        // the smaller alignment.
        const Type *SBP = Type::getInt8PtrTy(Context);
        const Type *IntPtr = getTargetData().getIntPtrType(Context);
        Value *Ops[5] = {
          Builder.CreateCast(Instruction::BitCast, Loc, SBP),
          Builder.CreateCast(Instruction::BitCast, AI, SBP),
          ConstantInt::get(IntPtr,
                           TREE_INT_CST_LOW(TYPE_SIZE_UNIT(type))),
          ConstantInt::get(Type::getInt32Ty(Context), 
                           LLVM_BYVAL_ALIGNMENT(type)),
          ConstantInt::get(Type::getInt1Ty(Context), false)
        };
        const Type *ArgTypes[3] = {SBP, SBP, IntPtr };
        Builder.CreateCall(Intrinsic::getDeclaration(TheModule, 
                                                     Intrinsic::memcpy,
                                                     ArgTypes, 3), Ops, Ops+5);

        AI->setName(NameStack.back());
      }
      ++AI;
    }

    void HandleFCAArgument(const llvm::Type * /*LLVMTy*/, tree /*type*/) {
      // Store the FCA argument into alloca.
      assert(!LocStack.empty());
      Value *Loc = LocStack.back();
      Builder.CreateStore(AI, Loc);
      AI->setName(NameStack.back());
      ++AI;
    }

    void HandleAggregateResultAsScalar(const Type * /*ScalarTy*/,
                                       unsigned Offset = 0) {
      this->Offset = Offset;
    }

    void EnterField(unsigned FieldNo, const llvm::Type *StructTy) {
      NameStack.push_back(NameStack.back()+"."+utostr(FieldNo));

      Value *Loc = LocStack.back();
      // This cast only involves pointers, therefore BitCast.
      Loc = Builder.CreateBitCast(Loc, StructTy->getPointerTo());

      Loc = Builder.CreateStructGEP(Loc, FieldNo);
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
static bool isPassedByVal(tree type, const Type *Ty,
                          std::vector<const Type*> &ScalarArgs,
                          bool isShadowRet, CallingConv::ID &/*CC*/) {
  if (LLVM_SHOULD_PASS_AGGREGATE_USING_BYVAL_ATTR(type, Ty))
    return true;

  std::vector<const Type*> Args;
  if (LLVM_SHOULD_PASS_AGGREGATE_IN_MIXED_REGS(type, Ty, CC, Args) &&
      LLVM_AGGREGATE_PARTIALLY_PASSED_IN_REGS(Args, ScalarArgs, isShadowRet,
                                              CC))
    // We want to pass the whole aggregate in registers but only some of the
    // registers are available.
    return true;
  return false;
}

void TreeToLLVM::StartFunctionBody() {
  std::string Name = getLLVMAssemblerName(FnDecl).str();
  // TODO: Add support for dropping the leading '\1' in order to support
  //   unsigned bswap(unsigned) __asm__("llvm.bswap");
  // This would also require adjustments in make_decl_llvm.

  // Determine the FunctionType and calling convention for this function.
  tree static_chain = cfun->static_chain_decl;
  const FunctionType *FTy;
  CallingConv::ID CallingConv;
  AttrListPtr PAL;

  bool getFunctionTypeFromArgList = false;

  // If the function has no arguments and is varargs (...), turn it into a
  // non-varargs function by scanning the param list for the function.  This
  // allows C functions declared as "T foo() {}" to be treated like
  // "T foo(void) {}" and allows us to handle functions with K&R-style
  // definitions correctly.
  //
  // Note that we only do this in C/Objective-C.  Doing this in C++ for
  // functions explicitly declared as taking (...) is bad.
  if (TYPE_ARG_TYPES(TREE_TYPE(FnDecl)) == 0 && flag_vararg_requires_arguments)
    getFunctionTypeFromArgList = true;

  // When forcing vararg prototypes ensure that the function only gets a varargs
  // part if it was originally declared varargs.
  if (flag_force_vararg_prototypes) {
    tree Args = TYPE_ARG_TYPES(TREE_TYPE(FnDecl));
    while (Args && TREE_VALUE(Args) != void_type_node)
      Args = TREE_CHAIN(Args);
    if (Args != 0)
      getFunctionTypeFromArgList = true;
  }

  if (getFunctionTypeFromArgList)
    FTy = TheTypeConverter->ConvertArgListToFnType(TREE_TYPE(FnDecl),
                                                   DECL_ARGUMENTS(FnDecl),
                                                   static_chain,
                                                   CallingConv, PAL);
  else
    // Otherwise, just get the type from the function itself.
    FTy = TheTypeConverter->ConvertFunctionType(TREE_TYPE(FnDecl),
                                                FnDecl,
                                                static_chain,
                                                CallingConv, PAL);

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

  // Handle visibility style
  handleVisibility(FnDecl, Fn);

  // Register constructors and destructors.
  if (DECL_STATIC_CONSTRUCTOR(FnDecl))
    register_ctor_dtor(Fn, DECL_INIT_PRIORITY(FnDecl), true);
  if (DECL_STATIC_DESTRUCTOR(FnDecl))
    register_ctor_dtor(Fn, DECL_FINI_PRIORITY(FnDecl), false);

  // Handle attribute "aligned".
  if (DECL_ALIGN (FnDecl) != FUNCTION_BOUNDARY)
    Fn->setAlignment(DECL_ALIGN (FnDecl) / 8);

  // Handle functions in specified sections.
  if (DECL_SECTION_NAME(FnDecl))
    Fn->setSection(TREE_STRING_POINTER(DECL_SECTION_NAME(FnDecl)));

  // Handle used Functions
  if (lookup_attribute ("used", DECL_ATTRIBUTES (FnDecl)))
    AttributeUsedGlobals.insert(Fn);

  // Handle noinline Functions
  if (lookup_attribute ("noinline", DECL_ATTRIBUTES (FnDecl)))
    Fn->addFnAttr(Attribute::NoInline);

  // Handle always_inline attribute
  if (lookup_attribute ("always_inline", DECL_ATTRIBUTES (FnDecl)))
    Fn->addFnAttr(Attribute::AlwaysInline);

  // Pass inline keyword to optimizer.
  if (DECL_DECLARED_INLINE_P(FnDecl))
    Fn->addFnAttr(Attribute::InlineHint);

  if (optimize_size)
    Fn->addFnAttr(Attribute::OptimizeForSize);

  // Handle stack smashing protection.
  if (flag_stack_protect == 1)
    Fn->addFnAttr(Attribute::StackProtect);
  else if (flag_stack_protect == 2)
    Fn->addFnAttr(Attribute::StackProtectReq);

  // Handle naked attribute
  if (lookup_attribute ("naked", DECL_ATTRIBUTES (FnDecl)))
    Fn->addFnAttr(Attribute::Naked);

  // Handle annotate attributes
  if (DECL_ATTRIBUTES(FnDecl))
    AddAnnotateAttrsToGlobal(Fn, FnDecl);

  // Mark the function "nounwind" if not doing exception handling.
  if (!flag_exceptions)
    Fn->setDoesNotThrow();

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
  std::vector<const Type*> ScalarArgs;
  while (Args) {
    const char *Name = "unnamed_arg";
    if (DECL_NAME(Args)) Name = IDENTIFIER_POINTER(DECL_NAME(Args));

    const Type *ArgTy = ConvertType(TREE_TYPE(Args));
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
      if (POINTER_TYPE_P(TREE_TYPE(Args))
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

  // Create a new block for the return node, but don't insert it yet.
  ReturnBB = BasicBlock::Create(Context, "return");
}

/// DefineSSAName - Use the given value as the definition of the given SSA name.
/// Returns the provided value as a convenience.
Value *TreeToLLVM::DefineSSAName(tree reg, Value *Val) {
  assert(TREE_CODE(reg) == SSA_NAME && "Not an SSA name!");
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

  for (unsigned i = 0, e = PendingPhis.size(); i < e; ++i) {
    // The phi node to process.
    PhiRecord &P = PendingPhis[i];

    // Extract the incoming value for each predecessor from the GCC phi node.
    for (size_t i = 0, e = gimple_phi_num_args(P.gcc_phi); i != e; ++i) {
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
    PhiArguments.resize(Predecessors.size());
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
  // Insert the return block at the end of the function.
  BeginBlock(ReturnBB);

  SmallVector <Value *, 4> RetVals;

  // If the function returns a value, get it into a register and return it now.
  if (!Fn->getReturnType()->isVoidTy()) {
    tree TreeRetVal = DECL_RESULT(FnDecl);
    if (!AGGREGATE_TYPE_P(TREE_TYPE(TreeRetVal)) &&
        TREE_CODE(TREE_TYPE(TreeRetVal)) != COMPLEX_TYPE) {
      // If the DECL_RESULT is a scalar type, just load out the return value
      // and return it.
      Value *RetVal = Builder.CreateLoad(DECL_LOCAL(TreeRetVal), "retval");
      RetVal = Builder.CreateBitCast(RetVal, Fn->getReturnType());
      RetVals.push_back(RetVal);
    } else {
      Value *RetVal = DECL_LOCAL(TreeRetVal);
      if (const StructType *STy = dyn_cast<StructType>(Fn->getReturnType())) {
        Value *R1 = Builder.CreateBitCast(RetVal, STy->getPointerTo());

        llvm::Value *Idxs[2];
        Idxs[0] = ConstantInt::get(llvm::Type::getInt32Ty(Context), 0);
        for (unsigned ri = 0; ri < STy->getNumElements(); ++ri) {
          Idxs[1] = ConstantInt::get(llvm::Type::getInt32Ty(Context), ri);
          Value *GEP = Builder.CreateGEP(R1, Idxs, Idxs+2, "mrv_gep");
          Value *E = Builder.CreateLoad(GEP, "mrv");
          RetVals.push_back(E);
        }
        // If the return type specifies an empty struct then return one.
        if (RetVals.empty())
          RetVals.push_back(UndefValue::get(Fn->getReturnType()));
      } else {
        // Otherwise, this aggregate result must be something that is returned
        // in a scalar register for this target.  We must bit convert the
        // aggregate to the specified scalar type, which we do by casting the
        // pointer and loading.  The load does not necessarily start at the
        // beginning of the aggregate (x86-64).
        if (ReturnOffset) {
          RetVal = Builder.CreateBitCast(RetVal, Type::getInt8PtrTy(Context));
          RetVal = Builder.CreateGEP(RetVal,
                     ConstantInt::get(TD.getIntPtrType(Context), ReturnOffset));
        }
        RetVal = Builder.CreateBitCast(RetVal,
                                       Fn->getReturnType()->getPointerTo());
        RetVal = Builder.CreateLoad(RetVal, "retval");
        RetVals.push_back(RetVal);
      }
    }
  }
  if (RetVals.empty())
    Builder.CreateRetVoid();
  else if (RetVals.size() == 1 && RetVals[0]->getType() == Fn->getReturnType()){
    Builder.CreateRet(RetVals[0]);
  } else {
    assert(Fn->getReturnType()->isAggregateType() && "Return type mismatch!");
    Builder.CreateAggregateRet(RetVals.data(), RetVals.size());
  }

  // Populate phi nodes with their operands now that all ssa names have been
  // defined and all basic blocks output.
  PopulatePhiNodes();

  // Now that phi nodes have been output, emit pending exception handling code.
  EmitLandingPads();
  EmitFailureBlocks();
  EmitRewindBlock();

  if (EmitDebugInfo()) {
    // FIXME: This should be output just before the return call generated above.
    // But because EmitFunctionEnd pops the region stack, that means that if the
    // call to PopulatePhiNodes (for example) generates complicated debug info,
    // then the debug info logic barfs.  Testcases showing this are 20011126-2.c
    // or pr42221.c from the gcc testsuite compiled with -g -O3.
    TheDebugInfo->EmitStopPoint(ReturnBB, Builder);
    TheDebugInfo->EmitFunctionEnd(true);
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
        DieAbjectly("SSA name never defined!", I->first);
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
  assert(TREE_CODE(LabelDecl) == LABEL_DECL && "Isn't a label!?");
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
    const Type *Ty = GetRegType(TREE_TYPE(gimple_phi_result(gcc_phi)));
    PHINode *PHI = Builder.CreatePHI(Ty, gimple_phi_num_args(gcc_phi));

    // The phi defines the associated ssa name.
    tree name = gimple_phi_result(gcc_phi);
    assert(TREE_CODE(name) == SSA_NAME && "PHI result not an SSA name!");
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
      DieAbjectly("Unhandled GIMPLE statement during LLVM emission!", stmt);

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
      Builder.CreateBr(getBasicBlock(e->dest));
      break;
    }
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
  assert(AGGREGATE_TYPE_P(TREE_TYPE(exp)) && "Expected an aggregate type!");
  if (TREE_CODE(exp) == CONSTRUCTOR) {
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
    DieAbjectly("Unhandled lvalue expression!", exp);

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
  case TARGET_MEM_REF:
    LV = EmitLV_TARGET_MEM_REF(exp);
    break;

  // Constants.
  case LABEL_DECL: {
    LV = LValue(AddressOfLABEL_DECL(exp), 1);
    break;
  }
  case COMPLEX_CST:
  case REAL_CST:
  case STRING_CST: {
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
  }

  // Check that the type of the lvalue is indeed that of a pointer to the tree
  // node.  This may not hold for bitfields because the type of a bitfield need
  // not match the type of the value being loaded out of it.  Since LLVM has no
  // void* type, don't insist that void* be converted to a specific LLVM type.
  assert((LV.isBitfield() || VOID_TYPE_P(TREE_TYPE(exp)) ||
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
                                 const Type* DestTy, bool DestIsSigned) {
  const Type *SrcTy = V->getType();

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
      const Type *IntTy = IntegerType::get(Context, SrcBits);
      V = Builder.CreateBitCast(V, IntTy);
      return CastToAnyType(V, VisSigned, DestTy, DestIsSigned);
    }
    if (DestBits && !isa<IntegerType>(DestTy)) {
      const Type *IntTy = IntegerType::get(Context, DestBits);
      V = CastToAnyType(V, VisSigned, IntTy, DestIsSigned);
      return Builder.CreateBitCast(V, DestTy);
    }
    assert(false && "Unable to cast between these types!");
  }

  // The types are different so we must cast. Use getCastOpcode to create an
  // inferred cast opcode.
  Instruction::CastOps opc =
    CastInst::getCastOpcode(V, VisSigned, DestTy, DestIsSigned);

  // Generate the cast and return it.
  return Builder.CreateCast(opc, V, DestTy);
}

/// CastToFPType - Cast the specified value to the specified type assuming
/// that the value and type are floating point.
Value *TreeToLLVM::CastToFPType(Value *V, const Type* Ty) {
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
  if (FLOAT_TYPE_P(type))
    return Builder.CreateFAdd(LHS, RHS);
  return Builder.CreateAdd(LHS, RHS, "", hasNUW(type), hasNSW(type));
}

/// CreateAnyMul - Multiply two LLVM scalar values with the given GCC type.
/// Does not support complex numbers.  The type is used to set overflow flags.
Value *TreeToLLVM::CreateAnyMul(Value *LHS, Value *RHS, tree type) {
  if (FLOAT_TYPE_P(type))
    return Builder.CreateFMul(LHS, RHS);
  return Builder.CreateMul(LHS, RHS, "", hasNUW(type), hasNSW(type));
}

/// CreateAnyNeg - Negate an LLVM scalar value with the given GCC type.  Does
/// not support complex numbers.  The type is used to set overflow flags.
Value *TreeToLLVM::CreateAnyNeg(Value *V, tree type) {
  if (FLOAT_TYPE_P(type))
    return Builder.CreateFNeg(V);
  return Builder.CreateNeg(V, "", hasNUW(type), hasNSW(type));
}

/// CreateAnySub - Subtract two LLVM scalar values with the given GCC type.
/// Does not support complex numbers.  The type is used to set overflow flags.
Value *TreeToLLVM::CreateAnySub(Value *LHS, Value *RHS, tree type) {
  if (FLOAT_TYPE_P(type))
    return Builder.CreateFSub(LHS, RHS);
  return Builder.CreateSub(LHS, RHS, "", hasNUW(type), hasNSW(type));
}

/// CreateTemporary - Create a new alloca instruction of the specified type,
/// inserting it into the entry block and returning it.  The resulting
/// instruction's type is a pointer to the specified type.
AllocaInst *TreeToLLVM::CreateTemporary(const Type *Ty, unsigned align) {
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
  return new AllocaInst(Ty, 0, align, "memtmp", AllocaInsertionPoint);
}

/// CreateTempLoc - Like CreateTemporary, but returns a MemRef.
MemRef TreeToLLVM::CreateTempLoc(const Type *Ty) {
  AllocaInst *AI = CreateTemporary(Ty);
  // MemRefs do not allow alignment 0.
  if (!AI->getAlignment())
    AI->setAlignment(TD.getPrefTypeAlignment(Ty));
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

/// CopyAggregate - Recursively traverse the potientially aggregate src/dest
/// ptrs, copying all of the elements.
static void CopyAggregate(MemRef DestLoc, MemRef SrcLoc,
                          LLVMBuilder &Builder, tree gccType) {
  assert(DestLoc.Ptr->getType() == SrcLoc.Ptr->getType() &&
         "Cannot copy between two pointers of different type!");
  const Type *ElTy =
    cast<PointerType>(DestLoc.Ptr->getType())->getElementType();

  unsigned Alignment = std::min(DestLoc.getAlignment(), SrcLoc.getAlignment());

  if (ElTy->isSingleValueType()) {
    LoadInst *V = Builder.CreateLoad(SrcLoc.Ptr, SrcLoc.Volatile);
    StoreInst *S = Builder.CreateStore(V, DestLoc.Ptr, DestLoc.Volatile);
    V->setAlignment(Alignment);
    S->setAlignment(Alignment);
  } else if (const StructType *STy = dyn_cast<StructType>(ElTy)) {
    const StructLayout *SL = getTargetData().getStructLayout(STy);
    for (unsigned i = 0, e = STy->getNumElements(); i != e; ++i) {
      if (gccType && isPaddingElement(gccType, i))
        continue;
      Value *DElPtr = Builder.CreateStructGEP(DestLoc.Ptr, i);
      Value *SElPtr = Builder.CreateStructGEP(SrcLoc.Ptr, i);
      unsigned Align = MinAlign(Alignment, SL->getElementOffset(i));
      CopyAggregate(MemRef(DElPtr, Align, DestLoc.Volatile),
                    MemRef(SElPtr, Align, SrcLoc.Volatile),
                    Builder, 0);
    }
  } else {
    const ArrayType *ATy = cast<ArrayType>(ElTy);
    unsigned EltSize = getTargetData().getTypeAllocSize(ATy->getElementType());
    for (unsigned i = 0, e = ATy->getNumElements(); i != e; ++i) {
      Value *DElPtr = Builder.CreateStructGEP(DestLoc.Ptr, i);
      Value *SElPtr = Builder.CreateStructGEP(SrcLoc.Ptr, i);
      unsigned Align = MinAlign(Alignment, i * EltSize);
      CopyAggregate(MemRef(DElPtr, Align, DestLoc.Volatile),
                    MemRef(SElPtr, Align, SrcLoc.Volatile),
                    Builder, 0);
    }
  }
}

/// CountAggregateElements - Return the number of elements in the specified type
/// that will need to be loaded/stored if we copy this by explicit accesses.
static unsigned CountAggregateElements(const Type *Ty) {
  if (Ty->isSingleValueType()) return 1;

  if (const StructType *STy = dyn_cast<StructType>(Ty)) {
    unsigned NumElts = 0;
    for (unsigned i = 0, e = STy->getNumElements(); i != e; ++i)
      NumElts += CountAggregateElements(STy->getElementType(i));
    return NumElts;
  } else {
    const ArrayType *ATy = cast<ArrayType>(Ty);
    return ATy->getNumElements()*CountAggregateElements(ATy->getElementType());
  }
}

/// containsFPField - indicates whether the given LLVM type
/// contains any floating point elements.

static bool containsFPField(const Type *LLVMTy) {
  if (LLVMTy->isFloatingPointTy())
    return true;
  const StructType* STy = dyn_cast<StructType>(LLVMTy);
  if (STy) {
    for (StructType::element_iterator I = STy->element_begin(),
                                      E = STy->element_end(); I != E; I++) {
      const Type *Ty = *I;
      if (Ty->isFloatingPointTy())
        return true;
      if (Ty->isStructTy() && containsFPField(Ty))
        return true;
      const ArrayType *ATy = dyn_cast<ArrayType>(Ty);
      if (ATy && containsFPField(ATy->getElementType()))
        return true;
      const VectorType *VTy = dyn_cast<VectorType>(Ty);
      if (VTy && containsFPField(VTy->getElementType()))
        return true;
    }
  }
  return false;
}

#ifndef TARGET_LLVM_MIN_BYTES_COPY_BY_MEMCPY
#define TARGET_LLVM_MIN_BYTES_COPY_BY_MEMCPY 64
#endif

/// EmitAggregateCopy - Copy the elements from SrcLoc to DestLoc, using the
/// GCC type specified by GCCType to know which elements to copy.
void TreeToLLVM::EmitAggregateCopy(MemRef DestLoc, MemRef SrcLoc, tree type) {
  if (DestLoc.Ptr == SrcLoc.Ptr && !DestLoc.Volatile && !SrcLoc.Volatile)
    return;  // noop copy.

  // If the type is small, copy the elements instead of using a block copy.
  const Type *LLVMTy = ConvertType(type);
  unsigned NumElts = CountAggregateElements(LLVMTy);
  if (TREE_CODE(TYPE_SIZE(type)) == INTEGER_CST &&
      (NumElts == 1 ||
       TREE_INT_CST_LOW(TYPE_SIZE_UNIT(type)) <
       TARGET_LLVM_MIN_BYTES_COPY_BY_MEMCPY)) {

    // Some targets (x87) cannot pass non-floating-point values using FP
    // instructions.  The LLVM type for a union may include FP elements,
    // even if some of the union fields do not; it is unsafe to pass such
    // converted types element by element.  PR 2680.

    // If the GCC type is not fully covered by the LLVM type, use memcpy. This
    // can occur with unions etc.
    if ((TREE_CODE(type) != UNION_TYPE || !containsFPField(LLVMTy)) &&
        !TheTypeConverter->GCCTypeOverlapsWithLLVMTypePadding(type, LLVMTy) &&
        // Don't copy tons of tiny elements.
        NumElts <= 8) {
      DestLoc.Ptr = Builder.CreateBitCast(DestLoc.Ptr, LLVMTy->getPointerTo());
      SrcLoc.Ptr = Builder.CreateBitCast(SrcLoc.Ptr, LLVMTy->getPointerTo());
      CopyAggregate(DestLoc, SrcLoc, Builder, type);
      return;
    }
  }

  Value *TypeSize = EmitRegister(TYPE_SIZE_UNIT(type));
  EmitMemCpy(DestLoc.Ptr, SrcLoc.Ptr, TypeSize,
             std::min(DestLoc.getAlignment(), SrcLoc.getAlignment()));
}

/// ZeroAggregate - Recursively traverse the potentially aggregate DestLoc,
/// zero'ing all of the elements.
static void ZeroAggregate(MemRef DestLoc, LLVMBuilder &Builder) {
  const Type *ElTy =
    cast<PointerType>(DestLoc.Ptr->getType())->getElementType();
  if (ElTy->isSingleValueType()) {
    StoreInst *St = Builder.CreateStore(Constant::getNullValue(ElTy),
                                        DestLoc.Ptr, DestLoc.Volatile);
    St->setAlignment(DestLoc.getAlignment());
  } else if (const StructType *STy = dyn_cast<StructType>(ElTy)) {
    const StructLayout *SL = getTargetData().getStructLayout(STy);
    for (unsigned i = 0, e = STy->getNumElements(); i != e; ++i) {
      Value *Ptr = Builder.CreateStructGEP(DestLoc.Ptr, i);
      unsigned Alignment = MinAlign(DestLoc.getAlignment(),
                                    SL->getElementOffset(i));
      ZeroAggregate(MemRef(Ptr, Alignment, DestLoc.Volatile), Builder);
    }
  } else {
    const ArrayType *ATy = cast<ArrayType>(ElTy);
    unsigned EltSize = getTargetData().getTypeAllocSize(ATy->getElementType());
    for (unsigned i = 0, e = ATy->getNumElements(); i != e; ++i) {
      Value *Ptr = Builder.CreateStructGEP(DestLoc.Ptr, i);
      unsigned Alignment = MinAlign(DestLoc.getAlignment(), i * EltSize);
      ZeroAggregate(MemRef(Ptr, Alignment, DestLoc.Volatile), Builder);
    }
  }
}

/// EmitAggregateZero - Zero the elements of DestLoc.
void TreeToLLVM::EmitAggregateZero(MemRef DestLoc, tree type) {
  // If the type is small, copy the elements instead of using a block copy.
  if (TREE_CODE(TYPE_SIZE(type)) == INTEGER_CST &&
      TREE_INT_CST_LOW(TYPE_SIZE_UNIT(type)) < 128) {
    const Type *LLVMTy = ConvertType(type);

    // If the GCC type is not fully covered by the LLVM type, use memset. This
    // can occur with unions etc.
    if (!TheTypeConverter->GCCTypeOverlapsWithLLVMTypePadding(type, LLVMTy) &&
        // Don't zero tons of tiny elements.
        CountAggregateElements(LLVMTy) <= 8) {
      DestLoc.Ptr = Builder.CreateBitCast(DestLoc.Ptr, LLVMTy->getPointerTo());
      ZeroAggregate(DestLoc, Builder);
      return;
    }
  }

  EmitMemSet(DestLoc.Ptr, ConstantInt::get(Type::getInt8Ty(Context), 0),
             EmitRegister(TYPE_SIZE_UNIT(type)), DestLoc.getAlignment());
}

Value *TreeToLLVM::EmitMemCpy(Value *DestPtr, Value *SrcPtr, Value *Size,
                              unsigned Align) {
  const Type *SBP = Type::getInt8PtrTy(Context);
  const Type *IntPtr = TD.getIntPtrType(Context);
  Value *Ops[5] = {
    Builder.CreateBitCast(DestPtr, SBP),
    Builder.CreateBitCast(SrcPtr, SBP),
    Builder.CreateIntCast(Size, IntPtr, /*isSigned*/true),
    ConstantInt::get(Type::getInt32Ty(Context), Align),
    ConstantInt::get(Type::getInt1Ty(Context), false)
  };
  const Type *ArgTypes[3] = { SBP, SBP, IntPtr };

  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Intrinsic::memcpy,
                                               ArgTypes, 3), Ops, Ops+5);
  return Ops[0];
}

Value *TreeToLLVM::EmitMemMove(Value *DestPtr, Value *SrcPtr, Value *Size,
                               unsigned Align) {
  const Type *SBP = Type::getInt8PtrTy(Context);
  const Type *IntPtr = TD.getIntPtrType(Context);
  Value *Ops[5] = {
    Builder.CreateBitCast(DestPtr, SBP),
    Builder.CreateBitCast(SrcPtr, SBP),
    Builder.CreateIntCast(Size, IntPtr, /*isSigned*/true),
    ConstantInt::get(Type::getInt32Ty(Context), Align),
    ConstantInt::get(Type::getInt1Ty(Context), false)
  };
  const Type *ArgTypes[3] = { SBP, SBP, IntPtr };

  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Intrinsic::memmove,
                                               ArgTypes, 3), Ops, Ops+5);
  return Ops[0];
}

Value *TreeToLLVM::EmitMemSet(Value *DestPtr, Value *SrcVal, Value *Size,
                              unsigned Align) {
  const Type *SBP = Type::getInt8PtrTy(Context);
  const Type *IntPtr = TD.getIntPtrType(Context);
  Value *Ops[5] = {
    Builder.CreateBitCast(DestPtr, SBP),
    Builder.CreateIntCast(SrcVal, Type::getInt8Ty(Context), /*isSigned*/true),
    Builder.CreateIntCast(Size, IntPtr, /*isSigned*/true),
    ConstantInt::get(Type::getInt32Ty(Context), Align),
    ConstantInt::get(Type::getInt1Ty(Context), false)
  };
  const Type *ArgTypes[2] = { SBP, IntPtr };

  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Intrinsic::memset,
                                               ArgTypes, 2), Ops, Ops+5);
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
  const PointerType *Ty = Type::getInt8PtrTy(Context);
  V = Builder.CreateBitCast(V, Ty->getPointerTo());

  Value *Ops[2] = {
    V,
    ConstantPointerNull::get(Ty)
  };

  Builder.CreateCall(gcrootFun, Ops, Ops+2);
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
  const Type *SBP = Type::getInt8PtrTy(Context);
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
      assert(TREE_CODE(val) == STRING_CST &&
             "Annotate attribute arg should always be a string");
      const Type *SBP = Type::getInt8PtrTy(Context);
      Constant *strGV = AddressOf(val);
      Value *Ops[4] = {
        Builder.CreateBitCast(V, SBP),
        Builder.CreateBitCast(strGV, SBP),
        file,
        lineNo
      };

      Builder.CreateCall(annotateFun, Ops, Ops+4);
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
  if (TREE_CODE(decl) == VAR_DECL && DECL_VALUE_EXPR(decl))
    return;

  tree type = TREE_TYPE(decl);
  const Type *Ty;  // Type to allocate
  Value *Size = 0; // Amount to alloca (null for 1)

  if (DECL_SIZE(decl) == 0) {    // Variable with incomplete type.
    if (DECL_INITIAL(decl) == 0)
      return; // Error message was already done; now avoid a crash.
    else
      DieAbjectly("Initializer will decide the size of this array?", decl);
  } else if (TREE_CODE(DECL_SIZE_UNIT(decl)) == INTEGER_CST) {
    // Variable of fixed size that goes on the stack.
    Ty = ConvertType(type);
  } else {
    // Compute the variable's size in bytes.
    Size = EmitRegister(DECL_SIZE_UNIT(decl));
    Ty = Type::getInt8Ty(Context);
  }

  unsigned Alignment = 0; // Alignment in bytes.

  // Set the alignment for the local if one of the following condition is met
  // 1) DECL_ALIGN is better than the alignment as per ABI specification
  // 2) DECL_ALIGN is set by user.
  if (DECL_ALIGN(decl)) {
    unsigned TargetAlign = getTargetData().getABITypeAlignment(Ty);
    if (DECL_USER_ALIGN(decl) || 8 * TargetAlign < (unsigned)DECL_ALIGN(decl))
      Alignment = DECL_ALIGN(decl) / 8;
  }

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
  if (POINTER_TYPE_P(TREE_TYPE (decl))
      && lookup_attribute("gcroot", TYPE_ATTRIBUTES(TREE_TYPE (decl))))
    {
      // We should null out local variables so that a stack crawl
      // before initialization doesn't get garbage results to follow.
      const Type *T = cast<PointerType>(AI->getType())->getElementType();
      EmitTypeGcroot(AI);
      Builder.CreateStore(Constant::getNullValue(T), AI);
    }

  if (EmitDebugInfo()) {
    if (DECL_NAME(decl)) {
      TheDebugInfo->EmitDeclare(decl, dwarf::DW_TAG_auto_variable,
                                AI->getNameStr().c_str(), TREE_TYPE(decl), AI,
                                Builder);
    } else if (TREE_CODE(decl) == RESULT_DECL) {
      TheDebugInfo->EmitDeclare(decl, dwarf::DW_TAG_return_variable,
                                AI->getNameStr().c_str(), TREE_TYPE(decl), AI,
                                Builder);
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
  if (TYPE_P (type))
    type = lookup_type_for_runtime (type);
  STRIP_NOPS(type);
  if (TREE_CODE(type) == ADDR_EXPR)
    type = TREE_OPERAND(type, 0);
  return AddressOf(type);
}

/// getExceptionPtr - Return the local holding the exception pointer for the
/// given exception handling region, creating it if necessary.
AllocaInst *TreeToLLVM::getExceptionPtr(unsigned RegionNo) {
  if (RegionNo >= ExceptionPtrs.size())
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
AllocaInst *TreeToLLVM::getExceptionFilter(unsigned RegionNo) {
  if (RegionNo >= ExceptionFilters.size())
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
BasicBlock *TreeToLLVM::getFailureBlock(unsigned RegionNo) {
  if (RegionNo >= FailureBlocks.size())
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
  // eh region.  Calls to eh.exception and eh.selector will then go in the LLVM
  // landing pad, which branches to the GCC post landing pad.
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

  // Initialize the exception pointer and selector value for each exception
  // handling region at the start of the corresponding landing pad.  At this
  // point each exception handling region has its own landing pad, which is
  // only reachable via the unwind edges of the region's invokes.
  std::vector<Value*> Args;
  Function *ExcIntr = Intrinsic::getDeclaration(TheModule,
                                                Intrinsic::eh_exception);
  Function *SlctrIntr = Intrinsic::getDeclaration(TheModule,
                                                  Intrinsic::eh_selector);
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

    // Fetch the exception pointer.
    Value *ExcPtr = Builder.CreateCall(ExcIntr, "exc_ptr");

    // Store it if made use of elsewhere.
    if (RegionNo < ExceptionPtrs.size() && ExceptionPtrs[RegionNo])
      Builder.CreateStore(ExcPtr, ExceptionPtrs[RegionNo]);

    // Get the exception selector.  The first argument is the exception pointer.
    Args.push_back(ExcPtr);

    // It is followed by the personality function.
    tree personality = DECL_FUNCTION_PERSONALITY(FnDecl);
    if (!personality) {
      assert(function_needs_eh_personality(cfun) == eh_personality_any &&
             "No exception handling personality!");
      personality = lang_hooks.eh_personality();
    }
    Args.push_back(Builder.CreateBitCast(DECL_LLVM(personality),
                                         Type::getInt8PtrTy(Context)));

    Constant *CatchAll = TheModule->getGlobalVariable("llvm.eh.catch.all.value");
    if (!CatchAll) {
      // The representation of a catch-all is language specific.
      // TODO: Remove this hack.
      Constant *Init = 0;
      StringRef LanguageName = lang_hooks.name;
      if (LanguageName == "GNU Ada") {
        StringRef Name = "__gnat_all_others_value";
        Init = TheModule->getGlobalVariable(Name);
        if (!Init)
          Init = new GlobalVariable(*TheModule, ConvertType(integer_type_node),
                                    /*isConstant*/true,
                                    GlobalValue::ExternalLinkage,
                                    /*Initializer*/NULL, Name);
      } else {
        // Other languages use a null pointer.
        Init = Constant::getNullValue(Type::getInt8PtrTy(Context));
      }
      CatchAll = new GlobalVariable(*TheModule, Init->getType(), true,
                                    GlobalVariable::LinkOnceAnyLinkage,
                                    Init, "llvm.eh.catch.all.value");
      cast<GlobalVariable>(CatchAll)->setSection("llvm.metadata");
      AttributeUsedGlobals.insert(CatchAll);
    }

    bool AllCaught = false; // Did we saw a catch-all or no-throw?
    bool HasCleanup = false; // Did we see a cleanup?
    SmallSet<Constant *, 8> AlreadyCaught; // Typeinfos known caught already.
    for (; region && !AllCaught; region = region->outer)
      switch (region->type) {
      case ERT_ALLOWED_EXCEPTIONS: {
        // Filter.

        // Push a fake placeholder value for the length.  The real length is
        // computed below, once we know which typeinfos we are going to use.
        unsigned LengthIndex = Args.size();
        Args.push_back(NULL); // Fake length value.

        // Add the type infos.
        AllCaught = true;
        for (tree type = region->u.allowed.type_list; type;
             type = TREE_CHAIN(type)) {
          Constant *TypeInfo = ConvertTypeInfo(TREE_VALUE(type));
          // No point in permitting a typeinfo to be thrown if we know it can
          // never reach the filter.
          if (AlreadyCaught.count(TypeInfo))
            continue;
          Args.push_back(TypeInfo);
          AllCaught = false;
        }

        // The length is one more than the number of typeinfos.
        Args[LengthIndex] = ConstantInt::get(Type::getInt32Ty(Context),
                                             Args.size() - LengthIndex);
        break;
      }
      case ERT_CLEANUP:
        HasCleanup = true;
        break;
      case ERT_MUST_NOT_THROW:
        // Same as a zero-length filter.
        AllCaught = true;
        Args.push_back(ConstantInt::get(Type::getInt32Ty(Context), 1));
        break;
      case ERT_TRY:
        // Catches.
        for (eh_catch c = region->u.eh_try.first_catch; c ; c = c->next_catch)
          if (!c->type_list) {
            // Catch-all - push a null pointer.
            AllCaught = true;
            Args.push_back(Constant::getNullValue(Type::getInt8PtrTy(Context)));
          } else {
            // Add the type infos.
            for (tree type = c->type_list; type; type = TREE_CHAIN(type)) {
              Constant *TypeInfo = ConvertTypeInfo(TREE_VALUE(type));
              // No point in trying to catch a typeinfo that was already caught.
              if (!AlreadyCaught.insert(TypeInfo))
                continue;
              Args.push_back(TypeInfo);
              AllCaught = TypeInfo == CatchAll;
              if (AllCaught)
                break;
            }
          }
        break;
      }

    if (HasCleanup) {
      if (Args.size() == 2)
        // Insert a sentinel indicating that this is a cleanup-only selector.
        Args.push_back(ConstantInt::get(Type::getInt32Ty(Context), 0));
      else if (!AllCaught)
        // Some exceptions from this region may not be caught by any handler.
        // Since invokes are required to branch to the unwind label no matter
        // what exception is being unwound, append a catch-all.  I have a plan
        // that will make all such horrible hacks unnecessary, but unfortunately
        // this comment is too short to explain it.
        Args.push_back(CatchAll);
    }

    // Emit the selector call.
    Value *Filter = Builder.CreateCall(SlctrIntr, Args.begin(), Args.end(),
                                       "filter");

    // Store it if made use of elsewhere.
    if (RegionNo < ExceptionFilters.size() && ExceptionFilters[RegionNo])
      Builder.CreateStore(Filter, ExceptionFilters[RegionNo]);

    Args.clear();
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

      // Generate an empty (i.e. catch-all) filter in the landing pad.
      Function *ExcIntr = Intrinsic::getDeclaration(TheModule,
                                                    Intrinsic::eh_exception);
      Function *SlctrIntr = Intrinsic::getDeclaration(TheModule,
                                                      Intrinsic::eh_selector);
      Value *Args[3];
      // The exception pointer.
      Args[0] = Builder.CreateCall(ExcIntr, "exc_ptr");
      // The personality function.
      tree personality = DECL_FUNCTION_PERSONALITY(FnDecl);
      assert(personality && "No-throw region but no personality function!");
      Args[1] = Builder.CreateBitCast(DECL_LLVM(personality),
                                      Type::getInt8PtrTy(Context));
      // One more than the filter length.
      Args[2] = ConstantInt::get(Type::getInt32Ty(Context), 1);
      // Create the selector call.
      Builder.CreateCall(SlctrIntr, Args, Args + 3, "filter");

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

/// EmitRewindBlock - Emit the block containing code to continue unwinding an
/// exception.
void TreeToLLVM::EmitRewindBlock() {
  if (!RewindBB)
    return;

  BeginBlock (RewindBB);

  // The exception pointer to continue unwinding.
  assert(RewindTmp && "Rewind block but nothing to unwind?");
  Value *ExcPtr = Builder.CreateLoad(RewindTmp);

  // Generate an explicit call to _Unwind_Resume_or_Rethrow.
  // FIXME: On ARM this should be a call to __cxa_end_cleanup with no arguments.
  std::vector<const Type*> Params(1, Type::getInt8PtrTy(Context));
  FunctionType *FTy = FunctionType::get(Type::getVoidTy(Context), Params,
                                        false);
  Constant *RewindFn =
    TheModule->getOrInsertFunction("_Unwind_Resume_or_Rethrow", FTy);

  // Pass it to _Unwind_Resume_or_Rethrow.
  CallInst *Rewind = Builder.CreateCall(RewindFn, ExcPtr);

  // This call does not return.
  Rewind->setDoesNotReturn();
  Builder.CreateUnreachable();
}


//===----------------------------------------------------------------------===//
//                           ... Expressions ...
//===----------------------------------------------------------------------===//

static bool canEmitRegisterVariable(tree exp) {
  // Only variables can be marked as 'register'.
  if (TREE_CODE(exp) != VAR_DECL || !DECL_REGISTER(exp))
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
  const Type *Ty = ConvertType(TREE_TYPE(exp));
  unsigned Alignment = LV.getAlignment();

  if (!LV.isBitfield()) {
    // Scalar value: emit a load.
    return LoadRegisterFromMemory(LV, TREE_TYPE(exp), Builder);
  } else {
    // This is a bitfield reference.
    if (!LV.BitSize)
      return Constant::getNullValue(Ty);

    const Type *ValTy = cast<PointerType>(LV.Ptr->getType())->getElementType();
    unsigned ValSizeInBits = ValTy->getPrimitiveSizeInBits();

    // The number of loads needed to read the entire bitfield.
    unsigned Strides = 1 + (LV.BitStart + LV.BitSize - 1) / ValSizeInBits;

    assert(ValTy->isIntegerTy() && "Invalid bitfield lvalue!");
    assert(ValSizeInBits > LV.BitStart && "Bad bitfield lvalue!");
    assert(ValSizeInBits >= LV.BitSize && "Bad bitfield lvalue!");
    assert(2*ValSizeInBits > LV.BitSize+LV.BitStart && "Bad bitfield lvalue!");

    Value *Result = NULL;

    for (unsigned I = 0; I < Strides; I++) {
      unsigned Index = BYTES_BIG_ENDIAN ? I : Strides - I - 1; // MSB first
      unsigned ThisFirstBit = Index * ValSizeInBits;
      unsigned ThisLastBitPlusOne = ThisFirstBit + ValSizeInBits;
      if (ThisFirstBit < LV.BitStart)
        ThisFirstBit = LV.BitStart;
      if (ThisLastBitPlusOne > LV.BitStart+LV.BitSize)
        ThisLastBitPlusOne = LV.BitStart+LV.BitSize;

      Value *Ptr = Index ?
        Builder.CreateGEP(LV.Ptr,
                          ConstantInt::get(Type::getInt32Ty(Context), Index)) :
        LV.Ptr;
      LoadInst *LI = Builder.CreateLoad(Ptr, LV.Volatile);
      LI->setAlignment(Alignment);
      Value *Val = LI;

      unsigned BitsInVal = ThisLastBitPlusOne - ThisFirstBit;
      unsigned FirstBitInVal = ThisFirstBit % ValSizeInBits;

      if (BYTES_BIG_ENDIAN)
        FirstBitInVal = ValSizeInBits-FirstBitInVal-BitsInVal;

      // Mask the bits out by shifting left first, then shifting right.  The
      // LLVM optimizer will turn this into an AND if this is an unsigned
      // expression.

      if (FirstBitInVal+BitsInVal != ValSizeInBits) {
        Value *ShAmt = ConstantInt::get(ValTy, ValSizeInBits -
                                        (FirstBitInVal+BitsInVal));
        Val = Builder.CreateShl(Val, ShAmt);
      }

      // Shift right required?
      if (ValSizeInBits != BitsInVal) {
        bool AddSignBits = !TYPE_UNSIGNED(TREE_TYPE(exp)) && !Result;
        Value *ShAmt = ConstantInt::get(ValTy, ValSizeInBits-BitsInVal);
        Val = AddSignBits ?
          Builder.CreateAShr(Val, ShAmt) : Builder.CreateLShr(Val, ShAmt);
      }

      if (Result) {
        Value *ShAmt = ConstantInt::get(ValTy, BitsInVal);
        Result = Builder.CreateShl(Result, ShAmt);
        Result = Builder.CreateOr(Result, Val);
      } else {
        Result = Val;
      }
    }

    return Builder.CreateIntCast(Result, GetRegType(TREE_TYPE(exp)),
                                 /*isSigned*/!TYPE_UNSIGNED(TREE_TYPE(exp)));
  }
}

Value *TreeToLLVM::EmitADDR_EXPR(tree exp) {
  LValue LV = EmitLV(TREE_OPERAND(exp, 0));
  assert((!LV.isBitfield() || LV.BitStart == 0) &&
         "It is illegal to take the address of a bitfield!");
  // Perform a cast here if necessary.  For example, GCC sometimes forms an
  // ADDR_EXPR where the operand is an array, and the ADDR_EXPR type is a
  // pointer to the first element.
  return Builder.CreateBitCast(LV.Ptr, ConvertType(TREE_TYPE(exp)));
}

Value *TreeToLLVM::EmitOBJ_TYPE_REF(tree exp) {
  return Builder.CreateBitCast(EmitRegister(OBJ_TYPE_REF_EXPR(exp)),
                               ConvertType(TREE_TYPE(exp)));
}

/// EmitCONSTRUCTOR - emit the constructor into the location specified by
/// DestLoc.
Value *TreeToLLVM::EmitCONSTRUCTOR(tree exp, const MemRef *DestLoc) {
  tree type = TREE_TYPE(exp);
  const Type *Ty = ConvertType(type);
  if (const VectorType *VTy = dyn_cast<VectorType>(Ty)) {
    assert(DestLoc == 0 && "Dest location for vector value?");
    std::vector<Value *> BuildVecOps;
    BuildVecOps.reserve(VTy->getNumElements());

    // Insert all of the elements here.
    unsigned HOST_WIDE_INT idx;
    tree value;
    FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (exp), idx, value) {
      Value *Elt = EmitMemory(value);

      if (const VectorType *EltTy = dyn_cast<VectorType>(Elt->getType())) {
        // GCC allows vectors to be built up from vectors.  Extract all of the
        // vector elements and add them to the list of build vector operands.
        for (unsigned i = 0, e = EltTy->getNumElements(); i != e; ++i) {
          Value *Index = ConstantInt::get(llvm::Type::getInt32Ty(Context), i);
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

  assert(AGGREGATE_TYPE_P(type) && "Constructor for scalar type??");

  // Start out with the value zero'd out.
  EmitAggregateZero(*DestLoc, type);

  VEC(constructor_elt, gc) *elt = CONSTRUCTOR_ELTS(exp);
  switch (TREE_CODE(TREE_TYPE(exp))) {
  case ARRAY_TYPE:
  case RECORD_TYPE:
  default:
    if (elt && VEC_length(constructor_elt, elt))
      DieAbjectly("We don't handle elements yet!", exp);
    return 0;
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

    if (AGGREGATE_TYPE_P(TREE_TYPE(tree_purpose))) {
      EmitAggregate(tree_value, *DestLoc);
    } else {
      // Scalar value.  Evaluate to a register, then do the store.
      Value *V = EmitRegister(tree_value);
      StoreRegisterToMemory(V, *DestLoc, TREE_TYPE(tree_purpose), Builder);
    }
    break;
  }
  return 0;
}

/// llvm_load_scalar_argument - Load value located at LOC.
static Value *llvm_load_scalar_argument(Value *L,
                                        const llvm::Type *LLVMTy,
                                        unsigned RealSize,
                                        LLVMBuilder &Builder) {
  if (!RealSize)
    return UndefValue::get(LLVMTy);

  // Not clear what this is supposed to do on big endian machines...
  assert(!BYTES_BIG_ENDIAN && "Unsupported case - please report");
  assert(LLVMTy->isIntegerTy() && "Expected an integer value!");
  const Type *LoadType = IntegerType::get(Context, RealSize * 8);
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
    const FunctionType *FTy;
    const MemRef *DestLoc;
    bool useReturnSlot;
    LLVMBuilder &Builder;
    Value *TheValue;
    MemRef RetBuf;
    CallingConv::ID &CallingConv;
    bool isShadowRet;
    bool isAggrRet;
    unsigned Offset;

    FunctionCallArgumentConversion(SmallVector<Value*, 16> &ops,
                                   const FunctionType *FnTy,
                                   const MemRef *destloc,
                                   bool ReturnSlotOpt,
                                   LLVMBuilder &b,
                                   CallingConv::ID &CC)
      : CallOperands(ops), FTy(FnTy), DestLoc(destloc),
        useReturnSlot(ReturnSlotOpt), Builder(b), CallingConv(CC),
        isShadowRet(false), isAggrRet(false), Offset(0) { }

    /// getCallingConv - This provides the desired CallingConv for the function.
    CallingConv::ID& getCallingConv(void) { return CallingConv; }

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
    Value *getValue(const Type *Ty) {
      assert(!LocStack.empty());
      Value *Loc = LocStack.back();
      if (Loc) {
        // An address.  Convert to the right type and load the value out.
        Loc = Builder.CreateBitCast(Loc, Ty->getPointerTo());
        return Builder.CreateLoad(Loc, "val");
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
    Value *EmitShadowResult(tree type, const MemRef *DestLoc) {
      if (!RetBuf.Ptr)
        return 0;

      if (DestLoc) {
        // Copy out the aggregate return value now.
        assert(ConvertType(type) ==
               cast<PointerType>(RetBuf.Ptr->getType())->getElementType() &&
               "Inconsistent result types!");
        TheTreeToLLVM->EmitAggregateCopy(*DestLoc, RetBuf, type);
        return 0;
      } else {
        // Read out the scalar return value now.
        return Builder.CreateLoad(RetBuf.Ptr, "result");
      }
    }

    /// HandleScalarResult - This callback is invoked if the function returns a
    /// simple scalar result value.
    void HandleScalarResult(const Type * /*RetTy*/) {
      // There is nothing to do here if we return a scalar or void.
      assert(DestLoc == 0 &&
             "Call returns a scalar but caller expects aggregate!");
    }

    /// HandleAggregateResultAsScalar - This callback is invoked if the function
    /// returns an aggregate value by bit converting it to the specified scalar
    /// type and returning that.
    void HandleAggregateResultAsScalar(const Type * /*ScalarTy*/,
                                       unsigned Offset = 0) {
      this->Offset = Offset;
    }

    /// HandleAggregateResultAsAggregate - This callback is invoked if the
    /// function returns an aggregate value using multiple return values.
    void HandleAggregateResultAsAggregate(const Type * /*AggrTy*/) {
      // There is nothing to do here.
      isAggrRet = true;
    }

    /// HandleAggregateShadowResult - This callback is invoked if the function
    /// returns an aggregate value by using a "shadow" first parameter.  If
    /// RetPtr is set to true, the pointer argument itself is returned from the
    /// function.
    void HandleAggregateShadowResult(const PointerType *PtrArgTy, bool /*RetPtr*/) {
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

    void HandlePad(const llvm::Type *LLVMTy) {
      CallOperands.push_back(UndefValue::get(LLVMTy));
    }

    /// HandleScalarShadowResult - This callback is invoked if the function
    /// returns a scalar value by using a "shadow" first parameter, which is a
    /// pointer to the scalar, of type PtrArgTy.  If RetPtr is set to true,
    /// the pointer argument itself is returned from the function.
    void HandleScalarShadowResult(const PointerType *PtrArgTy,
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
    void HandleScalarArgument(const llvm::Type *LLVMTy, tree type,
                              unsigned RealSize = 0) {
      Value *Loc = NULL;
      if (RealSize) {
        Value *L = getAddress();
        Loc = LLVM_LOAD_SCALAR_ARGUMENT(L,LLVMTy,RealSize,Builder);
      } else
        Loc = getValue(LLVMTy);

      // Perform any implicit type conversions.
      if (CallOperands.size() < FTy->getNumParams()) {
        const Type *CalledTy= FTy->getParamType(CallOperands.size());
        if (Loc->getType() != CalledTy) {
          assert(type && "Inconsistent parameter types?");
          bool isSigned = !TYPE_UNSIGNED(type);
          Loc = TheTreeToLLVM->CastToAnyType(Loc, isSigned, CalledTy, false);
        }
      }

      CallOperands.push_back(Loc);
    }

    /// HandleByInvisibleReferenceArgument - This callback is invoked if a
    /// pointer (of type PtrTy) to the argument is passed rather than the
    /// argument itself.
    void HandleByInvisibleReferenceArgument(const llvm::Type *PtrTy,
                                            tree /*type*/) {
      Value *Loc = getAddress();
      Loc = Builder.CreateBitCast(Loc, PtrTy);
      CallOperands.push_back(Loc);
    }

    /// HandleByValArgument - This callback is invoked if the aggregate function
    /// argument is passed by value. It is lowered to a parameter passed by
    /// reference with an additional parameter attribute "ByVal".
    void HandleByValArgument(const llvm::Type *LLVMTy, tree /*type*/) {
      Value *Loc = getAddress();
      assert(LLVMTy->getPointerTo() == Loc->getType());
      (void)LLVMTy; // Otherwise unused if asserts off - avoid compiler warning.
      CallOperands.push_back(Loc);
    }

    /// HandleFCAArgument - This callback is invoked if the aggregate function
    /// argument is passed as a first class aggregate.
    void HandleFCAArgument(const llvm::Type *LLVMTy, tree /*type*/) {
      Value *Loc = getAddress();
      assert(LLVMTy->getPointerTo() == Loc->getType());
      (void)LLVMTy; // Otherwise unused if asserts off - avoid compiler warning.
      CallOperands.push_back(Builder.CreateLoad(Loc));
    }

    /// EnterField - Called when we're about the enter the field of a struct
    /// or union.  FieldNo is the number of the element we are entering in the
    /// LLVM Struct, StructTy is the LLVM type of the struct we are entering.
    void EnterField(unsigned FieldNo, const llvm::Type *StructTy) {
      Value *Loc = getAddress();
      Loc = Builder.CreateBitCast(Loc, StructTy->getPointerTo());
      pushAddress(Builder.CreateStructGEP(Loc, FieldNo, "elt"));
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
  if (!stmt_could_throw_p(stmt))
    // This call does not throw - mark it 'nounwind'.
    PAL = PAL.addAttr(~0, Attribute::NoUnwind);

  if (!PAL.paramHasAttr(~0, Attribute::NoUnwind)) {
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
  const PointerType *PFTy = cast<PointerType>(Callee->getType());
  const FunctionType *FTy = cast<FunctionType>(PFTy->getElementType());
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
  std::vector<const Type*> ScalarArgs;
  for (unsigned i = 0, e = gimple_call_num_args(stmt); i != e; ++i) {
    tree arg = gimple_call_arg(stmt, i);
    tree type = TREE_TYPE(arg);
    const Type *ArgTy = ConvertType(type);

    // Push the argument.
    if (ArgTy->isSingleValueType()) {
      // A scalar - push the value.
      Client.pushValue(EmitMemory(arg));
    } else if (LLVM_SHOULD_PASS_AGGREGATE_AS_FCA(type, ArgTy)) {
      if (AGGREGATE_TYPE_P(type)) {
        // Pass the aggregate as a first class value.
        LValue ArgVal = EmitLV(arg);
        Client.pushValue(Builder.CreateLoad(ArgVal.Ptr));
      } else {
        // Already first class (eg: a complex number) - push the value.
        Client.pushValue(EmitMemory(arg));
      }
    } else {
      if (AGGREGATE_TYPE_P(type)) {
        // An aggregate - push the address.
        LValue ArgVal = EmitLV(arg);
        assert(!ArgVal.isBitfield() && "Bitfields are first-class types!");
        Client.pushAddress(ArgVal.Ptr);
      } else {
        // A first class value (eg: a complex number).  Push the address of a
        // temporary copy.
        MemRef Copy = CreateTempLoc(ArgTy);
        StoreRegisterToMemory(EmitRegister(arg), Copy, type, Builder);
        Client.pushAddress(Copy.Ptr);
      }
    }

    Attributes Attrs = Attribute::None;

    unsigned OldSize = CallOperands.size();

    ABIConverter.HandleArgument(type, ScalarArgs, &Attrs);

    if (Attrs != Attribute::None) {
      // If the argument is split into multiple scalars, assign the
      // attributes to all scalars of the aggregate.
      for (unsigned i = OldSize + 1; i <= CallOperands.size(); ++i) {
        PAL = PAL.addAttr(i, Attrs);
      }
    }

    Client.clear();
  }

  // Compile stuff like:
  //   %tmp = call float (...)* bitcast (float ()* @foo to float (...)*)( )
  // to:
  //   %tmp = call float @foo( )
  // This commonly occurs due to C "implicit ..." semantics.
  if (ConstantExpr *CE = dyn_cast<ConstantExpr>(Callee)) {
    if (CallOperands.empty() && CE->getOpcode() == Instruction::BitCast) {
      Constant *RealCallee = CE->getOperand(0);
      assert(RealCallee->getType()->isPointerTy() &&
             "Bitcast to ptr not from ptr?");
      const PointerType *RealPT = cast<PointerType>(RealCallee->getType());
      if (const FunctionType *RealFT =
          dyn_cast<FunctionType>(RealPT->getElementType())) {
        const PointerType *ActualPT = cast<PointerType>(Callee->getType());
        const FunctionType *ActualFT =
          cast<FunctionType>(ActualPT->getElementType());
        if (RealFT->getReturnType() == ActualFT->getReturnType() &&
            RealFT->getNumParams() == 0)
          Callee = RealCallee;
      }
    }
  }

  // Unlike LLVM, GCC does not require that call statements provide a value for
  // every function argument (it passes rubbish for arguments with no value).
  // To get the same effect we pass 'undef' for any unspecified arguments.
  PFTy = cast<PointerType>(Callee->getType());
  FTy = cast<FunctionType>(PFTy->getElementType());
  if (CallOperands.size() < FTy->getNumParams())
    for (unsigned i = CallOperands.size(), e = FTy->getNumParams(); i != e; ++i)
      CallOperands.push_back(UndefValue::get(FTy->getParamType(i)));

  Value *Call;
  if (!LandingPad) {
    Call = Builder.CreateCall(Callee, CallOperands.begin(), CallOperands.end());
    cast<CallInst>(Call)->setCallingConv(CallingConvention);
    cast<CallInst>(Call)->setAttributes(PAL);
  } else {
    BasicBlock *NextBlock = BasicBlock::Create(Context);
    Call = Builder.CreateInvoke(Callee, NextBlock, LandingPad,
                                CallOperands.begin(), CallOperands.end());
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

  if (Client.isShadowReturn())
    return Client.EmitShadowResult(gimple_call_return_type(stmt), DestLoc);

  if (Call->getType()->isVoidTy())
    return 0;

  if (Client.isAggrReturn()) {
    MemRef Target;
    if (DestLoc)
      Target = *DestLoc;
    else
      // Destination is a first class value (eg: a complex number).  Extract to
      // a temporary then load the value out later.
      Target = CreateTempLoc(ConvertType(gimple_call_return_type(stmt)));

    if (TD.getTypeAllocSize(Call->getType()) <=
        TD.getTypeAllocSize(cast<PointerType>(Target.Ptr->getType())
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
    const Type *RetTy = ConvertType(gimple_call_return_type(stmt));
    if (Call->getType() == RetTy)
      return Call;   // Normal scalar return.

    // May be something as simple as a float being returned as an integer, or
    // something trickier like a complex int type { i32, i32 } being returned
    // as an i64.
    if (Call->getType()->canLosslesslyBitCastTo(RetTy))
      return Builder.CreateBitCast(Call, RetTy); // Simple case.
    // Probably a scalar to complex conversion.
    assert(TD.getTypeAllocSize(Call->getType()) == TD.getTypeAllocSize(RetTy) &&
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
  // AggTy - The type of the aggregate being stored to.
  const Type *AggTy = cast<PointerType>(Ptr->getType())->getElementType();
  // MaxStoreSize - The maximum number of bytes we can store without overflowing
  // the aggregate.
  int64_t MaxStoreSize = TD.getTypeAllocSize(AggTy);
  if (Client.Offset) {
    Ptr = Builder.CreateBitCast(Ptr, Type::getInt8PtrTy(Context));
    Ptr = Builder.CreateGEP(Ptr,
                    ConstantInt::get(TD.getIntPtrType(Context), Client.Offset));
    MaxStoreSize -= Client.Offset;
  }
  assert(MaxStoreSize > 0 && "Storing off end of aggregate?");
  Value *Val = Call;
  // Check whether storing the scalar directly would overflow the aggregate.
  if (TD.getTypeStoreSize(Call->getType()) > (uint64_t)MaxStoreSize) {
    // Chop down the size of the scalar to the maximum number of bytes that can
    // be stored without overflowing the destination.
    // TODO: Check whether this works correctly on big-endian machines.
    // Store the scalar to a temporary.
    Value *Tmp = CreateTemporary(Call->getType());
    Builder.CreateStore(Call, Tmp);
    // Load the desired number of bytes back out again as an integer of the
    // appropriate size.
    const Type *SmallTy = IntegerType::get(Context, MaxStoreSize*8);
    Tmp = Builder.CreateBitCast(Tmp, PointerType::getUnqual(SmallTy));
    Val = Builder.CreateLoad(Tmp);
    // Store the integer rather than the call result to the aggregate.
  }
  Ptr = Builder.CreateBitCast(Ptr, PointerType::getUnqual(Val->getType()));
  StoreInst *St = Builder.CreateStore(Val, Ptr, DestLoc->Volatile);
  St->setAlignment(DestLoc->getAlignment());
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

  const Type *RetTy = TREE_CODE(ret_type) == VOID_TYPE ?
    Type::getVoidTy(Context) : GetRegType(ret_type);

  // The LLVM argument types.
  std::vector<const Type*> ArgTys;
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
  const FunctionType *FTy = FunctionType::get(RetTy, ArgTys, /*isVarArg*/false);
  Constant *Func = TheModule->getOrInsertFunction(CalleeName, FTy);

  // If the function already existed with the wrong prototype then don't try to
  // muck with its calling convention.  Otherwise, set the calling convention.
  if (Function *F = dyn_cast<Function>(Func))
    F->setCallingConv(CC);

  // Finally, call the function.
  CallInst *CI = Builder.CreateCall(Func, Args.begin(), Args.end());
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
  const Type *MemTy = ConvertType(TREE_TYPE(decl));
  const Type *RegTy = GetRegType(TREE_TYPE(decl));

  // If there was an error, return something bogus.
  if (ValidateRegisterVariable(decl))
    return UndefValue::get(RegTy);

  // Turn this into a 'tmp = call Ty asm "", "={reg}"()'.
  FunctionType *FTy = FunctionType::get(MemTy, std::vector<const Type*>(),
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
  std::vector<const Type*> ArgTys;
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
          error_at(gimple_location(stmt),
                   "operand number missing after %%-letter");
          return Result;
        } else if (OpNum >= NumOperands) {
          error_at(gimple_location(stmt), "operand number out of range");
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
    case '=': assert(0 && "Should be after '='s");
    case '+': assert(0 && "'+' should already be expanded");
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
  if (TREE_CODE(Operand) == VAR_DECL && DECL_HARD_REGISTER(Operand)) {
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
  if (TREE_CODE(Operand) == INTEGER_CST) {
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
    std::vector<Constant*> CstOps;
    for (unsigned i = 0, e = Ops.size(); i != e; ++i)
      CstOps.push_back(cast<Constant>(Ops[i]));
    return ConstantVector::get(CstOps);
  }

  // Otherwise, insertelement the values to build the vector.
  Value *Result =
    UndefValue::get(VectorType::get(Ops[0]->getType(), Ops.size()));

  for (unsigned i = 0, e = Ops.size(); i != e; ++i)
    Result = Builder.CreateInsertElement(Result, Ops[i],
                                ConstantInt::get(Type::getInt32Ty(Context), i));

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
  std::vector<Constant*> Idxs;
  va_list VA;
  va_start(VA, InVec2);
  for (unsigned i = 0; i != NumElements; ++i) {
    int idx = va_arg(VA, int);
    if (idx == -1)
      Idxs.push_back(UndefValue::get(Type::getInt32Ty(Context)));
    else {
      assert((unsigned)idx < 2*NumElements && "Element index out of range!");
      Idxs.push_back(ConstantInt::get(Type::getInt32Ty(Context), idx));
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
  const Type *ResultType = ConvertType(TREE_TYPE(TREE_TYPE(fndecl)));
  std::vector<Value*> Operands;
  for (unsigned i = 0, e = gimple_call_num_args(stmt); i != e; ++i) {
    tree OpVal = gimple_call_arg(stmt, i);
    if (AGGREGATE_TYPE_P(TREE_TYPE(OpVal))) {
      MemRef OpLoc = CreateTempLoc(ConvertType(TREE_TYPE(OpVal)));
      EmitAggregate(OpVal, OpLoc);
      Operands.push_back(Builder.CreateLoad(OpLoc.Ptr));
    } else {
      Operands.push_back(EmitMemory(OpVal));
    }
  }

  return LLVM_TARGET_INTRINSIC_LOWER(stmt, fndecl, DestLoc, Result, ResultType,
                                     Operands);
#endif
  return false;
}

/// TargetBuiltinCache - A cache of builtin intrinsics indexed by the GCC
/// builtin number.
static std::vector<Constant*> TargetBuiltinCache;

void TreeToLLVM::EmitMemoryBarrier(bool ll, bool ls, bool sl, bool ss,
                                   bool device) {
  Value* C[5];
  C[0] = ConstantInt::get(Type::getInt1Ty(Context), ll);
  C[1] = ConstantInt::get(Type::getInt1Ty(Context), ls);
  C[2] = ConstantInt::get(Type::getInt1Ty(Context), sl);
  C[3] = ConstantInt::get(Type::getInt1Ty(Context), ss);
  C[4] = ConstantInt::get(Type::getInt1Ty(Context), device);

  Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                               Intrinsic::memory_barrier),
                     C, C + 5);
}

Value *
TreeToLLVM::BuildBinaryAtomicBuiltin(gimple stmt, Intrinsic::ID id) {
  tree return_type = gimple_call_return_type(stmt);
  const Type *ResultTy = ConvertType(return_type);
  Value* C[2] = {
    EmitMemory(gimple_call_arg(stmt, 0)),
    EmitMemory(gimple_call_arg(stmt, 1))
  };
  const Type* Ty[2];
  Ty[0] = ResultTy;
  Ty[1] = ResultTy->getPointerTo();
  C[0] = Builder.CreateBitCast(C[0], Ty[1]);
  C[1] = Builder.CreateIntCast(C[1], Ty[0],
                               /*isSigned*/!TYPE_UNSIGNED(return_type),
                               "cast");
  // The gcc builtins are also full memory barriers.
  // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
  EmitMemoryBarrier(true, true, true, true, false);
#else
  EmitMemoryBarrier(true, true, true, true, true);
#endif

  Value *Result =
    Builder.CreateCall(Intrinsic::getDeclaration(TheModule,  id,
                                                 Ty, 2),
    C, C + 2);

  // The gcc builtins are also full memory barriers.
  // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
  EmitMemoryBarrier(true, true, true, true, false);
#else
  EmitMemoryBarrier(true, true, true, true, true);
#endif

  Result = Builder.CreateIntToPtr(Result, ResultTy);
  return Result;
}

Value *
TreeToLLVM::BuildCmpAndSwapAtomicBuiltin(gimple stmt, tree type, bool isBool) {
  const Type *ResultTy = ConvertType(type);
  Value* C[3] = {
    EmitMemory(gimple_call_arg(stmt, 0)),
    EmitMemory(gimple_call_arg(stmt, 1)),
    EmitMemory(gimple_call_arg(stmt, 2))
  };
  const Type* Ty[2];
  Ty[0] = ResultTy;
  Ty[1] = ResultTy->getPointerTo();
  C[0] = Builder.CreateBitCast(C[0], Ty[1]);
  C[1] = Builder.CreateIntCast(C[1], Ty[0], /*isSigned*/!TYPE_UNSIGNED(type),
                               "cast");
  C[2] = Builder.CreateIntCast(C[2], Ty[0], /*isSigned*/!TYPE_UNSIGNED(type),
                               "cast");

  // The gcc builtins are also full memory barriers.
  // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
  EmitMemoryBarrier(true, true, true, true, false);
#else
  EmitMemoryBarrier(true, true, true, true, true);
#endif

  Value *Result =
    Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                 Intrinsic::atomic_cmp_swap,
                                                 Ty, 2),
    C, C + 3);

  // The gcc builtins are also full memory barriers.
  // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
  EmitMemoryBarrier(true, true, true, true, false);
#else
  EmitMemoryBarrier(true, true, true, true, true);
#endif

  if (isBool)
    Result = Builder.CreateIntCast(Builder.CreateICmpEQ(Result, C[1]),
                                   ConvertType(boolean_type_node),
                                   /*isSigned*/false);
  else
    Result = Builder.CreateIntToPtr(Result, ResultTy);
  return Result;
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
        error_at(gimple_location(stmt),
                 "unsupported target builtin %<%s%> used", BuiltinName);
        const Type *ResTy = ConvertType(gimple_call_return_type(stmt));
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
  case BUILT_IN_CONSTANT_P:     return EmitBuiltinConstantP(stmt, Result);
  case BUILT_IN_ALLOCA:         return EmitBuiltinAlloca(stmt, Result);
  case BUILT_IN_EXTEND_POINTER: return EmitBuiltinExtendPointer(stmt, Result);
  case BUILT_IN_EXPECT:         return EmitBuiltinExpect(stmt, Result);
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
  case BUILT_IN_BZERO:          return EmitBuiltinBZero(stmt, Result);
  case BUILT_IN_PREFETCH:       return EmitBuiltinPrefetch(stmt);
  case BUILT_IN_FRAME_ADDRESS:  return EmitBuiltinReturnAddr(stmt, Result,true);
  case BUILT_IN_RETURN_ADDRESS:
    return EmitBuiltinReturnAddr(stmt, Result,false);
  case BUILT_IN_STACK_SAVE:     return EmitBuiltinStackSave(stmt, Result);
  case BUILT_IN_STACK_RESTORE:  return EmitBuiltinStackRestore(stmt);
  case BUILT_IN_EXTRACT_RETURN_ADDR:
   return EmitBuiltinExtractReturnAddr(stmt, Result);
  case BUILT_IN_FROB_RETURN_ADDR:
   return EmitBuiltinFrobReturnAddr(stmt, Result);
  case BUILT_IN_ADJUST_TRAMPOLINE:
    return EmitBuiltinAdjustTrampoline(stmt, Result);
  case BUILT_IN_INIT_TRAMPOLINE:
    return EmitBuiltinInitTrampoline(stmt, Result);

  // Exception handling builtins.
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
    if (TREE_CODE (ObjSizeTree) != INTEGER_CST
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
    const Type* Ty = ConvertType(gimple_call_return_type(stmt));

    // Manually coerce the arg to the correct pointer type.
    Args[0] = Builder.CreateBitCast(Args[0], Type::getInt8PtrTy(Context));
    Args[1] = Builder.CreateIntCast(Args[1], Type::getInt1Ty(Context),
                                    /*isSigned*/false);

    Result = Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                          Intrinsic::objectsize,
                                                          &Ty,
                                                          1),
                                Args, Args + 2);
    return true;
  }
  // Unary bit counting intrinsics.
  // NOTE: do not merge these case statements.  That will cause the memoized
  // Function* to be incorrectly shared across the different typed functions.
  case BUILT_IN_CLZ:       // These GCC builtins always return int.
  case BUILT_IN_CLZL:
  case BUILT_IN_CLZLL: {
    Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
    EmitBuiltinUnaryOp(Amt, Result, Intrinsic::ctlz);
    tree return_type = gimple_call_return_type(stmt);
    const Type *DestTy = ConvertType(return_type);
    Result = Builder.CreateIntCast(Result, DestTy,
                                   /*isSigned*/!TYPE_UNSIGNED(return_type),
                                   "cast");
    return true;
  }
  case BUILT_IN_CTZ:       // These GCC builtins always return int.
  case BUILT_IN_CTZL:
  case BUILT_IN_CTZLL: {
    Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
    EmitBuiltinUnaryOp(Amt, Result, Intrinsic::cttz);
    tree return_type = gimple_call_return_type(stmt);
    const Type *DestTy = ConvertType(return_type);
    Result = Builder.CreateIntCast(Result, DestTy,
                                   /*isSigned*/!TYPE_UNSIGNED(return_type),
                                   "cast");
    return true;
  }
  case BUILT_IN_PARITYLL:
  case BUILT_IN_PARITYL:
  case BUILT_IN_PARITY: {
    Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
    EmitBuiltinUnaryOp(Amt, Result, Intrinsic::ctpop);
    Result = Builder.CreateBinOp(Instruction::And, Result,
                                 ConstantInt::get(Result->getType(), 1));
    tree return_type = gimple_call_return_type(stmt);
    const Type *DestTy = ConvertType(return_type);
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
    const Type *DestTy = ConvertType(return_type);
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
    const Type *DestTy = ConvertType(return_type);
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
    EmitBuiltinUnaryOp(Amt, Result, Intrinsic::cttz);
    Result = Builder.CreateAdd(Result,
      ConstantInt::get(Result->getType(), 1));
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
//TODO    const Type *SBP= Type::getInt8PtrTy(Context);
//TODO    file = TheFolder->CreateBitCast(file, SBP);
//TODO
//TODO    // Get arguments.
//TODO    tree arglist = CALL_EXPR_ARGS(stmt);
//TODO    Value *ExprVal = EmitMemory(gimple_call_arg(stmt, 0));
//TODO    const Type *Ty = ExprVal->getType();
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

  case BUILT_IN_SYNCHRONIZE: {
    // We assume like gcc appears to, that this only applies to cached memory.
    Value* C[5];
    C[0] = C[1] = C[2] = C[3] = ConstantInt::get(Type::getInt1Ty(Context), 1);
    C[4] = ConstantInt::get(Type::getInt1Ty(Context), 0);

    Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                 Intrinsic::memory_barrier),
                       C, C + 5);
    return true;
  }
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
  case BUILT_IN_BOOL_COMPARE_AND_SWAP_1: {
    Result = BuildCmpAndSwapAtomicBuiltin(stmt, unsigned_char_type_node, true);
    return true;
  }
  case BUILT_IN_BOOL_COMPARE_AND_SWAP_2: {
    Result = BuildCmpAndSwapAtomicBuiltin(stmt, short_unsigned_type_node, true);
    return true;
  }
  case BUILT_IN_BOOL_COMPARE_AND_SWAP_4: {
    Result = BuildCmpAndSwapAtomicBuiltin(stmt, unsigned_type_node, true);
    return true;
  }
  case BUILT_IN_BOOL_COMPARE_AND_SWAP_8: {
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
    Result = BuildCmpAndSwapAtomicBuiltin(stmt, long_long_unsigned_type_node,
                                          true);
    return true;
  }

  case BUILT_IN_VAL_COMPARE_AND_SWAP_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_VAL_COMPARE_AND_SWAP_1:
  case BUILT_IN_VAL_COMPARE_AND_SWAP_2:
  case BUILT_IN_VAL_COMPARE_AND_SWAP_4: {
    tree type = gimple_call_return_type(stmt);
    Result = BuildCmpAndSwapAtomicBuiltin(stmt, type, false);
    return true;
  }
  case BUILT_IN_FETCH_AND_ADD_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_FETCH_AND_ADD_1:
  case BUILT_IN_FETCH_AND_ADD_2:
  case BUILT_IN_FETCH_AND_ADD_4: {
    Result = BuildBinaryAtomicBuiltin(stmt, Intrinsic::atomic_load_add);
    return true;
  }
  case BUILT_IN_FETCH_AND_SUB_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_FETCH_AND_SUB_1:
  case BUILT_IN_FETCH_AND_SUB_2:
  case BUILT_IN_FETCH_AND_SUB_4: {
    Result = BuildBinaryAtomicBuiltin(stmt, Intrinsic::atomic_load_sub);
    return true;
  }
  case BUILT_IN_FETCH_AND_OR_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_FETCH_AND_OR_1:
  case BUILT_IN_FETCH_AND_OR_2:
  case BUILT_IN_FETCH_AND_OR_4: {
    Result = BuildBinaryAtomicBuiltin(stmt, Intrinsic::atomic_load_or);
    return true;
  }
  case BUILT_IN_FETCH_AND_AND_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_FETCH_AND_AND_1:
  case BUILT_IN_FETCH_AND_AND_2:
  case BUILT_IN_FETCH_AND_AND_4: {
    Result = BuildBinaryAtomicBuiltin(stmt, Intrinsic::atomic_load_and);
    return true;
  }
  case BUILT_IN_FETCH_AND_XOR_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_FETCH_AND_XOR_1:
  case BUILT_IN_FETCH_AND_XOR_2:
  case BUILT_IN_FETCH_AND_XOR_4: {
    Result = BuildBinaryAtomicBuiltin(stmt, Intrinsic::atomic_load_xor);
    return true;
  }
  case BUILT_IN_FETCH_AND_NAND_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_FETCH_AND_NAND_1:
  case BUILT_IN_FETCH_AND_NAND_2:
  case BUILT_IN_FETCH_AND_NAND_4: {
    Result = BuildBinaryAtomicBuiltin(stmt, Intrinsic::atomic_load_nand);
    return true;
  }
  case BUILT_IN_LOCK_TEST_AND_SET_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_LOCK_TEST_AND_SET_1:
  case BUILT_IN_LOCK_TEST_AND_SET_2:
  case BUILT_IN_LOCK_TEST_AND_SET_4: {
    Result = BuildBinaryAtomicBuiltin(stmt, Intrinsic::atomic_swap);
    return true;
  }

  case BUILT_IN_ADD_AND_FETCH_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_ADD_AND_FETCH_1:
  case BUILT_IN_ADD_AND_FETCH_2:
  case BUILT_IN_ADD_AND_FETCH_4: {
    tree return_type = gimple_call_return_type(stmt);
    const Type *ResultTy = ConvertType(return_type);
    Value* C[2] = {
      EmitMemory(gimple_call_arg(stmt, 0)),
      EmitMemory(gimple_call_arg(stmt, 1))
    };
    const Type* Ty[2];
    Ty[0] = ResultTy;
    Ty[1] = ResultTy->getPointerTo();
    C[0] = Builder.CreateBitCast(C[0], Ty[1]);
    C[1] = Builder.CreateIntCast(C[1], Ty[0],
                                 /*isSigned*/!TYPE_UNSIGNED(return_type),
                                 "cast");

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result =
      Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                   Intrinsic::atomic_load_add,
                                                   Ty, 2),
                         C, C + 2);

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result = Builder.CreateAdd(Result, C[1]);
    Result = Builder.CreateIntToPtr(Result, ResultTy);
    return true;
  }
  case BUILT_IN_SUB_AND_FETCH_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_SUB_AND_FETCH_1:
  case BUILT_IN_SUB_AND_FETCH_2:
  case BUILT_IN_SUB_AND_FETCH_4: {
    tree return_type = gimple_call_return_type(stmt);
    const Type *ResultTy = ConvertType(return_type);
    Value* C[2] = {
      EmitMemory(gimple_call_arg(stmt, 0)),
      EmitMemory(gimple_call_arg(stmt, 1))
    };
    const Type* Ty[2];
    Ty[0] = ResultTy;
    Ty[1] = ResultTy->getPointerTo();
    C[0] = Builder.CreateBitCast(C[0], Ty[1]);
    C[1] = Builder.CreateIntCast(C[1], Ty[0],
                                 /*isSigned*/!TYPE_UNSIGNED(return_type),
                                 "cast");

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result =
      Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                   Intrinsic::atomic_load_sub,
                                                   Ty, 2),
                         C, C + 2);

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result = Builder.CreateSub(Result, C[1]);
    Result = Builder.CreateIntToPtr(Result, ResultTy);
    return true;
  }
  case BUILT_IN_OR_AND_FETCH_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_OR_AND_FETCH_1:
  case BUILT_IN_OR_AND_FETCH_2:
  case BUILT_IN_OR_AND_FETCH_4: {
    tree return_type = gimple_call_return_type(stmt);
    const Type *ResultTy = ConvertType(return_type);
    Value* C[2] = {
      EmitMemory(gimple_call_arg(stmt, 0)),
      EmitMemory(gimple_call_arg(stmt, 1))
    };
    const Type* Ty[2];
    Ty[0] = ResultTy;
    Ty[1] = ResultTy->getPointerTo();
    C[0] = Builder.CreateBitCast(C[0], Ty[1]);
    C[1] = Builder.CreateIntCast(C[1], Ty[0],
                                 /*isSigned*/!TYPE_UNSIGNED(return_type),
                                 "cast");

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result =
      Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                   Intrinsic::atomic_load_or,
                                                   Ty, 2),
                         C, C + 2);

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result = Builder.CreateOr(Result, C[1]);
    Result = Builder.CreateIntToPtr(Result, ResultTy);
    return true;
  }
  case BUILT_IN_AND_AND_FETCH_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_AND_AND_FETCH_1:
  case BUILT_IN_AND_AND_FETCH_2:
  case BUILT_IN_AND_AND_FETCH_4: {
    tree return_type = gimple_call_return_type(stmt);
    const Type *ResultTy = ConvertType(return_type);
    Value* C[2] = {
      EmitMemory(gimple_call_arg(stmt, 0)),
      EmitMemory(gimple_call_arg(stmt, 1))
    };
    const Type* Ty[2];
    Ty[0] = ResultTy;
    Ty[1] = ResultTy->getPointerTo();
    C[0] = Builder.CreateBitCast(C[0], Ty[1]);
    C[1] = Builder.CreateIntCast(C[1], Ty[0],
                                 /*isSigned*/!TYPE_UNSIGNED(return_type),
                                 "cast");

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result =
      Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                   Intrinsic::atomic_load_and,
                                                   Ty, 2),
                         C, C + 2);

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result = Builder.CreateAnd(Result, C[1]);
    Result = Builder.CreateIntToPtr(Result, ResultTy);
    return true;
  }
  case BUILT_IN_XOR_AND_FETCH_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_XOR_AND_FETCH_1:
  case BUILT_IN_XOR_AND_FETCH_2:
  case BUILT_IN_XOR_AND_FETCH_4: {
    tree return_type = gimple_call_return_type(stmt);
    const Type *ResultTy = ConvertType(return_type);
    Value* C[2] = {
      EmitMemory(gimple_call_arg(stmt, 0)),
      EmitMemory(gimple_call_arg(stmt, 1))
    };
    const Type* Ty[2];
    Ty[0] = ResultTy;
    Ty[1] = ResultTy->getPointerTo();
    C[0] = Builder.CreateBitCast(C[0], Ty[1]);
    C[1] = Builder.CreateIntCast(C[1], Ty[0],
                                 /*isSigned*/!TYPE_UNSIGNED(return_type),
                                 "cast");

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result =
      Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                   Intrinsic::atomic_load_xor,
                                                   Ty, 2),
                         C, C + 2);

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result = Builder.CreateXor(Result, C[1]);
    Result = Builder.CreateIntToPtr(Result, ResultTy);
    return true;
  }
  case BUILT_IN_NAND_AND_FETCH_8:
#if defined(TARGET_POWERPC)
    if (!TARGET_64BIT)
      return false;
#endif
  case BUILT_IN_NAND_AND_FETCH_1:
  case BUILT_IN_NAND_AND_FETCH_2:
  case BUILT_IN_NAND_AND_FETCH_4: {
    tree return_type = gimple_call_return_type(stmt);
    const Type *ResultTy = ConvertType(return_type);
    Value* C[2] = {
      EmitMemory(gimple_call_arg(stmt, 0)),
      EmitMemory(gimple_call_arg(stmt, 1))
    };
    const Type* Ty[2];
    Ty[0] = ResultTy;
    Ty[1] = ResultTy->getPointerTo();
    C[0] = Builder.CreateBitCast(C[0], Ty[1]);
    C[1] = Builder.CreateIntCast(C[1], Ty[0],
                                 /*isSigned*/!TYPE_UNSIGNED(return_type),
                                 "cast");

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result =
      Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                   Intrinsic::atomic_load_nand,
                                                   Ty, 2),
                         C, C + 2);

    // The gcc builtins are also full memory barriers.
    // FIXME: __sync_lock_test_and_set and __sync_lock_release require less.
#if defined(TARGET_ARM) && defined(CONFIG_DARWIN_H)
    EmitMemoryBarrier(true, true, true, true, false);
#else
    EmitMemoryBarrier(true, true, true, true, true);
#endif

    Result = Builder.CreateAnd(Builder.CreateNot(Result), C[1]);
    Result = Builder.CreateIntToPtr(Result, ResultTy);
    return true;
  }

  case BUILT_IN_LOCK_RELEASE_1:
  case BUILT_IN_LOCK_RELEASE_2:
  case BUILT_IN_LOCK_RELEASE_4:
  case BUILT_IN_LOCK_RELEASE_8:
  case BUILT_IN_LOCK_RELEASE_16: {
    // This is effectively a volatile store of 0, and has no return value.
    // The argument has typically been coerced to "volatile void*"; the
    // only way to find the size of the operation is from the builtin
    // opcode.
    const Type *Ty;
    switch(DECL_FUNCTION_CODE(fndecl)) {
      case BUILT_IN_LOCK_RELEASE_16:    // not handled; should use SSE on x86
      default:
        DieAbjectly("Not handled; should use SSE on x86!");
      case BUILT_IN_LOCK_RELEASE_1:
        Ty = Type::getInt8Ty(Context); break;
      case BUILT_IN_LOCK_RELEASE_2:
        Ty = Type::getInt16Ty(Context); break;
      case BUILT_IN_LOCK_RELEASE_4:
        Ty = Type::getInt32Ty(Context); break;
      case BUILT_IN_LOCK_RELEASE_8:
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

      if (TREE_CODE(value) != INTEGER_CST ||
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
  case BUILT_IN_ARGS_INFO:
  case BUILT_IN_NEXT_ARG:
  case BUILT_IN_CLASSIFY_TYPE:
  case BUILT_IN_AGGREGATE_INCOMING_ADDRESS:
  case BUILT_IN_SETJMP_SETUP:
  case BUILT_IN_SETJMP_DISPATCHER:
  case BUILT_IN_SETJMP_RECEIVER:
  case BUILT_IN_UPDATE_SETJMP_BUF:

    // FIXME: HACK: Just ignore these.
    {
      const Type *Ty = ConvertType(gimple_call_return_type(stmt));
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
  const Type *Ty = InVal->getType();
  Result = Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Id, &Ty, 1),
                              InVal);
  return true;
}

Value *TreeToLLVM::EmitBuiltinSQRT(gimple stmt) {
  Value *Amt = EmitMemory(gimple_call_arg(stmt, 0));
  const Type* Ty = Amt->getType();

  return Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                      Intrinsic::sqrt, &Ty, 1),
                            Amt);
}

Value *TreeToLLVM::EmitBuiltinPOWI(gimple stmt) {
  if (!validate_gimple_arglist(stmt, REAL_TYPE, INTEGER_TYPE, VOID_TYPE))
    return 0;

  Value *Val = EmitMemory(gimple_call_arg(stmt, 0));
  Value *Pow = EmitMemory(gimple_call_arg(stmt, 1));
  const Type *Ty = Val->getType();
  Pow = Builder.CreateIntCast(Pow, Type::getInt32Ty(Context), /*isSigned*/true);

  SmallVector<Value *,2> Args;
  Args.push_back(Val);
  Args.push_back(Pow);
  return Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                      Intrinsic::powi, &Ty, 1),
                            Args.begin(), Args.end());
}

Value *TreeToLLVM::EmitBuiltinPOW(gimple stmt) {
  if (!validate_gimple_arglist(stmt, REAL_TYPE, REAL_TYPE, VOID_TYPE))
    return 0;

  Value *Val = EmitMemory(gimple_call_arg(stmt, 0));
  Value *Pow = EmitMemory(gimple_call_arg(stmt, 1));
  const Type *Ty = Val->getType();

  SmallVector<Value *,2> Args;
  Args.push_back(Val);
  Args.push_back(Pow);
  return Builder.CreateCall(Intrinsic::getDeclaration(TheModule,
                                                      Intrinsic::pow, &Ty, 1),
                            Args.begin(), Args.end());
}

Value *TreeToLLVM::EmitBuiltinLCEIL(gimple stmt) {
  if (!validate_gimple_arglist(stmt, REAL_TYPE, VOID_TYPE))
    return 0;

  // Cast the result of "ceil" to the appropriate integer type.
  // First call the appropriate version of "ceil".
  tree op = gimple_call_arg(stmt, 0);
  StringRef Name = SelectFPName(TREE_TYPE(op), "ceilf", "ceil", "ceill");
  CallInst *Call = EmitSimpleCall(Name, TREE_TYPE(op), op, NULL);
  Call->setDoesNotThrow();
  Call->setDoesNotAccessMemory();

  // Then type cast the result of the "ceil" call.
  tree type = gimple_call_return_type(stmt);
  const Type *RetTy = GetRegType(type);
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
  CallInst *Call = EmitSimpleCall(Name, TREE_TYPE(op), op, NULL);
  Call->setDoesNotThrow();
  Call->setDoesNotAccessMemory();

  // Then type cast the result of the "floor" call.
  tree type = gimple_call_return_type(stmt);
  const Type *RetTy = GetRegType(type);
  return TYPE_UNSIGNED(type) ? Builder.CreateFPToUI(Call, RetTy) :
    Builder.CreateFPToSI(Call, RetTy);
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
    warning_at (gimple_location(stmt), 0,
                "call to %D will always overflow destination buffer",
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
    ReadWrite = Constant::getNullValue(Type::getInt32Ty(Context));
  if (Locality == 0)
    Locality = ConstantInt::get(Type::getInt32Ty(Context), 3);

  Ptr = Builder.CreateBitCast(Ptr, Type::getInt8PtrTy(Context));

  Value *Ops[3] = { Ptr, ReadWrite, Locality };
  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Intrinsic::prefetch),
                     Ops, Ops+3);
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


// Exception handling builtins.

bool TreeToLLVM::EmitBuiltinEHPointer(gimple stmt, Value *&Result) {
  // Lookup the local that holds the exception pointer for this region.
  unsigned RegionNo = tree_low_cst(gimple_call_arg(stmt, 0), 0);
  AllocaInst *ExcPtr = getExceptionPtr(RegionNo);
  // Load the exception pointer out.
  Result = Builder.CreateLoad(ExcPtr);
  // Ensure the returned value has the right pointer type.
  tree type = gimple_call_return_type(stmt);
  Result = Builder.CreateBitCast(Result, ConvertType(type));
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
                       ConstantInt::get(Type::getInt32Ty(Context), cfa_offset));

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

  if (TREE_CODE (which) != INTEGER_CST) {
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

  const Type *IntPtr = TD.getIntPtrType(Context);
  Value *Offset = EmitMemory(gimple_call_arg(stmt, 0));
  Value *Handler = EmitMemory(gimple_call_arg(stmt, 1));

  Intrinsic::ID IID = IntPtr->isIntegerTy(32) ?
    Intrinsic::eh_return_i32 : Intrinsic::eh_return_i64;

  Offset = Builder.CreateIntCast(Offset, IntPtr, /*isSigned*/true);
  Handler = Builder.CreateBitCast(Handler, Type::getInt8PtrTy(Context));

  Value *Args[2] = { Offset, Handler };
  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, IID), Args, Args + 2);
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

      Size = ConstantInt::get(Type::getInt8Ty(Context), size);
      Idx  = ConstantInt::get(Type::getInt32Ty(Context), rnum);
      Builder.CreateStore(Size, Builder.CreateGEP(Addr, Idx), false);
    }
  }

  if (!wrote_return_column) {
    Size = ConstantInt::get(Type::getInt8Ty(Context),
                            GET_MODE_SIZE (Pmode));
    Idx  = ConstantInt::get(Type::getInt32Ty(Context),
                            DWARF_FRAME_RETURN_COLUMN);
    Builder.CreateStore(Size, Builder.CreateGEP(Addr, Idx), false);
  }

#ifdef DWARF_ALT_FRAME_RETURN_COLUMN
  Size = ConstantInt::get(Type::getInt8Ty(Context),
                          GET_MODE_SIZE (Pmode));
  Idx  = ConstantInt::get(Type::getInt32Ty(Context),
                          DWARF_ALT_FRAME_RETURN_COLUMN);
  Builder.CreateStore(Size, Builder.CreateGEP(Addr, Idx), false);
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
  Result = Builder.CreateAlloca(Type::getInt8Ty(Context), Amt);
  return true;
}

bool TreeToLLVM::EmitBuiltinExpect(gimple stmt, Value *&Result) {
  // Ignore the hint for now, just expand the expr.  This is safe, but not
  // optimal.
  Result = gimple_call_num_args(stmt) < 2 ?
    Constant::getNullValue(ConvertType(gimple_call_return_type(stmt))) :
    EmitMemory(gimple_call_arg(stmt, 0));
  return true;
}

bool TreeToLLVM::EmitBuiltinVAStart(gimple stmt) {
  if (gimple_call_num_args(stmt) < 2) {
    error_at (gimple_location(stmt),
              "too few arguments to function %<va_start%>");
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
  if (!AGGREGATE_TYPE_P(va_list_type_node)) {
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

  static const Type *VPTy = Type::getInt8PtrTy(Context);

  // FIXME: This ignores alignment and volatility of the arguments.
  SmallVector<Value *, 2> Args;
  Args.push_back(Builder.CreateBitCast(Arg1, VPTy));
  Args.push_back(Builder.CreateBitCast(Arg2, VPTy));

  Builder.CreateCall(Intrinsic::getDeclaration(TheModule, Intrinsic::vacopy),
                     Args.begin(), Args.end());
  return true;
}

bool TreeToLLVM::EmitBuiltinAdjustTrampoline(gimple stmt, Value *&Result) {
  if (!validate_gimple_arglist(stmt, POINTER_TYPE, VOID_TYPE))
    return false;

  const Type *ResultTy = ConvertType(gimple_call_return_type(stmt));

  // The adjusted value is stored as a pointer at the start of the storage GCC
  // allocated for the trampoline - load it out and return it.
  assert(TD.getPointerSize() <= TRAMPOLINE_SIZE &&
         "Trampoline smaller than a pointer!");
  Value *Tramp = EmitMemory(gimple_call_arg(stmt, 0));
  Tramp = Builder.CreateBitCast(Tramp, ResultTy->getPointerTo());
  Result = Builder.CreateLoad(Tramp, "adjusted");

  // The load has the alignment of the trampoline storage.
  unsigned Align = TYPE_ALIGN(TREE_TYPE(TREE_TYPE(gimple_call_arg(stmt, 0))))/8;
  cast<LoadInst>(Result)->setAlignment(Align);

  return true;
}

bool TreeToLLVM::EmitBuiltinInitTrampoline(gimple stmt, Value *&/*Result*/) {
  if (!validate_gimple_arglist(stmt, POINTER_TYPE, POINTER_TYPE, POINTER_TYPE,
                         VOID_TYPE))
    return false;

  // LLVM's trampoline intrinsic, llvm.init.trampoline, combines the effect of
  // GCC's init_trampoline and adjust_trampoline.  Calls to adjust_trampoline
  // should return the result of the llvm.init.trampoline call.  This is tricky
  // because the adjust_trampoline and init_trampoline calls need not occur in
  // the same function.  To overcome this, we don't store the trampoline machine
  // code in the storage GCC created for it, we store the result of the call to
  // llvm.init.trampoline there instead.  Since this storage is the argument to
  // adjust_trampoline, we turn adjust_trampoline into a load from its argument.
  // The trampoline machine code itself is stored in a stack temporary that we
  // create (one for each init_trampoline) in the function where init_trampoline
  // is called.
  static const Type *VPTy = Type::getInt8PtrTy(Context);

  // Create a stack temporary to hold the trampoline machine code.
  const Type *TrampType = ArrayType::get(Type::getInt8Ty(Context),
                                         TRAMPOLINE_SIZE);
  AllocaInst *TrampTmp = CreateTemporary(TrampType);
  TrampTmp->setAlignment(TRAMPOLINE_ALIGNMENT);
  TrampTmp->setName("TRAMP");

  Value *Func = EmitMemory(gimple_call_arg(stmt, 1));
  Value *Chain = EmitMemory(gimple_call_arg(stmt, 2));

  Value *Ops[3] = {
    Builder.CreateBitCast(TrampTmp, VPTy),
    Builder.CreateBitCast(Func, VPTy),
    Builder.CreateBitCast(Chain, VPTy)
  };

  Function *Intr = Intrinsic::getDeclaration(TheModule,
                                             Intrinsic::init_trampoline);
  Value *Adjusted = Builder.CreateCall(Intr, Ops, Ops + 3, "adjusted");

  // Store the llvm.init.trampoline result to the GCC trampoline storage.
  assert(TD.getPointerSize() <= TRAMPOLINE_SIZE &&
         "Trampoline smaller than a pointer!");
  Value *Tramp = EmitMemory(gimple_call_arg(stmt, 0));
  Tramp = Builder.CreateBitCast(Tramp, Adjusted->getType()->getPointerTo());
  StoreInst *Store = Builder.CreateStore(Adjusted, Tramp);

  // The store has the alignment of the trampoline storage.
  unsigned Align = TYPE_ALIGN(TREE_TYPE(TREE_TYPE(gimple_call_arg(stmt, 0))))/8;
  Store->setAlignment(Align);

  // The GCC trampoline storage is constant from this point on.   Tell this to
  // the optimizers.
  Intr = Intrinsic::getDeclaration(TheModule, Intrinsic::invariant_start);
  Ops[0] = ConstantInt::get(Type::getInt64Ty(Context), TRAMPOLINE_SIZE);
  Ops[1] = Builder.CreateBitCast(Tramp, VPTy);
  Builder.CreateCall(Intr, Ops, Ops + 2);

  return true;
}

//===----------------------------------------------------------------------===//
//                      ... Complex Math Expressions ...
//===----------------------------------------------------------------------===//

Value *TreeToLLVM::CreateComplex(Value *Real, Value *Imag, tree elt_type) {
  assert(Real->getType() == Imag->getType() && "Component type mismatch!");
  Real = Reg2Mem(Real, elt_type, Builder);
  Imag = Reg2Mem(Imag, elt_type, Builder);
  const Type *EltTy = Real->getType();
  Value *Result = UndefValue::get(StructType::get(Context, EltTy, EltTy, NULL));
  Result = Builder.CreateInsertValue(Result, Real, 0);
  Result = Builder.CreateInsertValue(Result, Imag, 1);
  return Result;
}

void TreeToLLVM::SplitComplex(Value *Complex, Value *&Real, Value *&Imag,
                              tree elt_type) {
  Real = Mem2Reg(Builder.CreateExtractValue(Complex, 0), elt_type, Builder);
  Imag = Mem2Reg(Builder.CreateExtractValue(Complex, 1), elt_type, Builder);
}


//===----------------------------------------------------------------------===//
//                         ... L-Value Expressions ...
//===----------------------------------------------------------------------===//

Value *TreeToLLVM::EmitFieldAnnotation(Value *FieldPtr, tree FieldDecl) {
  tree AnnotateAttr = lookup_attribute("annotate", DECL_ATTRIBUTES(FieldDecl));

  const Type *SBP = Type::getInt8PtrTy(Context);

  Function *Fn = Intrinsic::getDeclaration(TheModule,
                                           Intrinsic::ptr_annotation,
                                           &SBP, 1);

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
      assert(TREE_CODE(val) == STRING_CST &&
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

      const Type* FieldPtrType = FieldPtr->getType();
      FieldPtr = Builder.CreateCall(Fn, Ops, Ops+4);
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

  assert(TREE_CODE (ArrayTreeType) == ARRAY_TYPE && "Unknown ARRAY_REF!");

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

  const Type *IntPtrTy = getTargetData().getIntPtrType(Context);
  IndexVal = Builder.CreateIntCast(IndexVal, IntPtrTy,
                                   /*isSigned*/!TYPE_UNSIGNED(IndexType));

  // If we are indexing over a fixed-size type, just use a GEP.
  if (isSequentialCompatible(ArrayTreeType)) {
    // Avoid any assumptions about how the array type is represented in LLVM by
    // doing the GEP on a pointer to the first array element.
    const Type *EltTy = ConvertType(ElementType);
    ArrayAddr = Builder.CreateBitCast(ArrayAddr, EltTy->getPointerTo());
    Value *Ptr = POINTER_TYPE_OVERFLOW_UNDEFINED ?
      Builder.CreateInBoundsGEP(ArrayAddr, IndexVal) :
      Builder.CreateGEP(ArrayAddr, IndexVal);
    unsigned Alignment = MinAlign(ArrayAlign, TD.getABITypeAlignment(EltTy));
    return LValue(Builder.CreateBitCast(Ptr,
                  PointerType::getUnqual(ConvertType(TREE_TYPE(exp)))),
                  Alignment);
  }

  // Otherwise, just do raw, low-level pointer arithmetic.  FIXME: this could be
  // much nicer in cases like:
  //   float foo(int w, float A[][w], int g) { return A[g][0]; }

  if (VOID_TYPE_P(TREE_TYPE(ArrayTreeType))) {
    ArrayAddr = Builder.CreateBitCast(ArrayAddr, Type::getInt8PtrTy(Context));
    ArrayAddr = POINTER_TYPE_OVERFLOW_UNDEFINED ?
      Builder.CreateInBoundsGEP(ArrayAddr, IndexVal) :
      Builder.CreateGEP(ArrayAddr, IndexVal);
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
  const Type *ScaleType = IntegerType::get(Context, TYPE_ALIGN(ElementType));
  ArrayAddr = Builder.CreateBitCast(ArrayAddr, ScaleType->getPointerTo());

  IndexVal = Builder.CreateMul(IndexVal, ScaleFactor);
  unsigned Alignment = MinAlign(ArrayAlign, TYPE_ALIGN(ElementType) / 8);
  Value *Ptr = POINTER_TYPE_OVERFLOW_UNDEFINED ?
    Builder.CreateInBoundsGEP(ArrayAddr, IndexVal) :
    Builder.CreateGEP(ArrayAddr, IndexVal);
  return LValue(Builder.CreateBitCast(Ptr,
                PointerType::getUnqual(ConvertType(TREE_TYPE(exp)))),
                Alignment);
}

LValue TreeToLLVM::EmitLV_BIT_FIELD_REF(tree exp) {
  LValue Ptr = EmitLV(TREE_OPERAND(exp, 0));
  assert(!Ptr.isBitfield() && "BIT_FIELD_REF operands cannot be bitfields!");

  unsigned BitStart = (unsigned)TREE_INT_CST_LOW(TREE_OPERAND(exp, 2));
  unsigned BitSize = (unsigned)TREE_INT_CST_LOW(TREE_OPERAND(exp, 1));
  const Type *ValTy = ConvertType(TREE_TYPE(exp));

  unsigned ValueSizeInBits = TD.getTypeSizeInBits(ValTy);
  assert(BitSize <= ValueSizeInBits &&
         "ValTy isn't large enough to hold the value loaded!");

  assert(ValueSizeInBits == TD.getTypeAllocSizeInBits(ValTy) &&
         "FIXME: BIT_FIELD_REF logic is broken for non-round types");

  // BIT_FIELD_REF values can have BitStart values that are quite large.  We
  // know that the thing we are loading is ValueSizeInBits large.  If BitStart
  // is larger than ValueSizeInBits, bump the pointer over to where it should
  // be.
  if (unsigned UnitOffset = BitStart / ValueSizeInBits) {
    // TODO: If Ptr.Ptr is a struct type or something, we can do much better
    // than this.  e.g. check out when compiling unwind-dw2-fde-darwin.c.
    Ptr.Ptr = Builder.CreateBitCast(Ptr.Ptr, ValTy->getPointerTo());
    Ptr.Ptr = Builder.CreateGEP(Ptr.Ptr,
                                ConstantInt::get(Type::getInt32Ty(Context),
                                                 UnitOffset));
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

  assert((TREE_CODE(DECL_CONTEXT(FieldDecl)) == RECORD_TYPE ||
          TREE_CODE(DECL_CONTEXT(FieldDecl)) == UNION_TYPE  ||
          TREE_CODE(DECL_CONTEXT(FieldDecl)) == QUAL_UNION_TYPE));

  const Type *StructTy = ConvertType(DECL_CONTEXT(FieldDecl));

  assert((!StructAddrLV.isBitfield() ||
          StructAddrLV.BitStart == 0) && "structs cannot be bitfields!");

  StructAddrLV.Ptr = Builder.CreateBitCast(StructAddrLV.Ptr,
                                           StructTy->getPointerTo());
  const Type *FieldTy = ConvertType(TREE_TYPE(FieldDecl));

  // BitStart - This is the actual offset of the field from the start of the
  // struct, in bits.  For bitfields this may be on a non-byte boundary.
  unsigned BitStart;
  Value *FieldPtr;

  // If the GCC field directly corresponds to an LLVM field, handle it.
  unsigned MemberIndex = GetFieldIndex(FieldDecl, StructTy);
  if (MemberIndex < INT_MAX) {
    assert(!TREE_OPERAND(exp, 2) && "Constant not gimple min invariant?");
    // Get a pointer to the byte in which the GCC field starts.
    FieldPtr = Builder.CreateStructGEP(StructAddrLV.Ptr, MemberIndex);
    // Within that byte, the bit at which the GCC field starts.
    BitStart = TREE_INT_CST_LOW(DECL_FIELD_BIT_OFFSET(TREE_OPERAND(exp, 1)));
    BitStart &= 7;
  } else {
    // Offset will hold the field offset in octets.
    Value *Offset;

    assert(!(BITS_PER_UNIT & 7) && "Unit size not a multiple of 8 bits!");
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
    BitStart = getInt64(DECL_FIELD_BIT_OFFSET(FieldDecl), true);

    // Incorporate as much of it as possible into the pointer computation.
    unsigned ByteOffset = BitStart / 8;
    if (ByteOffset > 0) {
      Offset = Builder.CreateAdd(Offset,
        ConstantInt::get(Offset->getType(), ByteOffset));
      BitStart -= ByteOffset*8;
    }

    const Type *BytePtrTy = Type::getInt8PtrTy(Context);
    FieldPtr = Builder.CreateBitCast(StructAddrLV.Ptr, BytePtrTy);
    FieldPtr = Builder.CreateInBoundsGEP(FieldPtr, Offset);
    FieldPtr = Builder.CreateBitCast(FieldPtr, FieldTy->getPointerTo());
  }

  assert(BitStart < 8 && "Bit offset not properly incorporated in the pointer");

  // The alignment is given by DECL_ALIGN.  Be conservative and don't assume
  // that the field is properly aligned even if the type is not.
  LVAlign = MinAlign(LVAlign, DECL_ALIGN(FieldDecl) / 8);

  // If the FIELD_DECL has an annotate attribute on it, emit it.
  if (lookup_attribute("annotate", DECL_ATTRIBUTES(FieldDecl)))
    FieldPtr = EmitFieldAnnotation(FieldPtr, FieldDecl);

  if (!isBitfield(FieldDecl)) {
    assert(BitStart == 0 && "Not a bitfield but not at a byte offset!");
    // Make sure we return a pointer to the right type.
    const Type *EltTy = ConvertType(TREE_TYPE(exp));
    FieldPtr = Builder.CreateBitCast(FieldPtr, EltTy->getPointerTo());
    return LValue(FieldPtr, LVAlign);
  }

  // If this is a bitfield, the declared type must be an integral type.
  assert(FieldTy->isIntegerTy() && "Invalid bitfield");

  assert(DECL_SIZE(FieldDecl) &&
         TREE_CODE(DECL_SIZE(FieldDecl)) == INTEGER_CST &&
         "Variable sized bitfield?");
  unsigned BitfieldSize = TREE_INT_CST_LOW(DECL_SIZE(FieldDecl));

  const Type *LLVMFieldTy =
    cast<PointerType>(FieldPtr->getType())->getElementType();

  // If the LLVM notion of the field type contains the entire bitfield being
  // accessed, use the LLVM type.  This avoids pointer casts and other bad
  // things that are difficult to clean up later.  This occurs in cases like
  // "struct X{ unsigned long long x:50; unsigned y:2; }" when accessing y.
  // We want to access the field as a ulong, not as a uint with an offset.
  if (LLVMFieldTy->isIntegerTy() &&
      LLVMFieldTy->getPrimitiveSizeInBits() >= BitStart + BitfieldSize &&
      LLVMFieldTy->getPrimitiveSizeInBits() ==
      TD.getTypeAllocSizeInBits(LLVMFieldTy))
    FieldTy = LLVMFieldTy;
  else
    // If the field result type T is a bool or some other curiously sized
    // integer type, then not all bits may be accessible by advancing a T*
    // and loading through it.  For example, if the result type is i1 then
    // only the first bit in each byte would be loaded.  Even if T is byte
    // sized like an i24 there may be trouble: incrementing a T* will move
    // the position by 32 bits not 24, leaving the upper 8 of those 32 bits
    // inaccessible.  Avoid this by rounding up the size appropriately.
    FieldTy = IntegerType::get(Context, TD.getTypeAllocSizeInBits(FieldTy));

  assert(FieldTy->getPrimitiveSizeInBits() ==
         TD.getTypeAllocSizeInBits(FieldTy) && "Field type not sequential!");

  // If this is a bitfield, the field may span multiple fields in the LLVM
  // type.  As such, cast the pointer to be a pointer to the declared type.
  FieldPtr = Builder.CreateBitCast(FieldPtr, FieldTy->getPointerTo());

  unsigned LLVMValueBitSize = FieldTy->getPrimitiveSizeInBits();
  // Finally, because bitfields can span LLVM fields, and because the start
  // of the first LLVM field (where FieldPtr currently points) may be up to
  // 63 bits away from the start of the bitfield), it is possible that
  // *FieldPtr doesn't contain any of the bits for this bitfield. If needed,
  // adjust FieldPtr so that it is close enough to the bitfield that
  // *FieldPtr contains the first needed bit.  Be careful to make sure that
  // the pointer remains appropriately aligned.
  if (BitStart >= LLVMValueBitSize) {
    // In this case, we know that the alignment of the field is less than
    // the size of the field.  To get the pointer close enough, add some
    // number of alignment units to the pointer.
    unsigned ByteAlignment = TD.getABITypeAlignment(FieldTy);
    // It is possible that an individual field is Packed. This information is
    // not reflected in FieldTy. Check DECL_PACKED here.
    if (DECL_PACKED(FieldDecl))
      ByteAlignment = 1;
    assert(ByteAlignment*8 <= LLVMValueBitSize && "Unknown overlap case!");
    unsigned NumAlignmentUnits = BitStart/(ByteAlignment*8);
    assert(NumAlignmentUnits && "Not adjusting pointer?");

    // Compute the byte offset, and add it to the pointer.
    unsigned ByteOffset = NumAlignmentUnits*ByteAlignment;
    LVAlign = MinAlign(LVAlign, ByteOffset);

    Constant *Offset = ConstantInt::get(TD.getIntPtrType(Context), ByteOffset);
    FieldPtr = Builder.CreatePtrToInt(FieldPtr, Offset->getType());
    FieldPtr = Builder.CreateAdd(FieldPtr, Offset);
    FieldPtr = Builder.CreateIntToPtr(FieldPtr, FieldTy->getPointerTo());

    // Adjust bitstart to account for the pointer movement.
    BitStart -= ByteOffset*8;

    // Check that this worked.  Note that the bitfield may extend beyond
    // the end of *FieldPtr, for example because BitfieldSize is the same
    // as LLVMValueBitSize but BitStart > 0.
    assert(BitStart < LLVMValueBitSize &&
           BitStart+BitfieldSize < 2*LLVMValueBitSize &&
           "Couldn't get bitfield into value!");
  }

  // Okay, everything is good.  Return this as a bitfield if we can't
  // return it as a normal l-value. (e.g. "struct X { int X : 32 };" ).
  LValue LV(FieldPtr, LVAlign);
  if (BitfieldSize != LLVMValueBitSize || BitStart != 0) {
    // Writing these fields directly rather than using the appropriate LValue
    // constructor works around a miscompilation by gcc-4.4 in Release mode.
    LV.BitStart = BitStart;
    LV.BitSize = BitfieldSize;
  }
  return LV;
}

LValue TreeToLLVM::EmitLV_DECL(tree exp) {
  Value *Decl = DEFINITION_LOCAL(exp);
  if (Decl == 0) {
    if (errorcount || sorrycount) {
      const Type *Ty = ConvertType(TREE_TYPE(exp));
      const PointerType *PTy = Ty->getPointerTo();
      LValue LV(ConstantPointerNull::get(PTy), 1);
      return LV;
    }
    DieAbjectly("Referencing decl that hasn't been laid out!", exp);
  }

  const Type *Ty = ConvertType(TREE_TYPE(exp));
  // If we have "extern void foo", make the global have type {} instead of
  // type void.
  if (Ty->isVoidTy()) Ty = StructType::get(Context);
  const PointerType *PTy = Ty->getPointerTo();
  unsigned Alignment = Ty->isSized() ? TD.getABITypeAlignment(Ty) : 1;
  if (DECL_ALIGN(exp)) {
    if (DECL_USER_ALIGN(exp) || 8 * Alignment < (unsigned)DECL_ALIGN(exp))
      Alignment = DECL_ALIGN(exp) / 8;
  }

  return LValue(Builder.CreateBitCast(Decl, PTy), Alignment);
}

LValue TreeToLLVM::EmitLV_INDIRECT_REF(tree exp) {
  // The lvalue is just the address.
  LValue LV = LValue(EmitRegister(TREE_OPERAND(exp, 0)), expr_align(exp) / 8);
  // May need a useless type conversion (useless_type_conversion_p), for example
  // when INDIRECT_REF is applied to a void*, resulting in a non-void type.
  LV.Ptr = UselesslyTypeConvert(LV.Ptr,
                                ConvertType(TREE_TYPE(exp))->getPointerTo());
  return LV;
}

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
                         TD.getTypeAllocSize(Ptr.Ptr->getType()));
  return LValue(Builder.CreateStructGEP(Ptr.Ptr, Idx), Alignment);
}

LValue TreeToLLVM::EmitLV_SSA_NAME(tree exp) {
  // TODO: Check the ssa name is being used as an rvalue, see EmitLoadOfLValue.
  Value *Temp = CreateTemporary(ConvertType(TREE_TYPE(exp)));
  Builder.CreateStore(EmitReg_SSA_NAME(exp), Temp);
  return LValue(Temp, 1);
}

LValue TreeToLLVM::EmitLV_TARGET_MEM_REF(tree exp) {
  // TODO: Take the address space into account.
  // TODO: Improve the alignment estimate.
  struct mem_address addr;
  get_address_description (exp, &addr);

  LValue Ref;
  Value *Delta = 0; // Offset from base pointer in units
  if (addr.symbol) {
    Ref = EmitLV(addr.symbol);
    if (addr.base && !integer_zerop (addr.base))
      Delta = EmitRegister(addr.base);
  } else {
    assert(addr.base && "TARGET_MEM_REF has neither base nor symbol!");
    Ref = LValue(EmitRegister(addr.base), 1);
  }

  if (addr.index) {
    Value *Index = EmitRegister(addr.index);
    if (addr.step && !integer_onep (addr.step))
      Index = Builder.CreateMul(Index, EmitRegisterConstant(addr.step));
    Delta = Delta ? Builder.CreateAdd(Delta, Index) : Index;
  }

  if (addr.offset && !integer_zerop (addr.offset)) {
    Constant *Offset = EmitRegisterConstant(addr.offset);
    Delta = Delta ? Builder.CreateAdd(Delta, Offset) : Offset;
  }

  if (Delta) {
    // Advance the base pointer by the given number of units.
    Ref.Ptr = Builder.CreateBitCast(Ref.Ptr, GetUnitPointerType(Context));
    Ref.Ptr = POINTER_TYPE_OVERFLOW_UNDEFINED ?
      Builder.CreateInBoundsGEP(Ref.Ptr, Delta)
      : Builder.CreateGEP(Ref.Ptr, Delta);
    Ref.setAlignment(1); // Let the optimizers compute the alignment.
  }

  // The result can be of a different pointer type even if we didn't advance it.
  Ref.Ptr = UselesslyTypeConvert(Ref.Ptr,
                                 GetRegType(TREE_TYPE(exp))->getPointerTo());

  return Ref;
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
  Value *V = (TREE_CODE(reg) == ADDR_EXPR) ?
    EmitInvariantAddress(reg) : EmitRegisterConstant(reg);
  assert(V->getType() == GetRegType(TREE_TYPE(reg)) &&
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
  assert(TREE_CODE(addr) == ADDR_EXPR && "Invariant address not ADDR_EXPR!");
  Value *Address = EmitADDR_EXPR(addr);

  // Restore the entry block terminator.
  if (Terminator)
    EntryBlock->getInstList().push_back(Terminator);

  // Restore the builder insertion point.
  if (SavedInsertBB != EntryBlock)
    Builder.SetInsertPoint(SavedInsertBB, SavedInsertPoint);

  assert(Address->getType() == GetRegType(TREE_TYPE(addr)) &&
         "Invariant address has wrong type!");
  return Address;
}

/// EmitRegisterConstant - Convert the given global constant of register type to
/// an LLVM constant.  Creates no code, only constants.
Constant *TreeToLLVM::EmitRegisterConstant(tree reg) {
#ifndef NDEBUG
  if (!is_gimple_constant(reg))
    DieAbjectly("Not a gimple constant!", reg);
#endif
  assert(is_gimple_reg_type(TREE_TYPE(reg)) && "Not of register type!");

  switch (TREE_CODE(reg)) {
  default:
    DieAbjectly("Unhandled GIMPLE constant!", reg);

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
  return ConstantStruct::get(Context, Elts, 2, false);
}

/// EmitIntegerRegisterConstant - Turn the given INTEGER_CST into an LLVM
/// constant of the corresponding register type.
Constant *TreeToLLVM::EmitIntegerRegisterConstant(tree reg) {
  unsigned Precision = TYPE_PRECISION(TREE_TYPE(reg));

  ConstantInt *CI;
  if (HOST_BITS_PER_WIDE_INT < integerPartWidth) {
    assert(2 * HOST_BITS_PER_WIDE_INT <= integerPartWidth &&
           "Unsupported host integer precision!");
    unsigned ShiftAmt = HOST_BITS_PER_WIDE_INT;
    integerPart Val = (integerPart)(unsigned HOST_WIDE_INT)TREE_INT_CST_LOW(reg)
      + ((integerPart)(unsigned HOST_WIDE_INT)TREE_INT_CST_HIGH(reg) << ShiftAmt);
    CI = ConstantInt::get(Context, APInt(Precision, Val));
  } else {
    assert(HOST_BITS_PER_WIDE_INT == integerPartWidth &&
           "The engines cannae' take it captain!");
    integerPart Parts[] = { TREE_INT_CST_LOW(reg), TREE_INT_CST_HIGH(reg) };
    CI = ConstantInt::get(Context, APInt(Precision, 2, Parts));
  }

  // The destination can be a pointer, integer or floating point type so we need
  // a generalized cast here
  const Type *Ty = GetRegType(TREE_TYPE(reg));
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
  // The constant may have pretty much any type, for example it could be a bunch
  // of bytes.  Extract the vector elements from the constant.
  tree elt_type = TREE_TYPE (TREE_TYPE (reg));
  const Type *EltTy = GetRegType(elt_type);
  unsigned NumElts = TYPE_VECTOR_SUBPARTS(TREE_TYPE(reg));
  // Get the spacing between consecutive vector elements.  Obtain this from the
  // GCC type in case the LLVM type is something funky like i1.
  unsigned Stride = GET_MODE_BITSIZE (TYPE_MODE (elt_type));
  SmallVector<Constant*, 16> Vals(NumElts);
  for (unsigned i = 0; i != NumElts; ++i)
    Vals[i] = InterpretAsType(C, EltTy, i*Stride);
  return ConstantVector::get(Vals);
}

/// EmitVectorRegisterConstant - Turn the given VECTOR_CST into an LLVM constant
/// of the corresponding register type.
Constant *TreeToLLVM::EmitVectorRegisterConstant(tree reg) {
  // If there are no elements then immediately return the default value for a
  // small speedup.
  if (!TREE_VECTOR_CST_ELTS(reg))
    return getDefaultValue(GetRegType(TREE_TYPE(reg)));

  // Convert the elements.
  SmallVector<Constant*, 8> Elts;
  for (tree elt = TREE_VECTOR_CST_ELTS(reg); elt; elt = TREE_CHAIN(elt))
    Elts.push_back(EmitRegisterConstant(TREE_VALUE(elt)));

  // If there weren't enough elements then set the rest of the vector to the
  // default value.
  if (Elts.size() < TYPE_VECTOR_SUBPARTS(TREE_TYPE(reg))) {
    Constant *Default = getDefaultValue(GetRegType(TREE_TYPE(TREE_TYPE(reg))));
    Elts.append(TYPE_VECTOR_SUBPARTS(TREE_TYPE(reg)) - Elts.size(), Default);
  }

  return ConstantVector::get(Elts);
}

/// Mem2Reg - Convert a value of in-memory type (that given by ConvertType)
/// to in-register type (that given by GetRegType).
Value *TreeToLLVM::Mem2Reg(Value *V, tree type, LLVMBuilder &Builder) {
  const Type *MemTy = V->getType();
  const Type *RegTy = GetRegType(type);
  assert(MemTy == ConvertType(type) && "Not of memory type!");

  if (MemTy == RegTy)
    return V;

  assert(RegTy->isIntegerTy() && MemTy->isIntegerTy() &&
         "Unexpected type mismatch!");
  return Builder.CreateIntCast(V, RegTy, /*isSigned*/!TYPE_UNSIGNED(type));
}
Constant *TreeToLLVM::Mem2Reg(Constant *C, tree type, TargetFolder &Folder) {
  const Type *MemTy = C->getType();
  const Type *RegTy = GetRegType(type);
  assert(MemTy == ConvertType(type) && "Not of memory type!");

  if (MemTy == RegTy)
    return C;

  assert(RegTy->isIntegerTy() && MemTy->isIntegerTy() &&
         "Unexpected type mismatch!");
  return Folder.CreateIntCast(C, RegTy, /*isSigned*/!TYPE_UNSIGNED(type));
}

/// Reg2Mem - Convert a value of in-register type (that given by GetRegType)
/// to in-memory type (that given by ConvertType).
Value *TreeToLLVM::Reg2Mem(Value *V, tree type, LLVMBuilder &Builder) {
  const Type *RegTy = V->getType();
  const Type *MemTy = ConvertType(type);
  assert(RegTy == GetRegType(type) && "Not of register type!");

  if (RegTy == MemTy)
    return V;

  assert(RegTy->isIntegerTy() && MemTy->isIntegerTy() &&
         "Unexpected type mismatch!");
  return Builder.CreateIntCast(V, MemTy, /*isSigned*/!TYPE_UNSIGNED(type));
}

/// LoadRegisterFromMemory - Loads a value of the given scalar GCC type from
/// the memory location pointed to by Loc.  Takes care of adjusting for any
/// differences between in-memory and in-register types (the returned value
/// is of in-register type, as returned by GetRegType).
Value *TreeToLLVM::LoadRegisterFromMemory(MemRef Loc, tree type,
                                          LLVMBuilder &Builder) {
  const Type *MemTy = ConvertType(type);
  Value *Ptr = Builder.CreateBitCast(Loc.Ptr, MemTy->getPointerTo());
  LoadInst *LI = Builder.CreateLoad(Ptr, Loc.Volatile);
  LI->setAlignment(Loc.getAlignment());
  return Mem2Reg(LI, type, Builder);
}

/// StoreRegisterToMemory - Stores the given value to the memory pointed to by
/// Loc.  Takes care of adjusting for any differences between the value's type
/// (which is the in-register type given by GetRegType) and the in-memory type.
void TreeToLLVM::StoreRegisterToMemory(Value *V, MemRef Loc, tree type,
                                       LLVMBuilder &Builder) {
  const Type *MemTy = ConvertType(type);
  Value *Ptr = Builder.CreateBitCast(Loc.Ptr, MemTy->getPointerTo());
  StoreInst *SI = Builder.CreateStore(Reg2Mem(V, type, Builder), Ptr,
                                      Loc.Volatile);
  SI->setAlignment(Loc.getAlignment());
}


//===----------------------------------------------------------------------===//
//           ... EmitReg* - Convert register expression to LLVM...
//===----------------------------------------------------------------------===//

/// GetRegType - Returns the LLVM type to use for registers that hold a value
/// of the scalar GCC type 'type'.  All of the EmitReg* routines use this to
/// determine the LLVM type to return.
const Type *TreeToLLVM::GetRegType(tree type) {
  assert(!AGGREGATE_TYPE_P(type) && "Registers must have a scalar type!");
  assert(TREE_CODE(type) != VOID_TYPE && "Registers cannot have void type!");

  // For integral types, convert based on the type precision.
  if (TREE_CODE(type) == BOOLEAN_TYPE || TREE_CODE(type) == ENUMERAL_TYPE ||
      TREE_CODE(type) == INTEGER_TYPE)
    return IntegerType::get(Context, TYPE_PRECISION(type));

  // Otherwise, return the type used to represent memory.
  return ConvertType(type);
}

/// EmitMemory - Convert the specified gimple register or local constant of
/// register type to an LLVM value with in-memory type (given by ConvertType).
Value *TreeToLLVM::EmitMemory(tree reg) {
  return Reg2Mem(EmitRegister(reg), TREE_TYPE(reg), Builder);
}

/// EmitRegister - Convert the specified gimple register or local constant of
/// register type to an LLVM value.  Only creates code in the entry block.
Value *TreeToLLVM::EmitRegister(tree reg) {
  while (TREE_CODE(reg) == OBJ_TYPE_REF) reg = OBJ_TYPE_REF_EXPR(reg);
  return (TREE_CODE(reg) == SSA_NAME) ?
    EmitReg_SSA_NAME(reg) : EmitMinInvariant(reg);
}

/// EmitReg_SSA_NAME - Return the defining value of the given SSA_NAME.
/// Only creates code in the entry block.
Value *TreeToLLVM::EmitReg_SSA_NAME(tree reg) {
  assert(is_gimple_reg_type(TREE_TYPE(reg)) && "Not of register type!");

  // If we already found the definition of the SSA name, return it.
  if (Value *ExistingValue = SSANames[reg]) {
    assert(ExistingValue->getType() == GetRegType(TREE_TYPE(reg)) &&
           "SSA name has wrong type!");
    if (!isSSAPlaceholder(ExistingValue))
      return ExistingValue;
  }

  // If this is not the definition of the SSA name, return a placeholder value.
  if (!SSA_NAME_IS_DEFAULT_DEF(reg)) {
    if (Value *ExistingValue = SSANames[reg])
      return ExistingValue; // The type was sanity checked above.
    return SSANames[reg] = GetSSAPlaceholder(GetRegType(TREE_TYPE(reg)));
  }

  // This SSA name is the default definition for the underlying symbol.

  // The underlying symbol is an SSA variable.
  tree var = SSA_NAME_VAR(reg);
  assert(SSA_VAR_P(var) && "Not an SSA variable!");

  // If the variable is itself an ssa name, use its LLVM value.
  if (TREE_CODE (var) == SSA_NAME) {
    Value *Val = EmitReg_SSA_NAME(var);
    assert(Val->getType() == GetRegType(TREE_TYPE(reg)) &&
           "SSA name has wrong type!");
    return DefineSSAName(reg, Val);
  }

  // Otherwise the symbol is a VAR_DECL, PARM_DECL or RESULT_DECL.  Since a
  // default definition is only created if the very first reference to the
  // variable in the function is a read operation, and refers to the value
  // read, it has an undefined value except for PARM_DECLs.
  if (TREE_CODE(var) != PARM_DECL)
    return DefineSSAName(reg, UndefValue::get(GetRegType(TREE_TYPE(reg))));

  // Read the initial value of the parameter and associate it with the ssa name.
  assert(DECL_LOCAL_IF_SET(var) && "Parameter not laid out?");

  unsigned Alignment = DECL_ALIGN(var);
  assert(Alignment != 0 && "Parameter with unknown alignment!");

  // Perform the load in the entry block, after all parameters have been set up
  // with their initial values, and before any modifications to their values.

  // Create a builder that inserts code before the SSAInsertionPoint marker.
  LLVMBuilder SSABuilder(Context, Builder.getFolder());
  SSABuilder.SetInsertPoint(SSAInsertionPoint->getParent(), SSAInsertionPoint);

  // Use it to load the parameter value.
  MemRef ParamLoc(DECL_LOCAL_IF_SET(var), Alignment, false);
  Value *Def = LoadRegisterFromMemory(ParamLoc, TREE_TYPE(reg), SSABuilder);

  if (flag_verbose_asm)
    NameValue(Def, reg);
  return DefineSSAName(reg, Def);
}

// Unary expressions.
Value *TreeToLLVM::EmitReg_ABS_EXPR(tree op) {
  if (!FLOAT_TYPE_P(TREE_TYPE(op))) {
    Value *Op = EmitRegister(op);
    Value *OpN = Builder.CreateNeg(Op, Op->getName()+"neg");
    ICmpInst::Predicate pred = TYPE_UNSIGNED(TREE_TYPE(op)) ?
      ICmpInst::ICMP_UGE : ICmpInst::ICMP_SGE;
    Value *Cmp = Builder.CreateICmp(pred, Op,
                    Constant::getNullValue(Op->getType()), "abscond");
    return Builder.CreateSelect(Cmp, Op, OpN, Op->getName()+"abs");
  }

  // Turn FP abs into fabs/fabsf.
  StringRef Name = SelectFPName(TREE_TYPE(op), "fabsf", "fabs", "fabsl");
  CallInst *Call = EmitSimpleCall(Name, TREE_TYPE(op), op, NULL);
  Call->setDoesNotThrow();
  Call->setDoesNotAccessMemory();
  return Call;
}

Value *TreeToLLVM::EmitReg_BIT_NOT_EXPR(tree op) {
  Value *Op = EmitRegister(op);
  return Builder.CreateNot(Op, Op->getName()+"not");
}

Value *TreeToLLVM::EmitReg_CONJ_EXPR(tree op) {
  tree elt_type = TREE_TYPE(TREE_TYPE(op));
  Value *R, *I;
  SplitComplex(EmitRegister(op), R, I, elt_type);

  // ~(a+ib) = a + i*-b
  I = CreateAnyNeg(I, elt_type);

  return CreateComplex(R, I, elt_type);
}

Value *TreeToLLVM::EmitReg_CONVERT_EXPR(tree type, tree op) {
  return CastToAnyType(EmitRegister(op), !TYPE_UNSIGNED(TREE_TYPE(op)),
                       GetRegType(type), !TYPE_UNSIGNED(type));
}

Value *TreeToLLVM::EmitReg_NEGATE_EXPR(tree op) {
  Value *V = EmitRegister(op);
  tree type = TREE_TYPE(op);

  if (TREE_CODE(type) == COMPLEX_TYPE) {
    tree elt_type = TREE_TYPE(type);
    Value *R, *I; SplitComplex(V, R, I, elt_type);

    // -(a+ib) = -a + i*-b
    R = CreateAnyNeg(R, elt_type);
    I = CreateAnyNeg(I, elt_type);

    return CreateComplex(R, I, elt_type);
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
  return Builder.CreateIntCast(V, GetRegType(type), /*isSigned*/false);
}

// Comparisons.

/// EmitCompare - Compare LHS with RHS using the appropriate comparison code.
/// The result is an i1 boolean.
Value *TreeToLLVM::EmitCompare(tree lhs, tree rhs, unsigned code) {
  Value *LHS = EmitRegister(lhs);
  Value *RHS = UselesslyTypeConvert(EmitRegister(rhs), LHS->getType());

  // Compute the LLVM opcodes corresponding to the GCC comparison.
  CmpInst::Predicate UIPred = CmpInst::BAD_ICMP_PREDICATE;
  CmpInst::Predicate SIPred = CmpInst::BAD_ICMP_PREDICATE;
  CmpInst::Predicate FPPred = CmpInst::BAD_FCMP_PREDICATE;

  switch (code) {
  default:
    assert(false && "Unhandled condition code!");
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

  if (TREE_CODE(TREE_TYPE(lhs)) == COMPLEX_TYPE) {
    Value *LHSr, *LHSi;
    SplitComplex(LHS, LHSr, LHSi, TREE_TYPE(TREE_TYPE(lhs)));
    Value *RHSr, *RHSi;
    SplitComplex(RHS, RHSr, RHSi, TREE_TYPE(TREE_TYPE(lhs)));

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

Value *TreeToLLVM::EmitReg_MinMaxExpr(tree type, tree op0, tree op1,
                                      unsigned UIPred, unsigned SIPred,
                                      unsigned FPPred, bool isMax) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  const Type *Ty = GetRegType(type);

  // The LHS, RHS and Ty could be integer, floating or pointer typed. We need
  // to convert the LHS and RHS into the destination type before doing the
  // comparison. Use CastInst::getCastOpcode to get this right.
  bool TyIsSigned  = !TYPE_UNSIGNED(type);
  bool LHSIsSigned = !TYPE_UNSIGNED(TREE_TYPE(op0));
  bool RHSIsSigned = !TYPE_UNSIGNED(TREE_TYPE(op1));
  Instruction::CastOps opcode =
    CastInst::getCastOpcode(LHS, LHSIsSigned, Ty, TyIsSigned);
  LHS = Builder.CreateCast(opcode, LHS, Ty);
  opcode = CastInst::getCastOpcode(RHS, RHSIsSigned, Ty, TyIsSigned);
  RHS = Builder.CreateCast(opcode, RHS, Ty);

  Value *Compare;
  if (LHS->getType()->isFloatingPointTy())
    Compare = Builder.CreateFCmp(FCmpInst::Predicate(FPPred), LHS, RHS);
  else if (TYPE_UNSIGNED(type))
    Compare = Builder.CreateICmp(ICmpInst::Predicate(UIPred), LHS, RHS);
  else
    Compare = Builder.CreateICmp(ICmpInst::Predicate(SIPred), LHS, RHS);

  return Builder.CreateSelect(Compare, LHS, RHS, isMax ? "max" : "min");
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
  return Builder.CreateIntCast(Merge, GetRegType(type), /*isSigned*/false);
}

Value *TreeToLLVM::EmitReg_ShiftOp(tree op0, tree op1, unsigned Opc) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  if (RHS->getType() != LHS->getType())
    RHS = Builder.CreateIntCast(RHS, LHS->getType(), /*isSigned*/false,
                                RHS->getName()+".cast");

  return Builder.CreateBinOp((Instruction::BinaryOps)Opc, LHS, RHS);
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
  return Builder.CreateZExt(Res, GetRegType(type));
}

Value *TreeToLLVM::EmitReg_CEIL_DIV_EXPR(tree type, tree op0, tree op1) {
  // Notation: CEIL_DIV_EXPR <-> CDiv, TRUNC_DIV_EXPR <-> Div.

  // CDiv calculates LHS/RHS by rounding up to the nearest integer.  In terms
  // of Div this means if the values of LHS and RHS have opposite signs or if
  // LHS is zero, then CDiv necessarily equals Div; and
  //   LHS CDiv RHS = (LHS - Sign(RHS)) Div RHS + 1
  // otherwise.

  const Type *Ty = GetRegType(type);
  Constant *Zero = ConstantInt::get(Ty, 0);
  Constant *One = ConstantInt::get(Ty, 1);
  Constant *MinusOne = Constant::getAllOnesValue(Ty);

  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  if (!TYPE_UNSIGNED(type)) {
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
  return Builder.CreateAnd(EmitRegister(op0), EmitRegister(op1));
}

Value *TreeToLLVM::EmitReg_BIT_IOR_EXPR(tree op0, tree op1) {
  return Builder.CreateOr(EmitRegister(op0), EmitRegister(op1));
}

Value *TreeToLLVM::EmitReg_BIT_XOR_EXPR(tree op0, tree op1) {
  return Builder.CreateXor(EmitRegister(op0), EmitRegister(op1));
}

Value *TreeToLLVM::EmitReg_COMPLEX_EXPR(tree op0, tree op1) {
    return CreateComplex(EmitRegister(op0), EmitRegister(op1), TREE_TYPE(op1));
}

Value *TreeToLLVM::EmitReg_FLOOR_DIV_EXPR(tree type, tree op0, tree op1) {
  // Notation: FLOOR_DIV_EXPR <-> FDiv, TRUNC_DIV_EXPR <-> Div.
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  // FDiv calculates LHS/RHS by rounding down to the nearest integer.  In terms
  // of Div this means if the values of LHS and RHS have the same sign or if LHS
  // is zero, then FDiv necessarily equals Div; and
  //   LHS FDiv RHS = (LHS + Sign(RHS)) Div RHS - 1
  // otherwise.

  if (TYPE_UNSIGNED(type))
    // In the case of unsigned arithmetic, LHS and RHS necessarily have the
    // same sign, so FDiv is the same as Div.
    return Builder.CreateUDiv(LHS, RHS, "fdiv");

  const Type *Ty = GetRegType(type);
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

Value *TreeToLLVM::EmitReg_FLOOR_MOD_EXPR(tree type, tree op0, tree op1) {
  // Notation: FLOOR_MOD_EXPR <-> Mod, TRUNC_MOD_EXPR <-> Rem.

  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  // We express Mod in terms of Rem as follows: if RHS exactly divides LHS,
  // or the values of LHS and RHS have the same sign, then Mod equals Rem.
  // Otherwise Mod equals Rem + RHS.  This means that LHS Mod RHS traps iff
  // LHS Rem RHS traps.
  if (TYPE_UNSIGNED(type))
    // LHS and RHS values must have the same sign if their type is unsigned.
    return Builder.CreateURem(LHS, RHS);

  const Type *Ty = GetRegType(type);
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

  if (TREE_CODE(type) == COMPLEX_TYPE) {
    tree elt_type = TREE_TYPE(type);
    Value *LHSr, *LHSi; SplitComplex(LHS, LHSr, LHSi, elt_type);
    Value *RHSr, *RHSi; SplitComplex(RHS, RHSr, RHSi, elt_type);

    // (a+ib) - (c+id) = (a-c) + i(b-d)
    LHSr = CreateAnySub(LHSr, RHSr, elt_type);
    LHSi = CreateAnySub(LHSi, RHSi, elt_type);

    return CreateComplex(LHSr, LHSi, elt_type);
  }

  return CreateAnySub(LHS, RHS, type);
}

Value *TreeToLLVM::EmitReg_MULT_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  tree type = TREE_TYPE(op0);

  if (TREE_CODE(type) == COMPLEX_TYPE) {
    tree elt_type = TREE_TYPE(type);
    Value *LHSr, *LHSi; SplitComplex(LHS, LHSr, LHSi, elt_type);
    Value *RHSr, *RHSi; SplitComplex(RHS, RHSr, RHSi, elt_type);
    Value *DSTr, *DSTi;

    // (a+ib) * (c+id) = (ac-bd) + i(ad+cb)
    if (SCALAR_FLOAT_TYPE_P(elt_type)) {
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

    return CreateComplex(DSTr, DSTi, elt_type);
  }

  return CreateAnyMul(LHS, RHS, type);
}

Value *TreeToLLVM::EmitReg_PLUS_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  tree type = TREE_TYPE(op0);

  if (TREE_CODE(type) == COMPLEX_TYPE) {
    tree elt_type = TREE_TYPE(type);
    Value *LHSr, *LHSi; SplitComplex(LHS, LHSr, LHSi, elt_type);
    Value *RHSr, *RHSi; SplitComplex(RHS, RHSr, RHSi, elt_type);

    // (a+ib) + (c+id) = (a+c) + i(b+d)
    LHSr = CreateAnyAdd(LHSr, RHSr, elt_type);
    LHSi = CreateAnyAdd(LHSi, RHSi, elt_type);

    return CreateComplex(LHSr, LHSi, elt_type);
  }

  return CreateAnyAdd(LHS, RHS, type);
}

Value *TreeToLLVM::EmitReg_POINTER_PLUS_EXPR(tree type, tree op0, tree op1) {
  Value *Ptr = EmitRegister(op0); // The pointer.
  Value *Idx = EmitRegister(op1); // The offset in units.

  // Convert the pointer into an i8* and add the offset to it.
  Ptr = Builder.CreateBitCast(Ptr, GetUnitPointerType(Context));
  Value *GEP = POINTER_TYPE_OVERFLOW_UNDEFINED ?
    Builder.CreateInBoundsGEP(Ptr, Idx) : Builder.CreateGEP(Ptr, Idx);

  // The result may be of a different pointer type.
  return UselesslyTypeConvert(GEP, GetRegType(type));
}

Value *TreeToLLVM::EmitReg_RDIV_EXPR(tree op0, tree op1) {
  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);
  tree type = TREE_TYPE(op0);

  if (TREE_CODE(type) == COMPLEX_TYPE) {
    tree elt_type = TREE_TYPE(type);
    Value *LHSr, *LHSi; SplitComplex(LHS, LHSr, LHSi, elt_type);
    Value *RHSr, *RHSi; SplitComplex(RHS, RHSr, RHSi, elt_type);
    Value *DSTr, *DSTi;

    // (a+ib) / (c+id) = ((ac+bd)/(cc+dd)) + i((bc-ad)/(cc+dd))
    assert (SCALAR_FLOAT_TYPE_P(elt_type) && "RDIV_EXPR not floating point!");
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

    return CreateComplex(DSTr, DSTi, elt_type);
  }

  assert(FLOAT_TYPE_P(type) && "RDIV_EXPR not floating point!");
  return Builder.CreateFDiv(LHS, RHS);
}

Value *TreeToLLVM::EmitReg_ROUND_DIV_EXPR(tree type, tree op0, tree op1) {
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

  const Type *Ty = GetRegType(type);
  Constant *Zero = ConstantInt::get(Ty, 0);
  Constant *Two = ConstantInt::get(Ty, 2);

  Value *LHS = EmitRegister(op0);
  Value *RHS = EmitRegister(op1);

  if (!TYPE_UNSIGNED(type)) {
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

  if (TREE_CODE(type) == COMPLEX_TYPE) {
    tree elt_type = TREE_TYPE(type);
    Value *LHSr, *LHSi; SplitComplex(LHS, LHSr, LHSi, elt_type);
    Value *RHSr, *RHSi; SplitComplex(RHS, RHSr, RHSi, elt_type);
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

    return CreateComplex(DSTr, DSTi, elt_type);
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
      error_at(gimple_location(stmt), "operand constraints for %<asm%> differ "
               "in number of alternatives");
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
      error_at(gimple_location(stmt), "operand constraints for %<asm%> differ "
               "in number of alternatives");
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
  SmallVector<std::pair<const Type *, bool>, 4> CallResultTypes;

  // CallResultDests - Each result returned by the inline asm call is stored in
  // a memory location.  These are listed here along with a flag indicating if
  // the GCC type corresponding to the memory location is signed.  The type of
  // the memory location is allowed to differ from the type of the call result,
  // in which case the result is converted before being stored.
  SmallVector<std::pair<Value *, bool>, 4> CallResultDests;

  // CallOps - The operands pass to the inline asm call.
  std::vector<Value*> CallOps;

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
    if (TREE_CODE(Operand) == VAR_DECL && DECL_HARD_REGISTER(Operand)) {
      const char* RegName = extractRegisterName(Operand);
      int RegNum = decode_reg_name(RegName);
      if (RegNum >= 0) {
        RegName = LLVM_GET_REG_NAME(RegName, RegNum);
        unsigned RegNameLen = strlen(RegName);
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
    const Type *DestValTy = ConvertType(TREE_TYPE(Operand));
    if (TREE_CODE(Operand) == SSA_NAME) {
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
      const Type *LLVMTy = ConvertType(type);

      Value *Op = 0;
      const Type *OpTy = LLVMTy;
      if (LLVMTy->isSingleValueType()) {
        if (TREE_CODE(Val)==ADDR_EXPR &&
            TREE_CODE(TREE_OPERAND(Val,0))==LABEL_DECL) {
          // Emit the label, but do not assume it is going to be the target
          // of an indirect branch.  Having this logic here is a hack; there
          // should be a bit in the label identifying it as in an asm.
          Op = getLabelDeclBlock(TREE_OPERAND(Val, 0));
        } else if (TREE_CODE(Val) == VAR_DECL && DECL_HARD_REGISTER(Val)) {
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
        uint64_t TySize = TD.getTypeSizeInBits(LLVMTy);
        if (TySize == 1 || TySize == 8 || TySize == 16 ||
            TySize == 32 || TySize == 64 || (TySize == 128 && !AllowsMem)) {
          LLVMTy = IntegerType::get(Context, TySize);
          Op =
            Builder.CreateLoad(Builder.CreateBitCast(LV.Ptr,
                                                     LLVMTy->getPointerTo()));
        } else {
          // Codegen only supports indirect operands with mem constraints.
          if (!AllowsMem)
            error_at(gimple_location(stmt),
                     "aggregate does not match inline asm register constraint");
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
        unsigned Match = atoi(Constraint);
        // This output might have gotten put in either CallResult or CallArg
        // depending whether it's a register or not.  Find its type.
        const Type *OTy = 0;
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
          if (!(OTy->isIntegerTy() || OTy->isPointerTy()) ||
              !(OpTy->isIntegerTy() || OpTy->isPointerTy())) {
            error_at(gimple_location(stmt),
                     "unsupported inline asm: input constraint with a matching "
                     "output constraint of incompatible type!");
            return;
          }
          unsigned OTyBits = TD.getTypeSizeInBits(OTy);
          unsigned OpTyBits = TD.getTypeSizeInBits(OpTy);
          if (OTyBits == 0 || OpTyBits == 0) {
            error_at(gimple_location(stmt), "unsupported inline asm: input "
                     "constraint with a matching output constraint of "
                     "incompatible type!");
            return;
          } else if (OTyBits < OpTyBits) {
            // The output is smaller than the input.  If the output is not a
            // register then bail out.  Likewise, if the output is explicitly
            // mentioned in the asm string then we cannot safely promote it,
            // so bail out in this case too.
            if (!OutputLocations[Match].first ||
                isOperandMentioned(stmt, Match)) {
              error_at(gimple_location(stmt), "unsupported inline asm: input "
                       "constraint with a matching output constraint of "
                       "incompatible type!");
              return;
            }
            // Use the input type for the output, and arrange for the result to
            // be truncated to the original output type after the asm call.
            CallResultTypes[OutputIndex] = std::make_pair(OpTy, IsSigned);
          } else if (OTyBits > OpTyBits) {
            // The input is smaller than the output.  If the input is explicitly
            // mentioned in the asm string then we cannot safely promote it, so
            // bail out.
            if (isOperandMentioned(stmt, NumOutputs + i)) {
              error_at(gimple_location(stmt), "unsupported inline asm: input "
                       "constraint with a matching output constraint of "
                       "incompatible type!");
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
    if (TREE_CODE(Val) == VAR_DECL && DECL_HARD_REGISTER(Val)) {
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
      error_at(gimple_location(stmt), "unknown register name %qs in %<asm%>",
               RegName);
      return;
    case -3:     // cc
      ConstraintStr += ",~{cc}";
      break;
    case -4:     // memory
      ConstraintStr += ",~{memory}";
      break;
    default:     // Normal register name.
      RegName = LLVM_GET_REG_NAME(RegName, RegCode);
      ConstraintStr += ",~{";
      ConstraintStr += RegName;
      ConstraintStr += "}";
      break;
    }
  }

  // Compute the return type to use for the asm call.
  const Type *CallResultType;
  switch (CallResultTypes.size()) {
  // If there are no results then the return type is void!
  case 0: CallResultType = Type::getVoidTy(Context); break;
  // If there is one result then use the result's type as the return type.
  case 1: CallResultType = CallResultTypes[0].first; break;
  // If the asm returns multiple results then create a struct type with the
  // result types as its fields, and use it for the return type.
  default:
    std::vector<const Type*> Fields(CallResultTypes.size());
    for (unsigned i = 0, e = CallResultTypes.size(); i != e; ++i)
      Fields[i] = CallResultTypes[i].first;
    CallResultType = StructType::get(Context, Fields);
    break;
  }

  // Compute the types of the arguments to the asm call.
  std::vector<const Type*> CallArgTypes(CallOps.size());
  for (unsigned i = 0, e = CallOps.size(); i != e; ++i)
    CallArgTypes[i] = CallOps[i]->getType();

  // Get the type of the called asm "function".
  const FunctionType *FTy =
    FunctionType::get(CallResultType, CallArgTypes, false);

  // Remove the leading comma if we have operands.
  if (!ConstraintStr.empty())
    ConstraintStr.erase(ConstraintStr.begin());

  // Make sure we're created a valid inline asm expression.
  if (!InlineAsm::Verify(FTy, ConstraintStr)) {
    error_at(gimple_location(stmt), "Invalid or unsupported inline assembly!");
    return;
  }

  std::string NewAsmStr = ConvertInlineAsmStr(stmt, NumOutputs+NumInputs);
  Value *Asm = InlineAsm::get(FTy, NewAsmStr, ConstraintStr, HasSideEffects);
  CallInst *CV = Builder.CreateCall(Asm, CallOps.begin(), CallOps.end(),
                                    CallResultTypes.empty() ? "" : "asmtmp");
  CV->setDoesNotThrow();

  // If the call produces a value, store it into the destination.
  for (unsigned i = 0, NumResults = CallResultTypes.size(); i != NumResults;
       ++i) {
    Value *Val = NumResults == 1 ?
      CV : Builder.CreateExtractValue(CV, i, "asmresult");
    bool ValIsSigned = CallResultTypes[i].second;

    Value *Dest = CallResultDests[i].first;
    const Type *DestTy = cast<PointerType>(Dest->getType())->getElementType();
    bool DestIsSigned = CallResultDests[i].second;
    Val = CastToAnyType(Val, ValIsSigned, DestTy, DestIsSigned);
    Builder.CreateStore(Val, Dest);
  }

  // If the call defined any ssa names, associate them with their value.
  for (unsigned i = 0, e = SSADefinitions.size(); i != e; ++i) {
    tree Name = SSADefinitions[i].first;
    MemRef Loc = SSADefinitions[i].second;
    Value *Val = LoadRegisterFromMemory(Loc, TREE_TYPE(Name), Builder);
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
  if (AGGREGATE_TYPE_P(TREE_TYPE(lhs))) {
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
    if (!AGGREGATE_TYPE_P(gimple_call_return_type(stmt))) {
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

  if (AGGREGATE_TYPE_P(TREE_TYPE(lhs))) {
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
    DieAbjectly("Unexpected region type!");
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

        TypeInfo = Builder.CreateBitCast(TypeInfo, Type::getInt8PtrTy(Context));

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

  if (TREE_CODE(dest) == LABEL_DECL) {
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

  // The exception unwinds out of the function.  Note the exception to unwind.
  if (!RewindTmp) {
    RewindTmp = CreateTemporary(Type::getInt8PtrTy(Context));
    RewindTmp->setName("rewind_tmp");
  }
  Value *ExcPtr = Builder.CreateLoad(getExceptionPtr(src_rgn->index));
  Builder.CreateStore(ExcPtr, RewindTmp);

  // Jump to the block containing the rewind code.
  if (!RewindBB)
    RewindBB = BasicBlock::Create(Context, "rewind");
  Builder.CreateBr(RewindBB);
}

void TreeToLLVM::RenderGIMPLE_RETURN(gimple stmt) {
  tree retval = gimple_return_retval(stmt);
  tree result = DECL_RESULT(current_function_decl);

  if (retval && retval != error_mark_node && retval != result) {
    // Store the return value to the function's DECL_RESULT.
    MemRef DestLoc(DECL_LOCAL(result), 1, false); // FIXME: What alignment?
    if (AGGREGATE_TYPE_P(TREE_TYPE(result))) {
      EmitAggregate(retval, DestLoc);
    } else {
      Value *Val = Builder.CreateBitCast(EmitRegister(retval),
                                         GetRegType(TREE_TYPE(result)));
      StoreRegisterToMemory(Val, DestLoc, TREE_TYPE(result), Builder);
    }
  }

  // Emit a branch to the exit label.
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
  for (size_t i = 1, e = gimple_switch_num_labels(stmt); i != e; ++i) {
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
    SI->setSuccessor(0, IfBlock);
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
    assert(RHS->getType() == GetRegType(TREE_TYPE(gimple_assign_rhs1(stmt))) &&
           "RHS has wrong type!");
    return RHS;
  }

  // The RHS is a register expression.  Emit it now.
  tree type = TREE_TYPE(gimple_assign_lhs(stmt));
  tree_code code = gimple_assign_rhs_code(stmt);
  tree rhs1 = gimple_assign_rhs1(stmt);
  tree rhs2 = gimple_assign_rhs2(stmt);

  Value *RHS = 0;
  switch (code) {
  default:
    DieAbjectly("Unhandled GIMPLE assignment!", stmt);

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
    RHS = Builder.CreateZExt(EmitCompare(rhs1, rhs2, code), GetRegType(type));
    break;

  // Binary expressions.
  case BIT_AND_EXPR:
    RHS = EmitReg_BIT_AND_EXPR(rhs1, rhs2); break;
  case BIT_IOR_EXPR:
    RHS = EmitReg_BIT_IOR_EXPR(rhs1, rhs2); break;
  case BIT_XOR_EXPR:
    RHS = EmitReg_BIT_XOR_EXPR(rhs1, rhs2); break;
  case CEIL_DIV_EXPR:
    RHS = EmitReg_CEIL_DIV_EXPR(type, rhs1, rhs2); break;
  case COMPLEX_EXPR:
    RHS = EmitReg_COMPLEX_EXPR(rhs1, rhs2); break;
  case EXACT_DIV_EXPR:
    RHS = EmitReg_TRUNC_DIV_EXPR(rhs1, rhs2, /*isExact*/true); break;
  case FLOOR_DIV_EXPR:
    RHS = EmitReg_FLOOR_DIV_EXPR(type, rhs1, rhs2); break;
  case FLOOR_MOD_EXPR:
    RHS = EmitReg_FLOOR_MOD_EXPR(type, rhs1, rhs2); break;
  case LROTATE_EXPR:
    RHS = EmitReg_RotateOp(type, rhs1, rhs2, Instruction::Shl,
                           Instruction::LShr);
    break;
  case LSHIFT_EXPR:
    RHS = EmitReg_ShiftOp(rhs1, rhs2, Instruction::Shl); break;
  case MAX_EXPR:
    RHS = EmitReg_MinMaxExpr(type, rhs1, rhs2, ICmpInst::ICMP_UGE,
                             ICmpInst::ICMP_SGE, FCmpInst::FCMP_OGE, true);
    break;
  case MIN_EXPR:
    RHS = EmitReg_MinMaxExpr(type, rhs1, rhs2, ICmpInst::ICMP_ULE,
                             ICmpInst::ICMP_SLE, FCmpInst::FCMP_OLE, false);
    break;
  case MINUS_EXPR:
    RHS = EmitReg_MINUS_EXPR(rhs1, rhs2); break;
  case MULT_EXPR:
    RHS = EmitReg_MULT_EXPR(rhs1, rhs2); break;
  case PLUS_EXPR:
    RHS = EmitReg_PLUS_EXPR(rhs1, rhs2); break;
  case POINTER_PLUS_EXPR:
    RHS = EmitReg_POINTER_PLUS_EXPR(type, rhs1, rhs2); break;
  case RDIV_EXPR:
    RHS = EmitReg_RDIV_EXPR(rhs1, rhs2); break;
  case ROUND_DIV_EXPR:
    RHS = EmitReg_ROUND_DIV_EXPR(type, rhs1, rhs2); break;
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
  }

  assert(RHS->getType() == GetRegType(type) && "RHS has wrong type!");
  return RHS;
}

/// EmitAssignSingleRHS - Helper for EmitAssignRHS.  Handles those RHS that are
/// not register expressions.
Value *TreeToLLVM::EmitAssignSingleRHS(tree rhs) {
  assert(!AGGREGATE_TYPE_P(TREE_TYPE(rhs)) && "Expected a scalar type!");

  switch (TREE_CODE(rhs)) {
  // Catch-all for SSA names, constants etc.
  default: return EmitRegister(rhs);

  // Expressions (tcc_expression).
  case ADDR_EXPR:    return EmitADDR_EXPR(rhs);
  case OBJ_TYPE_REF: return EmitOBJ_TYPE_REF(rhs);

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
         (TREE_CODE(TREE_TYPE (call_expr)) == POINTER_TYPE ||
          TREE_CODE(TREE_TYPE (call_expr)) == REFERENCE_TYPE)
         && "Not calling a function pointer?");

  tree function_type = TREE_TYPE(TREE_TYPE (call_expr));
  Value *Callee = EmitRegister(call_expr);
  CallingConv::ID CallingConv;
  AttrListPtr PAL;

  const Type *Ty =
    TheTypeConverter->ConvertFunctionType(function_type,
                                          fndecl,
                                          gimple_call_chain(stmt),
                                          CallingConv, PAL);

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
  // Perform a useless type conversion (useless_type_conversion_p).
  RHS = UselesslyTypeConvert(RHS, GetRegType(TREE_TYPE(lhs)));

  // If this is the definition of an ssa name, record it in the SSANames map.
  if (TREE_CODE(lhs) == SSA_NAME) {
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
    StoreRegisterToMemory(RHS, LV, TREE_TYPE(lhs), Builder);
    return;
  }

  // Last case, this is a store to a bitfield, so we have to emit a
  // read/modify/write sequence.

  if (!LV.BitSize)
    return;

  unsigned Alignment = LV.getAlignment();

  const Type *ValTy = cast<PointerType>(LV.Ptr->getType())->getElementType();
  unsigned ValSizeInBits = ValTy->getPrimitiveSizeInBits();

  // The number of stores needed to write the entire bitfield.
  unsigned Strides = 1 + (LV.BitStart + LV.BitSize - 1) / ValSizeInBits;

  assert(ValTy->isIntegerTy() && "Invalid bitfield lvalue!");
  assert(ValSizeInBits > LV.BitStart && "Bad bitfield lvalue!");
  assert(ValSizeInBits >= LV.BitSize && "Bad bitfield lvalue!");
  assert(2*ValSizeInBits > LV.BitSize+LV.BitStart && "Bad bitfield lvalue!");

  bool Signed = !TYPE_UNSIGNED(TREE_TYPE(lhs));
  RHS = CastToAnyType(RHS, Signed, ValTy, Signed);

  for (unsigned I = 0; I < Strides; I++) {
    unsigned Index = BYTES_BIG_ENDIAN ? Strides - I - 1 : I; // LSB first
    unsigned ThisFirstBit = Index * ValSizeInBits;
    unsigned ThisLastBitPlusOne = ThisFirstBit + ValSizeInBits;
    if (ThisFirstBit < LV.BitStart)
      ThisFirstBit = LV.BitStart;
    if (ThisLastBitPlusOne > LV.BitStart+LV.BitSize)
      ThisLastBitPlusOne = LV.BitStart+LV.BitSize;

    Value *Ptr = Index ?
      Builder.CreateGEP(LV.Ptr,
                        ConstantInt::get(Type::getInt32Ty(Context), Index)) :
      LV.Ptr;
    LoadInst *LI = Builder.CreateLoad(Ptr, LV.Volatile);
    LI->setAlignment(Alignment);
    Value *OldVal = LI;
    Value *NewVal = RHS;

    unsigned BitsInVal = ThisLastBitPlusOne - ThisFirstBit;
    unsigned FirstBitInVal = ThisFirstBit % ValSizeInBits;

    if (BYTES_BIG_ENDIAN)
      FirstBitInVal = ValSizeInBits-FirstBitInVal-BitsInVal;

    // If not storing into the zero'th bit, shift the Src value to the left.
    if (FirstBitInVal) {
      Value *ShAmt = ConstantInt::get(ValTy, FirstBitInVal);
      NewVal = Builder.CreateShl(NewVal, ShAmt);
    }

    // Next, if this doesn't touch the top bit, mask out any bits that shouldn't
    // be set in the result.
    uint64_t MaskVal = 1;
    MaskVal = ((MaskVal << BitsInVal)-1) << FirstBitInVal;
    Constant *Mask = ConstantInt::get(Type::getInt64Ty(Context), MaskVal);
    Mask = Builder.getFolder().CreateTruncOrBitCast(Mask, ValTy);

    if (FirstBitInVal+BitsInVal != ValSizeInBits)
      NewVal = Builder.CreateAnd(NewVal, Mask);

    // Next, mask out the bits this bit-field should include from the old value.
    Mask = Builder.getFolder().CreateNot(Mask);
    OldVal = Builder.CreateAnd(OldVal, Mask);

    // Finally, merge the two together and store it.
    NewVal = Builder.CreateOr(OldVal, NewVal);

    StoreInst *SI = Builder.CreateStore(NewVal, Ptr, LV.Volatile);
    SI->setAlignment(Alignment);

    if (I + 1 < Strides) {
      Value *ShAmt = ConstantInt::get(ValTy, BitsInVal);
      RHS = Builder.CreateLShr(RHS, ShAmt);
    }
  }
}
