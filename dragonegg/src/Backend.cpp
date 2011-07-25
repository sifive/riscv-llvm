//===----------- Backend.cpp - High-level LLVM backend interface ----------===//
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
// This file defines the high-level LLVM backend interface.
//===----------------------------------------------------------------------===//

// Plugin headers
extern "C" {
#include "dragonegg/cache.h"
}
#include "dragonegg/Constants.h"
#include "dragonegg/Debug.h"
#include "dragonegg/OS.h"
#include "dragonegg/Target.h"
#include "dragonegg/Trees.h"

// LLVM headers
#define DEBUG_TYPE "plugin"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Assembly/PrintModulePass.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/CodeGen/RegAllocRegistry.h"
#include "llvm/MC/MCCodeGenInfo.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PassManagerBuilder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetLibraryInfo.h"
#include "llvm/Target/TargetRegistry.h"

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

#include "debug.h"
#include "diagnostic.h"
#include "flags.h"
#include "gcc-plugin.h"
#include "intl.h"
#include "langhooks.h"
#include "output.h"
#include "params.h"
#include "plugin-version.h"
#include "toplev.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "version.h"
}

#if (GCC_MAJOR != 4)
#error Unsupported GCC major version
#endif

// Non-zero if libcalls should not be simplified.
int flag_no_simplify_libcalls;

// Non-zero if red-zone is disabled.
//TODOstatic int flag_disable_red_zone = 0;

// Non-zero if implicit floating point instructions are disabled.
//TODOstatic int flag_no_implicit_float = 0;

/// llvm_asm_file_name - Name of file to use for assembly code output.
static const char *llvm_asm_file_name;

// Global state for the LLVM backend.
Module *TheModule = 0;
DebugInfo *TheDebugInfo = 0;
PassManagerBuilder PassBuilder;
TargetMachine *TheTarget = 0;
TargetFolder *TheFolder = 0;
TypeConverter *TheTypeConverter = 0;
raw_ostream *OutStream = 0; // Stream to write assembly code to.
formatted_raw_ostream FormattedOutStream;

static bool DebugPassArguments;
static bool DebugPassStructure;
static bool EnableGCCOptimizations;
static bool EmitIR;
static bool SaveGCCOutput;
static int LLVMCodeGenOptimizeArg = -1;
static int LLVMIROptimizeArg = -1;

std::vector<std::pair<Constant*, int> > StaticCtors, StaticDtors;
SmallSetVector<Constant*, 32> AttributeUsedGlobals;
SmallSetVector<Constant*, 32> AttributeCompilerUsedGlobals;
std::vector<Constant*> AttributeAnnotateGlobals;

/// PerFunctionPasses - This is the list of cleanup passes run per-function
/// as each is compiled.  In cases where we are not doing IPO, it includes the
/// code generator.
static FunctionPassManager *PerFunctionPasses = 0;
static PassManager *PerModulePasses = 0;
static FunctionPassManager *CodeGenPasses = 0;

static void createPerFunctionOptimizationPasses();
static void createPerModuleOptimizationPasses();

//===----------------------------------------------------------------------===//
//                   Matching LLVM Values with GCC DECL trees
//===----------------------------------------------------------------------===//

/// set_decl_llvm - Remember the LLVM value for a GCC declaration.
Value *set_decl_llvm (tree t, Value *V) {
  assert(HAS_RTL_P(t) && "Expected a declaration with RTL!");
  return (Value *)llvm_set_cached(t, V);
}

/// get_decl_llvm - Retrieve the LLVM value for a GCC declaration, or NULL.
Value *get_decl_llvm(tree t) {
  assert(HAS_RTL_P(t) && "Expected a declaration with RTL!");
  return (Value *)llvm_get_cached(t);
}

/// set_decl_index - Associate a non-negative number with the given GCC
/// declaration.
int set_decl_index(tree t, int i) {
  assert(!HAS_RTL_P(t) && "Expected a declaration without RTL!");
  assert(i >= 0 && "Negative indices not allowed!");
  // In order to use zero as a special value (see get_decl_index) map the range
  // 0 .. INT_MAX to -1 .. INT_MIN.
  llvm_set_cached(t, (void *)(intptr_t)(-i - 1));
  return i;
}

/// get_decl_index - Get the non-negative number associated with the given GCC
/// declaration.  Returns a negative value if no such association has been made.
int get_decl_index(tree t) {
  assert(!HAS_RTL_P(t) && "Expected a declaration without RTL!");
  // Map the range -1 .. INT_MIN back to 0 .. INT_MAX (see set_decl_index) and
  // send 0 (aka void) to -1.
  return -(1 + (int)(intptr_t)llvm_get_cached(t));
}

/// changeLLVMConstant - Replace Old with New everywhere, updating all maps
/// (except for AttributeAnnotateGlobals, which is a different kind of animal).
/// At this point we know that New is not in any of these maps.
void changeLLVMConstant(Constant *Old, Constant *New) {
  assert(Old->use_empty() && "Old value has uses!");

  if (AttributeUsedGlobals.count(Old)) {
    AttributeUsedGlobals.remove(Old);
    AttributeUsedGlobals.insert(New);
  }

  if (AttributeCompilerUsedGlobals.count(Old)) {
    AttributeCompilerUsedGlobals.remove(Old);
    AttributeCompilerUsedGlobals.insert(New);
  }

  for (unsigned i = 0, e = StaticCtors.size(); i != e; ++i) {
    if (StaticCtors[i].first == Old)
      StaticCtors[i].first = New;
  }

  for (unsigned i = 0, e = StaticDtors.size(); i != e; ++i) {
    if (StaticDtors[i].first == Old)
      StaticDtors[i].first = New;
  }

  llvm_replace_cached(Old, New);
}

/// handleVisibility - Forward decl visibility style to global.
void handleVisibility(tree decl, GlobalValue *GV) {
  // If decl has visibility specified explicitely (via attribute) - honour
  // it. Otherwise (e.g. visibility specified via -fvisibility=hidden) honour
  // only if symbol is local.
  if (TREE_PUBLIC(decl) &&
      (DECL_VISIBILITY_SPECIFIED(decl) || !DECL_EXTERNAL(decl))) {
    if (DECL_VISIBILITY(decl) == VISIBILITY_HIDDEN)
      GV->setVisibility(GlobalValue::HiddenVisibility);
    else if (DECL_VISIBILITY(decl) == VISIBILITY_PROTECTED)
      GV->setVisibility(GlobalValue::ProtectedVisibility);
    else if (DECL_VISIBILITY(decl) == VISIBILITY_DEFAULT)
      GV->setVisibility(Function::DefaultVisibility);
  }
}

/// CodeGenOptLevel - The optimization level to be used by the code generators.
static CodeGenOpt::Level CodeGenOptLevel() {
  int OptLevel = LLVMCodeGenOptimizeArg >= 0 ?
    LLVMCodeGenOptimizeArg : optimize;
  if (OptLevel <= 0)
    return CodeGenOpt::None;
  if (OptLevel == 1)
    return CodeGenOpt::Less;
  if (OptLevel == 2) // Includes -Os.
    return CodeGenOpt::Default;
  return CodeGenOpt::Aggressive;
}

/// PerFunctionOptLevel - The optimization level to be used by the per-function
/// IR optimizers.
static int PerFunctionOptLevel() {
  return LLVMIROptimizeArg >= 0 ? LLVMIROptimizeArg : optimize;
}

/// ModuleOptLevel - The optimization level to be used by the module level IR
/// optimizers.
static int ModuleOptLevel() {
  if (LLVMIROptimizeArg >= 0)
    return LLVMIROptimizeArg;
  if (EnableGCCOptimizations)
    return 0;
  return optimize;
}

// SizeOfGlobalMatchesDecl - Whether the size of the given global value is the
// same as that of the given GCC declaration.  Conservatively returns 'true' if
// the answer is unclear.
static LLVM_ATTRIBUTE_UNUSED // Only called from asserts.
bool SizeOfGlobalMatchesDecl(GlobalValue *GV, tree decl) {
  // If the GCC declaration has no size then nothing useful can be said here.
  if (!DECL_SIZE(decl))
    return true;
  assert(isInt64(DECL_SIZE(decl), true) && "Global decl with variable size!");

  Type *Ty = GV->getType()->getElementType();
  // If the LLVM type has no size then a useful comparison cannot be made.
  if (!Ty->isSized())
    return true;

  // DECL_SIZE need not be a multiple of the alignment, while the LLVM size
  // always is.  Correct for this.
  // TODO: Change getTypeSizeInBits for aggregate types so it is no longer
  // rounded up to the alignment.
  uint64_t gcc_size = getInt64(DECL_SIZE(decl), true);
  const TargetData *TD = TheTarget->getTargetData();
  unsigned Align = 8 * TD->getABITypeAlignment(Ty);
  return TheTarget->getTargetData()->getTypeAllocSizeInBits(Ty) ==
    ((gcc_size + Align - 1) / Align) * Align;
}

#ifndef LLVM_TARGET_NAME
#error LLVM_TARGET_NAME macro not specified
#endif

namespace llvm {
#define Declare2(TARG, MOD)   extern "C" void LLVMInitialize ## TARG ## MOD()
#define Declare(T, M) Declare2(T, M)
  Declare(LLVM_TARGET_NAME, TargetInfo);
  Declare(LLVM_TARGET_NAME, Target);
  Declare(LLVM_TARGET_NAME, TargetMC);
  Declare(LLVM_TARGET_NAME, AsmPrinter);
#undef Declare
#undef Declare2
}

/// ConfigureLLVM - Initialized and configure LLVM.
static void ConfigureLLVM(void) {
  // Initialize the LLVM backend.
#define DoInit2(TARG, MOD)  LLVMInitialize ## TARG ## MOD()
#define DoInit(T, M) DoInit2(T, M)
  DoInit(LLVM_TARGET_NAME, TargetInfo);
  DoInit(LLVM_TARGET_NAME, Target);
  DoInit(LLVM_TARGET_NAME, TargetMC);
  DoInit(LLVM_TARGET_NAME, AsmPrinter);
#undef DoInit
#undef DoInit2

  // Initialize LLVM command line options.
  std::vector<const char*> Args;
  Args.push_back(progname); // program name

//TODO  // Allow targets to specify PIC options and other stuff to the corresponding
//TODO  // LLVM backends.
//TODO#ifdef LLVM_SET_RED_ZONE_FLAG
//TODO  LLVM_SET_RED_ZONE_FLAG(flag_disable_red_zone)
//TODO#endif
#ifdef LLVM_SET_TARGET_OPTIONS
  LLVM_SET_TARGET_OPTIONS(Args);
#endif
#ifdef LLVM_SET_MACHINE_OPTIONS
  LLVM_SET_MACHINE_OPTIONS(Args);
#endif
//TODO#ifdef LLVM_SET_IMPLICIT_FLOAT
//TODO  LLVM_SET_IMPLICIT_FLOAT(flag_no_implicit_float)
//TODO#endif

  if (time_report || !quiet_flag  || flag_detailed_statistics)
    Args.push_back("--time-passes");
  if (!quiet_flag  || flag_detailed_statistics)
    Args.push_back("--stats");
#if (GCC_MINOR > 5)
  if (fast_math_flags_set_p(&global_options))
#else
  if (fast_math_flags_set_p())
#endif
    Args.push_back("--enable-unsafe-fp-math");
  if (flag_finite_math_only) {
    Args.push_back("--enable-no-nans-fp-math");
    Args.push_back("--enable-no-infs-fp-math");
  }
  if (!flag_omit_frame_pointer)
    Args.push_back("--disable-fp-elim");
  if (!flag_zero_initialized_in_bss)
    Args.push_back("--nozero-initialized-in-bss");
  if (flag_verbose_asm)
    Args.push_back("--asm-verbose");
  if (DebugPassStructure)
    Args.push_back("--debug-pass=Structure");
  if (DebugPassArguments)
    Args.push_back("--debug-pass=Arguments");
  if (!flag_schedule_insns)
    Args.push_back("--pre-RA-sched=source");
  if (flag_function_sections)
    Args.push_back("--ffunction-sections");
  if (flag_data_sections)
    Args.push_back("--fdata-sections");

  // If there are options that should be passed through to the LLVM backend
  // directly from the command line, do so now.  This is mainly for debugging
  // purposes, and shouldn't really be for general use.
  std::vector<std::string> ArgStrings;

//TODO  if (flag_limited_precision > 0) {
//TODO    std::string Arg("--limit-float-precision="+utostr(flag_limited_precision));
//TODO    ArgStrings.push_back(Arg);
//TODO  }

  if (flag_stack_protect > 0) {
    std::string Arg("--stack-protector-buffer-size=" +
                    utostr(PARAM_VALUE(PARAM_SSP_BUFFER_SIZE)));
    ArgStrings.push_back(Arg);
  }

  for (unsigned i = 0, e = ArgStrings.size(); i != e; ++i)
    Args.push_back(ArgStrings[i].c_str());

//TODO  std::vector<std::string> LLVM_Optns; // Avoid deallocation before opts parsed!
//TODO  if (llvm_optns) {
//TODO    llvm::SmallVector<llvm::StringRef, 16> Buf;
//TODO    SplitString(llvm_optns, Buf);
//TODO    for(unsigned i = 0, e = Buf.size(); i != e; ++i) {
//TODO      LLVM_Optns.push_back(Buf[i]);
//TODO      Args.push_back(LLVM_Optns.back().c_str());
//TODO    }
//TODO  }

  Args.push_back(0);  // Null terminator.
  int pseudo_argc = Args.size()-1;
  llvm::cl::ParseCommandLineOptions(pseudo_argc, const_cast<char**>(&Args[0]));
}

/// ComputeTargetTriple - Determine the target triple to use.
static std::string ComputeTargetTriple() {
  // If the target wants to override the architecture, e.g. turning
  // powerpc-darwin-... into powerpc64-darwin-... when -m64 is enabled, do so
  // now.
  std::string TargetTriple = TARGET_NAME;
#ifdef LLVM_OVERRIDE_TARGET_ARCH
  std::string Arch = LLVM_OVERRIDE_TARGET_ARCH();
  if (!Arch.empty()) {
    std::string::size_type DashPos = TargetTriple.find('-');
    if (DashPos != std::string::npos)// If we have a sane t-t, replace the arch.
      TargetTriple = Arch + TargetTriple.substr(DashPos);
  }
#endif
#ifdef LLVM_OVERRIDE_TARGET_VERSION
  char *NewTriple;
  bool OverRidden = LLVM_OVERRIDE_TARGET_VERSION(TargetTriple.c_str(),
                                                 &NewTriple);
  if (OverRidden)
    TargetTriple = std::string(NewTriple);
#endif
  return TargetTriple;
}

/// CreateTargetMachine - Create the TargetMachine we will generate code with.
static void CreateTargetMachine(const std::string &TargetTriple) {
  // FIXME: Figure out how to select the target and pass down subtarget info.
  std::string Err;
  const Target *TME =
    TargetRegistry::lookupTarget(TargetTriple, Err);
  if (!TME)
    report_fatal_error(Err);

  // Figure out the subtarget feature string we pass to the target.
  std::string FeatureStr;
  // The target can set LLVM_SET_SUBTARGET_FEATURES to configure the LLVM
  // backend.
  std::string CPU;
#ifdef LLVM_SET_SUBTARGET_FEATURES
  SubtargetFeatures Features;
  LLVM_SET_SUBTARGET_FEATURES(CPU, Features);
  FeatureStr = Features.getString();
#endif

  // The target can set LLVM_SET_RELOC_MODEL to configure the relocation
  // model used by the LLVM backend.
  Reloc::Model RelocModel = Reloc::Default;
#ifdef LLVM_SET_RELOC_MODEL
  LLVM_SET_RELOC_MODEL(RelocModel);
#endif
  TheTarget = TME->createTargetMachine(TargetTriple, CPU, FeatureStr,
                                       RelocModel);
  TheTarget->setMCUseCFI(flag_dwarf2_cfi_asm);
  assert(TheTarget->getTargetData()->isBigEndian() == BYTES_BIG_ENDIAN);
}

/// CreateModule - Create and initialize a module to output LLVM IR to.
static void CreateModule(const std::string &TargetTriple) {
  // Create the module itself.
  StringRef ModuleID = main_input_filename ? main_input_filename : "";
  TheModule = new Module(ModuleID, getGlobalContext());

  // Insert a special .ident directive to identify the version of the plugin
  // which compiled this code.  The format of the .ident string is patterned
  // after the ones produced by GCC.
#ifdef IDENT_ASM_OP
  if (!flag_no_ident) {
    const char *pkg_version = "(GNU) ";

    if (strcmp ("(GCC) ", pkgversion_string))
      pkg_version = pkgversion_string;

    std::string IdentString = IDENT_ASM_OP;
    IdentString += "\"GCC: ";
    IdentString += pkg_version;
    IdentString += version_string;
    IdentString += " LLVM: ";
    IdentString += REVISION;
    IdentString += "\"";
    TheModule->setModuleInlineAsm(IdentString);
  }
#endif

  // Install information about the target triple and data layout into the module
  // for optimizer use.
  TheModule->setTargetTriple(TargetTriple);
  TheModule->setDataLayout(TheTarget->getTargetData()->
                           getStringRepresentation());
}

/// flag_default_initialize_globals - Whether global variables with no explicit
/// initial value should be zero initialized.
bool flag_default_initialize_globals = true; // GCC always initializes to zero

/// flag_odr - Whether the language being compiled obeys the One Definition Rule
/// (i.e. if the same function is defined in multiple compilation units, all the
/// definitions are equivalent).
bool flag_odr;

/// flag_vararg_requires_arguments - Do not consider functions with no arguments
/// to take a variable number of arguments (...).  If set then a function like
/// "T foo() {}" will be treated like "T foo(void) {}" and not "T foo(...) {}".
bool flag_vararg_requires_arguments;

/// flag_force_vararg_prototypes - Force prototypes to take a variable number of
/// arguments (...).  This is helpful if the language front-end sometimes emits
/// calls where the call arguments do not match the callee function declaration.
bool flag_force_vararg_prototypes;

/// InstallLanguageSettings - Do any language-specific back-end configuration.
static void InstallLanguageSettings() {
  // The principal here is that not doing any language-specific configuration
  // should still result in correct code.  The language-specific settings are
  // only for obtaining better code, by exploiting language-specific features.
  StringRef LanguageName = lang_hooks.name;

  if (LanguageName == "GNU Ada") {
    flag_default_initialize_globals = false; // Uninitialized means what it says
    flag_odr = true; // Ada obeys the one-definition-rule
  } else if (LanguageName == "GNU C") {
    flag_vararg_requires_arguments = true; // "T foo() {}" -> "T foo(void) {}"
  } else if (LanguageName == "GNU C++") {
    flag_odr = true; // C++ obeys the one-definition-rule
  } else if (LanguageName == "GNU Fortran") {
    flag_force_vararg_prototypes = true;
  } else if (LanguageName == "GNU GIMPLE") { // LTO gold plugin
  } else if (LanguageName == "GNU Java") {
  } else if (LanguageName == "GNU Objective-C") {
    flag_vararg_requires_arguments = true; // "T foo() {}" -> "T foo(void) {}"
  } else if (LanguageName == "GNU Objective-C++") {
    flag_odr = true; // Objective C++ obeys the one-definition-rule
  }
}

/// InitializeBackend - Initialize the GCC to LLVM conversion machinery.
/// Can safely be called multiple times.
static void InitializeBackend(void) {
  static bool Initialized = false;
  if (Initialized)
    return;

  // Initialize and configure LLVM.
  ConfigureLLVM();

  // Create the target machine to generate code for.
  const std::string TargetTriple = ComputeTargetTriple();
  CreateTargetMachine(TargetTriple);

  // Create a module to hold the generated LLVM IR.
  CreateModule(TargetTriple);

  TheTypeConverter = new TypeConverter();
  TheFolder = new TargetFolder(TheTarget->getTargetData());

  if (debug_info_level > DINFO_LEVEL_NONE)
    TheDebugInfo = new DebugInfo(TheModule);
  if (TheDebugInfo)
    TheDebugInfo->Initialize();

  // Perform language specific configuration.
  InstallLanguageSettings();

  // Configure the pass builder.
  PassBuilder.SizeLevel = optimize_size;
  PassBuilder.DisableSimplifyLibCalls = flag_no_simplify_libcalls;
  PassBuilder.DisableUnrollLoops = !flag_unroll_loops;
  PassBuilder.DisableUnitAtATime = !flag_unit_at_a_time;

  PassBuilder.LibraryInfo =
    new TargetLibraryInfo((Triple)TheModule->getTargetTriple());
  if (flag_no_simplify_libcalls)
    PassBuilder.LibraryInfo->disableAllFunctions();

  Initialized = true;
}

/// InitializeOutputStreams - Initialize the assembly code output streams.
static void InitializeOutputStreams(bool Binary) {
  assert(!OutStream && "Output stream already initialized!");
  std::string Error;

  OutStream = new raw_fd_ostream(llvm_asm_file_name, Error,
                                 Binary ? raw_fd_ostream::F_Binary : 0);

  if (!Error.empty())
    report_fatal_error(Error);

  FormattedOutStream.setStream(*OutStream,
                               formatted_raw_ostream::PRESERVE_STREAM);
}

static void createPerFunctionOptimizationPasses() {
  if (PerFunctionPasses)
    return;

  // Create and set up the per-function pass manager.
  // FIXME: Move the code generator to be function-at-a-time.
  PerFunctionPasses = new FunctionPassManager(TheModule);
  PerFunctionPasses->add(new TargetData(TheModule));

#ifdef ENABLE_CHECKING
  PerFunctionPasses->add(createVerifierPass());
#endif

  PassBuilder.OptLevel = PerFunctionOptLevel();
  PassBuilder.populateFunctionPassManager(*PerFunctionPasses);

  // If there are no module-level passes that have to be run, we codegen as
  // each function is parsed.
  // FIXME: We can't figure this out until we know there are no always-inline
  // functions.
  // FIXME: This is disabled right now until bugs can be worked out.  Reenable
  // this for fast -O0 compiles!
  if (!EmitIR && 0) {
    FunctionPassManager *PM = PerFunctionPasses;

    // Request that addPassesToEmitFile run the Verifier after running
    // passes which modify the IR.
#ifndef NDEBUG
    bool DisableVerify = false;
#else
    bool DisableVerify = true;
#endif

    // Normal mode, emit a .s file by running the code generator.
    // Note, this also adds codegenerator level optimization passes.
    InitializeOutputStreams(false);
    if (TheTarget->addPassesToEmitFile(*PM, FormattedOutStream,
                                       TargetMachine::CGFT_AssemblyFile,
                                       CodeGenOptLevel(), DisableVerify))
      DieAbjectly("Error interfacing to target machine!");
  }

  PerFunctionPasses->doInitialization();
}

static void createPerModuleOptimizationPasses() {
  if (PerModulePasses)
    return;

  PerModulePasses = new PassManager();
  PerModulePasses->add(new TargetData(TheModule));

  bool NeedAlwaysInliner = false;
  llvm::Pass *InliningPass = 0;
  if (flag_inline_small_functions && !flag_no_inline) {
    // Inline small functions.  Figure out a reasonable threshold to pass llvm's
    // inliner.  GCC has many options that control inlining, but we have decided
    // not to support anything like that for dragonegg.
    unsigned Threshold;
    if (optimize_size)
      // Reduce inline limit.
      Threshold = 75;
    else if (ModuleOptLevel() >= 3)
      Threshold = 275;
    else
      Threshold = 225;
    InliningPass = createFunctionInliningPass(Threshold);
  } else {
    // If full inliner is not run, check if always-inline is needed to handle
    // functions that are  marked as always_inline.
    // TODO: Consider letting the GCC inliner do this.
    for (Module::iterator I = TheModule->begin(), E = TheModule->end();
         I != E; ++I)
      if (I->hasFnAttr(Attribute::AlwaysInline)) {
        NeedAlwaysInliner = true;
        break;
      }

    if (NeedAlwaysInliner)
      InliningPass = createAlwaysInlinerPass();  // Inline always_inline funcs
  }

  PassBuilder.OptLevel = ModuleOptLevel();
  PassBuilder.Inliner = InliningPass;
  PassBuilder.populateModulePassManager(*PerModulePasses);

  if (EmitIR && 0) {
    // Emit an LLVM .bc file to the output.  This is used when passed
    // -emit-llvm -c to the GCC driver.
    InitializeOutputStreams(true);
    PerModulePasses->add(createBitcodeWriterPass(*OutStream));
  } else if (EmitIR) {
    // Emit an LLVM .ll file to the output.  This is used when passed
    // -emit-llvm -S to the GCC driver.
    InitializeOutputStreams(false);
    PerModulePasses->add(createPrintModulePass(OutStream));
  } else {
    // If there are passes we have to run on the entire module, we do codegen
    // as a separate "pass" after that happens.
    // However if there are no module-level passes that have to be run, we
    // codegen as each function is parsed.
    // FIXME: This is disabled right now until bugs can be worked out.  Reenable
    // this for fast -O0 compiles!
    if (PerModulePasses || 1) {
      FunctionPassManager *PM = CodeGenPasses =
        new FunctionPassManager(TheModule);
      PM->add(new TargetData(*TheTarget->getTargetData()));

      // Request that addPassesToEmitFile run the Verifier after running
      // passes which modify the IR.
#ifndef NDEBUG
      bool DisableVerify = false;
#else
      bool DisableVerify = true;
#endif

      // Normal mode, emit a .s file by running the code generator.
      // Note, this also adds codegenerator level optimization passes.
      InitializeOutputStreams(false);
      if (TheTarget->addPassesToEmitFile(*PM, FormattedOutStream,
                                         TargetMachine::CGFT_AssemblyFile,
                                         CodeGenOptLevel(), DisableVerify))
        DieAbjectly("Error interfacing to target machine!");
    }
  }
}

/// ConvertStructorsList - Convert a list of static ctors/dtors to an
/// initializer suitable for the llvm.global_[cd]tors globals.
static void CreateStructorsList(std::vector<std::pair<Constant*, int> > &Tors,
                                const char *Name) {
  std::vector<Constant*> InitList;
  std::vector<Constant*> StructInit;
  StructInit.resize(2);

  LLVMContext &Context = getGlobalContext();

  Type *FPTy =
    FunctionType::get(Type::getVoidTy(Context),
                      std::vector<Type*>(), false);
  FPTy = FPTy->getPointerTo();

  for (unsigned i = 0, e = Tors.size(); i != e; ++i) {
    StructInit[0] = ConstantInt::get(Type::getInt32Ty(Context), Tors[i].second);

    // __attribute__(constructor) can be on a function with any type.  Make sure
    // the pointer is void()*.
    StructInit[1] = TheFolder->CreateBitCast(Tors[i].first, FPTy);
    InitList.push_back(ConstantStruct::getAnon(Context, StructInit));
  }
  Constant *Array = ConstantArray::get(
    ArrayType::get(InitList[0]->getType(), InitList.size()), InitList);
  new GlobalVariable(*TheModule, Array->getType(), false,
                     GlobalValue::AppendingLinkage,
                     Array, Name);
}

/// ConvertMetadataStringToGV - Convert string to global value. Use existing
/// global if possible.
Constant* ConvertMetadataStringToGV(const char *str) {

  Constant *Init = ConstantArray::get(getGlobalContext(), std::string(str));

  // Use cached string if it exists.
  static std::map<Constant*, GlobalVariable*> StringCSTCache;
  GlobalVariable *&Slot = StringCSTCache[Init];
  if (Slot) return Slot;

  // Create a new string global.
  GlobalVariable *GV = new GlobalVariable(*TheModule, Init->getType(), true,
                                          GlobalVariable::PrivateLinkage,
                                          Init, ".str");
  GV->setSection("llvm.metadata");
  Slot = GV;
  return GV;

}

/// AddAnnotateAttrsToGlobal - Adds decls that have a annotate attribute to a
/// vector to be emitted later.
void AddAnnotateAttrsToGlobal(GlobalValue *GV, tree decl) {
  LLVMContext &Context = getGlobalContext();

  // Handle annotate attribute on global.
  tree annotateAttr = lookup_attribute("annotate", DECL_ATTRIBUTES (decl));
  if (annotateAttr == 0)
    return;

  // Get file and line number
  Constant *lineNo = ConstantInt::get(Type::getInt32Ty(Context),
                                      DECL_SOURCE_LINE(decl));
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
      assert(TREE_CODE(val) == STRING_CST &&
             "Annotate attribute arg should always be a string");
      Constant *strGV = AddressOf(val);
      Constant *Element[4] = {
        TheFolder->CreateBitCast(GV,SBP),
        TheFolder->CreateBitCast(strGV,SBP),
        file,
        lineNo
      };

      AttributeAnnotateGlobals.push_back(ConstantStruct::getAnon(Element));
    }

    // Get next annotate attribute.
    annotateAttr = TREE_CHAIN(annotateAttr);
    if (annotateAttr)
      annotateAttr = lookup_attribute("annotate", annotateAttr);
  }
}

/// emit_global - Emit the specified VAR_DECL or aggregate CONST_DECL to LLVM as
/// a global variable.  This function implements the end of assemble_variable.
static void emit_global(tree decl) {
  // FIXME: Support alignment on globals: DECL_ALIGN.
  // FIXME: DECL_PRESERVE_P indicates the var is marked with attribute 'used'.

  // Global register variables don't turn into LLVM GlobalVariables.
  if (TREE_CODE(decl) == VAR_DECL && DECL_REGISTER(decl))
    return;

  // If we encounter a forward declaration then do not emit the global yet.
  if (!TYPE_SIZE(TREE_TYPE(decl)))
    return;

//TODO  timevar_push(TV_LLVM_GLOBALS);

  // Get or create the global variable now.
  GlobalVariable *GV = cast<GlobalVariable>(DECL_LLVM(decl));

  // Convert the initializer over.
  Constant *Init;
  if (DECL_INITIAL(decl) == 0 || DECL_INITIAL(decl) == error_mark_node) {
    // Reconvert the type in case the forward def of the global and the real def
    // differ in type (e.g. declared as 'int A[]', and defined as 'int A[100]').
    Type *Ty = ConvertType(TREE_TYPE(decl));
    Init = getDefaultValue(Ty);
  } else {
    // Temporarily set an initializer for the global, so we don't infinitely
    // recurse.  If we don't do this, we can hit cases where we see "oh a global
    // with an initializer hasn't been initialized yet, call emit_global on it".
    // When constructing the initializer it might refer to itself.
    // This can happen for things like void *G = &G;
    GV->setInitializer(UndefValue::get(GV->getType()->getElementType()));
    Init = ConvertInitializer(DECL_INITIAL(decl));
  }

  // If we had a forward definition that has a type that disagrees with our
  // initializer, insert a cast now.  This sort of thing occurs when we have a
  // global union, and the LLVM type followed a union initializer that is
  // different from the union element used for the type.
  if (GV->getType()->getElementType() != Init->getType()) {
    GV->removeFromParent();
    GlobalVariable *NGV = new GlobalVariable(*TheModule, Init->getType(),
                                             GV->isConstant(),
                                             GlobalValue::ExternalLinkage, 0,
                                             GV->getName());
    GV->replaceAllUsesWith(TheFolder->CreateBitCast(NGV, GV->getType()));
    changeLLVMConstant(GV, NGV);
    delete GV;
    SET_DECL_LLVM(decl, NGV);
    GV = NGV;
  }

  // Set the initializer.
  GV->setInitializer(Init);

  // Set thread local (TLS)
  if (TREE_CODE(decl) == VAR_DECL && DECL_THREAD_LOCAL_P(decl))
    GV->setThreadLocal(true);

  // Set the linkage.
  GlobalValue::LinkageTypes Linkage;

  if (CODE_CONTAINS_STRUCT (TREE_CODE (decl), TS_DECL_WITH_VIS)
      && false) {// FIXME DECL_LLVM_PRIVATE(decl)) {
    Linkage = GlobalValue::PrivateLinkage;
  } else if (CODE_CONTAINS_STRUCT (TREE_CODE (decl), TS_DECL_WITH_VIS)
             && false) {//FIXME DECL_LLVM_LINKER_PRIVATE(decl)) {
    Linkage = GlobalValue::LinkerPrivateLinkage;
  } else if (!TREE_PUBLIC(decl)) {
    Linkage = GlobalValue::InternalLinkage;
  } else if (DECL_WEAK(decl)) {
    // The user may have explicitly asked for weak linkage - ignore flag_odr.
    Linkage = GlobalValue::WeakAnyLinkage;
  } else if (DECL_ONE_ONLY(decl)) {
    Linkage = GlobalValue::getWeakLinkage(flag_odr);
  } else if (DECL_COMMON(decl) &&  // DECL_COMMON is only meaningful if no init
             (!DECL_INITIAL(decl) || DECL_INITIAL(decl) == error_mark_node)) {
    // llvm-gcc also includes DECL_VIRTUAL_P here.
    Linkage = GlobalValue::CommonLinkage;
  } else if (DECL_COMDAT(decl)) {
    Linkage = GlobalValue::getLinkOnceLinkage(flag_odr);
  } else {
    Linkage = GV->getLinkage();
  }

  // Allow loads from constants to be folded even if the constant has weak
  // linkage.  Do this by giving the constant weak_odr linkage rather than
  // weak linkage.  It is not clear whether this optimization is valid (see
  // gcc bug 36685), but mainline gcc chooses to do it, and fold may already
  // have done it, so we might as well join in with gusto.
  if (GV->isConstant()) {
    if (Linkage == GlobalValue::WeakAnyLinkage)
      Linkage = GlobalValue::WeakODRLinkage;
    else if (Linkage == GlobalValue::LinkOnceAnyLinkage)
      Linkage = GlobalValue::LinkOnceODRLinkage;
  }
  GV->setLinkage(Linkage);

#ifdef TARGET_ADJUST_LLVM_LINKAGE
  TARGET_ADJUST_LLVM_LINKAGE(GV, decl);
#endif /* TARGET_ADJUST_LLVM_LINKAGE */

  handleVisibility(decl, GV);

  // Set the section for the global.
  if (TREE_CODE(decl) == VAR_DECL) {
    if (DECL_SECTION_NAME(decl)) {
      GV->setSection(TREE_STRING_POINTER(DECL_SECTION_NAME(decl)));
#ifdef LLVM_IMPLICIT_TARGET_GLOBAL_VAR_SECTION
    } else if (const char *Section =
                LLVM_IMPLICIT_TARGET_GLOBAL_VAR_SECTION(decl)) {
      GV->setSection(Section);
#endif
    }

    // Set the alignment for the global if one of the following condition is met
    // 1) DECL_ALIGN is better than the alignment as per ABI specification
    // 2) DECL_ALIGN is set by user.
    if (DECL_ALIGN(decl)) {
      unsigned TargetAlign =
        getTargetData().getABITypeAlignment(GV->getType()->getElementType());
      if (DECL_USER_ALIGN(decl) ||
          8 * TargetAlign < (unsigned)DECL_ALIGN(decl)) {
        GV->setAlignment(DECL_ALIGN(decl) / 8);
      }
#ifdef TARGET_ADJUST_CSTRING_ALIGN
      else if (DECL_INITIAL(decl) != error_mark_node && // uninitialized?
               DECL_INITIAL(decl) &&
               TREE_CODE(DECL_INITIAL(decl)) == STRING_CST) {
        TARGET_ADJUST_CSTRING_ALIGN(GV);
      }
#endif
    }

    // Handle used decls
    if (DECL_PRESERVE_P (decl)) {
      if (false)//FIXME DECL_LLVM_LINKER_PRIVATE (decl))
        AttributeCompilerUsedGlobals.insert(GV);
      else
        AttributeUsedGlobals.insert(GV);
    }

    // Add annotate attributes for globals
    if (DECL_ATTRIBUTES(decl))
      AddAnnotateAttrsToGlobal(GV, decl);

#ifdef LLVM_IMPLICIT_TARGET_GLOBAL_VAR_SECTION
  } else if (TREE_CODE(decl) == CONST_DECL) {
    if (const char *Section =
        LLVM_IMPLICIT_TARGET_GLOBAL_VAR_SECTION(decl)) {
      GV->setSection(Section);

      /* LLVM LOCAL - begin radar 6389998 */
#ifdef TARGET_ADJUST_CFSTRING_NAME
      TARGET_ADJUST_CFSTRING_NAME(GV, Section);
#endif
      /* LLVM LOCAL - end radar 6389998 */
    }
#endif
  }

  if (TheDebugInfo)
    TheDebugInfo->EmitGlobalVariable(GV, decl);

  // Sanity check that the LLVM global has the right size.
  assert(SizeOfGlobalMatchesDecl(GV, decl) && "Global has wrong size!");

  // Mark the global as written so gcc doesn't waste time outputting it.
  TREE_ASM_WRITTEN(decl) = 1;

//TODO  timevar_pop(TV_LLVM_GLOBALS);
}


/// ValidateRegisterVariable - Check that a static "asm" variable is
/// well-formed.  If not, emit error messages and return true.  If so, return
/// false.
bool ValidateRegisterVariable(tree decl) {
  int RegNumber = decode_reg_name(extractRegisterName(decl));

  if (errorcount || sorrycount)
    return true;  // Do not process broken code.

  /* Detect errors in declaring global registers.  */
  if (RegNumber == -1)
    error("register name not specified for %q+D", decl);
  else if (RegNumber < 0)
    error("invalid register name for %q+D", decl);
  else if (TYPE_MODE(TREE_TYPE(decl)) == BLKmode)
    error("data type of %q+D isn%'t suitable for a register", decl);
#if 0 // FIXME: enable this.
  else if (!HARD_REGNO_MODE_OK(RegNumber, TYPE_MODE(TREE_TYPE(decl))))
    error("register specified for %q+D isn%'t suitable for data type",
          decl);
#endif
  else if (DECL_INITIAL(decl) != 0 && TREE_STATIC(decl))
    error("global register variable has initial value");
  else if (AGGREGATE_TYPE_P(TREE_TYPE(decl)))
    sorry("LLVM cannot handle register variable %q+D, report a bug",
          decl);
  else {
    if (TREE_THIS_VOLATILE(decl))
      warning(0, "volatile register variables don%'t work as you might wish");

    return false;  // Everything ok.
  }

  return true;
}


/// make_decl_llvm - Create the DECL_RTL for a VAR_DECL or FUNCTION_DECL.  DECL
/// should have static storage duration.  In other words, it should not be an
/// automatic variable, including PARM_DECLs.
///
/// There is, however, one exception: this function handles variables explicitly
/// placed in a particular register by the user.
///
/// This function corresponds to make_decl_rtl in varasm.c, and is implicitly
/// called by DECL_LLVM if a decl doesn't have an LLVM set.
Value *make_decl_llvm(tree decl) {
  // If we already made the LLVM, then return it.
  if (Value *V = get_decl_llvm(decl))
    return V;

#ifdef ENABLE_CHECKING
  // Check that we are not being given an automatic variable or a type or label.
  // A weak alias has TREE_PUBLIC set but not the other bits.
  if (TREE_CODE(decl) == PARM_DECL || TREE_CODE(decl) == RESULT_DECL ||
      TREE_CODE(decl) == TYPE_DECL || TREE_CODE(decl) == LABEL_DECL ||
      (TREE_CODE(decl) == VAR_DECL && !TREE_STATIC(decl) &&
       !TREE_PUBLIC(decl) && !DECL_EXTERNAL(decl) && !DECL_REGISTER(decl)))
    DieAbjectly("Cannot make a global for this kind of declaration!", decl);
#endif

  if (errorcount || sorrycount)
    return NULL;  // Do not process broken code.

  LLVMContext &Context = getGlobalContext();

  // Global register variable with asm name, e.g.:
  // register unsigned long esp __asm__("ebp");
  if (TREE_CODE(decl) != FUNCTION_DECL && DECL_REGISTER(decl)) {
    // This  just verifies that the variable is ok.  The actual "load/store"
    // code paths handle accesses to the variable.
    ValidateRegisterVariable(decl);
    return NULL;
  }

//TODO  timevar_push(TV_LLVM_GLOBALS);

  std::string Name;
  if (TREE_CODE(decl) != CONST_DECL) // CONST_DECLs do not have assembler names.
    Name = getLLVMAssemblerName(decl).str();

  // Now handle ordinary static variables and functions (in memory).
  // Also handle vars declared register invalidly.
  if (!Name.empty() && Name[0] == 1) {
#ifdef REGISTER_PREFIX
    if (strlen (REGISTER_PREFIX) != 0) {
      int reg_number = decode_reg_name(Name);
      if (reg_number >= 0 || reg_number == -3)
        error("register name given for non-register variable %q+D", decl);
    }
#endif
  }

  // Specifying a section attribute on a variable forces it into a
  // non-.bss section, and thus it cannot be common.
  if (TREE_CODE(decl) == VAR_DECL && DECL_SECTION_NAME(decl) != NULL_TREE &&
      DECL_INITIAL(decl) == NULL_TREE && DECL_COMMON(decl))
    DECL_COMMON(decl) = 0;

  // Variables can't be both common and weak.
  if (TREE_CODE(decl) == VAR_DECL && DECL_WEAK(decl))
    DECL_COMMON(decl) = 0;

  // Okay, now we need to create an LLVM global variable or function for this
  // object.  Note that this is quite possibly a forward reference to the
  // object, so its type may change later.
  if (TREE_CODE(decl) == FUNCTION_DECL) {
    assert(!Name.empty() && "Function with empty name!");
    // If this function has already been created, reuse the decl.  This happens
    // when we have something like __builtin_memset and memset in the same file.
    Function *FnEntry = TheModule->getFunction(Name);
    if (FnEntry == 0) {
      CallingConv::ID CC;
      AttrListPtr PAL;
      FunctionType *Ty =
        TheTypeConverter->ConvertFunctionType(TREE_TYPE(decl), decl, NULL,
                                              CC, PAL);
      FnEntry = Function::Create(Ty, Function::ExternalLinkage, Name, TheModule);
      FnEntry->setCallingConv(CC);
      FnEntry->setAttributes(PAL);

      // Check for external weak linkage.
      if (DECL_EXTERNAL(decl) && DECL_WEAK(decl))
        FnEntry->setLinkage(Function::ExternalWeakLinkage);

#ifdef TARGET_ADJUST_LLVM_LINKAGE
      TARGET_ADJUST_LLVM_LINKAGE(FnEntry,decl);
#endif /* TARGET_ADJUST_LLVM_LINKAGE */

      handleVisibility(decl, FnEntry);

      // If FnEntry got renamed, then there is already an object with this name
      // in the symbol table.  If this happens, the old one must be a forward
      // decl, just replace it with a cast of the new one.
      if (FnEntry->getName() != Name) {
        GlobalVariable *G = TheModule->getGlobalVariable(Name, true);
        assert(G && G->isDeclaration() && "A global turned into a function?");

        // Replace any uses of "G" with uses of FnEntry.
        Constant *GInNewType = TheFolder->CreateBitCast(FnEntry, G->getType());
        G->replaceAllUsesWith(GInNewType);

        // Update the decl that points to G.
        changeLLVMConstant(G, GInNewType);

        // Now we can give GV the proper name.
        FnEntry->takeName(G);

        // G is now dead, nuke it.
        G->eraseFromParent();
      }
    }
    return SET_DECL_LLVM(decl, FnEntry);
  } else {
    assert((TREE_CODE(decl) == VAR_DECL ||
            TREE_CODE(decl) == CONST_DECL) && "Not a function or var decl?");
    Type *Ty = ConvertType(TREE_TYPE(decl));
    GlobalVariable *GV ;

    // If we have "extern void foo", make the global have type {} instead of
    // type void.
    if (Ty->isVoidTy())
      Ty = StructType::get(Context);

    if (Name.empty()) {   // Global has no name.
      GV = new GlobalVariable(*TheModule, Ty, false,
                              GlobalValue::ExternalLinkage, 0, "");

      // Check for external weak linkage.
      if (DECL_EXTERNAL(decl) && DECL_WEAK(decl))
        GV->setLinkage(GlobalValue::ExternalWeakLinkage);

#ifdef TARGET_ADJUST_LLVM_LINKAGE
      TARGET_ADJUST_LLVM_LINKAGE(GV,decl);
#endif /* TARGET_ADJUST_LLVM_LINKAGE */

      handleVisibility(decl, GV);
    } else {
      // If the global has a name, prevent multiple vars with the same name from
      // being created.
      GlobalVariable *GVE = TheModule->getGlobalVariable(Name, true);

      if (GVE == 0) {
        GV = new GlobalVariable(*TheModule, Ty, false,
                                GlobalValue::ExternalLinkage, 0, Name);

        // Check for external weak linkage.
        if (DECL_EXTERNAL(decl) && DECL_WEAK(decl))
          GV->setLinkage(GlobalValue::ExternalWeakLinkage);

#ifdef TARGET_ADJUST_LLVM_LINKAGE
        TARGET_ADJUST_LLVM_LINKAGE(GV,decl);
#endif /* TARGET_ADJUST_LLVM_LINKAGE */

	handleVisibility(decl, GV);

        // If GV got renamed, then there is already an object with this name in
        // the symbol table.  If this happens, the old one must be a forward
        // decl, just replace it with a cast of the new one.
        if (GV->getName() != Name) {
          Function *F = TheModule->getFunction(Name);
          assert(F && F->isDeclaration() && "A function turned into a global?");

          // Replace any uses of "F" with uses of GV.
          Constant *FInNewType = TheFolder->CreateBitCast(GV, F->getType());
          F->replaceAllUsesWith(FInNewType);

          // Update the decl that points to F.
          changeLLVMConstant(F, FInNewType);

          // Now we can give GV the proper name.
          GV->takeName(F);

          // F is now dead, nuke it.
          F->eraseFromParent();
        }

      } else {
        GV = GVE;  // Global already created, reuse it.
      }
    }

    if ((TREE_READONLY(decl) && !TREE_SIDE_EFFECTS(decl)) ||
        TREE_CODE(decl) == CONST_DECL) {
      if (DECL_EXTERNAL(decl)) {
        // Mark external globals constant even though they could be marked
        // non-constant in the defining translation unit.  The definition of the
        // global determines whether the global is ultimately constant or not,
        // marking this constant will allow us to do some extra (legal)
        // optimizations that we would otherwise not be able to do.  (In C++,
        // any global that is 'C++ const' may not be readonly: it could have a
        // dynamic initializer.
        //
        GV->setConstant(true);
      } else {
        // Mark readonly globals with constant initializers constant.
        if (DECL_INITIAL(decl) != error_mark_node && // uninitialized?
            DECL_INITIAL(decl) &&
            (TREE_CONSTANT(DECL_INITIAL(decl)) ||
             TREE_CODE(DECL_INITIAL(decl)) == STRING_CST))
          GV->setConstant(true);
      }
    }

    // Set thread local (TLS)
    if (TREE_CODE(decl) == VAR_DECL && DECL_THREAD_LOCAL_P(decl))
      GV->setThreadLocal(true);

    assert((GV->isDeclaration() || SizeOfGlobalMatchesDecl(GV, decl)) &&
           "Global has unexpected initializer!");

    return SET_DECL_LLVM(decl, GV);
  }
//TODO  timevar_pop(TV_LLVM_GLOBALS);
}

/// make_definition_llvm - Ensures that the body or initial value of the given
/// GCC global will be output, and returns a declaration for it.
Value *make_definition_llvm(tree decl) {
  // Only need to do something special for global variables.
  if (TREE_CODE(decl) != CONST_DECL && TREE_CODE(decl) != VAR_DECL)
    return DECL_LLVM(decl);
  // Do not allocate storage for external references (eg: a "weakref" alias).
  if (DECL_EXTERNAL(decl))
    return DECL_LLVM(decl);
  // Can only assign initial values to global variables in static storage.
  if (!TREE_STATIC(decl)) {
    assert(!DECL_INITIAL(decl) && "Non-static global has initial value!");
    return DECL_LLVM(decl);
  }
  GlobalValue *GV = cast<GlobalValue>(DECL_LLVM(decl));
  // If we already output a definition for this declaration, then reuse it.
  if (!GV->isDeclaration())
    return GV;
  emit_global(decl);
  return DECL_LLVM(decl); // Decl could have changed if it changed type.
}

/// register_ctor_dtor - Called to register static ctors/dtors with LLVM.
/// Fn is a 'void()' ctor/dtor function to be run, initprio is the init
/// priority, and isCtor indicates whether this is a ctor or dtor.
void register_ctor_dtor(Function *Fn, int InitPrio, bool isCtor) {
  (isCtor ? &StaticCtors:&StaticDtors)->push_back(std::make_pair(Fn, InitPrio));
}

/// extractRegisterName - Get a register name given its decl. In 4.2 unlike 4.0
/// these names have been run through set_user_assembler_name which means they
/// may have a leading star at this point; compensate.
const char* extractRegisterName(tree decl) {
  const char* Name = IDENTIFIER_POINTER(DECL_ASSEMBLER_NAME(decl));
  return (*Name == '*') ? Name + 1 : Name;
}

/// getLLVMAssemblerName - Get the assembler name (DECL_ASSEMBLER_NAME) for the
/// declaration, with any leading star replaced by '\1'.
Twine getLLVMAssemblerName(union tree_node *decl) {
  tree Ident = DECL_ASSEMBLER_NAME(decl);
  if (!Ident)
    return "";

  const char *Name = IDENTIFIER_POINTER(Ident);
  if (*Name != '*')
    return Name;

  return "\1" + Twine(Name + 1);
}

/// FinalizePlugin - Shutdown the plugin.
static void FinalizePlugin(void) {
  static bool Finalized = false;
  if (Finalized)
    return;

#ifndef NDEBUG
  delete PerModulePasses;
  delete PerFunctionPasses;
  delete CodeGenPasses;
  delete TheModule;
  llvm_shutdown();
#endif

  Finalized = true;
}

/// TakeoverAsmOutput - Obtain exclusive use of the assembly code output file.
/// Any GCC output will be thrown away.
static void TakeoverAsmOutput(void) {
  // Calculate the output file name as in init_asm_output (toplev.c).
  if (!dump_base_name && main_input_filename)
    dump_base_name = main_input_filename[0] ? main_input_filename : "gccdump";

  if (!main_input_filename && !asm_file_name) {
    llvm_asm_file_name = "-";
  } else if (!asm_file_name) {
    int len = strlen(dump_base_name);
    char *dumpname = XNEWVEC(char, len + 6);

    memcpy(dumpname, dump_base_name, len + 1);
    strip_off_ending(dumpname, len);
    strcat(dumpname, ".s");
    llvm_asm_file_name = dumpname;
  } else {
    llvm_asm_file_name = asm_file_name;
  }

  if (!SaveGCCOutput) {
    // Redirect any GCC output to /dev/null.
    asm_file_name = HOST_BIT_BUCKET;
  } else {
    // Save GCC output to a special file.  Good for seeing how much pointless
    // output gcc is producing.
    int len = strlen(llvm_asm_file_name);
    char *name = XNEWVEC(char, len + 5);
    memcpy(name, llvm_asm_file_name, len + 1);
    asm_file_name = strcat(name, ".gcc");
  }
}

/// no_target_thunks - Hook for can_output_mi_thunk that always says "no".
static bool no_target_thunks(const_tree, HOST_WIDE_INT, HOST_WIDE_INT,
                             const_tree) {
  return false;
}


//===----------------------------------------------------------------------===//
//                             Plugin interface
//===----------------------------------------------------------------------===//

// This plugin's code is licensed under the GPLv2 or later.  The LLVM libraries
// use the GPL compatible University of Illinois/NCSA Open Source License.  The
// plugin is GPL compatible.
int plugin_is_GPL_compatible __attribute__ ((visibility("default")));


/// llvm_start_unit - Perform late initialization.  This is called by GCC just
/// before processing the compilation unit.
/// NOTE: called even when only doing syntax checking, so do not initialize the
/// module etc here.
static void llvm_start_unit(void * /*gcc_data*/, void * /*user_data*/) {
  if (!quiet_flag)
    errs() << "Starting compilation unit\n";

#ifdef ENABLE_LTO
  // Output LLVM IR if the user requested generation of lto data.
  EmitIR |= flag_generate_lto != 0;
  // We have the same needs as GCC's LTO.  Always claim to be doing LTO.
  flag_lto =
#if (GCC_MINOR > 5)
    "";
#else
    1;
#endif
  flag_generate_lto = 1;
  flag_whole_program = 0;
#else
# error "LTO support required but not enabled in GCC"
#endif

  // Stop GCC outputting serious amounts of debug info.
  debug_hooks = &do_nothing_debug_hooks;

  // Adjust the target machine configuration for the fact that we are really
  // targetting LLVM IR.

  // Ensure that thunks are turned into functions rather than output directly
  // as assembler.
  targetm.asm_out.can_output_mi_thunk = no_target_thunks;
}


/// gate_emission - Whether to turn gimple into LLVM IR.
static bool gate_emission(void) {
  // Don't bother doing anything if the program has errors.
  return !errorcount && !sorrycount; // Do not process broken code.
}

/// emit_current_function - Turn the current gimple function into LLVM IR.  This
/// is called once for each function in the compilation unit.
static void emit_current_function() {
  if (!quiet_flag && DECL_NAME(current_function_decl))
    errs() << IDENTIFIER_POINTER(DECL_NAME(current_function_decl));

  // Convert the AST to raw/ugly LLVM code.
  Function *Fn;
  {
    TreeToLLVM Emitter(current_function_decl);
    Fn = Emitter.EmitFunction();
  }

  if (!errorcount && !sorrycount) { // Do not process broken code.
    createPerFunctionOptimizationPasses();

    if (PerFunctionPasses)
      PerFunctionPasses->run(*Fn);

    // TODO: Nuke the .ll code for the function at -O[01] if we don't want to
    // inline it or something else.
  }
}

/// GetLinkageForAlias - The given GCC declaration is an alias.  Return the
/// appropriate LLVM linkage type for it.
static GlobalValue::LinkageTypes GetLinkageForAlias(tree decl) {
  if (DECL_COMDAT(decl))
    // Need not be put out unless needed in this translation unit.
    return GlobalValue::InternalLinkage;

  if (DECL_ONE_ONLY(decl))
    // Copies of this DECL in multiple translation units should be merged.
    return GlobalValue::getWeakLinkage(flag_odr);

  if (DECL_WEAK(decl))
    // The user may have explicitly asked for weak linkage - ignore flag_odr.
    return GlobalValue::WeakAnyLinkage;

  if (!TREE_PUBLIC(decl))
    // Not accessible from outside this translation unit.
    return GlobalValue::InternalLinkage;

  if (DECL_EXTERNAL(decl))
    // Do not allocate storage, and refer to a definition elsewhere.
    return GlobalValue::InternalLinkage;

  return GlobalValue::ExternalLinkage;
}

/// emit_alias - Given decl and target emit alias to target.
static void emit_alias(tree decl, tree target) {
  if (errorcount || sorrycount)
    return; // Do not process broken code.

  // Get or create LLVM global for our alias.
  GlobalValue *V = cast<GlobalValue>(DECL_LLVM(decl));

  bool weakref = lookup_attribute("weakref", DECL_ATTRIBUTES(decl));
  if (weakref)
    while (IDENTIFIER_TRANSPARENT_ALIAS(target))
      target = TREE_CHAIN(target);

  if (TREE_CODE(target) == IDENTIFIER_NODE) {
    if (struct cgraph_node *fnode = cgraph_node_for_asm(target))
      target = fnode->decl;
    else if (struct varpool_node *vnode = varpool_node_for_asm(target))
      target = vnode->decl;
  }

  GlobalValue *Aliasee = 0;
  if (TREE_CODE(target) == IDENTIFIER_NODE) {
    if (!weakref) {
      error("%q+D aliased to undefined symbol %qs", decl,
            IDENTIFIER_POINTER(target));
      return;
    }

    // weakref to external symbol.
    if (GlobalVariable *GV = dyn_cast<GlobalVariable>(V))
      Aliasee = new GlobalVariable(*TheModule, GV->getType(),
                                   GV->isConstant(),
                                   GlobalVariable::ExternalWeakLinkage, NULL,
                                   IDENTIFIER_POINTER(target));
    else if (Function *F = dyn_cast<Function>(V))
      Aliasee = Function::Create(F->getFunctionType(),
                                 Function::ExternalWeakLinkage,
                                 IDENTIFIER_POINTER(target),
                                 TheModule);
    else
      assert(0 && "Unsuported global value");
  } else {
    Aliasee = cast<GlobalValue>(DEFINITION_LLVM(target));
  }

  GlobalValue::LinkageTypes Linkage = GetLinkageForAlias(decl);

  if (Linkage != GlobalValue::InternalLinkage) {
    // Create the LLVM alias.
    GlobalAlias* GA = new GlobalAlias(Aliasee->getType(), Linkage, "",
                                      Aliasee, TheModule);
    handleVisibility(decl, GA);

    // Associate it with decl instead of V.
    V->replaceAllUsesWith(ConstantExpr::getBitCast(GA, V->getType()));
    changeLLVMConstant(V, GA);
    GA->takeName(V);
  } else {
    // Make all users of the alias directly use the aliasee instead.
    V->replaceAllUsesWith(ConstantExpr::getBitCast(Aliasee, V->getType()));
    changeLLVMConstant(V, Aliasee);
  }

  V->eraseFromParent();

  // Mark the alias as written so gcc doesn't waste time outputting it.
  TREE_ASM_WRITTEN(decl) = 1;
}

/// emit_same_body_alias - Turn a same-body alias into LLVM IR.
static void emit_same_body_alias(struct cgraph_node *alias,
                                 struct cgraph_node * /*target*/) {
  if (errorcount || sorrycount)
    return; // Do not process broken code.

  // If the target is not "extern inline" then output an ordinary alias.
  tree target = alias->thunk.alias;
  if (!DECL_EXTERNAL(target)) {
    emit_alias(alias->decl, target);
    return;
  }

  // Same body aliases have the property that if the body of the aliasee is not
  // output then neither are the aliases.  To arrange this for "extern inline"
  // functions, which have AvailableExternally linkage in LLVM, make all users
  // of the alias directly use the aliasee instead.
  GlobalValue *Alias = cast<GlobalValue>(DECL_LLVM(alias->decl));
  GlobalValue *Aliasee = cast<GlobalValue>(DEFINITION_LLVM(target));
  Alias->replaceAllUsesWith(ConstantExpr::getBitCast(Aliasee,Alias->getType()));
  changeLLVMConstant(Alias, Aliasee);
  Alias->eraseFromParent();

  // Mark the alias as written so gcc doesn't waste time outputting it.
  TREE_ASM_WRITTEN(alias->decl) = 1;
}

/// emit_file_scope_asm - Emit the specified string as a file-scope inline
/// asm block.
static void emit_file_scope_asm(tree string) {
  if (errorcount || sorrycount)
    return; // Do not process broken code.

  if (TREE_CODE(string) == ADDR_EXPR)
    string = TREE_OPERAND(string, 0);
  TheModule->appendModuleInlineAsm(TREE_STRING_POINTER (string));
}

/// emit_aliases - Convert same-body aliases and file-scope asm into LLVM IR.
static void emit_aliases(cgraph_node_set set
#if (GCC_MINOR > 5)
                         , varpool_node_set /*vset*/
#endif
                         ) {
  if (errorcount || sorrycount)
    return; // Do not process broken code.

  InitializeBackend();

  // Emit any same-body aliases in the order they were created.
  SmallPtrSet<tree, 32> Visited;
  for (cgraph_node_set_iterator csi = csi_start(set); !csi_end_p(csi);
       csi_next(&csi)) {
    struct cgraph_node *node = csi_node(csi);
    if (!Visited.insert(node->decl))
      continue;

    struct cgraph_node *alias, *next;
    for (alias = node->same_body; alias && alias->next; alias = alias->next) ;
    for (; alias; alias = next) {
      next = alias->previous;
      if (!alias->thunk.thunk_p)
        emit_same_body_alias(alias, node);
    }
  }

  // Emit any file-scope asms.
  for (struct cgraph_asm_node *can = cgraph_asm_nodes; can; can = can->next)
    emit_file_scope_asm(can->asm_str);

  // Remove the asms so gcc doesn't waste time outputting them.
  cgraph_asm_nodes = NULL;
}

/// pass_emit_aliases - IPA pass that converts same-body aliases and file-scope
/// asm into LLVM IR.
static struct ipa_opt_pass_d pass_emit_aliases = {
    {
      IPA_PASS,
      "emit_aliases",	/* name */
      gate_emission,	/* gate */
      NULL,		/* execute */
      NULL,		/* sub */
      NULL,		/* next */
      0,		/* static_pass_number */
      TV_NONE,		/* tv_id */
      0,		/* properties_required */
      0,		/* properties_provided */
      0,		/* properties_destroyed */
      0,		/* todo_flags_start */
      0			/* todo_flags_finish */
    },
    NULL,		/* generate_summary */
    emit_aliases,	/* write_summary */
    NULL,		/* read_summary */
#if (GCC_MINOR > 5)
    NULL,		/* write_optimization_summary */
    NULL,		/* read_optimization_summary */
#else
    NULL,		/* function_read_summary */
#endif
    NULL,		/* stmt_fixup */
    0,			/* function_transform_todo_flags_start */
    NULL,		/* function_transform */
    NULL		/* variable_transform */
};

/// rtl_emit_function - Turn a gimple function into LLVM IR.  This is called
/// once for each function in the compilation unit if GCC optimizations are
/// enabled.
static unsigned int rtl_emit_function (void) {
  InitializeBackend();

  // Convert the function.
  emit_current_function();

  // Free any data structures.
  execute_free_datastructures();

  // Finally, we have written out this function!
  TREE_ASM_WRITTEN(current_function_decl) = 1;
  return 0;
}

/// pass_rtl_emit_function - RTL pass that converts a function to LLVM IR.
static struct rtl_opt_pass pass_rtl_emit_function =
{
    {
      RTL_PASS,
      "rtl_emit_function",	/* name */
      gate_emission,		/* gate */
      rtl_emit_function,	/* execute */
      NULL,			/* sub */
      NULL,			/* next */
      0,			/* static_pass_number */
      TV_NONE,			/* tv_id */
      PROP_ssa | PROP_gimple_leh | PROP_cfg,
				/* properties_required */
      0,			/* properties_provided */
      PROP_ssa | PROP_trees,	/* properties_destroyed */
      TODO_verify_ssa | TODO_verify_flow
        | TODO_verify_stmts,	/* todo_flags_start */
      TODO_ggc_collect		/* todo_flags_finish */
    }
};


/// llvm_finish_unit - Finish the .s file.  This is called by GCC once the
/// compilation unit has been completely processed.
static void llvm_finish_unit(void * /*gcc_data*/, void * /*user_data*/) {
  if (errorcount || sorrycount)
    return; // Do not process broken code.

//TODO  timevar_push(TV_LLVM_PERFILE);
  if (!quiet_flag)
    errs() << "Finishing compilation unit\n";

  InitializeBackend();

  // Output all externally visible global variables and aliases as well as any
  // internal variables explicitly marked with the 'used' attribute.  All other
  // internal variables and aliases are output when their user is, or discarded
  // if unused.
  for (struct varpool_node *vnode = varpool_nodes; vnode; vnode = vnode->next) {
    if (!vnode->needed)
      continue;
#if (GCC_MINOR > 5)
    if (vnode->in_other_partition)
      continue;
#endif
    tree decl = vnode->decl;
    if (vnode->alias) {
      tree alias = lookup_attribute("alias", DECL_ATTRIBUTES(decl));
      assert(alias && "Have alias target but no alias!");
      alias = TREE_VALUE(TREE_VALUE(alias));
      alias = get_identifier(TREE_STRING_POINTER(alias));
      emit_alias(decl, alias);
      continue;
    }
    if (TREE_CODE(decl) == VAR_DECL && !DECL_EXTERNAL(decl) &&
        (TREE_PUBLIC(decl) || DECL_PRESERVE_P(decl) ||
         TREE_THIS_VOLATILE(decl)))
      emit_global(decl);
  }

  LLVMContext &Context = getGlobalContext();

  createPerFunctionOptimizationPasses();

//TODO  for (Module::iterator I = TheModule->begin(), E = TheModule->end();
//TODO       I != E; ++I)
//TODO    if (!I->isDeclaration()) {
//TODO      if (flag_disable_red_zone)
//TODO        I->addFnAttr(Attribute::NoRedZone);
//TODO      if (flag_no_implicit_float)
//TODO        I->addFnAttr(Attribute::NoImplicitFloat);
//TODO    }

  // Add an llvm.global_ctors global if needed.
  if (!StaticCtors.empty())
    CreateStructorsList(StaticCtors, "llvm.global_ctors");
  // Add an llvm.global_dtors global if needed.
  if (!StaticDtors.empty())
    CreateStructorsList(StaticDtors, "llvm.global_dtors");

  if (!AttributeUsedGlobals.empty()) {
    std::vector<Constant *> AUGs;
    Type *SBP = Type::getInt8PtrTy(Context);
    for (SmallSetVector<Constant *,32>::iterator
           AI = AttributeUsedGlobals.begin(),
           AE = AttributeUsedGlobals.end(); AI != AE; ++AI) {
      Constant *C = *AI;
      AUGs.push_back(TheFolder->CreateBitCast(C, SBP));
    }

    ArrayType *AT = ArrayType::get(SBP, AUGs.size());
    Constant *Init = ConstantArray::get(AT, AUGs);
    GlobalValue *gv = new GlobalVariable(*TheModule, AT, false,
                                         GlobalValue::AppendingLinkage, Init,
                                         "llvm.used");
    gv->setSection("llvm.metadata");
    AttributeUsedGlobals.clear();
  }

  if (!AttributeCompilerUsedGlobals.empty()) {
    std::vector<Constant *> ACUGs;
    Type *SBP = Type::getInt8PtrTy(Context);
    for (SmallSetVector<Constant *,32>::iterator
           AI = AttributeCompilerUsedGlobals.begin(),
           AE = AttributeCompilerUsedGlobals.end(); AI != AE; ++AI) {
      Constant *C = *AI;
      ACUGs.push_back(TheFolder->CreateBitCast(C, SBP));
    }

    ArrayType *AT = ArrayType::get(SBP, ACUGs.size());
    Constant *Init = ConstantArray::get(AT, ACUGs);
    GlobalValue *gv = new GlobalVariable(*TheModule, AT, false,
                                         GlobalValue::AppendingLinkage, Init,
                                         "llvm.compiler.used");
    gv->setSection("llvm.metadata");
    AttributeCompilerUsedGlobals.clear();
  }

  // Add llvm.global.annotations
  if (!AttributeAnnotateGlobals.empty()) {
    Constant *Array = ConstantArray::get(
      ArrayType::get(AttributeAnnotateGlobals[0]->getType(),
                                      AttributeAnnotateGlobals.size()),
                       AttributeAnnotateGlobals);
    GlobalValue *gv = new GlobalVariable(*TheModule, Array->getType(), false,
                                         GlobalValue::AppendingLinkage, Array,
                                         "llvm.global.annotations");
    gv->setSection("llvm.metadata");
    AttributeAnnotateGlobals.clear();
  }

  // Finish off the per-function pass.
  if (PerFunctionPasses)
    PerFunctionPasses->doFinalization();

  // Run module-level optimizers, if any are present.
  createPerModuleOptimizationPasses();
  if (PerModulePasses)
    PerModulePasses->run(*TheModule);

  // Run the code generator, if present.
  if (CodeGenPasses) {
    CodeGenPasses->doInitialization();
    for (Module::iterator I = TheModule->begin(), E = TheModule->end();
         I != E; ++I)
      if (!I->isDeclaration())
        CodeGenPasses->run(*I);
    CodeGenPasses->doFinalization();
  }

  FormattedOutStream.flush();
  OutStream->flush();
//TODO  timevar_pop(TV_LLVM_PERFILE);

  // We have finished - shutdown the plugin.  Doing this here ensures that timer
  // info and other statistics are not intermingled with those produced by GCC.
  FinalizePlugin();
}

/// llvm_finish - Run shutdown code when GCC exits.
static void llvm_finish(void * /*gcc_data*/, void * /*user_data*/) {
  FinalizePlugin();
}


/// gate_null - Gate method for a pass that does nothing.
static bool gate_null (void) {
  return false;
}

/// pass_gimple_null - Gimple pass that does nothing.
static struct gimple_opt_pass pass_gimple_null =
{
    {
      GIMPLE_PASS,
      "*gimple_null",	/* name */
      gate_null,	/* gate */
      NULL,		/* execute */
      NULL,		/* sub */
      NULL,		/* next */
      0,		/* static_pass_number */
      TV_NONE,		/* tv_id */
      0,		/* properties_required */
      0,		/* properties_provided */
      0,		/* properties_destroyed */
      0,		/* todo_flags_start */
      0			/* todo_flags_finish */
    }
};

/// execute_correct_state - Correct the cgraph state to ensure that newly
/// inserted functions are processed before being converted to LLVM IR.
static unsigned int execute_correct_state (void) {
  if (cgraph_state < CGRAPH_STATE_IPA_SSA)
    cgraph_state = CGRAPH_STATE_IPA_SSA;
  return 0;
}

/// gate_correct_state - Gate method for pass_gimple_correct_state.
static bool gate_correct_state (void) {
  return true;
}

/// pass_gimple_correct_state - Gimple pass that corrects the cgraph state so
/// newly inserted functions are processed before being converted to LLVM IR.
static struct gimple_opt_pass pass_gimple_correct_state =
{
    {
      GIMPLE_PASS,
      "*gimple_correct_state",	/* name */
      gate_correct_state,	/* gate */
      execute_correct_state,	/* execute */
      NULL,			/* sub */
      NULL,			/* next */
      0,			/* static_pass_number */
      TV_NONE,			/* tv_id */
      0,			/* properties_required */
      0,			/* properties_provided */
      0,			/* properties_destroyed */
      0,			/* todo_flags_start */
      0				/* todo_flags_finish */
    }
};

/// pass_ipa_null - IPA pass that does nothing.
static struct ipa_opt_pass_d pass_ipa_null = {
    {
      IPA_PASS,
      "*ipa_null",	/* name */
      gate_null,	/* gate */
      NULL,		/* execute */
      NULL,		/* sub */
      NULL,		/* next */
      0,		/* static_pass_number */
      TV_NONE,		/* tv_id */
      0,		/* properties_required */
      0,		/* properties_provided */
      0,		/* properties_destroyed */
      0,		/* todo_flags_start */
      0			/* todo_flags_finish */
    },
    NULL,	/* generate_summary */
    NULL,	/* write_summary */
    NULL,	/* read_summary */
#if (GCC_MINOR > 5)
    NULL,		/* write_optimization_summary */
    NULL,		/* read_optimization_summary */
#else
    NULL,		/* function_read_summary */
#endif
    NULL,		/* stmt_fixup */
    0,			/* function_transform_todo_flags_start */
    NULL,		/* function_transform */
    NULL		/* variable_transform */
};

/// pass_rtl_null - RTL pass that does nothing.
static struct rtl_opt_pass pass_rtl_null =
{
    {
      RTL_PASS,
      "*rtl_null",	/* name */
      gate_null,	/* gate */
      NULL,		/* execute */
      NULL,		/* sub */
      NULL,		/* next */
      0,		/* static_pass_number */
      TV_NONE,		/* tv_id */
      0,		/* properties_required */
      0,		/* properties_provided */
      0,		/* properties_destroyed */
      0,		/* todo_flags_start */
      0			/* todo_flags_finish */
    }
};

/// pass_simple_ipa_null - Simple IPA pass that does nothing.
static struct simple_ipa_opt_pass pass_simple_ipa_null =
{
    {
      SIMPLE_IPA_PASS,
      "*simple_ipa_null",	/* name */
      gate_null,		/* gate */
      NULL,			/* execute */
      NULL,			/* sub */
      NULL,			/* next */
      0,			/* static_pass_number */
      TV_NONE,			/* tv_id */
      0,			/* properties_required */
      0,			/* properties_provided */
      0,			/* properties_destroyed */
      0,			/* todo_flags_start */
      0				/* todo_flags_finish */
    }
};


// Garbage collector roots.
extern const struct ggc_cache_tab gt_ggc_rc__gt_cache_h[];


/// PluginFlags - Flag arguments for the plugin.

struct FlagDescriptor {
  const char *Key; // The plugin argument is -fplugin-arg-llvm-KEY.
  bool *Flag;      // Set to true if the flag is seen.
};

static FlagDescriptor PluginFlags[] = {
    { "debug-pass-structure", &DebugPassStructure},
    { "debug-pass-arguments", &DebugPassArguments},
    { "enable-gcc-optzns", &EnableGCCOptimizations },
    { "emit-ir", &EmitIR },
    { "save-gcc-output", &SaveGCCOutput },
    { NULL, NULL } // Terminator.
};


/// llvm_plugin_info - Information about this plugin.  Users can access this
/// using "gcc --help -v".
static struct plugin_info llvm_plugin_info = {
  REVISION,	// version
  // TODO provide something useful here
  NULL		// help
};

static bool version_check(struct plugin_gcc_version *gcc_version,
                          struct plugin_gcc_version *plugin_version) {
  // Make it possible to turn off the version check - useful for testing gcc
  // bootstrap.
  if (getenv("dragonegg_disable_version_check"))
    return true;

  // Check that the running gcc has exactly the same version as the gcc we were
  // built against.  This strict check seems wise when developing against a fast
  // moving gcc tree.  TODO: Use a milder check if doing a "release build".
  return plugin_default_version_check (gcc_version, plugin_version);
}


/// plugin_init - Plugin initialization routine, called by GCC.  This is the
/// first code executed in the plugin (except for constructors).  Configure
/// the plugin and setup GCC, taking over optimization and code generation.
int __attribute__ ((visibility("default")))
plugin_init(struct plugin_name_args *plugin_info,
            struct plugin_gcc_version *version) {
  const char *plugin_name = plugin_info->base_name;
  struct register_pass_info pass_info;

  // Check that the plugin is compatible with the running gcc.
  if (!version_check (&gcc_version, version)) {
    errs() << "Incompatible plugin version\n";
    return 1;
  }

  // Provide GCC with our version and help information.
  register_callback(plugin_name, PLUGIN_INFO, NULL, &llvm_plugin_info);

  // Process any plugin arguments.
  {
    struct plugin_argument *argv = plugin_info->argv;
    int argc = plugin_info->argc;

    for (int i = 0; i < argc; ++i) {
      if (!strcmp (argv[i].key, "llvm-ir-optimize") ||
          !strcmp (argv[i].key, "llvm-codegen-optimize")) {
        if (!argv[i].value) {
          error(G_("no value supplied for option '-fplugin-arg-%s-%s'"),
                plugin_name, argv[i].key);
          continue;
        }
        if (argv[i].value[0] < '0' || argv[i].value[0] > '9' || argv[i].value[1]) {
          error(G_("invalid option argument '-fplugin-arg-%s-%s=%s'"),
                plugin_name, argv[i].key, argv[i].value);
          continue;
        }
        int OptLevel = argv[i].value[0] - '0';
        if (argv[i].key[5] == 'i')
          LLVMIROptimizeArg = OptLevel;
        else
          LLVMCodeGenOptimizeArg = OptLevel;
        continue;
      }

      // All remaining options are flags, so complain if there is an argument.
      if (argv[i].value) {
        error(G_("invalid option argument '-fplugin-arg-%s-%s=%s'"),
              plugin_name, argv[i].key, argv[i].value);
        continue;
      }

      // Look for a matching flag.
      bool Found = false;
      for (FlagDescriptor *F = PluginFlags; F->Key; ++F)
        if (!strcmp (argv[i].key, F->Key)) {
          Found = true;
          *F->Flag = true;
          break;
        }

      if (!Found)
        warning(0, G_("unrecognised option '-fplugin-arg-%s-%s'"),
              plugin_name, argv[i].key);
    }
  }

  // Obtain exclusive use of the assembly code output file.  This stops GCC from
  // writing anything at all to the assembly file - only we get to write to it.
  TakeoverAsmOutput();

  // Register our garbage collector roots.
  register_callback(plugin_name, PLUGIN_REGISTER_GGC_CACHES, NULL,
                    (void *)gt_ggc_rc__gt_cache_h);

  // Perform late initialization just before processing the compilation unit.
  register_callback(plugin_name, PLUGIN_START_UNIT, llvm_start_unit, NULL);

  // Turn off all gcc optimization passes.
  if (!EnableGCCOptimizations) {
    // TODO: figure out a good way of turning off ipa optimization passes.
    // Could just set optimize to zero (after taking a copy), but this would
    // also impact front-end optimizations.

    // Leave pass_inline_parameters.  Otherwise our vector lowering fails since
    // immediates have not been propagated into builtin callsites.

    // Leave pass_ipa_function_and_variable_visibility.  Needed for correctness.

#if (GCC_MINOR < 6)
    // Turn off pass_ipa_early_inline.
    pass_info.pass = &pass_simple_ipa_null.pass;
    pass_info.reference_pass_name = "einline_ipa";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);
#endif

    // Leave pass_ipa_free_lang_data.

    // Leave pass pass_early_local_passes::pass_fixup_cfg. ???

    // Leave pass pass_early_local_passes::pass_tree_profile.

    // Leave pass_early_local_passes::pass_cleanup_cfg. ???

    // Leave pass_early_local_passes::pass_init_datastructures. ???

    // Leave pass_early_local_passes::pass_expand_omp.

    // Leave pass_early_local_passes::pass_referenced_vars. ???

    // Leave pass_early_local_passes::pass_build_ssa.

    // Leave pass_early_local_passes::pass_early_warn_uninitialized.

    // Leave pass_early_local_passes::pass_rebuild_cgraph_edges. ???

    // Leave pass_early_local_passes::pass_early_inline.  Otherwise our vector
    // lowering fails since immediates have not been propagated into builtin
    // callsites.

    // Insert a pass that ensures that any newly inserted functions, for example
    // those generated by OMP expansion, are processed before being converted to
    // LLVM IR.
    pass_info.pass = &pass_gimple_correct_state.pass;
    pass_info.reference_pass_name = "early_optimizations";
    pass_info.ref_pass_instance_number = 1;
    pass_info.pos_op = PASS_POS_INSERT_BEFORE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Turn off pass_early_local_passes::pass_all_early_optimizations.
    pass_info.pass = &pass_gimple_null.pass;
    pass_info.reference_pass_name = "early_optimizations";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Leave pass_early_local_passes::pass_release_ssa_names. ???

    // Leave pass_early_local_passes::pass_rebuild_cgraph_edges. ???

    // Leave pass_inline_parameters.  Otherwise our vector lowering fails since
    // immediates have not been propagated into builtin callsites.

    // Turn off pass_ipa_increase_alignment.
    pass_info.pass = &pass_simple_ipa_null.pass;
    pass_info.reference_pass_name = "increase_alignment";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Turn off pass_ipa_matrix_reorg.
    pass_info.pass = &pass_simple_ipa_null.pass;
    pass_info.reference_pass_name = "matrix-reorg";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Leave pass_ipa_whole_program_visibility. ???

    // Turn off pass_ipa_cp.
    pass_info.pass = &pass_ipa_null.pass;
    pass_info.reference_pass_name = "cp";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Turn off pass_ipa_inline.
    pass_info.pass = &pass_ipa_null.pass;
    pass_info.reference_pass_name = "inline";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Turn off pass_ipa_reference.
    pass_info.pass = &pass_ipa_null.pass;
    pass_info.reference_pass_name = "static-var";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Turn off pass_ipa_pure_const.
    pass_info.pass = &pass_ipa_null.pass;
    pass_info.reference_pass_name = "pure-const";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Turn off pass_ipa_type_escape.
    pass_info.pass = &pass_simple_ipa_null.pass;
    pass_info.reference_pass_name = "type-escape-var";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Turn off pass_ipa_pta.
    pass_info.pass = &pass_simple_ipa_null.pass;
    pass_info.reference_pass_name = "pta";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Turn off pass_ipa_struct_reorg.
    pass_info.pass = &pass_simple_ipa_null.pass;
    pass_info.reference_pass_name = "ipa_struct_reorg";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);
  }

  // Replace the LTO gimple pass with a pass that converts same-body aliases and
  // file-scope asm to LLVM IR.
  pass_info.pass = &pass_emit_aliases.pass;
  pass_info.reference_pass_name = "lto_gimple_out";
  pass_info.ref_pass_instance_number = 0;
  pass_info.pos_op = PASS_POS_REPLACE;
  register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

  // Disable any other LTO passes.
  pass_info.pass = &pass_ipa_null.pass;
  pass_info.reference_pass_name = "lto_decls_out";
  pass_info.ref_pass_instance_number = 0;
  pass_info.pos_op = PASS_POS_REPLACE;
  register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

#if (GCC_MINOR < 6)
  pass_info.pass = &pass_ipa_null.pass;
  pass_info.reference_pass_name = "lto_wpa_fixup";
  pass_info.ref_pass_instance_number = 0;
  pass_info.pos_op = PASS_POS_REPLACE;
  register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);
#endif

  if (!EnableGCCOptimizations) {
    // Disable pass_lower_eh_dispatch, which runs after LLVM conversion.
    pass_info.pass = &pass_gimple_null.pass;
    pass_info.reference_pass_name = "ehdisp";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Disable pass_all_optimizations, which runs after LLVM conversion.
    pass_info.pass = &pass_gimple_null.pass;
    pass_info.reference_pass_name = "*all_optimizations";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Disable pass_lower_complex_O0, which runs after LLVM conversion.
    pass_info.pass = &pass_gimple_null.pass;
    pass_info.reference_pass_name = "cplxlower0";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Disable pass_cleanup_eh, which runs after LLVM conversion.
    pass_info.pass = &pass_gimple_null.pass;
    pass_info.reference_pass_name = "ehcleanup";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Disable pass_lower_resx, which runs after LLVM conversion.
    pass_info.pass = &pass_gimple_null.pass;
    pass_info.reference_pass_name = "resx";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Disable pass_nrv, which runs after LLVM conversion.
    pass_info.pass = &pass_gimple_null.pass;
    pass_info.reference_pass_name = "nrv";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Disable pass_mudflap_2, which runs after LLVM conversion.
    pass_info.pass = &pass_gimple_null.pass;
    pass_info.reference_pass_name = "mudflap2";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // Disable pass_cleanup_cfg_post_optimizing, which runs after LLVM conversion.
    pass_info.pass = &pass_gimple_null.pass;
    pass_info.reference_pass_name = "optimized";
    pass_info.ref_pass_instance_number = 0;
    pass_info.pos_op = PASS_POS_REPLACE;
    register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    // TODO: Disable pass_warn_function_noreturn?
  }

  // Replace rtl expansion with a pass that converts functions to LLVM IR.
  pass_info.pass = &pass_rtl_emit_function.pass;
  pass_info.reference_pass_name = "expand";
  pass_info.ref_pass_instance_number = 0;
  pass_info.pos_op = PASS_POS_REPLACE;
  register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

  // Turn off all other rtl passes.
  pass_info.pass = &pass_gimple_null.pass;
  pass_info.reference_pass_name = "*rest_of_compilation";
  pass_info.ref_pass_instance_number = 0;
  pass_info.pos_op = PASS_POS_REPLACE;
  register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

  pass_info.pass = &pass_rtl_null.pass;
  pass_info.reference_pass_name = "*clean_state";
  pass_info.ref_pass_instance_number = 0;
  pass_info.pos_op = PASS_POS_REPLACE;
  register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

  // Output GCC global variables to the LLVM IR and finish the .s file once the
  // compilation unit has been completely processed.
  register_callback(plugin_name, PLUGIN_FINISH_UNIT, llvm_finish_unit, NULL);

  // Run shutdown code when GCC exits.
  register_callback(plugin_name, PLUGIN_FINISH, llvm_finish, NULL);

  return 0;
}
