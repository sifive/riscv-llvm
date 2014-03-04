//===------ Debug.h - Interface for generating debug info -------*- C++ -*-===//
//
// Copyright (C) 2006 to 2013  Jim Laskey, Duncan Sands et al.
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
// This file declares the debug interfaces shared among the dragonegg files.
//===----------------------------------------------------------------------===//

#ifndef DRAGONEGG_DEBUG_H
#define DRAGONEGG_DEBUG_H

// Plugin headers
#include "dragonegg/Internals.h"

// LLVM headers
#include "llvm/DebugInfo.h"
#include "llvm/DIBuilder.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/Support/Allocator.h"

// System headers
#include <map>

// Forward declarations
namespace llvm {
class AllocaInst;
class BasicBlock;
class CallInst;
class Function;
class Module;
}

/// DebugInfo - This class gathers all debug information during compilation and
/// is responsible for emitting to llvm globals or pass directly to the backend.
class DebugInfo {
private:
  llvm::SmallVector<llvm::WeakVH, 4> RegionStack;
  // Stack to track declarative scopes.

  std::map<tree_node *, llvm::WeakVH> RegionMap;

  llvm::Module &M;
  llvm::LLVMContext &VMContext;
  llvm::DIBuilder Builder;
  llvm::Function *DeclareFn; // llvm.dbg.declare
  llvm::Function *ValueFn;   // llvm.dbg.value

  const char *CurFullPath;  // Previous location file encountered.
  const char *PrevFullPath; // Previous location file encountered.
  int CurLineNo;            // Previous location line# encountered.
  int PrevLineNo;           // Previous location line# encountered.
  llvm::BasicBlock *PrevBB;       // Last basic block encountered.

  std::map<tree_node *, llvm::WeakVH> TypeCache;
  // Cache of previously constructed
  // Types.
  std::map<tree_node *, llvm::WeakVH> SPCache;
  // Cache of previously constructed
  // Subprograms.
  std::map<tree_node *, llvm::WeakVH> NameSpaceCache;
  // Cache of previously constructed name
  // spaces.

  /// FunctionNames - This is a storage for function names that are
  /// constructed on demand. For example, C++ destructors, C++ operators etc..
  llvm::BumpPtrAllocator FunctionNames;

public:
  DebugInfo(llvm::Module *m);

  ~DebugInfo() { Builder.finalize(); }

  /// Initialize - Initialize debug info by creating compile unit for
  /// main_input_filename. This must be invoked after language dependent
  /// initialization is done.
  void Initialize();

  // Accessors.
  void setLocationFile(const char *FullPath) { CurFullPath = FullPath; }
  void setLocationLine(int LineNo) { CurLineNo = LineNo; }

  /// EmitFunctionStart - Constructs the debug code for entering a function -
  /// "llvm.dbg.func.start."
  void EmitFunctionStart(tree_node *FnDecl, llvm::Function *Fn);

  /// EmitFunctionEnd - Constructs the debug code for exiting a declarative
  /// region - "llvm.dbg.region.end."
  void EmitFunctionEnd(bool EndFunction);

  /// EmitDeclare - Constructs the debug code for allocation of a new variable.
  /// region - "llvm.dbg.declare."
  void EmitDeclare(tree_node *decl, unsigned Tag, llvm::StringRef Name,
                   tree_node *type, llvm::Value *AI, LLVMBuilder &Builder);

  /// EmitStopPoint - Emit a call to llvm.dbg.stoppoint to indicate a change of
  /// source line.
  void EmitStopPoint(llvm::BasicBlock *CurBB, LLVMBuilder &Builder);

  /// EmitGlobalVariable - Emit information about a global variable.
  ///
  void EmitGlobalVariable(llvm::GlobalVariable *GV, tree_node *decl);

  /// getOrCreateType - Get the type from the cache or create a new type if
  /// necessary.
  llvm::DIType getOrCreateType(tree_node *type);

  /// createBasicType - Create BasicType.
  llvm::DIType createBasicType(tree_node *type);

  /// createMethodType - Create MethodType.
  llvm::DIType createMethodType(tree_node *type);

  /// createPointerType - Create PointerType.
  llvm::DIType createPointerType(tree_node *type);

  /// createArrayType - Create ArrayType.
  llvm::DIType createArrayType(tree_node *type);

  /// createEnumType - Create EnumType.
  llvm::DIType createEnumType(tree_node *type);

  /// createStructType - Create StructType for struct or union or class.
  llvm::DIType createStructType(tree_node *type);

  /// createVarinatType - Create variant type or return MainTy.
  llvm::DIType createVariantType(tree_node *type, llvm::DIType MainTy);

  /// getOrCreateCompileUnit - Create a new compile unit.
  void getOrCreateCompileUnit(const char *FullPath, bool isMain = false);

  /// getOrCreateFile - Get DIFile descriptor.
  llvm::DIFile getOrCreateFile(const char *FullPath);

  /// findRegion - Find tree_node N's region.
  llvm::DIDescriptor findRegion(tree_node *n);

  /// getOrCreateNameSpace - Get name space descriptor for the tree node.
  llvm::DINameSpace getOrCreateNameSpace(tree_node *Node,
                                         llvm::DIDescriptor Context);

  /// getFunctionName - Get function name for the given FnDecl. If the
  /// name is constructred on demand (e.g. C++ destructor) then the name
  /// is stored on the side.
  llvm::StringRef getFunctionName(tree_node *FnDecl);

private:
  /// CreateDerivedType - Create a derived type like const qualified type,
  /// pointer, typedef, etc.
  llvm::DIDerivedType CreateDerivedType(
      unsigned Tag, llvm::DIDescriptor Context, llvm::StringRef Name,
      llvm::DIFile F, unsigned LineNumber, uint64_t SizeInBits,
      uint64_t AlignInBits, uint64_t OffsetInBits, unsigned Flags,
      llvm::DIType DerivedFrom);

  /// CreateCompositeType - Create a composite type like array, struct, etc.
  llvm::DICompositeType CreateCompositeType(
      unsigned Tag, llvm::DIDescriptor Context, llvm::StringRef Name,
      llvm::DIFile F, unsigned LineNumber, uint64_t SizeInBits,
      uint64_t AlignInBits, uint64_t OffsetInBits, unsigned Flags,
      llvm::DIType DerivedFrom, llvm::DIArray Elements,
      unsigned RunTimeLang = 0, llvm::MDNode *ContainingType = 0);

  /// CreateSubprogram - Create a new descriptor for the specified subprogram.
  /// See comments in DISubprogram for descriptions of these fields.
  llvm::DISubprogram CreateSubprogram(
      llvm::DIDescriptor Context, llvm::StringRef Name,
      llvm::StringRef DisplayName, llvm::StringRef LinkageName, llvm::DIFile F,
      unsigned LineNo, llvm::DIType Ty, bool isLocalToUnit, bool isDefinition,
      unsigned VK = 0, unsigned VIndex = 0,
      llvm::DIType ContainingType = llvm::DIType(), unsigned Flags = 0,
      bool isOptimized = false, llvm::Function *Fn = 0);

  /// CreateSubprogramDefinition - Create new subprogram descriptor for the
  /// given declaration.
  llvm::DISubprogram
  CreateSubprogramDefinition(llvm::DISubprogram &SPDeclaration,
                             unsigned LineNo, llvm::Function *Fn);

  /// InsertDeclare - Insert a new llvm.dbg.declare intrinsic call.
  llvm::Instruction *
  InsertDeclare(llvm::Value *Storage, llvm::DIVariable D,
                llvm::BasicBlock *InsertAtEnd);

  /// InsertDeclare - Insert a new llvm.dbg.declare intrinsic call.
  llvm::Instruction *
  InsertDeclare(llvm::Value *Storage, llvm::DIVariable D,
                llvm::Instruction *InsertBefore);

  /// InsertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.
  llvm::Instruction *
  InsertDbgValueIntrinsic(llvm::Value *V, uint64_t Offset, llvm::DIVariable D,
                          llvm::BasicBlock *InsertAtEnd);

  /// InsertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.
  llvm::Instruction *InsertDbgValueIntrinsic(llvm::Value *V, uint64_t Offset,
                                             llvm::DIVariable D,
                                             llvm::Instruction *InsertBefore);
};

#endif /* DRAGONEGG_DEBUG_H */
