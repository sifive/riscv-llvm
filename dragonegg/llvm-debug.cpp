/* High-level LLVM backend interface 
Copyright (C) 2005 Free Software Foundation, Inc.
Contributed by Jim Laskey (jlaskey@apple.com)

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
// This is a C++ source file that implements the debug information gathering.
//===----------------------------------------------------------------------===//

// LLVM headers
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"
#include "llvm/Intrinsics.h"
#include "llvm/Module.h"
#include "llvm/Analysis/DebugInfo.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Dwarf.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/SmallVector.h"

// System headers
#include <gmp.h>

// GCC headers
#undef VISIBILITY_HIDDEN

extern "C" {
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"

#include "flags.h"
#include "langhooks.h"
#include "toplev.h"
#include "version.h"
}

// Plugin headers
#include "llvm-abi.h"
#include "llvm-debug.h"
#include "llvm-internal.h"
#include "bits_and_bobs.h"

using namespace llvm;
using namespace llvm::dwarf;

#ifndef LLVMTESTDEBUG
#define DEBUGASSERT(S) ((void)0)
#else
#define DEBUGASSERT(S) assert(S)
#endif


/// DirectoryAndFile - Extract the directory and file name from a path.  If no
/// directory is specified, then use the source working directory.
static void DirectoryAndFile(const std::string &FullPath,
                             std::string &Directory, std::string &FileName) {
  // Look for the directory slash.
  size_t Slash = FullPath.rfind('/');
  
  // If no slash
  if (Slash == std::string::npos) {
    // The entire path is the file name.
    Directory = "";
    FileName = FullPath;
  } else {
    // Separate the directory from the file name.
    Directory = FullPath.substr(0, Slash);
    FileName = FullPath.substr(Slash + 1);
  }
  
  // If no directory present then use source working directory.
  if (Directory.empty() || Directory[0] != '/') {
    Directory = std::string(get_src_pwd()) + "/" + Directory;
  }
}

/// NodeSizeInBits - Returns the size in bits stored in a tree node regardless
/// of whether the node is a TYPE or DECL.
static uint64_t NodeSizeInBits(tree Node) {
  if (TREE_CODE(Node) == ERROR_MARK) {
    return BITS_PER_WORD;
  } else if (TYPE_P(Node)) {
    if (TYPE_SIZE(Node) == NULL_TREE)
      return 0;
    else if (isInt64(TYPE_SIZE(Node), 1))
      return getINTEGER_CSTVal(TYPE_SIZE(Node));
    else
      return TYPE_ALIGN(Node);
  } else if (DECL_P(Node)) {
    if (DECL_SIZE(Node) == NULL_TREE)
      return 0;
    else if (isInt64(DECL_SIZE(Node), 1))
      return getINTEGER_CSTVal(DECL_SIZE(Node));
    else
      return DECL_ALIGN(Node);
  }
  
  return 0;
}

/// NodeAlignInBits - Returns the alignment in bits stored in a tree node
/// regardless of whether the node is a TYPE or DECL.
static uint64_t NodeAlignInBits(tree Node) {
  if (TREE_CODE(Node) == ERROR_MARK) return BITS_PER_WORD;
  if (TYPE_P(Node)) return TYPE_ALIGN(Node);
  if (DECL_P(Node)) return DECL_ALIGN(Node);
  return BITS_PER_WORD;
}

/// FieldType - Returns the type node of a structure member field.
///
static tree FieldType(tree Field) {
  if (TREE_CODE (Field) == ERROR_MARK) return integer_type_node;
  return getDeclaredType(Field);
}

/// GetNodeName - Returns the name stored in a node regardless of whether the
/// node is a TYPE or DECL.
static StringRef GetNodeName(tree Node) {
  tree Name = NULL;
  
  if (DECL_P(Node)) {
    Name = DECL_NAME(Node);
  } else if (TYPE_P(Node)) {
    Name = TYPE_NAME(Node);
  }

  if (Name) {
    if (TREE_CODE(Name) == IDENTIFIER_NODE) {
      return IDENTIFIER_POINTER(Name);
    } else if (TREE_CODE(Name) == TYPE_DECL && DECL_NAME(Name) &&
               !DECL_IGNORED_P(Name)) {
      return StringRef(IDENTIFIER_POINTER(DECL_NAME(Name)));
    }
  }
  
  return StringRef();
}

/// GetNodeLocation - Returns the location stored in a node  regardless of
/// whether the node is a TYPE or DECL.  UseStub is true if we should consider
/// the type stub as the actually location (ignored in struct/unions/enums.)
static expanded_location GetNodeLocation(tree Node, bool UseStub = true) {
  expanded_location Location = { NULL, 0 };

  if (Node == NULL_TREE)
    return Location;

  tree Name = NULL;
  
  if (DECL_P(Node)) {
    Name = DECL_NAME(Node);
  } else if (TYPE_P(Node)) {
    Name = TYPE_NAME(Node);
  }
  
  if (Name) {
    if (TYPE_STUB_DECL(Name)) {
      tree Stub = TYPE_STUB_DECL(Name);
      Location = expand_location(DECL_SOURCE_LOCATION(Stub));
    } else if (DECL_P(Name)) {
      Location = expand_location(DECL_SOURCE_LOCATION(Name));
    }
  }
  
  if (!Location.line) {
    if (UseStub && TYPE_STUB_DECL(Node)) {
      tree Stub = TYPE_STUB_DECL(Node);
      Location = expand_location(DECL_SOURCE_LOCATION(Stub));
    } else if (DECL_P(Node)) {
      Location = expand_location(DECL_SOURCE_LOCATION(Node));
    }
  }
  
  return Location;
}

static StringRef getLinkageName(tree Node) {

  // Use llvm value name as linkage name if it is available.
  if (DECL_LLVM_SET_P(Node)) {
    Value *V = DECL_LLVM(Node);
    return V->getName();
  }

  tree decl_name = DECL_NAME(Node);
  if (decl_name != NULL && IDENTIFIER_POINTER (decl_name) != NULL) {
    if (TREE_PUBLIC(Node) &&
        DECL_ASSEMBLER_NAME(Node) != DECL_NAME(Node) && 
        !DECL_ABSTRACT(Node)) {
      return StringRef(IDENTIFIER_POINTER(DECL_ASSEMBLER_NAME(Node)));
    } 
  }
  return StringRef();
}

DebugInfo::DebugInfo(Module *m)
: M(m)
, DebugFactory(*m)
, CurFullPath("")
, CurLineNo(0)
, PrevFullPath("")
, PrevLineNo(0)
, PrevBB(NULL)
, FwdTypeCount(0)
, RegionStack()
{}

/// getFunctionName - Get function name for the given FnDecl. If the
/// name is constructred on demand (e.g. C++ destructor) then the name
/// is stored on the side.
StringRef DebugInfo::getFunctionName(tree FnDecl) {
  StringRef FnNodeName = GetNodeName(FnDecl);
  // Use dwarf_name to construct function names. In C++ this is used to
  // create human readable destructor names.
  StringRef FnName = lang_hooks.dwarf_name(FnDecl, 0);
  if (FnNodeName.equals(FnName))
    return FnNodeName;

  // Use name returned by dwarf_name. It is in a temp. storage so make a 
  // copy first.
  char *StrPtr = FunctionNames.Allocate<char>(FnName.size() + 1);
  strncpy(StrPtr, FnName.data(), FnName.size());
  StrPtr[FnName.size()] = 0;
  return StringRef(StrPtr);
}

/// EmitFunctionStart - Constructs the debug code for entering a function -
/// "llvm.dbg.func.start."
void DebugInfo::EmitFunctionStart(tree FnDecl, Function *Fn,
                                  BasicBlock *CurBB) {

  DIType FNType = getOrCreateType(TREE_TYPE(FnDecl));

  std::map<tree_node *, WeakVH >::iterator I = SPCache.find(FnDecl);
  if (I != SPCache.end()) {
    DISubprogram SPDecl(cast<MDNode>(I->second));
    DISubprogram SP = 
      DebugFactory.CreateSubprogramDefinition(SPDecl);
    SPDecl.getNode()->replaceAllUsesWith(SP.getNode());

    // Push function on region stack.
    RegionStack.push_back(WeakVH(SP.getNode()));
    RegionMap[FnDecl] = WeakVH(SP.getNode());
    return;
  } 

  bool ArtificialFnWithAbstractOrigin = false;
  // If this artificial function has abstract origin then put this function
  // at module scope. The abstract copy will be placed in appropriate region.
  if (DECL_ARTIFICIAL (FnDecl)
      && DECL_ABSTRACT_ORIGIN (FnDecl)
      && DECL_ABSTRACT_ORIGIN (FnDecl) != FnDecl)
    ArtificialFnWithAbstractOrigin = true;

  DIDescriptor SPContext = ArtificialFnWithAbstractOrigin ?
    getOrCreateFile(main_input_filename) :
    findRegion (DECL_CONTEXT(FnDecl));

  // Creating context may have triggered creation of this SP descriptor. So
  // check the cache again.
  I = SPCache.find(FnDecl);
  if (I != SPCache.end()) {
    DISubprogram SPDecl(cast<MDNode>(I->second));
    DISubprogram SP = 
      DebugFactory.CreateSubprogramDefinition(SPDecl);
    SPDecl.getNode()->replaceAllUsesWith(SP.getNode());

    // Push function on region stack.
    RegionStack.push_back(WeakVH(SP.getNode()));
    RegionMap[FnDecl] = WeakVH(SP.getNode());
    return;
  } 

  // Gather location information.
  expanded_location Loc = GetNodeLocation(FnDecl, false);
  StringRef LinkageName = getLinkageName(FnDecl);

  unsigned lineno = CurLineNo;

  unsigned Virtuality = 0;
  unsigned VIndex = 0;
  DIType ContainingType;
  if (DECL_VINDEX (FnDecl) &&
      DECL_CONTEXT (FnDecl) && TYPE_P((DECL_CONTEXT (FnDecl)))) { // Workaround GCC PR42653
    if (host_integerp (DECL_VINDEX (FnDecl), 0))
      VIndex = tree_low_cst (DECL_VINDEX (FnDecl), 0);
    Virtuality = dwarf::DW_VIRTUALITY_virtual;
    ContainingType = getOrCreateType(DECL_CONTEXT (FnDecl));
  }

  StringRef FnName = getFunctionName(FnDecl);

  DISubprogram SP = 
    DebugFactory.CreateSubprogram(SPContext,
                                  FnName, FnName,
                                  LinkageName,
                                  getOrCreateFile(Loc.file), lineno,
                                  FNType,
                                  Fn->hasInternalLinkage(),
                                  true /*definition*/,
                                  Virtuality, VIndex, ContainingType);
                          

  SPCache[FnDecl] = WeakVH(SP.getNode());

  // Push function on region stack.
  RegionStack.push_back(WeakVH(SP.getNode()));
  RegionMap[FnDecl] = WeakVH(SP.getNode());
}

/// getOrCreateNameSpace - Get name space descriptor for the tree node.
DINameSpace DebugInfo::getOrCreateNameSpace(tree Node, DIDescriptor Context) {
  std::map<tree_node *, WeakVH >::iterator I = 
    NameSpaceCache.find(Node);
  if (I != NameSpaceCache.end())
    return DINameSpace(cast<MDNode>(I->second));

  expanded_location Loc = GetNodeLocation(Node, false);
  DINameSpace DNS =
    DebugFactory.CreateNameSpace(Context, GetNodeName(Node),
                                 getOrCreateFile(Loc.file), Loc.line);

  NameSpaceCache[Node] = WeakVH(DNS.getNode());
  return DNS;
}

/// findRegion - Find tree_node N's region.
DIDescriptor DebugInfo::findRegion(tree Node) {
  if (Node == NULL_TREE)
    return getOrCreateFile(main_input_filename);

  std::map<tree_node *, WeakVH>::iterator I = RegionMap.find(Node);
  if (I != RegionMap.end())
    if (MDNode *R = dyn_cast_or_null<MDNode>(I->second))
      return DIDescriptor(R);

  if (TYPE_P (Node)) {
    DIType Ty = getOrCreateType(Node);
    return DIDescriptor(Ty.getNode());
  } else if (DECL_P (Node)) {
    if (TREE_CODE (Node) == NAMESPACE_DECL) {
      DIDescriptor NSContext = findRegion(DECL_CONTEXT(Node));
      DINameSpace NS = getOrCreateNameSpace(Node, NSContext);
      return DIDescriptor(NS.getNode());
    }
    return findRegion (DECL_CONTEXT (Node));
  }

  // Otherwise main compile unit covers everything.
  return getOrCreateFile(main_input_filename);
}

/// EmitFunctionEnd - Constructs the debug code for exiting a declarative
/// region - "llvm.dbg.region.end."
void DebugInfo::EmitFunctionEnd(BasicBlock *CurBB, bool EndFunction) {
  assert(!RegionStack.empty() && "Region stack mismatch, stack empty!");
  RegionStack.pop_back();
  // Blocks get erased; clearing these is needed for determinism, and also
  // a good idea if the next function gets inlined.
  if (EndFunction) {
    PrevBB = NULL;
    PrevLineNo = 0;
    PrevFullPath = NULL;
  }
}

/// EmitDeclare - Constructs the debug code for allocation of a new variable.
/// region - "llvm.dbg.declare."
void DebugInfo::EmitDeclare(tree decl, unsigned Tag, const char *Name,
                            tree type, Value *AI, LLVMBuilder &Builder) {

  // Do not emit variable declaration info, for now.
  if (optimize)
    return;

  // Ignore compiler generated temporaries.
  if (DECL_IGNORED_P(decl))
    return;

  assert(!RegionStack.empty() && "Region stack mismatch, stack empty!");

  expanded_location Loc = GetNodeLocation(decl, false);

  // Construct variable.
  DIScope VarScope = DIScope(cast<MDNode>(RegionStack.back()));
  DIType Ty = getOrCreateType(type);
  if (DECL_ARTIFICIAL (decl))
      Ty = DebugFactory.CreateArtificialType(Ty);
  llvm::DIVariable D =
    DebugFactory.CreateVariable(Tag, VarScope,
                                Name, getOrCreateFile(Loc.file),
                                Loc.line, Ty);

  // Insert an llvm.dbg.declare into the current block.
  Instruction *Call = DebugFactory.InsertDeclare(AI, D, 
                                                 Builder.GetInsertBlock());
  Call->setDebugLoc(DebugLoc::get(CurLineNo, 0, VarScope.getNode()));
}

/// EmitStopPoint - Emit a call to llvm.dbg.stoppoint to indicate a change of 
/// source line - "llvm.dbg.stoppoint."  Now enabled at -O.
void DebugInfo::EmitStopPoint(Function *Fn, BasicBlock *CurBB,
                              LLVMBuilder &Builder) {
  // Don't bother if things are the same as last time.
  if (PrevLineNo == CurLineNo &&
      PrevBB == CurBB &&
      (PrevFullPath == CurFullPath ||
       !strcmp(PrevFullPath, CurFullPath))) return;
  if (!CurFullPath[0] || CurLineNo == 0) return;
  
  // Update last state.
  PrevFullPath = CurFullPath;
  PrevLineNo = CurLineNo;
  PrevBB = CurBB;

    if (RegionStack.empty())
      return;
    MDNode *Scope = cast<MDNode>(RegionStack.back());
    Builder.SetCurrentDebugLocation(DebugLoc::get(CurLineNo,0/*col*/,Scope));
}

/// EmitGlobalVariable - Emit information about a global variable.
///
void DebugInfo::EmitGlobalVariable(GlobalVariable *GV, tree decl) {
  if (DECL_ARTIFICIAL(decl))
    return;
  // Gather location information.
  expanded_location Loc = expand_location(DECL_SOURCE_LOCATION(decl));
  DIType TyD = getOrCreateType(TREE_TYPE(decl));
  StringRef DispName = GV->getName();
  if (DECL_NAME(decl)) {
    if (IDENTIFIER_POINTER(DECL_NAME(decl)))
      DispName = IDENTIFIER_POINTER(DECL_NAME(decl));
  }
  StringRef LinkageName;
  // The gdb does not expect linkage names for function local statics.
  if (DECL_CONTEXT (decl))
    if (TREE_CODE (DECL_CONTEXT (decl)) != FUNCTION_DECL)
      LinkageName = GV->getName();
  DebugFactory.CreateGlobalVariable(findRegion(DECL_CONTEXT(decl)),
                                    DispName, DispName, LinkageName,
                                    getOrCreateFile(Loc.file), Loc.line,
                                    TyD, GV->hasInternalLinkage(),
                                    true/*definition*/, GV);
}

/// createBasicType - Create BasicType.
DIType DebugInfo::createBasicType(tree type) {

  StringRef TypeName = GetNodeName(type);
  uint64_t Size = NodeSizeInBits(type);
  uint64_t Align = NodeAlignInBits(type);

  unsigned Encoding = 0;
  
  switch (TREE_CODE(type)) {
  case INTEGER_TYPE:
    if (TYPE_STRING_FLAG (type)) {
      if (TYPE_UNSIGNED (type))
        Encoding = DW_ATE_unsigned_char;
      else
        Encoding = DW_ATE_signed_char;
    }
    else if (TYPE_UNSIGNED (type))
      Encoding = DW_ATE_unsigned;
    else
      Encoding = DW_ATE_signed;
    break;
  case REAL_TYPE:
    Encoding = DW_ATE_float;
    break;
  case COMPLEX_TYPE:
    Encoding = TREE_CODE(TREE_TYPE(type)) == REAL_TYPE ?
      DW_ATE_complex_float : DW_ATE_lo_user;
    break;
  case BOOLEAN_TYPE:
    Encoding = DW_ATE_boolean;
    break;
  default: { 
    DEBUGASSERT(0 && "Basic type case missing");
    Encoding = DW_ATE_signed;
    Size = BITS_PER_WORD;
    Align = BITS_PER_WORD;
    break;
  }
  }

  return 
    DebugFactory.CreateBasicType(getOrCreateFile(main_input_filename),
                                 TypeName, 
                                 getOrCreateFile(main_input_filename),
                                 0, Size, Align,
                                 0, 0, Encoding);
}

/// isArtificialArgumentType - Return true if arg_type represents artificial,
/// i.e. "this" in c++, argument.
static bool isArtificialArgumentType(tree arg_type, tree method_type) {
  if (TREE_CODE (method_type) != METHOD_TYPE) return false;
  if (TREE_CODE (arg_type) != POINTER_TYPE) return false;
  if (TREE_TYPE (arg_type) == TYPE_METHOD_BASETYPE (method_type))
    return true;
  if (TYPE_MAIN_VARIANT (TREE_TYPE (arg_type))
      && TYPE_MAIN_VARIANT (TREE_TYPE (arg_type)) != TREE_TYPE (arg_type)
      && (TYPE_MAIN_VARIANT (TREE_TYPE (arg_type))
          == TYPE_METHOD_BASETYPE (method_type)))
    return true;
  return false;
}

/// createMethodType - Create MethodType.
DIType DebugInfo::createMethodType(tree type) {

  // Create a  place holder type first. The may be used as a context
  // for the argument types.
  char *FwdTypeName = (char *)alloca(65);
  sprintf(FwdTypeName, "fwd.type.%d", FwdTypeCount++);
  llvm::DIType FwdType = 
    DebugFactory.CreateCompositeType(llvm::dwarf::DW_TAG_subroutine_type,
                                     getOrCreateFile(main_input_filename),
                                     FwdTypeName,
                                     getOrCreateFile(main_input_filename),
                                     0, 0, 0, 0, 0,
                                     llvm::DIType(), llvm::DIArray());
  llvm::TrackingVH<llvm::MDNode> FwdTypeNode = FwdType.getNode();
  TypeCache[type] = WeakVH(FwdType.getNode());
  // Push the struct on region stack.
  RegionStack.push_back(WeakVH(FwdType.getNode()));
  RegionMap[type] = WeakVH(FwdType.getNode());
  
  llvm::SmallVector<llvm::DIDescriptor, 16> EltTys;
  
  // Add the result type at least.
  EltTys.push_back(getOrCreateType(TREE_TYPE(type)));
  
  // Set up remainder of arguments.
  bool ProcessedFirstArg = false;
  for (tree arg = TYPE_ARG_TYPES(type); arg; arg = TREE_CHAIN(arg)) {
    tree formal_type = TREE_VALUE(arg);
    if (formal_type == void_type_node) break;
    llvm::DIType FormalType = getOrCreateType(formal_type);
    if (!ProcessedFirstArg && isArtificialArgumentType(formal_type, type)) {
      DIType AFormalType = DebugFactory.CreateArtificialType(FormalType);
      EltTys.push_back(AFormalType);
    } else
      EltTys.push_back(FormalType);
    if (!ProcessedFirstArg)
      ProcessedFirstArg = true;
  }
  
  llvm::DIArray EltTypeArray =
    DebugFactory.GetOrCreateArray(EltTys.data(), EltTys.size());

  RegionStack.pop_back();
  std::map<tree_node *, WeakVH>::iterator RI = RegionMap.find(type);
  if (RI != RegionMap.end())
    RegionMap.erase(RI);

  llvm::DIType RealType =
    DebugFactory.CreateCompositeType(llvm::dwarf::DW_TAG_subroutine_type,
                                     findRegion(TYPE_CONTEXT(type)), 
                                     StringRef(),
                                     getOrCreateFile(main_input_filename),
                                     0, 0, 0, 0, 0,
                                     llvm::DIType(), EltTypeArray);

  // Now that we have a real decl for the struct, replace anything using the
  // old decl with the new one.  This will recursively update the debug info.
  llvm::DIDerivedType(FwdTypeNode).replaceAllUsesWith(RealType);

  return RealType;
}

/// createPointerType - Create PointerType.
DIType DebugInfo::createPointerType(tree type) {

  DIType FromTy = getOrCreateType(TREE_TYPE(type));
  // type* and type&
  // FIXME: Should BLOCK_POINTER_TYP have its own DW_TAG?
  unsigned Tag = TREE_CODE(type) == POINTER_TYPE ?
    DW_TAG_pointer_type :
    DW_TAG_reference_type;
  unsigned Flags = 0;

  // Check if this pointer type has a name.
  if (tree TyName = TYPE_NAME(type)) 
    if (TREE_CODE(TyName) == TYPE_DECL && !DECL_ORIGINAL_TYPE(TyName)) {
      expanded_location TypeNameLoc = GetNodeLocation(TyName);
      DIType Ty = 
        DebugFactory.CreateDerivedType(Tag, findRegion(DECL_CONTEXT(TyName)),
                                       GetNodeName(TyName), 
                                       getOrCreateFile(TypeNameLoc.file),
                                       TypeNameLoc.line,
                                       0 /*size*/,
                                       0 /*align*/,
                                       0 /*offset */, 
                                       0 /*flags*/, 
                                       FromTy);
      TypeCache[TyName] = WeakVH(Ty.getNode());
      return Ty;
    }
  
  StringRef PName = FromTy.getName();
  DIType PTy = 
    DebugFactory.CreateDerivedType(Tag, findRegion(TYPE_CONTEXT(type)), 
                                   Tag == DW_TAG_pointer_type ? 
                                   StringRef() : PName,
                                   getOrCreateFile(main_input_filename),
                                   0 /*line no*/, 
                                   NodeSizeInBits(type),
                                   NodeAlignInBits(type),
                                   0 /*offset */, 
                                   Flags, 
                                   FromTy);
  return PTy;
}

/// createArrayType - Create ArrayType.
DIType DebugInfo::createArrayType(tree type) {

  // type[n][m]...[p]
  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_STRING_FLAG(type) && TREE_CODE(TREE_TYPE(type)) == INTEGER_TYPE){
    DEBUGASSERT(0 && "Don't support pascal strings");
    return DIType();
  }
  
  unsigned Tag = 0;
  
  if (TREE_CODE(type) == VECTOR_TYPE) {
    Tag = DW_TAG_vector_type;
    type = TREE_TYPE (TYPE_FIELDS (TYPE_DEBUG_REPRESENTATION_TYPE (type)));
  }
  else
    Tag = DW_TAG_array_type;
  
  // Add the dimensions of the array.  FIXME: This loses CV qualifiers from
  // interior arrays, do we care?  Why aren't nested arrays represented the
  // obvious/recursive way?
  llvm::SmallVector<llvm::DIDescriptor, 8> Subscripts;
  
  // There will be ARRAY_TYPE nodes for each rank.  Followed by the derived
  // type.
  tree atype = type;
  tree EltTy = TREE_TYPE(atype);
  for (; TREE_CODE(atype) == ARRAY_TYPE; 
       atype = TREE_TYPE(atype)) {
    tree Domain = TYPE_DOMAIN(atype);
    if (Domain) {
      // FIXME - handle dynamic ranges
      tree MinValue = TYPE_MIN_VALUE(Domain);
      tree MaxValue = TYPE_MAX_VALUE(Domain);
      uint64_t Low = 0;
      uint64_t Hi = 0;
      if (MinValue && isInt64(MinValue, 0))
        Low = getINTEGER_CSTVal(MinValue);
      if (MaxValue && isInt64(MaxValue, 0))
        Hi = getINTEGER_CSTVal(MaxValue);
      Subscripts.push_back(DebugFactory.GetOrCreateSubrange(Low, Hi));
    }
    EltTy = TREE_TYPE(atype);
  }
  
  llvm::DIArray SubscriptArray =
    DebugFactory.GetOrCreateArray(Subscripts.data(), Subscripts.size());
  expanded_location Loc = GetNodeLocation(type);
  return DebugFactory.CreateCompositeType(llvm::dwarf::DW_TAG_array_type,
                                          findRegion(TYPE_CONTEXT(type)), 
                                          StringRef(),
                                          getOrCreateFile(Loc.file), 0, 
                                          NodeSizeInBits(type), 
                                          NodeAlignInBits(type), 0, 0,
                                          getOrCreateType(EltTy),
                                          SubscriptArray);
}

/// createEnumType - Create EnumType.
DIType DebugInfo::createEnumType(tree type) {
  // enum { a, b, ..., z };
  llvm::SmallVector<llvm::DIDescriptor, 32> Elements;
  
  if (TYPE_SIZE(type)) {
    for (tree Link = TYPE_VALUES(type); Link; Link = TREE_CHAIN(Link)) {
      tree EnumValue = TREE_VALUE(Link);
      if (TREE_CODE(EnumValue) == CONST_DECL)
        EnumValue = DECL_INITIAL(EnumValue);
      int64_t Value = getINTEGER_CSTVal(EnumValue);
      const char *EnumName = IDENTIFIER_POINTER(TREE_PURPOSE(Link));
      Elements.push_back(DebugFactory.CreateEnumerator(EnumName, Value));
    }
  }
  
  llvm::DIArray EltArray =
    DebugFactory.GetOrCreateArray(Elements.data(), Elements.size());
  
  expanded_location Loc = { NULL, 0 };
  if (TYPE_SIZE(type)) 
    // Incomplete enums do not  have any location info.
    Loc = GetNodeLocation(TREE_CHAIN(type), false);

  return DebugFactory.CreateCompositeType(llvm::dwarf::DW_TAG_enumeration_type,
                                          findRegion(TYPE_CONTEXT(type)), 
                                          GetNodeName(type), 
                                          getOrCreateFile(Loc.file), 
                                          Loc.line,
                                          NodeSizeInBits(type), 
                                          NodeAlignInBits(type), 0, 0,
                                          llvm::DIType(), EltArray);
}

/// createStructType - Create StructType for struct or union or class.
DIType DebugInfo::createStructType(tree type) {

  // struct { a; b; ... z; }; | union { a; b; ... z; };
  unsigned Tag = TREE_CODE(type) == RECORD_TYPE ? DW_TAG_structure_type :
    DW_TAG_union_type;
  
  unsigned RunTimeLang = 0;
//TODO  if (TYPE_LANG_SPECIFIC (type)
//TODO      && lang_hooks.types.is_runtime_specific_type (type))
//TODO    {
//TODO      unsigned CULang = TheCU.getLanguage();
//TODO      switch (CULang) {
//TODO      case DW_LANG_ObjC_plus_plus :
//TODO        RunTimeLang = DW_LANG_ObjC_plus_plus;
//TODO        break;
//TODO      case DW_LANG_ObjC :
//TODO        RunTimeLang = DW_LANG_ObjC;
//TODO        break;
//TODO      case DW_LANG_C_plus_plus :
//TODO        RunTimeLang = DW_LANG_C_plus_plus;
//TODO        break;
//TODO      default:
//TODO        break;
//TODO      }
//TODO    }
    
  // Records and classes and unions can all be recursive.  To handle them,
  // we first generate a debug descriptor for the struct as a forward 
  // declaration. Then (if it is a definition) we go through and get debug 
  // info for all of its members.  Finally, we create a descriptor for the
  // complete type (which may refer to the forward decl if the struct is 
  // recursive) and replace all  uses of the forward declaration with the 
  // final definition. 
  expanded_location Loc = GetNodeLocation(TREE_CHAIN(type), false);
  // FIXME: findRegion() is not able to find context all the time. This
  // means when type names in different context match then FwdDecl is
  // reused because MDNodes are uniqued. To avoid this, use type context
  /// also while creating FwdDecl for now.
  std::string FwdName;
  if (TYPE_CONTEXT(type)) {
    StringRef TypeContextName = GetNodeName(TYPE_CONTEXT(type));
    if (!TypeContextName.empty())
      FwdName = TypeContextName;
  }
  StringRef TypeName = GetNodeName(type);
  if (!TypeName.empty())
    FwdName = FwdName + TypeName.data();
  unsigned SFlags = 0;
  DIDescriptor TyContext =  findRegion(TYPE_CONTEXT(type));

  // Check if this type is created while creating context information 
  // descriptor. 
  std::map<tree_node *, WeakVH >::iterator I = TypeCache.find(type);
  if (I != TypeCache.end())
    if (MDNode *TN = dyn_cast_or_null<MDNode>(I->second))
      return DIType(TN);
  
  llvm::DICompositeType FwdDecl =
    DebugFactory.CreateCompositeType(Tag, 
                                     TyContext,
                                     FwdName.c_str(),
                                     getOrCreateFile(Loc.file), 
                                     Loc.line, 
                                     0, 0, 0, SFlags | llvm::DIType::FlagFwdDecl,
                                     llvm::DIType(), llvm::DIArray(),
                                     RunTimeLang);
  
  // forward declaration, 
  if (TYPE_SIZE(type) == 0) 
    return FwdDecl;
  
  // Insert into the TypeCache so that recursive uses will find it.
  llvm::TrackingVH<llvm::MDNode> FwdDeclNode = FwdDecl.getNode();
  TypeCache[type] =  WeakVH(FwdDecl.getNode());

  // Push the struct on region stack.
  RegionStack.push_back(WeakVH(FwdDecl.getNode()));
  RegionMap[type] = WeakVH(FwdDecl.getNode());
  
  // Convert all the elements.
  llvm::SmallVector<llvm::DIDescriptor, 16> EltTys;
  
  if (tree binfo = TYPE_BINFO(type)) {
    VEC(tree,gc) *accesses = BINFO_BASE_ACCESSES (binfo);

    for (unsigned i = 0, e = BINFO_N_BASE_BINFOS(binfo); i != e; ++i) {
      tree BInfo = BINFO_BASE_BINFO(binfo, i);
      tree BInfoType = BINFO_TYPE (BInfo);
      DIType BaseClass = getOrCreateType(BInfoType);
      unsigned BFlags = 0;
      if (BINFO_VIRTUAL_P (BInfo))
        BFlags = llvm::DIType::FlagVirtual;
      if (accesses) {
        tree access = VEC_index (tree, accesses, i);
        if (access == access_protected_node)
          BFlags |= llvm::DIType::FlagProtected;
        else if (access == access_private_node)
          BFlags |= llvm::DIType::FlagPrivate;
      }

      // Check for zero BINFO_OFFSET. 
      // FIXME : Is this correct ?
      unsigned Offset = BINFO_OFFSET(BInfo) ? 
	getINTEGER_CSTVal(BINFO_OFFSET(BInfo))*8 : 0;

      if (BINFO_VIRTUAL_P (BInfo))
        Offset = 0 - getINTEGER_CSTVal(BINFO_VPTR_FIELD (BInfo));
      // FIXME : name, size, align etc...
      DIType DTy = 
        DebugFactory.CreateDerivedType(DW_TAG_inheritance, 
                                       findRegion(TYPE_CONTEXT(type)), StringRef(),
                                       llvm::DIFile(), 0,0,0, 
                                       Offset,
                                       BFlags, BaseClass);
      EltTys.push_back(DTy);
    }
  }
  
  // Now add members of this class.
  for (tree Member = TYPE_FIELDS(type); Member;
       Member = TREE_CHAIN(Member)) {
    // Should we skip.
    if (DECL_P(Member) && DECL_IGNORED_P(Member)) continue;

    // Get the location of the member.
    expanded_location MemLoc = GetNodeLocation(Member, false);

    if (TREE_CODE(Member) != FIELD_DECL)
      // otherwise is a static variable, whose debug info is emitted
      // when through EmitGlobalVariable().
      continue;
      
    if (!OffsetIsLLVMCompatible(Member))
      // FIXME: field with variable or humongous offset.
      // Skip it for now.
      continue;
      
    /* Ignore nameless fields.  */
    if (DECL_NAME (Member) == NULL_TREE
        && !(TREE_CODE (TREE_TYPE (Member)) == UNION_TYPE
             || TREE_CODE (TREE_TYPE (Member)) == RECORD_TYPE))
      continue;
    
    // Field type is the declared type of the field.
    tree FieldNodeType = FieldType(Member);
    DIType MemberType = getOrCreateType(FieldNodeType);
    StringRef MemberName = GetNodeName(Member);
    unsigned MFlags = 0;
    if (TREE_PROTECTED(Member))
      MFlags = llvm::DIType::FlagProtected;
    else if (TREE_PRIVATE(Member))
      MFlags = llvm::DIType::FlagPrivate;
    
    DIType DTy =
      DebugFactory.CreateDerivedType(DW_TAG_member, 
                                     findRegion(DECL_CONTEXT(Member)),
                                     MemberName, 
                                     getOrCreateFile(MemLoc.file),
                                     MemLoc.line, NodeSizeInBits(Member),
                                     NodeAlignInBits(FieldNodeType),
                                     int_bit_position(Member), 
                                     MFlags, MemberType);
    EltTys.push_back(DTy);
  }
  
  for (tree Member = TYPE_METHODS(type); Member;
       Member = TREE_CHAIN(Member)) {
    
    if (DECL_ABSTRACT_ORIGIN (Member)) continue;
    // Ignore unused aritificial members.
    if (DECL_ARTIFICIAL (Member) && !TREE_USED (Member)) continue;
    // In C++, TEMPLATE_DECLs are marked Ignored, and should be.
    if (DECL_P (Member) && DECL_IGNORED_P (Member)) continue;

    std::map<tree_node *, WeakVH >::iterator I = SPCache.find(Member);
    if (I != SPCache.end())
      EltTys.push_back(DISubprogram(cast<MDNode>(I->second)));
    else {
      // Get the location of the member.
      expanded_location MemLoc = GetNodeLocation(Member, false);
      StringRef MemberName = getFunctionName(Member);
      StringRef LinkageName = getLinkageName(Member);
      DIType SPTy = getOrCreateType(TREE_TYPE(Member));
      unsigned Virtuality = 0;
      unsigned VIndex = 0;
      DIType ContainingType;
      if (DECL_VINDEX (Member)) {
        if (host_integerp (DECL_VINDEX (Member), 0))
          VIndex = tree_low_cst (DECL_VINDEX (Member), 0);
        Virtuality = dwarf::DW_VIRTUALITY_virtual;
        ContainingType = getOrCreateType(DECL_CONTEXT(Member));
      }
      DISubprogram SP = 
        DebugFactory.CreateSubprogram(findRegion(DECL_CONTEXT(Member)), 
                                      MemberName, MemberName,
                                      LinkageName, 
                                      getOrCreateFile(MemLoc.file),
                                      MemLoc.line, SPTy, false, false,
                                      Virtuality, VIndex, ContainingType,
                                      DECL_ARTIFICIAL (Member));
      EltTys.push_back(SP);
      SPCache[Member] = WeakVH(SP.getNode());
    }
  }
  
  llvm::DIArray Elements =
    DebugFactory.GetOrCreateArray(EltTys.data(), EltTys.size());

  RegionStack.pop_back();
  std::map<tree_node *, WeakVH>::iterator RI = RegionMap.find(type);
  if (RI != RegionMap.end())
    RegionMap.erase(RI);

  llvm::DIType ContainingType;
  if (TYPE_VFIELD (type)) {
    tree vtype = DECL_FCONTEXT (TYPE_VFIELD (type));
    ContainingType = getOrCreateType(vtype);
  }
  llvm::DICompositeType RealDecl =
    DebugFactory.CreateCompositeType(Tag, findRegion(TYPE_CONTEXT(type)),
                                     GetNodeName(type),
                                     getOrCreateFile(Loc.file),
                                     Loc.line, 
                                     NodeSizeInBits(type), NodeAlignInBits(type),
                                     0, SFlags, llvm::DIType(), Elements,
                                     RunTimeLang, ContainingType.getNode());
  RegionMap[type] = WeakVH(RealDecl.getNode());

  // Now that we have a real decl for the struct, replace anything using the
  // old decl with the new one.  This will recursively update the debug info.
  llvm::DIDerivedType(FwdDeclNode).replaceAllUsesWith(RealDecl);

  return RealDecl;
}

/// createVarinatType - Create variant type or return MainTy.
DIType DebugInfo::createVariantType(tree type, DIType MainTy) {
  
  DIType Ty;
  if (tree TyDef = TYPE_NAME(type)) {
      std::map<tree_node *, WeakVH >::iterator I = TypeCache.find(TyDef);
      if (I != TypeCache.end())
        if (Value *M = I->second)
          return DIType(cast<MDNode>(M));
    if (TREE_CODE(TyDef) == TYPE_DECL &&  DECL_ORIGINAL_TYPE(TyDef)) {
      expanded_location TypeDefLoc = GetNodeLocation(TyDef);
      Ty = DebugFactory.CreateDerivedType(DW_TAG_typedef, 
                                          findRegion(DECL_CONTEXT(TyDef)),
                                          GetNodeName(TyDef), 
                                          getOrCreateFile(TypeDefLoc.file),
                                          TypeDefLoc.line,
                                          0 /*size*/,
                                          0 /*align*/,
                                          0 /*offset */, 
                                          0 /*flags*/, 
                                          MainTy);
      TypeCache[TyDef] = WeakVH(Ty.getNode());
      return Ty;
    }
  }

  if (TYPE_VOLATILE(type)) {
    Ty = DebugFactory.CreateDerivedType(DW_TAG_volatile_type, 
                                        findRegion(TYPE_CONTEXT(type)), 
                                        StringRef(),
                                        getOrCreateFile(main_input_filename),
                                        0 /*line no*/, 
                                        NodeSizeInBits(type),
                                        NodeAlignInBits(type),
                                        0 /*offset */, 
                                        0 /* flags */, 
                                        MainTy);
    MainTy = Ty;
  }

  if (TYPE_READONLY(type)) 
    Ty =  DebugFactory.CreateDerivedType(DW_TAG_const_type, 
                                         findRegion(TYPE_CONTEXT(type)), 
                                         StringRef(),
                                         getOrCreateFile(main_input_filename),
                                         0 /*line no*/, 
                                         NodeSizeInBits(type),
                                         NodeAlignInBits(type),
                                         0 /*offset */, 
                                         0 /* flags */, 
                                         MainTy);
  
  if (TYPE_VOLATILE(type) || TYPE_READONLY(type)) {
    TypeCache[type] = WeakVH(Ty.getNode());
    return Ty;
  }

  // If, for some reason, main type varaint type is seen then use it.
  return MainTy;
}

/// getOrCreateType - Get the type from the cache or create a new type if
/// necessary.
DIType DebugInfo::getOrCreateType(tree type) {
  DEBUGASSERT(type != NULL_TREE && type != error_mark_node &&
              "Not a type.");
  if (type == NULL_TREE || type == error_mark_node) return DIType();

  // Should only be void if a pointer/reference/return type.  Returning NULL
  // allows the caller to produce a non-derived type.
  if (TREE_CODE(type) == VOID_TYPE) return DIType();
  
  // Check to see if the compile unit already has created this type.
  std::map<tree_node *, WeakVH >::iterator I = TypeCache.find(type);
  if (I != TypeCache.end())
    if (Value *M = I->second)
      return DIType(cast<MDNode>(M));

  DIType MainTy;
  if (type != TYPE_MAIN_VARIANT(type) && TYPE_MAIN_VARIANT(type))
    MainTy = getOrCreateType(TYPE_MAIN_VARIANT(type));

  DIType Ty = createVariantType(type, MainTy);
  if (Ty.isValid())
    return Ty;

  // Work out details of type.
  switch (TREE_CODE(type)) {
    case ERROR_MARK:
    case LANG_TYPE:
    case TRANSLATION_UNIT_DECL:
    default: {
      DEBUGASSERT(0 && "Unsupported type");
      return DIType();
    }
    
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      // Do not cache pointer type. The pointer may point to forward declared
      // struct.
      return createPointerType(type);
      break;

    case OFFSET_TYPE: {
      // gen_type_die(TYPE_OFFSET_BASETYPE(type), context_die);
      // gen_type_die(TREE_TYPE(type), context_die);
      // gen_ptr_to_mbr_type_die(type, context_die);
      break;
    }

    case FUNCTION_TYPE:
    case METHOD_TYPE: 
      Ty = createMethodType(type);
      break;
      
    case VECTOR_TYPE:
    case ARRAY_TYPE: 
      Ty = createArrayType(type);
      break;
    
    case ENUMERAL_TYPE: 
      Ty = createEnumType(type);
      break;
    
    case RECORD_TYPE:
    case QUAL_UNION_TYPE:
    case UNION_TYPE: 
      return createStructType(type);
      break;

    case INTEGER_TYPE:
    case REAL_TYPE:   
    case COMPLEX_TYPE:
    case BOOLEAN_TYPE:
      Ty = createBasicType(type);
      break;
  }
  TypeCache[type] = WeakVH(Ty.getNode());
  return Ty;
}

/// Initialize - Initialize debug info by creating compile unit for
/// main_input_filename. This must be invoked after language dependent
/// initialization is done.
void DebugInfo::Initialize() {

  // Each input file is encoded as a separate compile unit in LLVM
  // debugging information output. However, many target specific tool chains
  // prefer to encode only one compile unit in an object file. In this 
  // situation, the LLVM code generator will include  debugging information
  // entities in the compile unit that is marked as main compile unit. The 
  // code generator accepts maximum one main compile unit per module. If a
  // module does not contain any main compile unit then the code generator 
  // will emit multiple compile units in the output object file.
  if (!strcmp (main_input_filename, ""))
    TheCU = getOrCreateCompileUnit("<stdin>", true);
  else
    TheCU = getOrCreateCompileUnit(main_input_filename, true);
}

/// getOrCreateCompileUnit - Get the compile unit from the cache or 
/// create a new one if necessary.
DICompileUnit DebugInfo::getOrCreateCompileUnit(const char *FullPath,
                                                bool isMain) {
  if (!FullPath) {
    if (!strcmp (main_input_filename, ""))
      FullPath = "<stdin>";
    else
      FullPath = main_input_filename;
  }

  // Get source file information.
  std::string Directory;
  std::string FileName;
  DirectoryAndFile(FullPath, Directory, FileName);
  
  // Set up Language number.
  unsigned LangTag;
  const std::string LanguageName(lang_hooks.name);
  if (LanguageName == "GNU C")
    LangTag = DW_LANG_C89;
  else if (LanguageName == "GNU C++")
    LangTag = DW_LANG_C_plus_plus;
  else if (LanguageName == "GNU Ada")
    LangTag = DW_LANG_Ada95;
  else if (LanguageName == "GNU F77")
    LangTag = DW_LANG_Fortran77;
  else if (LanguageName == "GNU Pascal")
    LangTag = DW_LANG_Pascal83;
  else if (LanguageName == "GNU Java")
    LangTag = DW_LANG_Java;
  else if (LanguageName == "GNU Objective-C")
    LangTag = DW_LANG_ObjC;
  else if (LanguageName == "GNU Objective-C++") 
    LangTag = DW_LANG_ObjC_plus_plus;
  else
    LangTag = DW_LANG_C89;

  StringRef Flags;
  
  // flag_objc_abi represents Objective-C runtime version number. It is zero
  // for all other language.
  unsigned ObjcRunTimeVer = 0;
//  if (flag_objc_abi != 0 && flag_objc_abi != -1)
//    ObjcRunTimeVer = flag_objc_abi;
  return DebugFactory.CreateCompileUnit(LangTag, FileName.c_str(),
                                        Directory.c_str(),
                                        version_string, isMain,
                                        optimize, Flags,
                                        ObjcRunTimeVer);
}

/// getOrCreateFile - Get DIFile descriptor.
DIFile DebugInfo::getOrCreateFile(const char *FullPath) {
  if (!FullPath) {
    if (!strcmp (main_input_filename, ""))
      FullPath = "<stdin>";
    else
      FullPath = main_input_filename;
  }

  // Get source file information.
  std::string Directory;
  std::string FileName;
  DirectoryAndFile(FullPath, Directory, FileName);
  return DebugFactory.CreateFile(FileName, Directory, TheCU);
}
