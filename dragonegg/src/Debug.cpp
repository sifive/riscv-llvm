//===-------------- Debug.cpp - Debug information gathering ---------------===//
//
// Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011  Jim Laskey,
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
// This file implements debug information gathering.
//===----------------------------------------------------------------------===//

// Plugin headers
#include "dragonegg/Debug.h"
#include "dragonegg/Trees.h"

// LLVM headers
#include "llvm/Module.h"
#include "llvm/ADT/STLExtras.h"

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

#include "flags.h"
#include "langhooks.h"
#include "toplev.h"
#include "version.h"
}

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
    else if (isInt64(TYPE_SIZE(Node), true))
      return getInt64(TYPE_SIZE(Node), true);
    else
      return TYPE_ALIGN(Node);
  } else if (DECL_P(Node)) {
    if (DECL_SIZE(Node) == NULL_TREE)
      return 0;
    else if (isInt64(DECL_SIZE(Node), 1))
      return getInt64(DECL_SIZE(Node), 1);
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
  return DECL_BIT_FIELD_TYPE(Field) ?
    DECL_BIT_FIELD_TYPE(Field) : TREE_TYPE (Field);
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
  expanded_location Location = { NULL, 0, 0, false };

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

/// EmitFunctionStart - Constructs the debug code for entering a function.
void DebugInfo::EmitFunctionStart(tree FnDecl, Function *Fn) {
  DIType FNType = getOrCreateType(TREE_TYPE(FnDecl));

  std::map<tree_node *, WeakVH >::iterator I = SPCache.find(FnDecl);
  if (I != SPCache.end()) {
    DISubprogram SPDecl(cast<MDNode>(I->second));
    DISubprogram SP =
      DebugFactory.CreateSubprogramDefinition(SPDecl);
    SPDecl->replaceAllUsesWith(SP);

    // Push function on region stack.
    RegionStack.push_back(WeakVH(SP));
    RegionMap[FnDecl] = WeakVH(SP);
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
    SPDecl->replaceAllUsesWith(SP);

    // Push function on region stack.
    RegionStack.push_back(WeakVH(SP));
    RegionMap[FnDecl] = WeakVH(SP);
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
                                  Virtuality, VIndex, ContainingType,
                                  DECL_ARTIFICIAL (FnDecl), optimize);

  SPCache[FnDecl] = WeakVH(SP);

  // Push function on region stack.
  RegionStack.push_back(WeakVH(SP));
  RegionMap[FnDecl] = WeakVH(SP);
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

  NameSpaceCache[Node] = WeakVH(DNS);
  return DNS;
}

/// findRegion - Find tree_node N's region.
DIDescriptor DebugInfo::findRegion(tree Node) {
  if (Node == NULL_TREE)
    return getOrCreateFile(main_input_filename);

  std::map<tree_node *, WeakVH>::iterator I = RegionMap.find(Node);
  if (I != RegionMap.end())
    if (MDNode *R = dyn_cast_or_null<MDNode>(&*I->second))
      return DIDescriptor(R);

  if (TYPE_P (Node)) {
    DIType Ty = getOrCreateType(Node);
    return DIDescriptor(Ty);
  } else if (DECL_P (Node)) {
    if (TREE_CODE (Node) == NAMESPACE_DECL) {
      DIDescriptor NSContext = findRegion(DECL_CONTEXT(Node));
      DINameSpace NS = getOrCreateNameSpace(Node, NSContext);
      return DIDescriptor(NS);
    }
    return findRegion (DECL_CONTEXT (Node));
  }

  // Otherwise main compile unit covers everything.
  return getOrCreateFile(main_input_filename);
}

/// EmitFunctionEnd - Pop the region stack and reset current lexical block.
void DebugInfo::EmitFunctionEnd(bool EndFunction) {
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
void DebugInfo::EmitDeclare(tree decl, unsigned Tag, const char *Name,
                            tree type, Value *AI, LLVMBuilder &Builder) {

  // Ignore compiler generated temporaries.
  if (DECL_IGNORED_P(decl))
    return;

  assert(!RegionStack.empty() && "Region stack mismatch, stack empty!");

  expanded_location Loc = GetNodeLocation(decl, false);

  // Construct variable.
  DIScope VarScope = DIScope(cast<MDNode>(RegionStack.back()));
  DIType Ty = getOrCreateType(type);
  if (!Ty && TREE_CODE(type) == OFFSET_TYPE)
    Ty = createPointerType(TREE_TYPE(type));
  if (Ty && DECL_ARTIFICIAL (decl))
      Ty = DebugFactory.CreateArtificialType(Ty);
  // If type info is not available then do not emit debug info for this var.
  if (!Ty)
    return;
  llvm::DIVariable D =
    DebugFactory.CreateVariable(Tag, VarScope,
                                Name, getOrCreateFile(Loc.file),
                                Loc.line, Ty, optimize);

  Instruction *Call =
    DebugFactory.InsertDeclare(AI, D, Builder.GetInsertBlock());

  Call->setDebugLoc(DebugLoc::get(Loc.line, 0, VarScope));
}

/// EmitStopPoint - Set current source location. 
void DebugInfo::EmitStopPoint(BasicBlock *CurBB, LLVMBuilder &Builder) {
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
  if (DECL_ARTIFICIAL(decl) || DECL_IGNORED_P(decl))
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
  llvm::DIType FwdType = DebugFactory.CreateTemporaryType();
  llvm::MDNode *FTN = FwdType;
  llvm::TrackingVH<llvm::MDNode> FwdTypeNode = FTN;
  TypeCache[type] = WeakVH(FwdType);
  // Push the struct on region stack.
  RegionStack.push_back(WeakVH(FwdType));
  RegionMap[type] = WeakVH(FwdType);

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
  llvm::DIType(FwdTypeNode).replaceAllUsesWith(RealType);

  return RealType;
}

/// createPointerType - Create PointerType.
DIType DebugInfo::createPointerType(tree type) {

  DIType FromTy = getOrCreateType(TREE_TYPE(type));
  // type* and type&
  // FIXME: Should BLOCK_POINTER_TYP have its own DW_TAG?
  unsigned Tag = TREE_CODE(type) == REFERENCE_TYPE ?
    DW_TAG_reference_type: DW_TAG_pointer_type;
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
      TypeCache[TyName] = WeakVH(Ty);
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

  // Add the dimensions of the array.  FIXME: This loses CV qualifiers from
  // interior arrays, do we care?  Why aren't nested arrays represented the
  // obvious/recursive way?
  llvm::SmallVector<llvm::DIDescriptor, 8> Subscripts;

  // There will be ARRAY_TYPE nodes for each rank.  Followed by the derived
  // type.
  tree EltTy = TREE_TYPE(type);
  if (TREE_CODE(type) == ARRAY_TYPE) {
    tree atype = type;
    for (; TREE_CODE(atype) == ARRAY_TYPE; atype = TREE_TYPE(atype)) {
      tree Domain = TYPE_DOMAIN(atype);
      if (Domain) {
        // FIXME - handle dynamic ranges
        tree MinValue = TYPE_MIN_VALUE(Domain);
        tree MaxValue = TYPE_MAX_VALUE(Domain);
        int64_t Low = 0;
        int64_t Hi = 0;
        if (isInt64(MinValue, false))
          Low = getInt64(MinValue, false);
        if (isInt64(MaxValue, false))
          Hi = getInt64(MaxValue, false);
        Subscripts.push_back(DebugFactory.GetOrCreateSubrange(Low, Hi));
      }
      EltTy = TREE_TYPE(atype);
    }
  } else {
    assert(TREE_CODE(type) == VECTOR_TYPE && "Not an array or vector type!");
    unsigned Length = TYPE_VECTOR_SUBPARTS(type);
    Subscripts.push_back(DebugFactory.GetOrCreateSubrange(0, Length));
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
      int64_t Value = getInt64(EnumValue, false);
      const char *EnumName = IDENTIFIER_POINTER(TREE_PURPOSE(Link));
      Elements.push_back(DebugFactory.CreateEnumerator(EnumName, Value));
    }
  }

  llvm::DIArray EltArray =
    DebugFactory.GetOrCreateArray(Elements.data(), Elements.size());

  expanded_location Loc = { NULL, 0, 0, false };
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
  unsigned SFlags = 0;
  DIDescriptor TyContext =  findRegion(TYPE_CONTEXT(type));

  // Check if this type is created while creating context information
  // descriptor.
  std::map<tree_node *, WeakVH >::iterator I = TypeCache.find(type);
  if (I != TypeCache.end())
    if (MDNode *TN = dyn_cast_or_null<MDNode>(&*I->second))
      return DIType(TN);

  // forward declaration,
  if (TYPE_SIZE(type) == 0) {
    llvm::DICompositeType FwdDecl =
      DebugFactory.CreateCompositeType(Tag,
                                       TyContext,
                                       GetNodeName(type),
                                       getOrCreateFile(Loc.file),
                                       Loc.line,
                                       0, 0, 0,
                                       SFlags | llvm::DIType::FlagFwdDecl,
                                       llvm::DIType(), llvm::DIArray(),
                                       RunTimeLang);
    return FwdDecl;
  }

  llvm::DIType FwdDecl = DebugFactory.CreateTemporaryType();

  // Insert into the TypeCache so that recursive uses will find it.
  llvm::MDNode *FDN = FwdDecl;
  llvm::TrackingVH<llvm::MDNode> FwdDeclNode = FDN;
  TypeCache[type] = WeakVH(FwdDecl);

  // Push the struct on region stack.
  RegionStack.push_back(WeakVH(FwdDecl));
  RegionMap[type] = WeakVH(FwdDecl);

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
        getInt64(BINFO_OFFSET(BInfo), true)*8 : 0;

      if (BINFO_VIRTUAL_P (BInfo))
        Offset = 0 - getInt64(BINFO_VPTR_FIELD (BInfo), false);
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
                                      DECL_ARTIFICIAL (Member), optimize);
      EltTys.push_back(SP);
      SPCache[Member] = WeakVH(SP);
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
                                     RunTimeLang, ContainingType);
  RegionMap[type] = WeakVH(RealDecl);

  // Now that we have a real decl for the struct, replace anything using the
  // old decl with the new one.  This will recursively update the debug info.
  llvm::DIType(FwdDeclNode).replaceAllUsesWith(RealDecl);

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
      TypeCache[TyDef] = WeakVH(Ty);
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
    TypeCache[type] = WeakVH(Ty);
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
      // PR 7104
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
  TypeCache[type] = WeakVH(Ty);
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

//===----------------------------------------------------------------------===//
// DIFactory: Basic Helpers
//===----------------------------------------------------------------------===//

DIFactory::DIFactory(Module &m)
  : M(m), VMContext(M.getContext()), DeclareFn(0), ValueFn(0) {}

Constant *DIFactory::GetTagConstant(unsigned TAG) {
  assert((TAG & LLVMDebugVersionMask) == 0 &&
         "Tag too large for debug encoding!");
  return ConstantInt::get(Type::getInt32Ty(VMContext), TAG | LLVMDebugVersion);
}

//===----------------------------------------------------------------------===//
// DIFactory: Primary Constructors
//===----------------------------------------------------------------------===//

/// GetOrCreateArray - Create an descriptor for an array of descriptors.
/// This implicitly uniques the arrays created.
DIArray DIFactory::GetOrCreateArray(DIDescriptor *Tys, unsigned NumTys) {
  if (NumTys == 0) {
    Value *Null = llvm::Constant::getNullValue(Type::getInt32Ty(VMContext));
    return DIArray(MDNode::get(VMContext, Null));
  }

  SmallVector<Value *, 16> Elts(Tys, Tys+NumTys);
  return DIArray(MDNode::get(VMContext, Elts));
}

/// GetOrCreateSubrange - Create a descriptor for a value range.  This
/// implicitly uniques the values returned.
DISubrange DIFactory::GetOrCreateSubrange(int64_t Lo, int64_t Hi) {
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_subrange_type),
    ConstantInt::get(Type::getInt64Ty(VMContext), Lo),
    ConstantInt::get(Type::getInt64Ty(VMContext), Hi)
  };

  return DISubrange(MDNode::get(VMContext, Elts));
}

/// CreateUnspecifiedParameter - Create unspeicified type descriptor
/// for the subroutine type.
DIDescriptor DIFactory::CreateUnspecifiedParameter() {
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_unspecified_parameters)
  };
  return DIDescriptor(MDNode::get(VMContext, Elts));
}

/// CreateCompileUnit - Create a new descriptor for the specified compile
/// unit.  Note that this does not unique compile units within the module.
DICompileUnit DIFactory::CreateCompileUnit(unsigned LangID,
                                           StringRef Filename,
                                           StringRef Directory,
                                           StringRef Producer,
                                           bool isMain,
                                           bool isOptimized,
                                           StringRef Flags,
                                           unsigned RunTimeVer) {
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_compile_unit),
    llvm::Constant::getNullValue(Type::getInt32Ty(VMContext)),
    ConstantInt::get(Type::getInt32Ty(VMContext), LangID),
    MDString::get(VMContext, Filename),
    MDString::get(VMContext, Directory),
    MDString::get(VMContext, Producer),
    ConstantInt::get(Type::getInt1Ty(VMContext), isMain),
    ConstantInt::get(Type::getInt1Ty(VMContext), isOptimized),
    MDString::get(VMContext, Flags),
    ConstantInt::get(Type::getInt32Ty(VMContext), RunTimeVer)
  };

  return DICompileUnit(MDNode::get(VMContext, Elts));
}

/// CreateFile -  Create a new descriptor for the specified file.
DIFile DIFactory::CreateFile(StringRef Filename,
                             StringRef Directory,
                             DICompileUnit CU) {
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_file_type),
    MDString::get(VMContext, Filename),
    MDString::get(VMContext, Directory),
    CU
  };

  return DIFile(MDNode::get(VMContext, Elts));
}

/// CreateEnumerator - Create a single enumerator value.
DIEnumerator DIFactory::CreateEnumerator(StringRef Name, uint64_t Val){
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_enumerator),
    MDString::get(VMContext, Name),
    ConstantInt::get(Type::getInt64Ty(VMContext), Val)
  };
  return DIEnumerator(MDNode::get(VMContext, Elts));
}


/// CreateBasicType - Create a basic type like int, float, etc.
DIBasicType DIFactory::CreateBasicType(DIDescriptor Context,
                                       StringRef Name,
                                       DIFile F,
                                       unsigned LineNumber,
                                       uint64_t SizeInBits,
                                       uint64_t AlignInBits,
                                       uint64_t OffsetInBits, unsigned Flags,
                                       unsigned Encoding) {
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_base_type),
    Context,
    MDString::get(VMContext, Name),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNumber),
    ConstantInt::get(Type::getInt64Ty(VMContext), SizeInBits),
    ConstantInt::get(Type::getInt64Ty(VMContext), AlignInBits),
    ConstantInt::get(Type::getInt64Ty(VMContext), OffsetInBits),
    ConstantInt::get(Type::getInt32Ty(VMContext), Flags),
    ConstantInt::get(Type::getInt32Ty(VMContext), Encoding)
  };
  return DIBasicType(MDNode::get(VMContext, Elts));
}


/// CreateBasicType - Create a basic type like int, float, etc.
DIBasicType DIFactory::CreateBasicTypeEx(DIDescriptor Context,
                                         StringRef Name,
                                         DIFile F,
                                         unsigned LineNumber,
                                         Constant *SizeInBits,
                                         Constant *AlignInBits,
                                         Constant *OffsetInBits, unsigned Flags,
                                         unsigned Encoding) {
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_base_type),
    Context,
    MDString::get(VMContext, Name),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNumber),
    SizeInBits,
    AlignInBits,
    OffsetInBits,
    ConstantInt::get(Type::getInt32Ty(VMContext), Flags),
    ConstantInt::get(Type::getInt32Ty(VMContext), Encoding)
  };
  return DIBasicType(MDNode::get(VMContext, Elts));
}

/// CreateArtificialType - Create a new DIType with "artificial" flag set.
DIType DIFactory::CreateArtificialType(DIType Ty) {
  if (Ty.isArtificial())
    return Ty;

  SmallVector<Value *, 9> Elts;
  MDNode *N = Ty;
  assert (N && "Unexpected input DIType!");
  for (unsigned i = 0, e = N->getNumOperands(); i != e; ++i) {
    if (Value *V = N->getOperand(i))
      Elts.push_back(V);
    else
      Elts.push_back(Constant::getNullValue(Type::getInt32Ty(VMContext)));
  }

  unsigned CurFlags = Ty.getFlags();
  CurFlags = CurFlags | DIType::FlagArtificial;

  // Flags are stored at this slot.
  Elts[8] =  ConstantInt::get(Type::getInt32Ty(VMContext), CurFlags);

  return DIType(MDNode::get(VMContext, Elts));
}

/// CreateDerivedType - Create a derived type like const qualified type,
/// pointer, typedef, etc.
DIDerivedType DIFactory::CreateDerivedType(unsigned Tag,
                                           DIDescriptor Context,
                                           StringRef Name,
                                           DIFile F,
                                           unsigned LineNumber,
                                           uint64_t SizeInBits,
                                           uint64_t AlignInBits,
                                           uint64_t OffsetInBits,
                                           unsigned Flags,
                                           DIType DerivedFrom) {
  Value *Elts[] = {
    GetTagConstant(Tag),
    Context,
    MDString::get(VMContext, Name),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNumber),
    ConstantInt::get(Type::getInt64Ty(VMContext), SizeInBits),
    ConstantInt::get(Type::getInt64Ty(VMContext), AlignInBits),
    ConstantInt::get(Type::getInt64Ty(VMContext), OffsetInBits),
    ConstantInt::get(Type::getInt32Ty(VMContext), Flags),
    DerivedFrom,
  };
  return DIDerivedType(MDNode::get(VMContext, Elts));
}


/// CreateDerivedType - Create a derived type like const qualified type,
/// pointer, typedef, etc.
DIDerivedType DIFactory::CreateDerivedTypeEx(unsigned Tag,
                                             DIDescriptor Context,
                                             StringRef Name,
                                             DIFile F,
                                             unsigned LineNumber,
                                             Constant *SizeInBits,
                                             Constant *AlignInBits,
                                             Constant *OffsetInBits,
                                             unsigned Flags,
                                             DIType DerivedFrom) {
  Value *Elts[] = {
    GetTagConstant(Tag),
    Context,
    MDString::get(VMContext, Name),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNumber),
    SizeInBits,
    AlignInBits,
    OffsetInBits,
    ConstantInt::get(Type::getInt32Ty(VMContext), Flags),
    DerivedFrom,
  };
  return DIDerivedType(MDNode::get(VMContext, Elts));
}


/// CreateCompositeType - Create a composite type like array, struct, etc.
DICompositeType DIFactory::CreateCompositeType(unsigned Tag,
                                               DIDescriptor Context,
                                               StringRef Name,
                                               DIFile F,
                                               unsigned LineNumber,
                                               uint64_t SizeInBits,
                                               uint64_t AlignInBits,
                                               uint64_t OffsetInBits,
                                               unsigned Flags,
                                               DIType DerivedFrom,
                                               DIArray Elements,
                                               unsigned RuntimeLang,
                                               MDNode *ContainingType) {

  Value *Elts[] = {
    GetTagConstant(Tag),
    Context,
    MDString::get(VMContext, Name),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNumber),
    ConstantInt::get(Type::getInt64Ty(VMContext), SizeInBits),
    ConstantInt::get(Type::getInt64Ty(VMContext), AlignInBits),
    ConstantInt::get(Type::getInt64Ty(VMContext), OffsetInBits),
    ConstantInt::get(Type::getInt32Ty(VMContext), Flags),
    DerivedFrom,
    Elements,
    ConstantInt::get(Type::getInt32Ty(VMContext), RuntimeLang),
    ContainingType
  };

  MDNode *Node = MDNode::get(VMContext, Elts);
  // Create a named metadata so that we do not lose this enum info.
  if (Tag == dwarf::DW_TAG_enumeration_type) {
    NamedMDNode *NMD = M.getOrInsertNamedMetadata("llvm.dbg.enum");
    NMD->addOperand(Node);
  }
  return DICompositeType(Node);
}

/// CreateTemporaryType - Create a temporary forward-declared type.
DIType DIFactory::CreateTemporaryType() {
  // Give the temporary MDNode a tag. It doesn't matter what tag we
  // use here as long as DIType accepts it.
  Value *Elts[] = {
    GetTagConstant(DW_TAG_base_type)
  };
  MDNode *Node = MDNode::getTemporary(VMContext, Elts);
  return DIType(Node);
}

/// CreateTemporaryType - Create a temporary forward-declared type.
DIType DIFactory::CreateTemporaryType(DIFile F) {
  // Give the temporary MDNode a tag. It doesn't matter what tag we
  // use here as long as DIType accepts it.
  Value *Elts[] = {
    GetTagConstant(DW_TAG_base_type),
    F.getCompileUnit(),
    NULL,
    F
  };
  MDNode *Node = MDNode::getTemporary(VMContext, Elts);
  return DIType(Node);
}

/// CreateCompositeType - Create a composite type like array, struct, etc.
DICompositeType DIFactory::CreateCompositeTypeEx(unsigned Tag,
                                                 DIDescriptor Context,
                                                 StringRef Name,
                                                 DIFile F,
                                                 unsigned LineNumber,
                                                 Constant *SizeInBits,
                                                 Constant *AlignInBits,
                                                 Constant *OffsetInBits,
                                                 unsigned Flags,
                                                 DIType DerivedFrom,
                                                 DIArray Elements,
                                                 unsigned RuntimeLang,
                                                 MDNode *ContainingType) {
  Value *Elts[] = {
    GetTagConstant(Tag),
    Context,
    MDString::get(VMContext, Name),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNumber),
    SizeInBits,
    AlignInBits,
    OffsetInBits,
    ConstantInt::get(Type::getInt32Ty(VMContext), Flags),
    DerivedFrom,
    Elements,
    ConstantInt::get(Type::getInt32Ty(VMContext), RuntimeLang),
    ContainingType
  };
  MDNode *Node = MDNode::get(VMContext, Elts);
  // Create a named metadata so that we do not lose this enum info.
  if (Tag == dwarf::DW_TAG_enumeration_type) {
    NamedMDNode *NMD = M.getOrInsertNamedMetadata("llvm.dbg.enum");
    NMD->addOperand(Node);
  }
  return DICompositeType(Node);
}


/// CreateSubprogram - Create a new descriptor for the specified subprogram.
/// See comments in DISubprogram for descriptions of these fields.  This
/// method does not unique the generated descriptors.
DISubprogram DIFactory::CreateSubprogram(DIDescriptor Context,
                                         StringRef Name,
                                         StringRef DisplayName,
                                         StringRef LinkageName,
                                         DIFile F,
                                         unsigned LineNo, DIType Ty,
                                         bool isLocalToUnit,
                                         bool isDefinition,
                                         unsigned VK, unsigned VIndex,
                                         DIType ContainingType,
                                         unsigned Flags,
                                         bool isOptimized,
                                         Function *Fn) {

  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_subprogram),
    llvm::Constant::getNullValue(Type::getInt32Ty(VMContext)),
    Context,
    MDString::get(VMContext, Name),
    MDString::get(VMContext, DisplayName),
    MDString::get(VMContext, LinkageName),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNo),
    Ty,
    ConstantInt::get(Type::getInt1Ty(VMContext), isLocalToUnit),
    ConstantInt::get(Type::getInt1Ty(VMContext), isDefinition),
    ConstantInt::get(Type::getInt32Ty(VMContext), (unsigned)VK),
    ConstantInt::get(Type::getInt32Ty(VMContext), VIndex),
    ContainingType,
    ConstantInt::get(Type::getInt32Ty(VMContext), Flags),
    ConstantInt::get(Type::getInt1Ty(VMContext), isOptimized),
    Fn
  };
  MDNode *Node = MDNode::get(VMContext, Elts);

  // Create a named metadata so that we do not lose this mdnode.
  NamedMDNode *NMD = M.getOrInsertNamedMetadata("llvm.dbg.sp");
  NMD->addOperand(Node);
  return DISubprogram(Node);
}

/// CreateSubprogramDefinition - Create new subprogram descriptor for the
/// given declaration.
DISubprogram DIFactory::CreateSubprogramDefinition(DISubprogram &SPDeclaration){
  if (SPDeclaration.isDefinition())
    return DISubprogram(SPDeclaration);

  MDNode *DeclNode = SPDeclaration;
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_subprogram),
    llvm::Constant::getNullValue(Type::getInt32Ty(VMContext)),
    DeclNode->getOperand(2), // Context
    DeclNode->getOperand(3), // Name
    DeclNode->getOperand(4), // DisplayName
    DeclNode->getOperand(5), // LinkageName
    DeclNode->getOperand(6), // CompileUnit
    DeclNode->getOperand(7), // LineNo
    DeclNode->getOperand(8), // Type
    DeclNode->getOperand(9), // isLocalToUnit
    ConstantInt::get(Type::getInt1Ty(VMContext), true),
    DeclNode->getOperand(11), // Virtuality
    DeclNode->getOperand(12), // VIndex
    DeclNode->getOperand(13), // Containting Type
    DeclNode->getOperand(14), // Flags
    DeclNode->getOperand(15), // isOptimized
    SPDeclaration.getFunction()
  };
  MDNode *Node =MDNode::get(VMContext, Elts);

  // Create a named metadata so that we do not lose this mdnode.
  NamedMDNode *NMD = M.getOrInsertNamedMetadata("llvm.dbg.sp");
  NMD->addOperand(Node);
  return DISubprogram(Node);
}

/// CreateGlobalVariable - Create a new descriptor for the specified global.
DIGlobalVariable
DIFactory::CreateGlobalVariable(DIDescriptor Context, StringRef Name,
                                StringRef DisplayName,
                                StringRef LinkageName,
                                DIFile F,
                                unsigned LineNo, DIType Ty,bool isLocalToUnit,
                                bool isDefinition, llvm::GlobalVariable *Val) {
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_variable),
    llvm::Constant::getNullValue(Type::getInt32Ty(VMContext)),
    Context,
    MDString::get(VMContext, Name),
    MDString::get(VMContext, DisplayName),
    MDString::get(VMContext, LinkageName),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNo),
    Ty,
    ConstantInt::get(Type::getInt1Ty(VMContext), isLocalToUnit),
    ConstantInt::get(Type::getInt1Ty(VMContext), isDefinition),
    Val
  };

  MDNode *Node = MDNode::get(VMContext, Elts);

  // Create a named metadata so that we do not lose this mdnode.
  NamedMDNode *NMD = M.getOrInsertNamedMetadata("llvm.dbg.gv");
  NMD->addOperand(Node);

  return DIGlobalVariable(Node);
}

/// CreateGlobalVariable - Create a new descriptor for the specified constant.
DIGlobalVariable
DIFactory::CreateGlobalVariable(DIDescriptor Context, StringRef Name,
                                StringRef DisplayName,
                                StringRef LinkageName,
                                DIFile F,
                                unsigned LineNo, DIType Ty,bool isLocalToUnit,
                                bool isDefinition, llvm::Constant *Val) {
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_variable),
    llvm::Constant::getNullValue(Type::getInt32Ty(VMContext)),
    Context,
    MDString::get(VMContext, Name),
    MDString::get(VMContext, DisplayName),
    MDString::get(VMContext, LinkageName),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNo),
    Ty,
    ConstantInt::get(Type::getInt1Ty(VMContext), isLocalToUnit),
    ConstantInt::get(Type::getInt1Ty(VMContext), isDefinition),
    Val
  };

  MDNode *Node = MDNode::get(VMContext, Elts);

  // Create a named metadata so that we do not lose this mdnode.
  NamedMDNode *NMD = M.getOrInsertNamedMetadata("llvm.dbg.gv");
  NMD->addOperand(Node);

  return DIGlobalVariable(Node);
}

/// CreateVariable - Create a new descriptor for the specified variable.
DIVariable DIFactory::CreateVariable(unsigned Tag, DIDescriptor Context,
                                     StringRef Name,
                                     DIFile F,
                                     unsigned LineNo,
                                     DIType Ty, bool AlwaysPreserve,
                                     unsigned Flags) {
  Value *Elts[] = {
    GetTagConstant(Tag),
    Context,
    MDString::get(VMContext, Name),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNo),
    Ty,
    ConstantInt::get(Type::getInt32Ty(VMContext), Flags)
  };
  MDNode *Node = MDNode::get(VMContext, Elts);
  if (AlwaysPreserve) {
    // The optimizer may remove local variable. If there is an interest
    // to preserve variable info in such situation then stash it in a
    // named mdnode.
    DISubprogram Fn(getDISubprogram(Context));
    StringRef FName = "fn";
    if (Fn.getFunction())
      FName = Fn.getFunction()->getName();
    char One = '\1';
    if (FName.startswith(StringRef(&One, 1)))
      FName = FName.substr(1);


    NamedMDNode *FnLocals = getOrInsertFnSpecificMDNode(M, FName);
    FnLocals->addOperand(Node);
  }
  return DIVariable(Node);
}


/// CreateComplexVariable - Create a new descriptor for the specified variable
/// which has a complex address expression for its address.
DIVariable DIFactory::CreateComplexVariable(unsigned Tag, DIDescriptor Context,
                                            StringRef Name, DIFile F,
                                            unsigned LineNo,
                                            DIType Ty, Value *const *Addr,
                                            unsigned NumAddr) {
  SmallVector<Value *, 15> Elts;
  Elts.push_back(GetTagConstant(Tag));
  Elts.push_back(Context);
  Elts.push_back(MDString::get(VMContext, Name));
  Elts.push_back(F);
  Elts.push_back(ConstantInt::get(Type::getInt32Ty(VMContext), LineNo));
  Elts.push_back(Ty);
  Elts.append(Addr, Addr+NumAddr);

  return DIVariable(MDNode::get(VMContext, Elts));
}


/// CreateBlock - This creates a descriptor for a lexical block with the
/// specified parent VMContext.
DILexicalBlock DIFactory::CreateLexicalBlock(DIDescriptor Context,
                                             DIFile F, unsigned LineNo,
                                             unsigned Col) {
  // Defeat MDNode uniqing for lexical blocks.
  static unsigned int unique_id = 0;
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_lexical_block),
    Context,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNo),
    ConstantInt::get(Type::getInt32Ty(VMContext), Col),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), unique_id++)
  };
  return DILexicalBlock(MDNode::get(VMContext, Elts));
}

/// CreateNameSpace - This creates new descriptor for a namespace
/// with the specified parent context.
DINameSpace DIFactory::CreateNameSpace(DIDescriptor Context, StringRef Name,
                                       DIFile F,
                                       unsigned LineNo) {
  Value *Elts[] = {
    GetTagConstant(dwarf::DW_TAG_namespace),
    Context,
    MDString::get(VMContext, Name),
    F,
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNo)
  };
  return DINameSpace(MDNode::get(VMContext, Elts));
}

/// CreateLocation - Creates a debug info location.
DILocation DIFactory::CreateLocation(unsigned LineNo, unsigned ColumnNo,
                                     DIScope S, DILocation OrigLoc) {
  Value *Elts[] = {
    ConstantInt::get(Type::getInt32Ty(VMContext), LineNo),
    ConstantInt::get(Type::getInt32Ty(VMContext), ColumnNo),
    S,
    OrigLoc,
  };
  return DILocation(MDNode::get(VMContext, Elts));
}

//===----------------------------------------------------------------------===//
// DIFactory: Routines for inserting code into a function
//===----------------------------------------------------------------------===//

/// InsertDeclare - Insert a new llvm.dbg.declare intrinsic call.
Instruction *DIFactory::InsertDeclare(Value *Storage, DIVariable D,
                                      Instruction *InsertBefore) {
  assert(Storage && "no storage passed to dbg.declare");
  assert(D.Verify() && "empty DIVariable passed to dbg.declare");
  if (!DeclareFn)
    DeclareFn = Intrinsic::getDeclaration(&M, Intrinsic::dbg_declare);

  Value *Args[] = { MDNode::get(Storage->getContext(), Storage), 
                    D };
  return CallInst::Create(DeclareFn, Args, Args+2, "", InsertBefore);
}

/// InsertDeclare - Insert a new llvm.dbg.declare intrinsic call.
Instruction *DIFactory::InsertDeclare(Value *Storage, DIVariable D,
                                      BasicBlock *InsertAtEnd) {
  assert(Storage && "no storage passed to dbg.declare");
  assert(D.Verify() && "invalid DIVariable passed to dbg.declare");
  if (!DeclareFn)
    DeclareFn = Intrinsic::getDeclaration(&M, Intrinsic::dbg_declare);

  Value *Args[] = { MDNode::get(Storage->getContext(), Storage),
                    D };

  // If this block already has a terminator then insert this intrinsic
  // before the terminator.
  if (TerminatorInst *T = InsertAtEnd->getTerminator())
    return CallInst::Create(DeclareFn, Args, Args+2, "", T);
  else
    return CallInst::Create(DeclareFn, Args, Args+2, "", InsertAtEnd);}

/// InsertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.
Instruction *DIFactory::InsertDbgValueIntrinsic(Value *V, uint64_t Offset,
                                                DIVariable D,
                                                Instruction *InsertBefore) {
  assert(V && "no value passed to dbg.value");
  assert(D.Verify() && "invalid DIVariable passed to dbg.value");
  if (!ValueFn)
    ValueFn = Intrinsic::getDeclaration(&M, Intrinsic::dbg_value);

  Value *Args[] = { MDNode::get(V->getContext(), V),
                    ConstantInt::get(Type::getInt64Ty(V->getContext()), Offset),
                    D };
  return CallInst::Create(ValueFn, Args, Args+3, "", InsertBefore);
}

/// InsertDbgValueIntrinsic - Insert a new llvm.dbg.value intrinsic call.
Instruction *DIFactory::InsertDbgValueIntrinsic(Value *V, uint64_t Offset,
                                                DIVariable D,
                                                BasicBlock *InsertAtEnd) {
  assert(V && "no value passed to dbg.value");
  assert(D.Verify() && "invalid DIVariable passed to dbg.value");
  if (!ValueFn)
    ValueFn = Intrinsic::getDeclaration(&M, Intrinsic::dbg_value);

  Value *Args[] = { MDNode::get(V->getContext(), V),
                    ConstantInt::get(Type::getInt64Ty(V->getContext()), Offset),
                    D };
  return CallInst::Create(ValueFn, Args, Args+3, "", InsertAtEnd);
}

// RecordType - Record DIType in a module such that it is not lost even if
// it is not referenced through debug info anchors.
void DIFactory::RecordType(DIType T) {
  NamedMDNode *NMD = M.getOrInsertNamedMetadata("llvm.dbg.ty");
  NMD->addOperand(T);
}
