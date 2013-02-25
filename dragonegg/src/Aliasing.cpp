//=----------- Aliasing.cpp - Type-based alias analysis metadata ----------*-=//
//
// Copyright (C) 2012 to 2013  Duncan Sands.
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
// This file declares routines for generating TBAA metadata from what GCC knows
// about pointer aliasing.
//===----------------------------------------------------------------------===//

// Plugin headers
#include "dragonegg/Aliasing.h"
#include "llvm/ADT/SmallVector.h"

// LLVM headers
#include "llvm/ADT/Twine.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"

// System headers
#include <gmp.h>
#include <map>

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

#include "alias.h"
#ifndef ENABLE_BUILD_WITH_CXX
} // extern "C"
#endif

// Trees header.
#include "dragonegg/Trees.h"

using namespace llvm;

static LLVMContext &Context = getGlobalContext();

/// getTBAARoot - Return the root of the TBAA tree for this compilation unit.
static MDNode *getTBAARoot() {
  static MDNode *Root;
  if (!Root) {
    // Create the root node.  This must be unique to the compilation unit since
    // the names of the nodes we hang off it have no intrinsic meaning: nodes
    // from different compilation units must not be merged even if they have the
    // same name.
    MDBuilder MDHelper(Context);
    Root = MDHelper.createAnonymousTBAARoot();
  }
  return Root;
}

/// describeAliasSet - Return TBAA metadata describing what a load from or store
/// to the given tree may alias.
MDNode *describeAliasSet(tree t) {
  alias_set_type alias_set = get_alias_set(t);
  // Alias set 0 is the root of the alias graph and can alias anything.  A
  // negative value represents an unknown alias set, which as far as we know
  // may also alias anything.
  if (alias_set <= 0)
    return 0;

  // The difficulty here is that GCC's alias sets are the nodes of a directed
  // acyclic graph (DAG) rooted at 0, and in complicated cases it really is a
  // DAG and not a tree.  This is due to record types: the DAG has a node for
  // each record type with, for every field, an edge from the node to the node
  // for the field's type.  As a result the leaves of the DAG are usually scalar
  // types, with an incoming edge from every record type with a field with that
  // scalar type.  On the other hand, LLVM requires TBAA nodes to form a tree.
  // In short we need to come up with a tree and a graph map (i.e. a map that
  // takes nodes to nodes and edges to edges) from GCC's DAG to this tree.  (An
  // alternative is to complicate LLVM so that it too uses DAGs for TBAA).  An
  // additional difficulty is that we don't actually know the edges in the DAG:
  // GCC's alias analysis interface does not expose them.  All that we have is
  // alias_set_subset_of(s, t) which returns true iff there is a path from t to
  // s in the DAG.  Finally, we don't know the nodes of the DAG either!  We only
  // discover them progressively as we convert functions.
  // For the moment we take a very simple approach: we only use the leaf nodes
  // of GCC's DAG.  This means that we do a good job for scalars and a poor job
  // for record types, including complex types.
  static std::map<alias_set_type, MDNode *> NodeTags; // Node -> metadata map.
  static SmallVector<alias_set_type, 8> LeafNodes;    // Current set of leaves.

  std::map<alias_set_type, MDNode *>::iterator I = NodeTags.find(alias_set);
  if (I != NodeTags.end())
    return I->second;

  if (LeafNodes.empty())
    // Check for a GCC special case: a node can have an edge to the root node.
    // This is handled automatically (below) except when there are not yet any
    // known leaf nodes.
    if (alias_set_subset_of(0, alias_set)) {
      NodeTags[alias_set] = 0;
      return 0;
    }

  // If there is a path from this node to any leaf node then it is not a leaf
  // node and can be discarded.
  for (unsigned i = 0, e = (unsigned) LeafNodes.size(); i != e; ++i)
    if (alias_set_subset_of(LeafNodes[i], alias_set)) {
      NodeTags[alias_set] = 0;
      return 0;
    }
  assert(!alias_set_subset_of(0, alias_set) && "'May alias' not transitive?");

  // If there is a path from any leaf node to this one then no longer consider
  // that node to be a leaf.
  for (unsigned i = (unsigned) LeafNodes.size(); i;) {
    alias_set_type leaf_set = LeafNodes[--i];
    if (alias_set_subset_of(alias_set, leaf_set)) {
      LeafNodes.erase(LeafNodes.begin() + i);
      MDNode *&LeafTag = NodeTags[leaf_set];
      // It would be neat to strip the tbaa tag from any instructions using it
      // but it is simpler to just replace it with the root tag everywhere.
      LeafTag->replaceAllUsesWith(getTBAARoot());
      LeafTag = 0;
    }
  }

  // Create metadata describing the new node hanging off root.  The name doesn't
  // matter much but needs to be unique for the compilation unit.
  tree type =
      TYPE_CANONICAL(TYPE_MAIN_VARIANT(isa<TYPE>(t) ? t : TREE_TYPE(t)));
  std::string TreeName =
      ("alias set " + Twine(alias_set) + ": " + getDescriptiveName(type)).str();
  MDBuilder MDHelper(Context);

  MDNode *AliasTag = MDHelper.createTBAANode(TreeName, getTBAARoot());
  NodeTags[alias_set] = AliasTag;
  LeafNodes.push_back(alias_set);
  return AliasTag;
}
