//==----------- Cache.h - Caching values "in" GCC trees ----------*- C++ -*-==//
//
// Copyright (C) 2009 to 2013  Duncan Sands.
//
// This file is part of DragonEgg.
//
// DragonEgg is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2, or (at your option) any later
// version.
//
// DragonEgg is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
// You should have received a copy of the GNU General Public License along
// with DragonEgg; see the file COPYING.  If not, write to the Free Software
// Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA.
//
//===----------------------------------------------------------------------===//
// This code lets you associate values with a tree, as if it were cached inside
// the tree: if the tree is garbage collected and reallocated, then the cached
// value will have been cleared.
//===----------------------------------------------------------------------===//

// Plugin headers.
#include "dragonegg/Cache.h"

// LLVM headers
#include "llvm/IR/ValueHandle.h"

// System headers
#include <cassert>
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

#include "ggc.h"
#ifndef ENABLE_BUILD_WITH_CXX
} // extern "C"
#endif

using namespace llvm;

// Hash table mapping trees to integers.

struct GTY(()) tree2int {
  struct tree_map_base base;
  int GTY((skip)) val;
};

#define tree2int_eq tree_map_base_eq
#define tree2int_hash tree_map_base_hash
#define tree2int_marked_p tree_map_base_marked_p

static GTY((if_marked("tree2int_marked_p"), param_is(struct tree2int)))
    htab_t intCache;

// Hash table mapping trees to Type*.

// Forward declare Type for the benefit of gengtype.
#ifndef IN_GCC
struct Type;
#endif
struct GTY(()) tree2Type {
  struct tree_map_base base;
#ifndef IN_GCC
  struct
#endif
      Type *
          GTY((skip)) Ty;
};

#define tree2Type_eq tree_map_base_eq
#define tree2Type_hash tree_map_base_hash
#define tree2Type_marked_p tree_map_base_marked_p

static GTY((if_marked("tree2Type_marked_p"), param_is(struct tree2Type)))
    htab_t TypeCache;

// Hash table mapping trees to WeakVH.

// Forward declare WeakVH for the benefit of gengtype.
#ifndef IN_GCC
struct WeakVH;
#endif
struct GTY(()) tree2WeakVH {
  struct tree_map_base base;
#ifndef IN_GCC
  struct
#endif
      WeakVH
          GTY((skip)) V;
};

#define tree2WeakVH_eq tree_map_base_eq
#define tree2WeakVH_hash tree_map_base_hash
#define tree2WeakVH_marked_p tree_map_base_marked_p

static GTY((if_marked("tree2WeakVH_marked_p"), param_is(struct tree2WeakVH)))
    htab_t WeakVHCache;

// Include the garbage collector header.
#ifndef ENABLE_BUILD_WITH_CXX
extern "C" {
#endif
#if (GCC_MINOR > 5)
#include "dragonegg/gt-cache-4.6.inc"
#else
#include "dragonegg/gt-cache-4.5.inc"
#endif
#ifndef ENABLE_BUILD_WITH_CXX
} // extern "C"
#endif

bool getCachedInteger(tree t, int &Val) {
  if (!intCache)
    return false;
  tree_map_base in = { t };
  tree2int *h = (tree2int *)htab_find(intCache, &in);
  if (!h)
    return false;
  Val = h->val;
  return true;
}

void setCachedInteger(tree t, int Val) {
  if (!intCache)
    intCache = htab_create_ggc(1024, tree2int_hash, tree2int_eq, 0);

  tree_map_base in = { t };
  tree2int **slot = (tree2int **)htab_find_slot(intCache, &in, INSERT);
  assert(slot && "Failed to create hash table slot!");

  if (!*slot) {
    *slot =
#if (GCC_MINOR > 5)
        ggc_alloc_tree2int();
#else
    GGC_NEW(struct tree2int);
#endif
    (*slot)->base.from = t;
  }

  (*slot)->val = Val;
}

Type *getCachedType(tree t) {
  if (!TypeCache)
    return 0;
  tree_map_base in = { t };
  tree2Type *h = (tree2Type *)htab_find(TypeCache, &in);
  return h ? h->Ty : 0;
}

void setCachedType(tree t, Type *Ty) {
  tree_map_base in = { t };

  /* If deleting, remove the slot.  */
  if (!Ty) {
    if (TypeCache)
      htab_remove_elt(TypeCache, &in);
    return;
  }

  if (!TypeCache)
    TypeCache = htab_create_ggc(1024, tree2Type_hash, tree2Type_eq, 0);

  tree2Type **slot = (tree2Type **)htab_find_slot(TypeCache, &in, INSERT);
  assert(slot && "Failed to create hash table slot!");

  if (!*slot) {
    *slot =
#if (GCC_MINOR > 5)
        ggc_alloc_tree2Type();
#else
    GGC_NEW(struct tree2Type);
#endif
    (*slot)->base.from = t;
  }

  (*slot)->Ty = Ty;
}

/// getCachedValue - Returns the value associated with the given GCC tree, or
/// null if none.
Value *getCachedValue(tree t) {
  if (!WeakVHCache)
    return 0;
  tree_map_base in = { t };
  tree2WeakVH *h = (tree2WeakVH *)htab_find(WeakVHCache, &in);
  return h ? h->V : 0;
}

static void DestructWeakVH(void *p) {
  ((WeakVH *)&((tree2WeakVH *)p)->V)->~WeakVH();
}

/// setCachedValue - Associates the given value (which may be null) with the
/// given GCC tree.  The association is removed if tree is garbage collected
/// or the value deleted.
void setCachedValue(tree t, Value *V) {
  tree_map_base in = { t };

  // If deleting, remove the slot.
  if (!V) {
    if (WeakVHCache)
      htab_remove_elt(WeakVHCache, &in);
    return;
  }

  if (!WeakVHCache)
    WeakVHCache =
        htab_create_ggc(1024, tree2WeakVH_hash, tree2WeakVH_eq, DestructWeakVH);

  tree2WeakVH **slot = (tree2WeakVH **)htab_find_slot(WeakVHCache, &in, INSERT);
  assert(slot && "Failed to create hash table slot!");

  if (*slot) {
    (*slot)->V = V;
    return;
  }

  *slot =
#if (GCC_MINOR > 5)
      ggc_alloc_tree2WeakVH();
#else
  GGC_NEW(struct tree2WeakVH);
#endif
  (*slot)->base.from = t;
  WeakVH *W = new (&(*slot)->V) WeakVH(V);
  assert(W == &(*slot)->V && "Pointer was displaced!");
  (void)W;
}
