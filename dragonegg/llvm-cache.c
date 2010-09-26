/* Caching values "in" trees
Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.

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

/*===----------------------------------------------------------------------===
   This code lets you to associate a void* with a tree, as if it were cached
   inside the tree: if the tree is garbage collected and reallocated, then the
   cached value will have been cleared.
  ===----------------------------------------------------------------------===*/

/* Plugin headers.  */
#include "llvm-cache.h"

/* GCC headers.  */
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "ggc.h"

struct GTY(()) tree_llvm_map {
  struct tree_map_base base;
  const void * GTY((skip)) val;
};

#define tree_llvm_map_eq tree_map_base_eq
#define tree_llvm_map_hash tree_map_base_hash
#define tree_llvm_map_marked_p tree_map_base_marked_p

static GTY ((if_marked ("tree_llvm_map_marked_p"),
             param_is(struct tree_llvm_map)))
  htab_t llvm_cache;

/* Garbage collector header.  */
#include "gt-llvm-cache.h"

/* llvm_has_cached - Returns whether a value has been associated with the
   tree.  */
int llvm_has_cached(union tree_node *tree) {
  struct tree_map_base in;

  if (!llvm_cache)
    return false;

  in.from = tree;
  return htab_find(llvm_cache, &in) != NULL;
}

/* llvm_get_cached - Returns the value associated with the tree, or NULL.  */
const void *llvm_get_cached(union tree_node *tree) {
  struct tree_llvm_map *h;
  struct tree_map_base in;

  if (!llvm_cache)
    return NULL;

  in.from = tree;
  h = (struct tree_llvm_map *) htab_find(llvm_cache, &in);
  return h ? h->val : NULL;
}

/* llvm_set_cached - Associates the given value with the tree (and returns it).
   To delete an association, pass a NULL value here.  */
const void *llvm_set_cached(union tree_node *tree, const void *val) {
  struct tree_llvm_map **slot;
  struct tree_map_base in;

  in.from = tree;

  /* If deleting, remove the slot.  */
  if (val == NULL) {
    if (llvm_cache)
      htab_remove_elt(llvm_cache, &in);
    return NULL;
  }

  if (!llvm_cache)
    llvm_cache = htab_create_ggc(1024, tree_llvm_map_hash, tree_llvm_map_eq, NULL);

  slot = (struct tree_llvm_map **) htab_find_slot(llvm_cache, &in, INSERT);
  gcc_assert(slot);

  if (!*slot) {
    *slot = GGC_NEW(struct tree_llvm_map);
    (*slot)->base.from = tree;
  }

  (*slot)->val = val;

  return val;
}

struct update {
  const void *old_val;
  const void *new_val;
};

/* replace - If the current value for the slot matches old_val, then replace
   it with new_val, or delete it if new_val is NULL.  */
static int replace(void **slot, void *data) {
  struct tree_llvm_map *entry = *(struct tree_llvm_map **)slot;
  struct update *u = (struct update *)data;

  if (entry->val != u->old_val)
    return 1;

  if (u->new_val != NULL)
    entry->val = u->new_val;
  else
    htab_clear_slot(llvm_cache, slot);

  return 1;
}

/* llvm_replace_cached - Replaces all occurrences of old_val with new_val.  */
void llvm_replace_cached(const void *old_val, const void *new_val) {
  struct update u;
  u.old_val = old_val;
  u.new_val = new_val;

  if (!llvm_cache || old_val == NULL)
    return;

  htab_traverse(llvm_cache, replace, &u);
}
