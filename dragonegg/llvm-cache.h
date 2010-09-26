/* Caching values "in" trees
Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.
Contributed by Chris Lattner (sabre@nondot.org)

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

#ifndef LLVM_CACHE_H
#define LLVM_CACHE_H

union tree_node;

/* llvm_has_cached - Returns whether a value has been associated with the
   tree.  */
extern int llvm_has_cached(union tree_node *tree);

/* llvm_get_cached - Returns the value associated with the tree, or NULL.  */
extern const void *llvm_get_cached(union tree_node *tree);

/* llvm_set_cached - Associates the given value with the tree (and returns it).
   To delete an association, pass NULL for the value.  */
extern const void *llvm_set_cached(union tree_node *tree, const void *val);

/* llvm_replace_cached - Replaces all occurrences of old_val with new_val.  */
extern void llvm_replace_cached(const void *old_val, const void *new_val);

#endif /* LLVM_CACHE_H */
