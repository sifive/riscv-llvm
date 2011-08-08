/*===----------- cache.h - Caching values "in" GCC trees ----------*- C -*-===*\
|*                                                                            *|
|* Copyright (C) 2009, 2010, 2011  Duncan Sands.                              *|
|*                                                                            *|
|* This file is part of DragonEgg.                                            *|
|*                                                                            *|
|* DragonEgg is free software; you can redistribute it and/or modify it under *|
|* the terms of the GNU General Public License as published by the Free       *|
|* Software Foundation; either version 2, or (at your option) any later       *|
|* version.                                                                   *|
|*                                                                            *|
|* DragonEgg is distributed in the hope that it will be useful, but WITHOUT   *|
|* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or      *|
|* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for  *|
|* more details.                                                              *|
|* You should have received a copy of the GNU General Public License along    *|
|* with DragonEgg; see the file COPYING.  If not, write to the Free Software  *|
|* Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA.     *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|* This code lets you to associate a void* with a tree, as if it were cached  *|
|* inside the tree: if the tree is garbage collected and reallocated, then    *|
|* the cached value will have been cleared.                                   *|
\*===----------------------------------------------------------------------===*/

#ifndef DRAGONEGG_CACHE_H
#define DRAGONEGG_CACHE_H

union tree_node;

/* llvm_get_cached - Returns the value associated with the tree, or NULL.  */
extern const void *llvm_get_cached(union tree_node *tree);

/* llvm_set_cached - Associates the given value with the tree (and returns it).
   To delete an association, pass NULL for the value.  */
extern const void *llvm_set_cached(union tree_node *tree, const void *val);

/* llvm_replace_cached - Replaces all occurrences of old_val with new_val.  */
extern void llvm_replace_cached(const void *old_val, const void *new_val);

#endif /* DRAGONEGG_CACHE_H */
