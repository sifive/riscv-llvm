//==----------- Cache.h - Caching values "in" GCC trees ----------*- C++ -*-==//
//
// Copyright (C) 2009, 2010, 2011  Duncan Sands.
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
// This code lets you associate a value with a tree, as if it were cached inside
// the tree: if the tree is garbage collected and reallocated, then the cached
// value will have been cleared.
//===----------------------------------------------------------------------===//

#ifndef DRAGONEGG_CACHE_H
#define DRAGONEGG_CACHE_H

// Forward declarations.
namespace llvm {
  class Type;
  class Value;
}
union tree_node;

/// getCachedInteger - Returns true if there is an integer associated with the
/// given GCC tree and puts the integer in 'val'.  Otherwise returns false.
extern bool getCachedInteger(union tree_node *t, int &Val);

/// setCachedInteger - Associates the given integer with the given GCC tree, and
/// returns the integer.
extern void setCachedInteger(union tree_node *t, int Val);

/// getCachedType - Returns the type associated with the given GCC tree, or null
/// if none.
extern llvm::Type *getCachedType(union tree_node *t);

/// setCachedType - Associates the given type (which may be null) with the given
/// GCC tree, and returns the type.
extern void setCachedType(union tree_node *t, llvm::Type *Ty);

/// getCachedValue - Returns the value associated with the given GCC tree, or
/// null if none.
extern llvm::Value *getCachedValue(union tree_node *t);

/// setCachedValue - Associates the given value (which may be null) with the
/// given GCC tree.  The association is removed if tree is garbage collected
/// or the value deleted.
extern void setCachedValue(union tree_node *t, llvm::Value *V);

#endif /* DRAGONEGG_CACHE_H */
