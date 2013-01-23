//=------- Aliasing.h - Type-based alias analysis metadata --------*- C++ -*-=//
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

#ifndef DRAGONEGG_ALIASING_H
#define DRAGONEGG_ALIASING_H

// Forward declarations.
namespace llvm { class MDNode; }
union tree_node;

/// describeAliasSet - Return TBAA metadata describing what a load from or store
/// to the given tree may alias.
extern llvm::MDNode *describeAliasSet(tree_node *t);

#endif /* DRAGONEGG_ALIASING_H */
