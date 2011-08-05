//=---- Trees.h - Utility functions for working with GCC trees ----*- C++ -*-=//
//
// Copyright (C) 2010, 2011  Duncan Sands.
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
// This file declares utility functions for working with GCC trees.
//===----------------------------------------------------------------------===//

#ifndef DRAGONEGG_TREES_H
#define DRAGONEGG_TREES_H

// LLVM headers
#include "llvm/ADT/APInt.h"

// System headers
#include <string>

union tree_node;

/// getDescriptiveName - Return a helpful name for the given tree, or an empty
/// string if no sensible name was found.  These names are used to make the IR
/// more readable, and have no official status.
std::string getDescriptiveName(union tree_node *t);

/// hasNUW - Return whether overflowing unsigned operations on this type result
/// in undefined behaviour.
bool hasNUW(tree_node *type);

/// hasNSW - Return whether overflowing signed operations on this type result
/// in undefined behaviour.
bool hasNSW(tree_node *type);

/// getIntegerValue - Return the specified INTEGER_CST as an APInt.
llvm::APInt getIntegerValue(tree_node *exp);

/// isInt64 - Return true if t is an INTEGER_CST that fits in a 64 bit integer.
/// If Unsigned is false, returns whether it fits in a int64_t.  If Unsigned is
/// true, returns whether the value is non-negative and fits in a uint64_t.
/// Always returns false for overflowed constants or if t is NULL.
bool isInt64(tree_node *t, bool Unsigned);

/// getInt64 - Extract the value of an INTEGER_CST as a 64 bit integer.  If
/// Unsigned is false, the value must fit in a int64_t.  If Unsigned is true,
/// the value must be non-negative and fit in a uint64_t.  Must not be used on
/// overflowed constants.  These conditions can be checked by calling isInt64.
uint64_t getInt64(tree_node *t, bool Unsigned);

/// OffsetIsLLVMCompatible - Return true if the given field is offset from the
/// start of the record by a constant amount which is not humongously big.
extern bool OffsetIsLLVMCompatible(tree_node *field_decl);

/// getFieldOffsetInBits - Return the bit offset of a FIELD_DECL in a structure.
extern uint64_t getFieldOffsetInBits(tree_node *field);

/// isBitfield - Returns whether to treat the specified field as a bitfield.
bool isBitfield(tree_node *field_decl);

#endif /* DRAGONEGG_TREES_H */
