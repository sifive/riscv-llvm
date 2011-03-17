//=----- Constants.h - Converting and working with constants --*- C++ -*-----=//
//
// Copyright (C) 2011  Duncan Sands.
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
// This file declares functions for converting GCC constants to LLVM and working
// with them.
//===----------------------------------------------------------------------===//

#ifndef DRAGONEGG_CONSTANTS_H
#define DRAGONEGG_CONSTANTS_H

// Forward declarations.
namespace llvm {
  class Constant;
  class Type;
}
union tree_node;

/// AddressOf - Given an expression with a constant address such as a constant,
/// a global variable or a label, returns the address.  The type of the returned
/// is always a pointer type and, as long as 'exp' does not have void type, the
/// type of the pointee is the memory type that corresponds to the type of exp
/// (see ConvertType).
extern llvm::Constant *AddressOf(tree_node *exp);

/// ConvertInitializer - Convert the initial value for a global variable to an
/// equivalent LLVM constant.  Also handles constant constructors.  The type of
/// the returned value may be pretty much anything.  All that is guaranteed is
/// that it has the same alloc size as the original expression and has alignment
/// equal to or less than that of the original expression.
extern llvm::Constant *ConvertInitializer(tree_node *exp);

/// InterpretAsType - Interpret the bits of the given constant (starting from
/// StartingBit) as representing a constant of type 'Ty'.  This results in the
/// same constant as you would get by storing the bits of 'C' to memory (with
/// the first bit stored being 'StartingBit') and then loading out a (constant)
/// value of type 'Ty' from the stored to memory location.
extern llvm::Constant *InterpretAsType(llvm::Constant *C, const llvm::Type* Ty,
                                       unsigned StartingBit);

#endif /* DRAGONEGG_CONSTANTS_H */
