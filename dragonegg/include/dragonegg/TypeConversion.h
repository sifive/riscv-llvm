//=----- TypeConversion.h - Converting and working with types -----*- C++ -*-=//
//
// Copyright (C) 2011 to 2013  Duncan Sands.
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
// This file declares functions for converting GCC types to LLVM types, and for
// working with types.
//===----------------------------------------------------------------------===//

#ifndef DRAGONEGG_TYPES_H
#define DRAGONEGG_TYPES_H

// LLVM headers
#include "llvm/IR/CallingConv.h"

// Forward declarations.
namespace llvm {
class AttributeSet;
class FunctionType;
class LLVMContext;
class Type;
}
union tree_node;

//===----------------------------------------------------------------------===//
//                                 Utilities
//===----------------------------------------------------------------------===//

#define NO_LENGTH (~(uint64_t) 0)

/// ArrayLengthOf - Returns the length of the given gcc array type, or NO_LENGTH
/// if the array has variable or unknown length.
extern uint64_t ArrayLengthOf(tree_node *type);

/// GetFieldIndex - Return the index of the field in the given LLVM type that
/// corresponds to the GCC field declaration 'decl'.  This means that the LLVM
/// and GCC fields start in the same byte (if 'decl' is a bitfield, this means
/// that its first bit is within the byte the LLVM field starts at).  Returns
/// INT_MAX if there is no such LLVM field.
int GetFieldIndex(tree_node *decl, llvm::Type *Ty);

/// GetUnitType - Returns an integer one address unit wide if 'NumUnits' is 1;
/// otherwise returns an array of such integers with 'NumUnits' elements.  For
/// example, on a machine which has 16 bit bytes returns an i16 or an array of
/// i16.
extern llvm::Type *GetUnitType(llvm::LLVMContext &C, unsigned NumUnits = 1);

/// GetUnitPointerType - Returns an LLVM pointer type which points to memory one
/// address unit wide.  For example, on a machine which has 16 bit bytes returns
/// an i16*.
extern llvm::Type *
GetUnitPointerType(llvm::LLVMContext &C, unsigned AddrSpace = 0);

/// isSizeCompatible - Return true if the specified gcc type is guaranteed to be
/// turned by ConvertType into an LLVM type of the same size (i.e. TYPE_SIZE the
/// same as getTypeAllocSizeInBits).
extern bool isSizeCompatible(tree_node *type);

/// getRegType - Returns the LLVM type to use for registers that hold a value
/// of the scalar GCC type 'type'.  All of the EmitReg* routines use this to
/// determine the LLVM type to return.  Note that this only considers the main
/// variant of the type.
extern llvm::Type *getRegType(tree_node *type);

/// getPointerToType - Returns the LLVM register type to use for a pointer to
/// the given GCC type.
extern llvm::Type *getPointerToType(tree_node *type);

/// ConvertType - Returns the LLVM type to use for memory that holds a value
/// of the given GCC type (getRegType should be used for values in registers).
/// Note that the conversion only considers the main variant of the type.
extern llvm::Type *ConvertType(tree_node *type);

/// ConvertFunctionType - Convert the specified FUNCTION_TYPE or METHOD_TYPE
/// tree to an LLVM type.  This does the same thing that ConvertType does, but
/// it also returns the function's LLVM calling convention and attributes.
extern llvm::FunctionType *
ConvertFunctionType(tree_node *type, tree_node *decl, tree_node *static_chain,
                    llvm::CallingConv::ID &CC, llvm::AttributeSet &PAL);

/// ConvertArgListToFnType - Given a DECL_ARGUMENTS list on an GCC tree,
/// return the LLVM type corresponding to the function.  This is useful for
/// turning "T foo(...)" functions into "T foo(void)" functions.
llvm::FunctionType *ConvertArgListToFnType(
    tree_node *type, llvm::ArrayRef<tree_node *> arglist,
    tree_node *static_chain, bool KNRPromotion, llvm::CallingConv::ID &CC,
    llvm::AttributeSet &PAL);

#endif /* DRAGONEGG_TYPES_H */
