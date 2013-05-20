//=---- Trees.h - Utility functions for working with GCC trees ----*- C++ -*-=//
//
// Copyright (C) 2010 to 2013  Duncan Sands.
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

#if (GCC_MINOR < 7)
#include "flags.h" // For TYPE_OVERFLOW_UNDEFINED.
#endif

// IMPORTANT: ONLY INCLUDE GCC HEADERS IN THIS FILE!  ONLY INCLUDE THIS HEADER
// AMONGST THE STANDARD GCC HEADERS!

// This is the only dragonegg header that is allowed to include GCC headers.
// As GCC headers poison standard C library routines like malloc, including
// one before an LLVM or system header may break the build.  This is why GCC
// headers (and this one, as it may include GCC headers) are always included
// last.

// The following properties must hold if dragonegg is to work correctly.
#if ((BITS_PER_UNIT & 7) != 0)
#error BITS_PER_UNIT must be a multiple of 8
#endif

/// dragonegg_tree_code - Fake helper tree codes.
enum dragonegg_tree_code {
  ACCESS_TYPE,          // A pointer or reference type.
  AGGREGATE_TYPE,       // A record, union, qualified union or array type.
  FLOAT_TYPE,           // A scalar, complex or vector floating point type.
  INTEGRAL_TYPE,        // A enumeral, boolean or integer type.
  RECORD_OR_UNION_TYPE, // A record, union or qualified union type.
  TYPE                  // Any type.
};

/// isa - Return true if the given tree has the specified code.
template <enum tree_code code> bool isa(const_tree t) {
  return TREE_CODE(t) == code;
}
template <enum dragonegg_tree_code code> bool isa(const_tree t) {
  switch (code) {
  case ACCESS_TYPE:
    return POINTER_TYPE_P(t);
  case AGGREGATE_TYPE:
    return AGGREGATE_TYPE_P(t);
  case FLOAT_TYPE:
    return FLOAT_TYPE_P(t);
  case INTEGRAL_TYPE:
    return INTEGRAL_TYPE_P(t);
  case RECORD_OR_UNION_TYPE:
    return RECORD_OR_UNION_TYPE_P(t);
  case TYPE:
    return TYPE_P(t);
  }
}

/// getAssemblerName - Return the name to use for the given tree, or an empty
/// string if it does not have a name.  This is the official name that should
/// be used for everything that will end up with a name in the final assembler.
/// It should not be used for anything else: GCC will usually crash if you try
/// to use this with types, function arguments or anything else that doesn't
/// have a name in the final assembler.
std::string getAssemblerName(tree t);

/// getDescriptiveName - Return a helpful name for the given tree, or an empty
/// string if no sensible name was found.  These names are used to make the IR
/// more readable, and have no official status.  For example, they can be used
/// to name types because type names don't end up in the final assembler.
std::string getDescriptiveName(const_tree t);

/// main_type - Return the main variant of the given tree's type.
inline tree main_type(tree exp) { return TYPE_MAIN_VARIANT(TREE_TYPE(exp)); }
inline const_tree main_type(const_tree exp) {
  return TYPE_MAIN_VARIANT(TREE_TYPE(exp));
}

/// hasNUW - Return whether overflowing unsigned operations on this type result
/// in undefined behaviour.
inline bool hasNUW(const_tree type) {
  return TYPE_UNSIGNED(type) && TYPE_OVERFLOW_UNDEFINED(type);
}

/// hasNSW - Return whether overflowing signed operations on this type result
/// in undefined behaviour.
inline bool hasNSW(const_tree type) {
  return !TYPE_UNSIGNED(type) && TYPE_OVERFLOW_UNDEFINED(type);
}

/// getAPIntValue - Return the specified INTEGER_CST as an APInt.  The default
/// bitwidth used for the result is the precision of the constant's type, aka
/// TYPE_PRECISION.  If a larger bitwidth is specified then the value is sign-
/// or zero-extended to the larger size, following the signedness of the type.
/// If a smaller bitwidth is specified then the value is truncated.  This will
/// however result in an error if truncating changes the numerical value, i.e.
/// the truncated value must sign-/zero-extend to the original.
llvm::APInt getAPIntValue(const_tree exp, unsigned Bitwidth = 0);

/// isInt64 - Return true if t is an INTEGER_CST that fits in a 64 bit integer.
/// If Unsigned is false, returns whether it fits in a int64_t.  If Unsigned is
/// true, returns whether the value is non-negative and fits in a uint64_t.
/// Always returns false for overflowed constants or if t is NULL.
bool isInt64(const_tree t, bool Unsigned);

/// getInt64 - Extract the value of an INTEGER_CST as a 64 bit integer.  If
/// Unsigned is false, the value must fit in a int64_t.  If Unsigned is true,
/// the value must be non-negative and fit in a uint64_t.  Must not be used on
/// overflowed constants.  These conditions can be checked by calling isInt64.
uint64_t getInt64(const_tree t, bool Unsigned);

/// OffsetIsLLVMCompatible - Return true if the given field is offset from the
/// start of the record by a constant amount which is not humongously big.
inline bool OffsetIsLLVMCompatible(const_tree field_decl) {
  return isInt64(DECL_FIELD_OFFSET(field_decl), true);
}

/// getFieldOffsetInBits - Return the bit offset of a FIELD_DECL in a structure.
uint64_t getFieldOffsetInBits(const_tree field);

/// getFieldAlignment - Return (in octets) the alignment within a structure of
/// the octet containing the first bit of the given FIELD_DECL.
unsigned getFieldAlignment(const_tree field);

/// isBitfield - Returns whether to treat the specified field as a bitfield.
bool isBitfield(const_tree field_decl);

// Compatibility hacks for older versions of GCC.
#if (GCC_MINOR < 8)
// Supported allocation types:
struct va_gc {
}; // Allocation uses ggc_alloc.

// Fake vector class specialized below.
template <typename T, typename A> class vec {
};

#define INSTANTIATE_VECTOR(TT)                                                 \
  template <> class vec<TT, va_gc> {                                           \
    VEC(TT, gc) & v;                                                           \
  public:                                                                      \
    vec(VEC(TT, gc) & V) : v(V) {}                                             \
                                                                               \
    bool is_empty() const { return VEC_empty(TT, &v); }                        \
    unsigned length() const { return VEC_length(TT, &v); }                     \
    TT &operator[](unsigned i) const { return *VEC_index(TT, &v, i); }         \
    bool iterate(unsigned ix, TT **ptr) const {                                \
      return VEC_iterate(TT, &v, ix, *ptr);                                    \
    }                                                                          \
  }
#endif

#endif /* DRAGONEGG_TREES_H */
