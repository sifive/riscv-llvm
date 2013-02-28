//===------ Trees.cpp - Utility functions for working with GCC trees ------===//
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
// This file defines utility functions for working with GCC trees.
//===----------------------------------------------------------------------===//

// LLVM headers
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/Twine.h"

// System headers
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

#include "flags.h"
#ifndef ENABLE_BUILD_WITH_CXX
} // extern "C"
#endif

// Trees header.
#include "dragonegg/Trees.h"

using namespace llvm;

/// concatIfNotEmpty - Concatenate the given strings if they are both non-empty.
/// Otherwise return the empty string.
static std::string
concatIfNotEmpty(const std::string &Left, const std::string &Right) {
  if (Left.empty() || Right.empty())
    return std::string();
  return Left + Right;
}

/// getAssemblerName - Return the name to use for the given tree, or an empty
/// string if it does not have a name.  This is the official name that should
/// be used for everything that will end up in the final assembler.
std::string getAssemblerName(tree t) {
  tree ident = DECL_ASSEMBLER_NAME(t);
  if (!ident)
    // Does not have a name.
    return std::string();

  // Replace any leading star by '\1'.
  const char *Name = IDENTIFIER_POINTER(ident);
  if (*Name != '*')
    return std::string(Name, IDENTIFIER_LENGTH(ident));

  return "\1" + std::string(Name + 1, IDENTIFIER_LENGTH(ident) - 1);
}

/// getDescriptiveName - Return a helpful name for the given tree, or an empty
/// string if no sensible name was found.  These names are used to make the IR
/// more readable, and have no official status.
std::string getDescriptiveName(const_tree t) {
  if (!t)
    return std::string(); // Occurs when recursing.

  // Name identifier nodes after their contents.  This gives the desired effect
  // when called recursively.
  if (isa<IDENTIFIER_NODE>(t))
    return std::string(IDENTIFIER_POINTER(t), IDENTIFIER_LENGTH(t));

  // Handle declarations of all kinds.
  if (DECL_P(t)) {
    // If the declaration comes with a name then use it.
    if (DECL_NAME(t)) // Always an identifier node.
      return getDescriptiveName(DECL_NAME(t));
    // Use a generic name for function results.
    if (isa<RESULT_DECL>(t))
      return "<retval>";
    // Labels have their own numeric unique identifiers.
    if (isa<LABEL_DECL>(t) && LABEL_DECL_UID(t) != -1) {
      Twine LUID(LABEL_DECL_UID(t));
      return ("L" + LUID).str();
    }
    // Otherwise use the generic UID.
    const char *Annotation = isa<CONST_DECL>(t) ? "C." : "D.";
    Twine UID(DECL_UID(t));
    return (Annotation + UID).str();
  }

  // Handle types of all kinds.
  if (isa<TYPE>(t)) {
    // If the type comes with a name then use it.
    const std::string &TypeName = getDescriptiveName(TYPE_NAME(t));
    if (!TypeName.empty()) {
      // Annotate the name with a description of the type's class.
      if (isa<ENUMERAL_TYPE>(t))
        return "enum." + TypeName;
      if (isa<RECORD_TYPE>(t))
        return "struct." + TypeName;
      if (isa<QUAL_UNION_TYPE>(t))
        return "qualunion." + TypeName;
      if (isa<UNION_TYPE>(t))
        return "union." + TypeName;
      return TypeName;
    }

    // Try to deduce a useful name.
    if (isa<ARRAY_TYPE>(t))
      // If the element type is E, name the array E[] (regardless of the number
      // of dimensions).
      return concatIfNotEmpty(getDescriptiveName(TREE_TYPE(t)), "[]");
    if (isa<COMPLEX_TYPE>(t))
      // If the element type is E, name the complex number complex.E.
      return concatIfNotEmpty("complex.", getDescriptiveName(TREE_TYPE(t)));
    if (isa<POINTER_TYPE>(t))
      // If the element type is E, name the pointer E*.
      return concatIfNotEmpty(getDescriptiveName(TREE_TYPE(t)), "*");
    if (isa<REFERENCE_TYPE>(t))
      // If the element type is E, name the reference E&.
      return concatIfNotEmpty(getDescriptiveName(TREE_TYPE(t)), "&");

    return TypeName;
  }

  // Handle SSA names.
  if (isa<SSA_NAME>(t)) {
    Twine NameVersion(SSA_NAME_VERSION(t));
    return concatIfNotEmpty(getDescriptiveName(SSA_NAME_VAR(t)),
                            ("_" + NameVersion).str());
  }

  // A mysterious tree, just give up.
  return std::string();
}

/// getAPIntValue - Return the specified INTEGER_CST as an APInt.  The default
/// bitwidth used for the result is the precision of the constant's type, aka
/// TYPE_PRECISION.  If a larger bitwidth is specified then the value is sign-
/// or zero-extended to the larger size, following the signedness of the type.
/// If a smaller bitwidth is specified then the value is truncated.  This will
/// however result in an error if truncating changes the numerical value, i.e.
/// the truncated value must sign-/zero-extend to the original.
APInt getAPIntValue(const_tree exp, unsigned Bitwidth) {
  assert(isa<INTEGER_CST>(exp) && "Expected an integer constant!");
  double_int val = tree_to_double_int(exp);
  unsigned DefaultWidth = TYPE_PRECISION(TREE_TYPE(exp));

  APInt DefaultValue;
  if (integerPartWidth == HOST_BITS_PER_WIDE_INT) {
    DefaultValue = APInt(DefaultWidth, /*numWords*/ 2, (integerPart *)&val);
  } else {
    assert(integerPartWidth == 2 * HOST_BITS_PER_WIDE_INT &&
           "Unsupported host integer width!");
    unsigned ShiftAmt = HOST_BITS_PER_WIDE_INT;
    integerPart Part =
        integerPart((unsigned HOST_WIDE_INT) val.low) +
        (integerPart((unsigned HOST_WIDE_INT) val.high) << ShiftAmt);
    DefaultValue = APInt(DefaultWidth, Part);
  }

  if (!Bitwidth || Bitwidth == DefaultWidth)
    return DefaultValue;

  if (Bitwidth > DefaultWidth)
    return TYPE_UNSIGNED(TREE_TYPE(exp)) ? DefaultValue.zext(Bitwidth)
                                         : DefaultValue.sext(Bitwidth);

  assert((TYPE_UNSIGNED(TREE_TYPE(exp)) ||
          DefaultValue.trunc(Bitwidth).sext(DefaultWidth) == DefaultValue) &&
         "Truncating changed signed value!");
  assert((!TYPE_UNSIGNED(TREE_TYPE(exp)) ||
          DefaultValue.trunc(Bitwidth).zext(DefaultWidth) == DefaultValue) &&
         "Truncating changed unsigned value!");
  return DefaultValue.trunc(Bitwidth);
}

/// isInt64 - Return true if t is an INTEGER_CST that fits in a 64 bit integer.
/// If Unsigned is false, returns whether it fits in a int64_t.  If Unsigned is
/// true, returns whether the value is non-negative and fits in a uint64_t.
/// Always returns false for overflowed constants.
bool isInt64(const_tree t, bool Unsigned) {
  if (!t)
    return false;
  if (HOST_BITS_PER_WIDE_INT == 64)
    return host_integerp(t, Unsigned) && !TREE_OVERFLOW(t);
  assert(HOST_BITS_PER_WIDE_INT == 32 &&
         "Only 32- and 64-bit hosts supported!");
  return (isa<INTEGER_CST>(t) && !TREE_OVERFLOW(t)) &&
         ((TYPE_UNSIGNED(TREE_TYPE(t)) == Unsigned) ||
          // If the constant is signed and we want an unsigned result, check
          // that the value is non-negative.  If the constant is unsigned and
          // we want a signed result, check it fits in 63 bits.
          (HOST_WIDE_INT) TREE_INT_CST_HIGH(t) >= 0);
}

/// getInt64 - Extract the value of an INTEGER_CST as a 64 bit integer.  If
/// Unsigned is false, the value must fit in a int64_t.  If Unsigned is true,
/// the value must be non-negative and fit in a uint64_t.  Must not be used on
/// overflowed constants.  These conditions can be checked by calling isInt64.
uint64_t getInt64(const_tree t, bool Unsigned) {
  assert(isInt64(t, Unsigned) && "invalid constant!");
  (void) Unsigned; // Otherwise unused if asserts off - avoid compiler warning.
  unsigned HOST_WIDE_INT LO = (unsigned HOST_WIDE_INT) TREE_INT_CST_LOW(t);
  if (HOST_BITS_PER_WIDE_INT == 64) {
    return (uint64_t) LO;
  } else {
    assert(HOST_BITS_PER_WIDE_INT == 32 &&
           "Only 32- and 64-bit hosts supported!");
    unsigned HOST_WIDE_INT HI = (unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH(t);
    return ((uint64_t) HI << 32) | (uint64_t) LO;
  }
}

/// getFieldOffsetInBits - Return the bit offset of a FIELD_DECL in a structure.
uint64_t getFieldOffsetInBits(const_tree field) {
  assert(OffsetIsLLVMCompatible(field) && "Offset is not constant!");
  uint64_t Result = getInt64(DECL_FIELD_BIT_OFFSET(field), true);
  Result += getInt64(DECL_FIELD_OFFSET(field), true) * BITS_PER_UNIT;
  return Result;
}

/// getFieldAlignment - Return (in octets) the alignment within a structure of
/// the octet containing the first bit of the given FIELD_DECL.
unsigned getFieldAlignment(const_tree field) {
  // DECL_OFFSET_ALIGN is the maximum power-of-two that is known to divide
  // DECL_FIELD_OFFSET*BITS_PER_UNIT.  The field itself is offset a further
  // DECL_FIELD_BIT_OFFSET bits.
  assert(DECL_OFFSET_ALIGN(field) > 0 && (DECL_OFFSET_ALIGN(field) & 7) == 0 &&
         "Field has no or wrong alignment!");
  uint64_t FieldBitOffset = getInt64(DECL_FIELD_BIT_OFFSET(field), true);
  return MinAlign(DECL_OFFSET_ALIGN(field) / 8, FieldBitOffset / 8);
}

/// isBitfield - Returns whether to treat the specified field as a bitfield.
bool isBitfield(const_tree field_decl) {
  if (!DECL_BIT_FIELD(field_decl))
    return false;

  // A bitfield.  But do we need to treat it as one?

  assert(DECL_FIELD_BIT_OFFSET(field_decl) && "Bitfield with no bit offset!");
  if (TREE_INT_CST_LOW(DECL_FIELD_BIT_OFFSET(field_decl)) & 7)
    // Does not start on a byte boundary - must treat as a bitfield.
    return true;

  if (!isInt64(TYPE_SIZE(TREE_TYPE(field_decl)), true))
    // No size or variable sized - play safe, treat as a bitfield.
    return true;

  uint64_t TypeSizeInBits = getInt64(TYPE_SIZE(TREE_TYPE(field_decl)), true);
  assert(!(TypeSizeInBits & 7) && "A type with a non-byte size!");

  assert(DECL_SIZE(field_decl) && "Bitfield with no bit size!");
  uint64_t FieldSizeInBits = getInt64(DECL_SIZE(field_decl), true);
  if (FieldSizeInBits < TypeSizeInBits)
    // Not wide enough to hold the entire type - treat as a bitfield.
    return true;

  return false;
}
