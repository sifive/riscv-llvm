//===------ Trees.cpp - Utility functions for working with GCC trees ------===//
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
// This file defines utility functions for working with GCC trees.
//===----------------------------------------------------------------------===//

// Plugin headers
#include "dragonegg/Trees.h"

// LLVM headers
#include "llvm/ADT/Twine.h"

// System headers
#include <gmp.h>

// GCC headers
extern "C" {
#include "config.h"
// Stop GCC declaring 'getopt' as it can clash with the system's declaration.
#undef HAVE_DECL_GETOPT
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"

#include "flags.h"
}

using namespace llvm;

/// concatIfNotEmpty - Concatenate the given strings if they are both non-empty.
/// Otherwise return the empty string.
static std::string concatIfNotEmpty(const std::string &Left,
                                    const std::string &Right) {
  if (Left.empty() || Right.empty())
    return std::string();
  return Left + Right;
}

/// getDescriptiveName - Return a helpful name for the given tree, or an empty
/// string if no sensible name was found.  These names are used to make the IR
/// more readable, and have no official status.
std::string getDescriptiveName(tree t) {
  if (!t) return std::string(); // Occurs when recursing.

  // Name identifier nodes after their contents.  This gives the desired effect
  // when called recursively.
  if (TREE_CODE(t) == IDENTIFIER_NODE)
    return std::string(IDENTIFIER_POINTER(t), IDENTIFIER_LENGTH(t));

  // Handle declarations of all kinds.
  if (DECL_P(t)) {
    // If the declaration comes with a name then use it.
    if (DECL_NAME(t)) // Always an identifier node.
      return std::string(IDENTIFIER_POINTER(DECL_NAME(t)),
                         IDENTIFIER_LENGTH(DECL_NAME(t)));
    // Use a generic name for function results.
    if (TREE_CODE(t) == RESULT_DECL)
      return "<retval>";
    // Labels have their own numeric unique identifiers.
    if (TREE_CODE(t) == LABEL_DECL && LABEL_DECL_UID(t) != -1) {
      Twine LUID(LABEL_DECL_UID(t));
      return ("L" + LUID).str();
    }
    // Otherwise use the generic UID.
    const char *Annotation = TREE_CODE(t) == CONST_DECL ? "C." : "D.";
    Twine UID(DECL_UID(t));
    return (Annotation + UID).str();
  }

  // Handle types of all kinds.
  if (TYPE_P(t)) {
    // If the type comes with a name then use it.
    const std::string &TypeName = getDescriptiveName(TYPE_NAME(t));
    if (!TypeName.empty()) {
      // Annotate the name with a description of the type's class.
      if (TREE_CODE(t) == ENUMERAL_TYPE)
        return "enum." + TypeName;
      if (TREE_CODE(t) == RECORD_TYPE)
        return "struct." + TypeName;
      if (TREE_CODE(t) == QUAL_UNION_TYPE)
        return "qualunion." + TypeName;
      if (TREE_CODE(t) == UNION_TYPE)
        return "union." + TypeName;
      return TypeName;
    }

    // Try to deduce a useful name.
    if (TREE_CODE(t) == ARRAY_TYPE)
      // If the element type is E, name the array E[] (regardless of the number
      // of dimensions).
      return concatIfNotEmpty(getDescriptiveName(TREE_TYPE(t)), "[]");
    if (TREE_CODE(t) == COMPLEX_TYPE)
      // If the element type is E, name the complex number complex.E.
      return concatIfNotEmpty("complex.", getDescriptiveName(TREE_TYPE(t)));
    if (TREE_CODE(t) == POINTER_TYPE)
      // If the element type is E, name the pointer E*.
      return concatIfNotEmpty(getDescriptiveName(TREE_TYPE(t)), "*");
    if (TREE_CODE(t) == REFERENCE_TYPE)
      // If the element type is E, name the reference E&.
      return concatIfNotEmpty(getDescriptiveName(TREE_TYPE(t)), "&");

    return TypeName;
  }

  // Handle SSA names.
  if (TREE_CODE(t) == SSA_NAME) {
    Twine NameVersion(SSA_NAME_VERSION(t));
    return concatIfNotEmpty(getDescriptiveName(SSA_NAME_VAR(t)),
                            ("_" + NameVersion).str());
  }

  // A mysterious tree, just give up.
  return std::string();
}

/// hasNUW - Return whether overflowing unsigned operations on this type result
/// in undefined behaviour.
bool hasNUW(tree type) {
  return TYPE_UNSIGNED(type) && !TYPE_OVERFLOW_WRAPS(type);
}

/// hasNSW - Return whether overflowing signed operations on this type result
/// in undefined behaviour.
bool hasNSW(tree type) {
  return !TYPE_UNSIGNED(type) && !TYPE_OVERFLOW_WRAPS(type);
}

/// getIntegerValue - Return the specified INTEGER_CST as an APInt.
APInt getIntegerValue(tree exp) {
  double_int val = tree_to_double_int(exp);
  unsigned NumBits = TYPE_PRECISION(TREE_TYPE(exp));

  if (integerPartWidth == HOST_BITS_PER_WIDE_INT)
    return APInt(NumBits, /*numWords*/2, (integerPart*)&val);
  assert(integerPartWidth == 2 * HOST_BITS_PER_WIDE_INT &&
         "Unsupported host integer width!");
  unsigned ShiftAmt = HOST_BITS_PER_WIDE_INT;
  integerPart Part = integerPart((unsigned HOST_WIDE_INT)val.low) +
    (integerPart((unsigned HOST_WIDE_INT)val.high) << ShiftAmt);
  return APInt(NumBits, Part);
}

/// isInt64 - Return true if t is an INTEGER_CST that fits in a 64 bit integer.
/// If Unsigned is false, returns whether it fits in a int64_t.  If Unsigned is
/// true, returns whether the value is non-negative and fits in a uint64_t.
/// Always returns false for overflowed constants.
bool isInt64(tree t, bool Unsigned) {
  if (!t)
    return false;
  if (HOST_BITS_PER_WIDE_INT == 64)
    return host_integerp(t, Unsigned) && !TREE_OVERFLOW (t);
  assert(HOST_BITS_PER_WIDE_INT == 32 &&
         "Only 32- and 64-bit hosts supported!");
  return
    (TREE_CODE (t) == INTEGER_CST && !TREE_OVERFLOW (t))
    && ((TYPE_UNSIGNED(TREE_TYPE(t)) == Unsigned) ||
        // If the constant is signed and we want an unsigned result, check
        // that the value is non-negative.  If the constant is unsigned and
        // we want a signed result, check it fits in 63 bits.
        (HOST_WIDE_INT)TREE_INT_CST_HIGH(t) >= 0);
}

/// getInt64 - Extract the value of an INTEGER_CST as a 64 bit integer.  If
/// Unsigned is false, the value must fit in a int64_t.  If Unsigned is true,
/// the value must be non-negative and fit in a uint64_t.  Must not be used on
/// overflowed constants.  These conditions can be checked by calling isInt64.
uint64_t getInt64(tree t, bool Unsigned) {
  assert(isInt64(t, Unsigned) && "invalid constant!");
  (void)Unsigned; // Otherwise unused if asserts off - avoid compiler warning.
  unsigned HOST_WIDE_INT LO = (unsigned HOST_WIDE_INT)TREE_INT_CST_LOW(t);
  if (HOST_BITS_PER_WIDE_INT == 64) {
    return (uint64_t)LO;
  } else {
    assert(HOST_BITS_PER_WIDE_INT == 32 &&
           "Only 32- and 64-bit hosts supported!");
    unsigned HOST_WIDE_INT HI = (unsigned HOST_WIDE_INT)TREE_INT_CST_HIGH(t);
    return ((uint64_t)HI << 32) | (uint64_t)LO;
  }
}
