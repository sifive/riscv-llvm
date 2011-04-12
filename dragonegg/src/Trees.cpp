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
