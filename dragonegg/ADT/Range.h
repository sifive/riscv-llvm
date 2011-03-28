//=------------------ Range.h - Interval of values ----------------*- C++ -*-=//
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
// This file declares a utility class for representing an interval of values.
//===----------------------------------------------------------------------===//

#ifndef DRAGONEGG_RANGE_H
#define DRAGONEGG_RANGE_H

/// Range - Represents the interval [First, Last).
template<typename T>
class Range {
  T First, Last;
public:
  Range() : First(0), Last(0) {}
  Range(T first, T last) : First(first), Last(last) {}

  bool operator==(const Range &other) const {
    return (empty() && other.empty()) ||
      (First == other.First && Last == other.Last);
  }

  /// empty - Return whether the range is empty.
  bool empty() const {
    return Last <= First;
  }

  /// getFirst - Return the value defining the start of the range.
  T getFirst() const {
    assert(!empty() && "An empty range has no starting value!");
    return First;
  }

  /// getLast - Return the value defining the end of the range.
  T getLast() const {
    assert(!empty() && "An empty range has no ending value!");
    return Last;
  }

  /// getWidth - Return the number of values in the range.
  T getWidth() const {
    return empty() ? 0 : Last - First;
  }

  /// contains - Return true if the given range is contained in this one.
  bool contains(Range r) const {
    if (r.empty())
      return true;
    if (empty())
      return false;
    return First <= r.First && Last >= r.Last;
  }

  /// intersects - Return true if the given range intersects this one.
  bool intersects(Range r) const {
    if (empty() || r.empty())
      return false;
    return r.First < Last && r.Last > First;
  }

  /// Displace - Return the range obtained by adding the given offset.
  Range Displace(T Offset) const {
    if (empty())
      return Range();
    assert(((Offset >= 0 && First + Offset >= First && Last + Offset >= Last) ||
            (Offset < 0 && First + Offset < First && Last + Offset < Last)) &&
           "Displacement wrapped range!");
    return Range(First + Offset, Last + Offset);
  }

  /// Join - Return the smallest range containing this range and the given one.
  Range Join(Range other) const {
    if (empty())
      return other;
    if (other.empty())
      return *this;
    return Range(First < other.First ? First : other.First,
                 Last > other.Last ? Last : other.Last);
  }

  /// Meet - Return the intersection of this range and the given one.
  Range Meet(Range other) const {
    if (empty() || other.empty())
      return Range();
    return Range(First > other.First ? First : other.First,
                 Last < other.Last ? Last : other.Last);
  }
};

#endif /* DRAGONEGG_RANGE_H */
