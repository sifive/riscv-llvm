//=--------- IntervalList.h - List of disjoint intervals ----------*- C++ -*-=//
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
// This file declares a utility class for maintaining a collection of pairwise
// disjoint intervals.
//===----------------------------------------------------------------------===//

#ifndef DRAGONEGG_INTERVALLIST_H
#define DRAGONEGG_INTERVALLIST_H

// Plugin headers
#include "dragonegg/ADT/Range.h"

// LLVM headers
#include "llvm/ADT/SmallVector.h"

/// IntervalList - Maintains a list of disjoint intervals.  Type 'T' represents
/// an interval, and should have a getRange method which returns a range of 'U'
/// values.  In addition it should provide ChangeRangeTo for growing, shrinking
/// and otherwise changing the shape of the interval; and JoinWith for replacing
/// the interval with the convex hull of its union with another interval (which
/// is guaranteed to be disjoint from the original).
template <class T, typename U, unsigned N> class IntervalList {
  typedef typename llvm::SmallVector<T, N> List;
  typedef typename List::iterator iterator;

  List Intervals;
  // The actual intervals.  Always disjoint, sorted and non-empty.

  /// CmpFirst - Compare intervals based on where they start.
  static bool CmpFirst(const T &L, const T &R) {
    return L.getRange().getFirst() < R.getRange().getFirst();
  }

  /// CmpLast - Compare intervals based on where they stop.
  static bool CmpLast(const T &L, const T &R) {
    return L.getRange().getLast() < R.getRange().getLast();
  }

  /// isSane - Return true if the intervals are non-empty, disjoint and
  /// sorted.
  bool isSane() const {
    if (Intervals.size() != (unsigned) Intervals.size())
      return false; // Too many intervals.
    for (unsigned i = 0, e = (unsigned) Intervals.size(); i < e; ++i) {
      if (Intervals[i].getRange().empty())
        return false;
      if (i && Intervals[i].getRange().getFirst() <
               Intervals[i - 1].getRange().getLast())
        return false;
    }
    return true;
  }

public:

  /// AddInterval - Add the given interval to the list.  If it overlaps any
  /// existing intervals then the existing intervals are pruned by removing
  /// exactly the parts of them that overlap the new interval.  If the added
  /// interval is empty then it will be discarded.
  void AddInterval(const T &S);

  /// getNumIntervals - Return the number of intervals in the list.
  unsigned getNumIntervals() const { return (unsigned) Intervals.size(); }

  /// getInterval - Return the interval with the given index.
  T getInterval(unsigned Idx) { return Intervals[Idx]; }

  /// AlignBoundaries - Ensure that all intervals begin and end on a multiple of
  /// the given value.
  void AlignBoundaries(unsigned Alignment);
};

/// AddInterval - Add the given interval to the list.  If it overlaps any
/// existing intervals then the existing intervals are pruned by removing
/// exactly the parts of them that overlap the new interval.  If the added
/// interval is empty then it will be discarded.
template <class T, typename U, unsigned N>
void IntervalList<T, U, N>::AddInterval(const T &Interval) {
  const Range<U> NewRange = Interval.getRange();

  // If the new interval is empty then there is no point in adding it.
  if (NewRange.empty())
    return;

  // If this is the first interval then it cannot overlap any others.
  if (Intervals.empty()) {
    Intervals.push_back(Interval);
    return;
  }

  // Check for overlap with existing intervals.
  iterator Lo =
      std::lower_bound(Intervals.begin(), Intervals.end(), Interval, CmpFirst);
  iterator Hi =
      std::upper_bound(Intervals.begin(), Intervals.end(), Interval, CmpLast);
  if (Lo < Hi) {
    // Intervals with index in [Lo, Hi) are those completely covered by the new
    // interval.  Throw them away.
    for (iterator I = Lo; I != Hi; ++I)
      assert(NewRange.contains(I->getRange()) && "Old interval not covered!");
    Intervals.erase(Lo, Hi);
    Hi = Lo;
  } else if (Hi < Lo) {
    // The new interval is contained in Hi with an excedent at each end.  Chop
    // the old interval into two pieces (the lower and upper parts) and insert
    // the new interval between them.
    const Range<U> OldRange = Hi->getRange();
    assert(OldRange.contains(NewRange) && "New interval not contained in old!");
    const Range<U> LowerRange(OldRange.getFirst(), NewRange.getFirst());
    const Range<U> UpperRange(NewRange.getLast(), OldRange.getLast());
    assert(!LowerRange.empty() && !UpperRange.empty() && "Degenerate end!");
    T UpperPart = *Hi;
    Hi->ChangeRangeTo(LowerRange);
    UpperPart.ChangeRangeTo(UpperRange);
    Lo = Intervals.insert(Lo, UpperPart);
    Intervals.insert(Lo, Interval);
    assert(isSane() && "Interval added wrong!");
    return;
  }
  assert(Lo == Hi);
  // Check for overlap with the preceding interval.
  if (Lo != Intervals.begin()) {
    const iterator Prev = Lo - 1;
    const Range<U> PrevRange = Prev->getRange();
    if (NewRange.getFirst() < PrevRange.getLast())
      // Shrink the previous interval to remove the overlap.
      Prev->ChangeRangeTo(Range<U>(PrevRange.getFirst(), NewRange.getFirst()));
  }
  // Check for overlap with the following interval.
  if (Lo != Intervals.end()) {
    const iterator Next = Lo;
    const Range<U> NextRange = Next->getRange();
    if (NextRange.getFirst() < NewRange.getLast())
      // Shrink the next interval to remove the overlap.
      Next->ChangeRangeTo(Range<U>(NewRange.getLast(), NextRange.getLast()));
  }
  // The new interval is now disjoint from any existing intervals.  Insert it.
  Intervals.insert(Lo, Interval);
  assert(isSane() && "Interval added wrong!");
}

/// AlignBoundaries - Ensure that all intervals begin and end on a multiple of
/// the given value.
template <class T, typename U, unsigned N>
void IntervalList<T, U, N>::AlignBoundaries(unsigned Alignment) {
  assert(Alignment > 0 && "Alignment should be positive!");
  for (iterator SI = Intervals.begin(); SI != Intervals.end(); ++SI) {
    T &Interval = *SI;
    Range<U> OrigRange = Interval.getRange();

    // Round the start of the interval down and the end of the interval up to
    // the nearest multiple of the alignment.
    U RoundedFirst = OrigRange.getFirst() - (OrigRange.getFirst() % Alignment);
    U RoundedLast = OrigRange.getLast() + Alignment - 1;
    RoundedLast -= RoundedLast % Alignment;
    Range<U> AlignedRange(RoundedFirst, RoundedLast);

    // There is nothing to do if the interval is already aligned.
    if (OrigRange == AlignedRange)
      continue;

    // Merge in all following intervals that start before RoundedLast.
    iterator Next = SI + 1;
    for (; Next != Intervals.end() && Next->getRange().getFirst() < RoundedLast;
         ++Next)
      Interval.JoinWith(*Next);
    assert(Interval.getRange().getFirst() == OrigRange.getFirst() &&
           "Merging at end changed start!");

    // If merging caused the interval to extend beyond RoundedLast then chop the
    // interval in two at RoundedLast.  This stops intervals getting huge due to
    // repeated merging.
    if (Interval.getRange().getLast() > RoundedLast) {
      Range<U> LowerR(OrigRange.getFirst(), RoundedLast);
      Range<U> UpperR(RoundedLast, Interval.getRange().getLast());
      // We must have merged in at least the next interval.  Reuse it to hold
      // the part we chop off the end.
      T &J = *(SI + 1) = Interval;
      // Chop the end off the original interval so that it stops at RoundedLast
      // and at the same time extend the start of the original interval down to
      // the alignment boundary.
      Interval.ChangeRangeTo(AlignedRange);
      // Chop the start off the new (following) interval so that it begins at
      // RoundedLast.
      J.ChangeRangeTo(UpperR);
      // Delete any other merged intervals.
      Intervals.erase(SI + 2, Next);
    } else {
      // The interval didn't grow beyond the original alignment boundary.  Round
      // it to those boundaries.
      Interval.ChangeRangeTo(AlignedRange);
      // Delete any merged intervals.
      Intervals.erase(SI + 1, Next);
    }
  }
}

#endif /* DRAGONEGG_INTERVALLIST_H */
