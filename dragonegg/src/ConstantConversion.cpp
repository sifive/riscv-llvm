//===--- ConstantConversion.cpp - Converting and working with constants ---===//
//
// Copyright (C) 2011 to 2012  Duncan Sands
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
// This is the code that converts GCC constants to LLVM.
//===----------------------------------------------------------------------===//

// Plugin headers
#include "dragonegg/Cache.h"
#include "dragonegg/ConstantConversion.h"
#include "dragonegg/Internals.h"
#include "dragonegg/TypeConversion.h"
#include "dragonegg/ADT/IntervalList.h"
#include "dragonegg/ADT/Range.h"

// LLVM headers
#include "llvm/GlobalVariable.h"
#include "llvm/LLVMContext.h"
#include "llvm/Support/Host.h"
#include "llvm/DataLayout.h"

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

#if (GCC_MINOR < 7)
#include "flags.h" // For POINTER_TYPE_OVERFLOW_UNDEFINED.
#endif
#include "tm_p.h"  // For CONSTANT_ALIGNMENT.
#ifndef ENABLE_BUILD_WITH_CXX
} // extern "C"
#endif

// Trees header.
#include "dragonegg/Trees.h"

using namespace llvm;

static LLVMContext &Context = getGlobalContext();

// Forward declarations.
static Constant *ConvertInitializerImpl(tree, TargetFolder &);
static Constant *AddressOfImpl(tree, TargetFolder &);

//===----------------------------------------------------------------------===//
//                           ... InterpretAsType ...
//===----------------------------------------------------------------------===//

// TODO: Implement InterpretAsType more efficiently.  Turning everything into
// bits is simple but can involve a lot of work when dealing with large arrays.
// For example:
//  struct task_struct {
//    char comm[16];
//  };
//  union task_union {
//    struct task_struct task;
//    unsigned long stack[2048*sizeof(long)/sizeof(long)];
//  };
//  union task_union init_task_union = { { comm: "swapper" } };

typedef Range<int> SignedRange;

/// BitSlice - A contiguous range of bits held in memory.
namespace {

class BitSlice {
  SignedRange R;
  Constant *Contents; // Null if and only if the range is empty.

  bool contentsValid() const {
    if (empty())
      return !Contents;
    return Contents && isa<IntegerType>(Contents->getType()) &&
      getBitWidth() == Contents->getType()->getPrimitiveSizeInBits();
  }

  /// ExtendRange - Extend the slice to a wider range.  All added bits are zero.
  BitSlice ExtendRange(SignedRange r, TargetFolder &Folder) const;

  /// ReduceRange - Reduce the slice to a smaller range discarding any bits that
  /// do not belong to the new range.
  BitSlice ReduceRange(SignedRange r, TargetFolder &Folder) const;

public:
  /// BitSlice - Default constructor: empty bit range.
  BitSlice() : R(), Contents(0) {}

  /// BitSlice - Constructor for the given range of bits.  The bits themselves
  /// are supplied in 'contents' as a constant of integer type (if the range is
  /// empty then 'contents' must be null).  On little-endian machines the least
  /// significant bit of 'contents' corresponds to the first bit of the range
  /// (aka "First"), while on big-endian machines it corresponds to the last bit
  /// of the range (aka "Last-1").
  BitSlice(SignedRange r, Constant *contents) : R(r), Contents(contents) {
    assert(contentsValid() && "Contents do not match range");
  }

  /// BitSlice - Constructor for the range of bits ['first', 'last').
  BitSlice(int first, int last, Constant *contents)
    : R(first, last), Contents(contents) {
    assert(contentsValid() && "Contents do not match range");
  }

  /// empty - Return whether the bit range is empty.
  bool empty() const {
    return R.empty();
  }

  /// getBitWidth - Return the number of bits in the range.
  unsigned getBitWidth() const {
    return (unsigned)R.getWidth();
  }

  /// getRange - Return the range of bits in this slice.
  SignedRange getRange() const {
    return R;
  }

  /// Displace - Return the result of sliding all bits by the given offset.
  BitSlice Displace(int Offset) const {
    return BitSlice(R.Displace(Offset), Contents);
  }

  /// getBits - Return the bits in the given range.  The supplied range need not
  /// be contained in the range of the slice, but if not then the bits outside
  /// the slice get an undefined value.  The bits are returned as a constant of
  /// integer type.  On little-endian machine the least significant bit of the
  /// returned value corresponds to the first bit of the range (aka "First"),
  /// while on big-endian machines it corresponds to the last bit of the range
  /// (aka "Last-1").
  Constant *getBits(SignedRange r, TargetFolder &Folder) const;

  /// Merge - Join the slice with another (which must be disjoint), forming the
  /// convex hull of the ranges.  The bits in the range of one of the slices are
  /// those of that slice.  Any other bits have an undefined value.
  void Merge(const BitSlice &other, TargetFolder &Folder);
};

} // Unnamed namespace.

/// ExtendRange - Extend the slice to a wider range.  All added bits are zero.
BitSlice BitSlice::ExtendRange(SignedRange r, TargetFolder &Folder) const {
  assert(r.contains(R) && "Not an extension!");
  // Quick exit if the range did not actually increase.
  if (R == r)
    return *this;
  assert(!r.empty() && "Empty ranges did not evaluate as equal?");
  Type *ExtTy = IntegerType::get(Context, (unsigned)r.getWidth());
  // If the slice contains no bits then every bit of the extension is zero.
  if (empty())
    return BitSlice(r, Constant::getNullValue(ExtTy));
  // Extend the contents to the new type.
  Constant *C = Folder.CreateZExtOrBitCast(Contents, ExtTy);
  // Position the old contents correctly inside the new contents.
  unsigned deltaFirst = (unsigned)(R.getFirst() - r.getFirst());
  unsigned deltaLast = (unsigned)(r.getLast() - R.getLast());
  if (BYTES_BIG_ENDIAN && deltaLast) {
    (void)deltaFirst; // Avoid unused variable warning.
    Constant *ShiftAmt = ConstantInt::get(C->getType(), deltaLast);
    C = Folder.CreateShl(C, ShiftAmt);
  } else if (!BYTES_BIG_ENDIAN && deltaFirst) {
    (void)deltaLast; // Avoid unused variable warning.
    Constant *ShiftAmt = ConstantInt::get(C->getType(), deltaFirst);
    C = Folder.CreateShl(C, ShiftAmt);
  }
  return BitSlice(r, C);
}

/// getBits - Return the bits in the given range.  The supplied range need not
/// be contained in the range of the slice, but if not then the bits outside
/// the slice get an undefined value.  The bits are returned as a constant of
/// integer type.  On little-endian machine the least significant bit of the
/// returned value corresponds to the first bit of the range (aka "First"),
/// while on big-endian machines it corresponds to the last bit of the range
/// (aka "Last-1").
Constant *BitSlice::getBits(SignedRange r, TargetFolder &Folder) const {
  assert(!r.empty() && "Bit range is empty!");
  // Quick exit if the desired range matches that of the slice.
  if (R == r)
    return Contents;
  Type *RetTy = IntegerType::get(Context, (unsigned)r.getWidth());
  // If the slice contains no bits then every returned bit is undefined.
  if (empty())
    return UndefValue::get(RetTy);
  // Extend to the convex hull of the two ranges.
  BitSlice Slice = ExtendRange(R.Join(r), Folder);
  // Chop the slice down to the requested range.
  Slice = Slice.ReduceRange(r, Folder);
  // Now we can just return the bits contained in the slice.
  return Slice.Contents;
}

/// Merge - Join the slice with another (which must be disjoint), forming the
/// convex hull of the ranges.  The bits in the range of one of the slices are
/// those of that slice.  Any other bits have an undefined value.
void BitSlice::Merge(const BitSlice &other, TargetFolder &Folder) {
  // If the other slice is empty, the result is this slice.
  if (other.empty())
    return;
  // If this slice is empty, the result is the other slice.
  if (empty()) {
    *this = other;
    return;
  }
  assert(!R.intersects(other.getRange()) && "Slices overlap!");

  // Extend each slice to the convex hull of the ranges.
  SignedRange Hull = R.Join(other.getRange());
  BitSlice ExtThis = ExtendRange(Hull, Folder);
  BitSlice ExtOther = other.ExtendRange(Hull, Folder);

  // Since the slices are disjoint and all added bits are zero they can be
  // joined via a simple 'or'.
  *this = BitSlice(Hull, Folder.CreateOr(ExtThis.Contents, ExtOther.Contents));
}

/// ReduceRange - Reduce the slice to a smaller range discarding any bits that
/// do not belong to the new range.
BitSlice BitSlice::ReduceRange(SignedRange r, TargetFolder &Folder) const {
  assert(R.contains(r) && "Not a reduction!");
  // Quick exit if the range did not actually decrease.
  if (R == r)
    return *this;
  // The trivial case of reducing to an empty range.
  if (r.empty())
    return BitSlice();
  assert(!R.empty() && "Empty ranges did not evaluate as equal?");
  // Move the least-significant bit to the correct position.
  Constant *C = Contents;
  unsigned deltaFirst = (unsigned)(r.getFirst() - R.getFirst());
  unsigned deltaLast = (unsigned)(R.getLast() - r.getLast());
  if (BYTES_BIG_ENDIAN && deltaLast) {
    (void)deltaFirst; // Avoid unused variable warning.
    Constant *ShiftAmt = ConstantInt::get(C->getType(), deltaLast);
    C = Folder.CreateLShr(C, ShiftAmt);
  } else if (!BYTES_BIG_ENDIAN && deltaFirst) {
    (void)deltaLast; // Avoid unused variable warning.
    Constant *ShiftAmt = ConstantInt::get(C->getType(), deltaFirst);
    C = Folder.CreateLShr(C, ShiftAmt);
  }
  // Truncate to the new type.
  Type *RedTy = IntegerType::get(Context, (unsigned)r.getWidth());
  C = Folder.CreateTruncOrBitCast(C, RedTy);
  return BitSlice(r, C);
}

/// ViewAsBits - View the given constant as a bunch of bits, i.e. as one big
/// integer.  Only the bits in the given range are needed, so there is no need
/// to supply bits outside this range though it is harmless to do so.  There is
/// also no need to supply undefined bits inside the range.
static BitSlice ViewAsBits(Constant *C, SignedRange R, TargetFolder &Folder) {
  if (R.empty())
    return BitSlice();

  // Sanitize the range to make life easier in what follows.
  Type *Ty = C->getType();
  int StoreSize = getDataLayout().getTypeStoreSizeInBits(Ty);
  R = R.Meet(SignedRange(0, StoreSize));

  // Quick exit if it is clear that there are no bits in the range.
  if (R.empty())
    return BitSlice();
  assert(StoreSize > 0 && "Empty range not eliminated?");

  switch (Ty->getTypeID()) {
  default:
    llvm_unreachable("Unsupported type!");
  case Type::PointerTyID: {
    // Cast to an integer with the same number of bits and return that.
    IntegerType *IntTy = getDataLayout().getIntPtrType(Ty);
    return BitSlice(0, StoreSize, Folder.CreatePtrToInt(C, IntTy));
  }
  case Type::DoubleTyID:
  case Type::FloatTyID:
  case Type::FP128TyID:
  case Type::IntegerTyID:
  case Type::PPC_FP128TyID:
  case Type::X86_FP80TyID:
  case Type::X86_MMXTyID: {
    // Bitcast to an integer with the same number of bits and return that.
    unsigned BitWidth = Ty->getPrimitiveSizeInBits();
    IntegerType *IntTy = IntegerType::get(Context, BitWidth);
    C = Folder.CreateBitCast(C, IntTy);
    // Be careful about where the bits are placed in case this is a funky type
    // like i1.  If the width is a multiple of the address unit then there is
    // nothing to worry about: the bits occupy the range [0, StoreSize).  But
    // if not then endianness matters: on big-endian machines there are padding
    // bits at the start, while on little-endian machines they are at the end.
    return BYTES_BIG_ENDIAN ?
      BitSlice(StoreSize - BitWidth, StoreSize, C) : BitSlice(0, BitWidth, C);
  }

  case Type::ArrayTyID: {
    ArrayType *ATy = cast<ArrayType>(Ty);
    Type *EltTy = ATy->getElementType();
    const unsigned Stride = getDataLayout().getTypeAllocSizeInBits(EltTy);
    assert(Stride > 0 && "Store size smaller than alloc size?");
    // Elements with indices in [FirstElt, LastElt) overlap the range.
    unsigned FirstElt = R.getFirst() / Stride;
    unsigned LastElt = (R.getLast() + Stride - 1) / Stride;
    assert(LastElt <= ATy->getNumElements() && "Store size bigger than array?");
    // Visit all elements that overlap the requested range, accumulating their
    // bits in Bits.
    BitSlice Bits;
    SignedRange StrideRange(0, Stride);
    for (unsigned i = FirstElt; i < LastElt; ++i) {
      int EltOffsetInBits = i * Stride;
      // Extract the element.
      Constant *Elt = Folder.CreateExtractValue(C, i);
      // View it as a bunch of bits.
      SignedRange NeededBits = StrideRange.Meet(R.Displace(-EltOffsetInBits));
      assert(!NeededBits.empty() && "Used element computation wrong!");
      BitSlice EltBits = ViewAsBits(Elt, NeededBits, Folder);
      // Add to the already known bits.
      Bits.Merge(EltBits.Displace(EltOffsetInBits), Folder);
    }
    return Bits;
  }

  case Type::StructTyID: {
    StructType *STy = cast<StructType>(Ty);
    const StructLayout *SL = getDataLayout().getStructLayout(STy);
    // Fields with indices in [FirstIdx, LastIdx) overlap the range.
    unsigned FirstIdx = SL->getElementContainingOffset(R.getFirst()/8);
    unsigned LastIdx = 1 + SL->getElementContainingOffset((R.getLast()-1)/8);
    // Visit all fields that overlap the requested range, accumulating their
    // bits in Bits.
    BitSlice Bits;
    for (unsigned i = FirstIdx; i < LastIdx; ++i) {
      int FieldOffsetInBits = SL->getElementOffset(i) * 8;
      // Extract the field.
      Constant *Field = Folder.CreateExtractValue(C, i);
      // Only part of the field may be needed.  Compute which bits they are.
      Type *FieldTy = Field->getType();
      unsigned FieldStoreSize = getDataLayout().getTypeStoreSizeInBits(FieldTy);
      SignedRange NeededBits(0, FieldStoreSize);
      NeededBits = NeededBits.Meet(R.Displace(-FieldOffsetInBits));
      // View the needed part of the field as a bunch of bits.
      if (!NeededBits.empty()) { // No field bits needed if only using padding.
        BitSlice FieldBits = ViewAsBits(Field, NeededBits, Folder);
        // Add to the already known bits.
        Bits.Merge(FieldBits.Displace(FieldOffsetInBits), Folder);
      }
    }
    return Bits;
  }

  case Type::VectorTyID: {
    VectorType *VTy = cast<VectorType>(Ty);
    Type *EltTy = VTy->getElementType();
    const unsigned Stride = getDataLayout().getTypeAllocSizeInBits(EltTy);
    assert(Stride > 0 && "Store size smaller than alloc size?");
    // Elements with indices in [FirstElt, LastElt) overlap the range.
    unsigned FirstElt = R.getFirst() / Stride;
    unsigned LastElt = (R.getLast() + Stride - 1) / Stride;
    assert(LastElt <= VTy->getNumElements() && "Store size bigger than vector?");
    // Visit all elements that overlap the requested range, accumulating their
    // bits in Bits.
    BitSlice Bits;
    SignedRange StrideRange(0, Stride);
    for (unsigned i = FirstElt; i < LastElt; ++i) {
      int EltOffsetInBits = i * Stride;
      // Extract the element.
      ConstantInt *Idx = ConstantInt::get(Type::getInt32Ty(Context), i);
      Constant *Elt = Folder.CreateExtractElement(C, Idx);
      // View it as a bunch of bits.
      SignedRange NeededBits = StrideRange.Meet(R.Displace(-EltOffsetInBits));
      assert(!NeededBits.empty() && "Used element computation wrong!");
      BitSlice EltBits = ViewAsBits(Elt, NeededBits, Folder);
      // Add to the already known bits.
      Bits.Merge(EltBits.Displace(EltOffsetInBits), Folder);
    }
    return Bits;
  }
  }
}

/// InterpretAsType - Interpret the bits of the given constant (starting from
/// StartingBit) as representing a constant of type 'Ty'.  This results in the
/// same constant as you would get by storing the bits of 'C' to memory (with
/// the first bit stored being 'StartingBit') and then loading out a (constant)
/// value of type 'Ty' from the stored to memory location.
static Constant *InterpretAsType(Constant *C, Type* Ty, int StartingBit,
                                 TargetFolder &Folder) {
  // Efficient handling for some common cases.
  if (C->getType() == Ty)
    return C;

  if (isa<UndefValue>(C))
    return UndefValue::get(Ty);

  if (C->isNullValue())
    return Constant::getNullValue(Ty);

  // The general case.
  switch (Ty->getTypeID()) {
  default:
    llvm_unreachable("Unsupported type!");
  case Type::IntegerTyID: {
    unsigned BitWidth = Ty->getPrimitiveSizeInBits();
    unsigned StoreSize = getDataLayout().getTypeStoreSizeInBits(Ty);
    // Convert the constant into a bunch of bits.  Only the bits to be "loaded"
    // out are needed, so rather than converting the entire constant this only
    // converts enough to get all of the required bits.
    BitSlice Bits = ViewAsBits(C, SignedRange(StartingBit,
                                              StartingBit + StoreSize), Folder);
    // Extract the bits used by the integer.  If the integer width is a multiple
    // of the address unit then the endianness of the target doesn't matter.  If
    // not then the padding bits come at the start on big-endian machines and at
    // the end on little-endian machines.
    Bits = Bits.Displace(-StartingBit);
    return BYTES_BIG_ENDIAN ?
      Bits.getBits(SignedRange(StoreSize - BitWidth, StoreSize), Folder) :
      Bits.getBits(SignedRange(0, BitWidth), Folder);
  }

  case Type::PointerTyID: {
    // Interpret as an integer with the same number of bits then cast back to
    // the original type.
    IntegerType *IntTy = getDataLayout().getIntPtrType(Ty);
    C = InterpretAsType(C, IntTy, StartingBit, Folder);
    return Folder.CreateIntToPtr(C, Ty);
  }
  case Type::DoubleTyID:
  case Type::FloatTyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID:
  case Type::X86_FP80TyID:
  case Type::X86_MMXTyID: {
    // Interpret as an integer with the same number of bits then cast back to
    // the original type.
    unsigned BitWidth = Ty->getPrimitiveSizeInBits();
    IntegerType *IntTy = IntegerType::get(Context, BitWidth);
    return Folder.CreateBitCast(InterpretAsType(C, IntTy, StartingBit, Folder),
                                Ty);
  }

  case Type::ArrayTyID: {
    // Interpret each array element in turn.
    ArrayType *ATy = cast<ArrayType>(Ty);
    Type *EltTy = ATy->getElementType();
    const unsigned Stride = getDataLayout().getTypeAllocSizeInBits(EltTy);
    const unsigned NumElts = ATy->getNumElements();
    std::vector<Constant*> Vals(NumElts);
    for (unsigned i = 0; i != NumElts; ++i)
      Vals[i] = InterpretAsType(C, EltTy, StartingBit + i*Stride, Folder);
    return ConstantArray::get(ATy, Vals); // TODO: Use ArrayRef constructor.
  }

  case Type::StructTyID: {
    // Interpret each struct field in turn.
    StructType *STy = cast<StructType>(Ty);
    const StructLayout *SL = getDataLayout().getStructLayout(STy);
    unsigned NumElts = STy->getNumElements();
    std::vector<Constant*> Vals(NumElts);
    for (unsigned i = 0; i != NumElts; ++i)
      Vals[i] = InterpretAsType(C, STy->getElementType(i),
                                StartingBit + SL->getElementOffsetInBits(i),
                                Folder);
    return ConstantStruct::get(STy, Vals); // TODO: Use ArrayRef constructor.
  }

  case Type::VectorTyID: {
    // Interpret each vector element in turn.
    VectorType *VTy = cast<VectorType>(Ty);
    Type *EltTy = VTy->getElementType();
    const unsigned Stride = getDataLayout().getTypeAllocSizeInBits(EltTy);
    const unsigned NumElts = VTy->getNumElements();
    SmallVector<Constant*, 16> Vals(NumElts);
    for (unsigned i = 0; i != NumElts; ++i)
      Vals[i] = InterpretAsType(C, EltTy, StartingBit + i*Stride, Folder);
    return ConstantVector::get(Vals);
  }
  }
}


//===----------------------------------------------------------------------===//
//                    ... ExtractRegisterFromConstant ...
//===----------------------------------------------------------------------===//

/// ExtractRegisterFromConstantImpl - Implementation of
/// ExtractRegisterFromConstant.
static Constant *ExtractRegisterFromConstantImpl(Constant *C, tree type,
                                                 int StartingByte,
                                                 TargetFolder &Folder) {
  // NOTE: Needs to be kept in sync with getRegType and RepresentAsMemory.
  int StartingBit = StartingByte * BITS_PER_UNIT;
  switch (TREE_CODE(type)) {

  default:
    debug_tree(type);
    llvm_unreachable("Unknown register type!");

  case BOOLEAN_TYPE:
  case ENUMERAL_TYPE:
  case INTEGER_TYPE: {
    // For integral types, extract an integer with size equal to the mode size,
    // then truncate down to the precision.  For example, when extracting a bool
    // this probably first loads out an i8 or i32 which is then truncated to i1.
    // This roundabout approach means we get the right result on both little and
    // big endian machines.
    unsigned Size = GET_MODE_BITSIZE(TYPE_MODE(type));
    Type *MemTy = IntegerType::get(Context, Size);
    C = InterpretAsType(C, MemTy, StartingBit, Folder);
    return Folder.CreateTruncOrBitCast(C, getRegType(type));
  }

  case COMPLEX_TYPE: {
    tree elt_type = main_type(type);
    unsigned Stride = GET_MODE_BITSIZE(TYPE_MODE(elt_type));
    Constant *Vals[2] = {
      ExtractRegisterFromConstantImpl(C, elt_type, StartingBit, Folder),
      ExtractRegisterFromConstantImpl(C, elt_type, StartingBit + Stride, Folder)
    };
    return ConstantStruct::getAnon(Vals);
  }

  case OFFSET_TYPE:
  case POINTER_TYPE:
  case REFERENCE_TYPE:
    return InterpretAsType(C, getRegType(type), StartingBit, Folder);

  case REAL_TYPE:
    // NOTE: This might be wrong for floats with precision less than their alloc
    // size on big-endian machines.
    return InterpretAsType(C, getRegType(type), StartingBit, Folder);

  case VECTOR_TYPE: {
    tree elt_type = main_type(type);
    unsigned NumElts = TYPE_VECTOR_SUBPARTS(type);
    unsigned Stride = GET_MODE_BITSIZE(TYPE_MODE(elt_type));
    SmallVector<Constant*, 16> Vals(NumElts);
    for (unsigned i = 0; i != NumElts; ++i) {
      Vals[i] = ExtractRegisterFromConstantImpl(C, elt_type,
                                                StartingBit+i*Stride, Folder);
      // LLVM does not support vectors of pointers, so turn any pointers into
      // integers.
      if (isa<PointerType>(Vals[i]->getType())) {
        IntegerType *IntPtrTy =
          getDataLayout().getIntPtrType(Vals[i]->getType());
        Vals[i] = Folder.CreatePtrToInt(Vals[i], IntPtrTy);
      }
    }
    return ConstantVector::get(Vals);
  }

  }
}

/// ExtractRegisterFromConstant - Extract a value of the given scalar GCC type
/// from a constant.  The returned value is of in-register type, as returned by
/// getRegType, and is what you would get by storing the constant to memory and
/// using LoadRegisterFromMemory to load a register value back out starting from
/// byte StartingByte.
Constant *ExtractRegisterFromConstant(Constant *C, tree type, int StartingByte) {
  TargetFolder Folder(&getDataLayout());
  return ExtractRegisterFromConstantImpl(C, type, StartingByte, Folder);
}


//===----------------------------------------------------------------------===//
//                       ... ConvertInitializer ...
//===----------------------------------------------------------------------===//

/// getAsRegister - Turn the given GCC scalar constant into an LLVM constant of
/// register type.
static Constant *getAsRegister(tree exp, TargetFolder &Folder) {
  Constant *C = ConvertInitializerImpl(exp, Folder);
  return ExtractRegisterFromConstantImpl(C, main_type(exp), 0, Folder);
}

/// RepresentAsMemory - Turn a constant of in-register type (corresponding
/// to the given GCC type) into an in-memory constant.  The result has the
/// property that applying ExtractRegisterFromConstant to it gives you the
/// original in-register constant back again.
static Constant *RepresentAsMemory(Constant *C, tree type,
                                   TargetFolder &Folder) {
  // NOTE: Needs to be kept in sync with ExtractRegisterFromConstant.
  assert(C->getType() == getRegType(type) && "Constant has wrong type!");
  Constant *Result;

  switch (TREE_CODE(type)) {

  default:
    debug_tree(type);
    llvm_unreachable("Unknown register type!");

  case BOOLEAN_TYPE:
  case ENUMERAL_TYPE:
  case INTEGER_TYPE: {
    // For integral types extend to an integer with size equal to the mode size.
    // For example, when inserting a bool this probably extends it to an i8 or
    // to an i32.  This approach means we get the right result on both little
    // and big endian machines.
    unsigned Size = GET_MODE_BITSIZE(TYPE_MODE(type));
    Type *MemTy = IntegerType::get(Context, Size);
    bool isSigned = !TYPE_UNSIGNED(type);
    Result = isSigned ? Folder.CreateSExtOrBitCast(C, MemTy) :
      Folder.CreateZExtOrBitCast(C, MemTy);
    break;
  }

  case COMPLEX_TYPE: {
    tree elt_type = main_type(type);
    Constant *Real = Folder.CreateExtractValue(C, 0);
    Constant *Imag = Folder.CreateExtractValue(C, 1);
    Real = RepresentAsMemory(Real, elt_type, Folder);
    Imag = RepresentAsMemory(Imag, elt_type, Folder);
    Constant *Vals[2] = { Real, Imag };
    Result = ConstantStruct::getAnon(Vals);
    break;
  }

  case OFFSET_TYPE:
  case POINTER_TYPE:
  case REFERENCE_TYPE:
    Result = C;
    break;

  case REAL_TYPE:
    // NOTE: This might be wrong for floats with precision less than their alloc
    // size on big-endian machines.
    // If the float precision is less than the alloc size then it will be padded
    // out below.
    Result = C;
    break;

  case VECTOR_TYPE: {
    tree elt_type = main_type(type);
    unsigned NumElts = TYPE_VECTOR_SUBPARTS(type);
    std::vector<Constant*> Vals(NumElts);
    for (unsigned i = 0; i != NumElts; ++i) {
      ConstantInt *Idx = ConstantInt::get(Type::getInt32Ty(Context), i);
      Vals[i] = Folder.CreateExtractElement(C, Idx);
      Vals[i] = RepresentAsMemory(Vals[i], elt_type, Folder);
    }
    // The elements may have funky types, so forming a vector may not always be
    // possible.
    Result = ConstantStruct::getAnon(Vals);
    break;
  }

  }

  // Ensure that the result satisfies the guarantees given by ConvertInitializer
  // by turning it into a type with the right size and an appropriate alignment.
  Result = InterpretAsType(Result, ConvertType(type), 0, Folder);

  assert(C == ExtractRegisterFromConstantImpl(Result, type, 0, Folder) &&
         "Register inserted wrong!");

  return Result;
}

/// ConvertInitializerWithCast - Convert the initial value for a global variable
/// to an equivalent LLVM constant then cast to the given type if both the type
/// and the initializer are scalar, or if the initializer's type only differs in
/// trivial ways from the given type.  This is convenient for avoiding confusing
/// and pointless type changes in the IR, and for making explicit the implicit
/// scalar casts that GCC allows in "assignments" such as initializing a record
/// field.
static Constant *ConvertInitializerWithCast(tree exp, tree type,
                                            TargetFolder &Folder) {
  // Convert the initializer.  Note that the type of the returned value may be
  // pretty much anything.
  Constant *C = ConvertInitializerImpl(exp, Folder);

  // If casting to or from an aggregate then just return the initializer as is.
  // If the types differ then this is probably something like a struct ending in
  // a flexible array being initialized with a struct ending in an array of some
  // definite size.
  if (isa<AGGREGATE_TYPE>(type) || isa<AGGREGATE_TYPE>(TREE_TYPE(exp)))
    return C;

  // Scalar to scalar cast.  This is where the implicit scalar casts that GCC
  // permits are made explicit.
  Type *DestTy = getRegType(type);
  if (C->getType() == DestTy)
    // No cast is needed if the type is already correct.
    return C;

  // Ensure that the initializer has a sensible type.  Note that it would be
  // wrong to just interpret the constant as being of type DestTy here since
  // that would not perform a value extension (adding extra zeros or sign bits
  // when casting to a larger integer type for example): any extra bits would
  // wrongly get an undefined value instead.
  C = ExtractRegisterFromConstantImpl(C, main_type(exp), 0, Folder);

  // Cast to the desired type.
  bool SrcIsSigned = !TYPE_UNSIGNED(TREE_TYPE(exp));
  bool DestIsSigned = !TYPE_UNSIGNED(type);
  Instruction::CastOps opcode = CastInst::getCastOpcode(C, SrcIsSigned, DestTy,
                                                        DestIsSigned);
  C = Folder.CreateCast(opcode, C, DestTy);

  return RepresentAsMemory(C, type, Folder);
}

/// ConvertCST - Return the given simple constant as an array of bytes.  For the
/// moment only INTEGER_CST, REAL_CST, COMPLEX_CST and VECTOR_CST are supported.
static Constant *ConvertCST(tree exp, TargetFolder &) {
  const tree type = main_type(exp);
  unsigned SizeInChars = (TREE_INT_CST_LOW(TYPE_SIZE(type)) + CHAR_BIT - 1) /
    CHAR_BIT;
  // Encode the constant in Buffer in target format.
  SmallVector<uint8_t, 16> Buffer(SizeInChars);
  unsigned CharsWritten = native_encode_expr(exp, &Buffer[0], SizeInChars);
  assert(CharsWritten == SizeInChars && "Failed to fully encode expression!");
  (void)CharsWritten; // Avoid unused variable warning when assertions disabled.
  // Turn it into an LLVM byte array.
  return ConstantDataArray::get(Context, Buffer);
}

static Constant *ConvertSTRING_CST(tree exp, TargetFolder &) {
  // TODO: Enhance GCC's native_encode_expr to handle arbitrary strings and not
  // just those with a byte component type; then ConvertCST can handle strings.
  ArrayType *StrTy = cast<ArrayType>(ConvertType(TREE_TYPE(exp)));
  Type *ElTy = StrTy->getElementType();

  unsigned Len = (unsigned)TREE_STRING_LENGTH(exp);

  std::vector<Constant*> Elts;
  if (ElTy->isIntegerTy(8)) {
    const unsigned char *InStr =(const unsigned char *)TREE_STRING_POINTER(exp);
    for (unsigned i = 0; i != Len; ++i)
      Elts.push_back(ConstantInt::get(Type::getInt8Ty(Context), InStr[i]));
  } else if (ElTy->isIntegerTy(16)) {
    assert((Len&1) == 0 &&
           "Length in bytes should be a multiple of element size");
    const uint16_t *InStr =
      (const unsigned short *)TREE_STRING_POINTER(exp);
    for (unsigned i = 0; i != Len/2; ++i) {
      // gcc has constructed the initializer elements in the target endianness,
      // but we're going to treat them as ordinary shorts from here, with
      // host endianness.  Adjust if necessary.
      if (llvm::sys::isBigEndianHost() == BYTES_BIG_ENDIAN)
        Elts.push_back(ConstantInt::get(Type::getInt16Ty(Context), InStr[i]));
      else
        Elts.push_back(ConstantInt::get(Type::getInt16Ty(Context),
                                        ByteSwap_16(InStr[i])));
    }
  } else if (ElTy->isIntegerTy(32)) {
    assert((Len&3) == 0 &&
           "Length in bytes should be a multiple of element size");
    const uint32_t *InStr = (const uint32_t *)TREE_STRING_POINTER(exp);
    for (unsigned i = 0; i != Len/4; ++i) {
      // gcc has constructed the initializer elements in the target endianness,
      // but we're going to treat them as ordinary ints from here, with
      // host endianness.  Adjust if necessary.
      if (llvm::sys::isBigEndianHost() == BYTES_BIG_ENDIAN)
        Elts.push_back(ConstantInt::get(Type::getInt32Ty(Context), InStr[i]));
      else
        Elts.push_back(ConstantInt::get(Type::getInt32Ty(Context),
                                        ByteSwap_32(InStr[i])));
    }
  } else {
    llvm_unreachable("Unknown character type!");
  }

  unsigned LenInElts = Len /
          TREE_INT_CST_LOW(TYPE_SIZE_UNIT(main_type(main_type(exp))));
  unsigned ConstantSize = StrTy->getNumElements();

  if (LenInElts != ConstantSize) {
    // If this is a variable sized array type, set the length to LenInElts.
    if (ConstantSize == 0) {
      tree Domain = TYPE_DOMAIN(main_type(exp));
      if (!Domain || !TYPE_MAX_VALUE(Domain)) {
        ConstantSize = LenInElts;
        StrTy = ArrayType::get(ElTy, LenInElts);
      }
    }

    if (ConstantSize < LenInElts) {
      // Only some chars are being used, truncate the string: char X[2] = "foo";
      Elts.resize(ConstantSize);
    } else {
      // Fill the end of the string with nulls.
      Constant *C = Constant::getNullValue(ElTy);
      for (; LenInElts != ConstantSize; ++LenInElts)
        Elts.push_back(C);
    }
  }
  return ConstantArray::get(StrTy, Elts);
}

static Constant *ConvertADDR_EXPR(tree exp, TargetFolder &Folder) {
  return AddressOfImpl(TREE_OPERAND(exp, 0), Folder);
}

/// ConvertArrayCONSTRUCTOR - Convert a CONSTRUCTOR with array or vector type.
static Constant *ConvertArrayCONSTRUCTOR(tree exp, TargetFolder &Folder) {
  const DataLayout &TD = getDataLayout();

  tree init_type = main_type(exp);
  Type *InitTy = ConvertType(init_type);

  tree elt_type = main_type(init_type);
  Type *EltTy = ConvertType(elt_type);

  // Check that the element type has a known, constant size.
  assert(isSizeCompatible(elt_type) && "Variable sized array element!");
  uint64_t EltSize = TD.getTypeAllocSizeInBits(EltTy);

  /// Elts - The initial values to use for the array elements.  A null entry
  /// means that the corresponding array element should be default initialized.
  std::vector<Constant*> Elts;

  // Resize to the number of array elements if known.  This ensures that every
  // element will be at least default initialized even if no initial value is
  // given for it.
  uint64_t TypeElts = isa<ARRAY_TYPE>(init_type) ?
    ArrayLengthOf(init_type) : TYPE_VECTOR_SUBPARTS(init_type);
  if (TypeElts != NO_LENGTH)
    Elts.resize(TypeElts);

  // If GCC indices into the array need adjusting to make them zero indexed then
  // record here the value to subtract off.
  tree lower_bnd = NULL_TREE;
  if (isa<ARRAY_TYPE>(init_type) && TYPE_DOMAIN(init_type) &&
      !integer_zerop(TYPE_MIN_VALUE(TYPE_DOMAIN(init_type))))
    lower_bnd = TYPE_MIN_VALUE(TYPE_DOMAIN(init_type));

  unsigned NextIndex = 0;
  unsigned HOST_WIDE_INT ix;
  tree elt_index, elt_value;
  FOR_EACH_CONSTRUCTOR_ELT(CONSTRUCTOR_ELTS(exp), ix, elt_index, elt_value) {
    // Find and decode the constructor's value.
    Constant *Val = ConvertInitializerWithCast(elt_value, elt_type, Folder);
    uint64_t ValSize = TD.getTypeAllocSizeInBits(Val->getType());
    assert(ValSize <= EltSize && "Element initial value too big!");

    // If the initial value is smaller than the element size then pad it out.
    if (ValSize < EltSize) {
      unsigned PadBits = EltSize - ValSize;
      assert(PadBits % BITS_PER_UNIT == 0 && "Non-unit type size?");
      unsigned Units = PadBits / BITS_PER_UNIT;
      Constant *PaddedElt[] = {
        Val, UndefValue::get(GetUnitType(Context, Units))
      };

      Val = ConstantStruct::getAnon(PaddedElt);
    }

    // Get the index position of the element within the array.  Note that this
    // can be NULL_TREE, which means that it belongs in the next available slot.
    tree index = elt_index;

    // The first and last elements to fill in, inclusive.
    unsigned FirstIndex, LastIndex;
    if (!index) {
      LastIndex = FirstIndex = NextIndex;
    } else if (isa<RANGE_EXPR>(index)) {
      tree first = TREE_OPERAND(index, 0);
      tree last  = TREE_OPERAND(index, 1);

      // Subtract off the lower bound if any to ensure indices start from zero.
      if (lower_bnd != NULL_TREE) {
        first = fold_build2(MINUS_EXPR, main_type(first), first, lower_bnd);
        last = fold_build2(MINUS_EXPR, main_type(last), last, lower_bnd);
      }

      assert(host_integerp(first, 1) && host_integerp(last, 1) &&
             "Unknown range_expr!");
      FirstIndex = tree_low_cst(first, 1);
      LastIndex = tree_low_cst(last, 1);
    } else {
      // Subtract off the lower bound if any to ensure indices start from zero.
      if (lower_bnd != NULL_TREE)
        index = fold_build2(MINUS_EXPR, main_type(index), index, lower_bnd);
      assert(host_integerp(index, 1));
      FirstIndex = tree_low_cst(index, 1);
      LastIndex = FirstIndex;
    }

    // Process all of the elements in the range.
    if (LastIndex >= Elts.size())
      Elts.resize(LastIndex + 1);
    for (; FirstIndex <= LastIndex; ++FirstIndex)
      Elts[FirstIndex] = Val;

    NextIndex = FirstIndex;
  }

  unsigned NumElts = Elts.size();

  // Zero length array.
  if (!NumElts)
    return getDefaultValue(InitTy);

  // Default initialize any elements that had no initial value specified.
  Constant *DefaultElt = getDefaultValue(EltTy);
  for (unsigned i = 0; i != NumElts; ++i)
    if (!Elts[i])
      Elts[i] = DefaultElt;

  // Check whether any of the elements have different types.  If so we need to
  // return a struct instead of an array.  This can occur in cases where we have
  // an array of unions, and the various unions had different parts initialized.
  // While there, compute the maximum element alignment.
  bool isHomogeneous = true;
  Type *ActualEltTy = Elts[0]->getType();
  unsigned MaxAlign = TD.getABITypeAlignment(ActualEltTy);
  for (unsigned i = 1; i != NumElts; ++i)
    if (Elts[i]->getType() != ActualEltTy) {
      MaxAlign = std::max(TD.getABITypeAlignment(Elts[i]->getType()), MaxAlign);
      isHomogeneous = false;
    }

  // We guarantee that initializers are always at least as big as the LLVM type
  // for the initializer.  If needed, append padding to ensure this.
  uint64_t TypeSize = TD.getTypeAllocSizeInBits(InitTy);
  if (NumElts * EltSize < TypeSize) {
    unsigned PadBits = TypeSize - NumElts * EltSize;
    assert(PadBits % BITS_PER_UNIT == 0 && "Non-unit type size?");
    unsigned Units = PadBits / BITS_PER_UNIT;
    Elts.push_back(UndefValue::get(GetUnitType(Context, Units)));
    isHomogeneous = false;
  }

  // If any elements are more aligned than the GCC type then we need to return a
  // packed struct.  This can happen if the user forced a small alignment on the
  // array type.
  if (MaxAlign * 8 > TYPE_ALIGN(main_type(exp)))
    return ConstantStruct::getAnon(Context, Elts, /*Packed*/true);

  // Return as a struct if the contents are not homogeneous.
  if (!isHomogeneous) {
    std::vector<Constant*> StructElts;
    unsigned First = 0, E = Elts.size();
    while (First < E) {
      // Find the maximal value of Last s.t. all elements in the range
      // [First, Last) have the same type.
      Type *Ty = Elts[First]->getType();
      unsigned Last = First + 1;
      for (; Last != E; ++Last)
        if (Elts[Last]->getType() != Ty)
          break;
      unsigned NumSameType = Last - First;
      Constant *StructElt;
      if (NumSameType == 1)
        StructElt = Elts[First];
      else
        StructElt = ConstantArray::get(ArrayType::get(Ty, NumSameType),
                                       ArrayRef<Constant*>(&Elts[First],
                                                           NumSameType));
      StructElts.push_back(StructElt);
      First = Last;
    }
    return ConstantStruct::getAnon(Context, StructElts);
  }

  // Make the IR more pleasant by returning as a vector if the GCC type was a
  // vector.  However this is only correct if the initial values had the same
  // type as the vector element type, rather than some random other type.
  if (isa<VECTOR_TYPE>(init_type) && ActualEltTy == EltTy) {
    // If this is a vector of pointers, convert it to a vector of integers.
    if (isa<PointerType>(EltTy)) {
      IntegerType *IntPtrTy = getDataLayout().getIntPtrType(EltTy);
      for (unsigned i = 0, e = Elts.size(); i != e; ++i)
        Elts[i] = Folder.CreatePtrToInt(Elts[i], IntPtrTy);
    }
    return ConstantVector::get(Elts);
  }
  return ConstantArray::get(ArrayType::get(ActualEltTy, Elts.size()), Elts);
}

/// FieldContents - A constant restricted to a range of bits.  Any part of the
/// constant outside of the range is discarded.  The range may be bigger than
/// the constant in which case any extra bits have an undefined value.
namespace {

class FieldContents {
  TargetFolder &Folder;
  SignedRange R; // The range of bits occupied by the constant.
  Constant *C;   // The constant.  May be null if the range is empty.
  int Starts;    // The first bit of the constant is positioned at this offset.

  FieldContents(SignedRange r, Constant *c, int starts, TargetFolder &folder)
    : Folder(folder), R(r), C(c), Starts(starts) {
    assert((R.empty() || C) && "Need constant when range not empty!");
  }

  /// getAsBits - Return the bits in the range as an integer (or null if the
  /// range is empty).
  Constant *getAsBits() const {
    if (R.empty())
      return 0;
    Type *IntTy = IntegerType::get(Context, R.getWidth());
    return InterpretAsType(C, IntTy, R.getFirst() - Starts, Folder);
  }

  /// isSafeToReturnContentsDirectly - Return whether the current value for the
  /// constant properly represents the bits in the range and so can be handed to
  /// the user as is.
  bool isSafeToReturnContentsDirectly(const DataLayout &TD) const {
    // If there is no constant (allowed when the range is empty) then one needs
    // to be created.
    if (!C)
      return false;
    // If the first bit of the constant is not the first bit of the range then
    // it needs to be displaced before being passed to the user.
    if (!R.empty() && R.getFirst() != Starts)
      return false;
    Type *Ty = C->getType();
    // Check that the type isn't something like i17.  Avoiding types like this
    // is not needed for correctness, but makes life easier for the optimizers.
    if ((Ty->getPrimitiveSizeInBits() % BITS_PER_UNIT) != 0)
      return false;
    // If the constant is wider than the range then it needs to be truncated
    // before being passed to the user.
    unsigned AllocBits = TD.getTypeAllocSizeInBits(Ty);
    return AllocBits <= (unsigned)R.getWidth();
  }

public:
  /// get - Fill the range [first, last) with the given constant.
  static FieldContents get(int first, int last, Constant *c,
                           TargetFolder &folder) {
    return FieldContents(SignedRange(first, last), c, first, folder);
  }

  // Copy assignment operator.
  FieldContents &operator=(const FieldContents &other) {
    R = other.R; C = other.C; Starts = other.Starts; Folder = other.Folder;
    return *this;
  }

  /// getRange - Return the range occupied by this field.
  SignedRange getRange() const { return R; }

  /// ChangeRangeTo - Change the range occupied by this field.
  void ChangeRangeTo(SignedRange r) { R = r; }

  /// JoinWith - Form the union of this field with another field (which must be
  /// disjoint from this one).  After this the range will be the convex hull of
  /// the ranges of the two fields.
  void JoinWith(const FieldContents &S);

  /// extractContents - Return the contained bits as a constant which contains
  /// every defined bit in the range, yet is guaranteed to have alloc size no
  /// larger than the width of the range.  Unlike the other methods for this
  /// class, this one requires that the width of the range be a multiple of an
  /// address unit, which usually means a multiple of 8.
  Constant *extractContents(const DataLayout &TD) {
    assert(R.getWidth() % BITS_PER_UNIT == 0 && "Boundaries not aligned?");
    /// If the current value for the constant can be used to represent the bits
    /// in the range then just return it.
    if (isSafeToReturnContentsDirectly(TD))
      return C;
    // If the range is empty then return a constant with zero size.
    if (R.empty()) {
      // Return an empty array.  Remember the returned value as an optimization
      // in case we are called again.
      C = UndefValue::get(GetUnitType(Context, 0));
      assert(isSafeToReturnContentsDirectly(TD) && "Unit over aligned?");
      return C;
    }
    // If the type is something like i17 then round it up to a multiple of a
    // byte.  This is not needed for correctness, but helps the optimizers.
    if ((C->getType()->getPrimitiveSizeInBits() % BITS_PER_UNIT) != 0) {
      Type *Ty = C->getType();
      assert(Ty->isIntegerTy() && "Non-integer type with non-byte size!");
      unsigned BitWidth = RoundUpToAlignment(Ty->getPrimitiveSizeInBits(),
                                             BITS_PER_UNIT);
      Ty = IntegerType::get(Context, BitWidth);
      C = TheFolder->CreateZExtOrBitCast(C, Ty);
      if (isSafeToReturnContentsDirectly(TD))
        return C;
    }
    // Turn the contents into a bunch of bytes.  Remember the returned value as
    // an optimization in case we are called again.
    // TODO: If the contents only need to be truncated and have struct or array
    // type then we could try to do the truncation by dropping or modifying the
    // last elements of the constant, maybe yielding something less horrible.
    unsigned Units = R.getWidth() / BITS_PER_UNIT;
    C = InterpretAsType(C, GetUnitType(Context, Units), R.getFirst() - Starts,
                        Folder);
    Starts = R.getFirst();
    assert(isSafeToReturnContentsDirectly(TD) && "Unit over aligned?");
    return C;
  }
};

} // Unnamed namespace.

/// JoinWith - Form the union of this field with another field (which must be
/// disjoint from this one).  After this the range will be the convex hull of
/// the ranges of the two fields.
void FieldContents::JoinWith(const FieldContents &S) {
  if (S.R.empty())
    return;
  if (R.empty()) {
    *this = S;
    return;
  }
  // Consider the contents of the fields to be bunches of bits and paste them
  // together.  This can result in a nasty integer constant expression, but as
  // we only get here for bitfields that's mostly harmless.
  BitSlice Bits(R, getAsBits());
  Bits.Merge (BitSlice(S.R, S.getAsBits()), Folder);
  R = Bits.getRange();
  C = Bits.getBits(R, Folder);
  Starts = R.empty() ? 0 : R.getFirst();
}

static Constant *ConvertRecordCONSTRUCTOR(tree exp, TargetFolder &Folder) {
  // FIXME: This new logic, especially the handling of bitfields, is untested
  // and probably wrong on big-endian machines.
  IntervalList<FieldContents, int, 8> Layout;
  const DataLayout &TD = getDataLayout();
  tree type = main_type(exp);
  Type *Ty = ConvertType(type);
  uint64_t TypeSize = TD.getTypeAllocSizeInBits(Ty);

  // Ensure that fields without an initial value are default initialized by
  // explicitly setting the starting value for all fields to be zero.  If an
  // initial value is supplied for a field then the value will overwrite and
  // replace the zero starting value later.
  if (flag_default_initialize_globals) {
    // Record all interesting fields so they can easily be visited backwards.
    SmallVector<tree, 16> Fields;
    for (tree field = TYPE_FIELDS(type); field; field = TREE_CHAIN(field)) {
      if (!isa<FIELD_DECL>(field)) continue;
      // Ignore fields with variable or unknown position since they cannot be
      // default initialized.
      if (!OffsetIsLLVMCompatible(field))
        continue;
      Fields.push_back(field);
    }

    // Process the fields in reverse order.  This is for the benefit of union
    // types for which the first field must be default initialized (iterating
    // in forward order would default initialize the last field).
    for (SmallVector<tree, 16>::reverse_iterator I = Fields.rbegin(),
         E = Fields.rend(); I != E; ++I) {
      tree field = *I;
      uint64_t FirstBit = getFieldOffsetInBits(field);
      assert(FirstBit <= TypeSize && "Field off end of type!");
      // Determine the width of the field.
      uint64_t BitWidth;
      Type *FieldTy = ConvertType(TREE_TYPE(field));
      if (isInt64(DECL_SIZE(field), true)) {
        // The field has a size and it is a constant, so use it.  Note that
        // this size may be smaller than the type size.  For example, if the
        // next field starts inside alignment padding at the end of this one
        // then DECL_SIZE will be the size with the padding used by the next
        // field not included.
        BitWidth = getInt64(DECL_SIZE(field), true);
      } else {
        // If the field has variable or unknown size then use the size of the
        // LLVM type instead as it gives the minimum size the field may have.
        if (!FieldTy->isSized())
          // An incomplete type - this field cannot be default initialized.
          continue;
        BitWidth = TD.getTypeAllocSizeInBits(FieldTy);
        if (FirstBit + BitWidth > TypeSize)
          BitWidth = TypeSize - FirstBit;
      }
      uint64_t LastBit = FirstBit + BitWidth;

      // Zero the bits occupied by the field.  It is safe to use FieldTy here as
      // it is guaranteed to cover all parts of the GCC type that can be default
      // initialized.  This makes for nicer IR than just using a bunch of bytes.
      Constant *Zero = Constant::getNullValue(FieldTy);
      Layout.AddInterval(FieldContents::get(FirstBit, LastBit, Zero, Folder));
    }
  }

  // For each field for which an initial value was specified, set the bits
  // occupied by the field to that value.
  unsigned HOST_WIDE_INT ix;
  tree field, next_field, value;
  next_field = TYPE_FIELDS(type);
  FOR_EACH_CONSTRUCTOR_ELT(CONSTRUCTOR_ELTS(exp), ix, field, value) {
    if (!field) {
      // Move on to the next FIELD_DECL, skipping contained methods, types etc.
      field = next_field;
      while (1) {
        assert(field && "Fell off end of record!");
        if (isa<FIELD_DECL>(field)) break;
        field = TREE_CHAIN(field);
      }
    }
    next_field = TREE_CHAIN(field);

    assert(isa<FIELD_DECL>(field) && "Initial value not for a field!");
    assert(OffsetIsLLVMCompatible(field) && "Field position not known!");
    // Turn the initial value for this field into an LLVM constant.
    Constant *Init = ConvertInitializerWithCast(value, main_type(field),
                                                Folder);
    // Work out the range of bits occupied by the field.
    uint64_t FirstBit = getFieldOffsetInBits(field);
    assert(FirstBit <= TypeSize && "Field off end of type!");
    // If a size was specified for the field then use it.  Otherwise take the
    // size from the initial value.
    uint64_t BitWidth = isInt64(DECL_SIZE(field), true) ?
      getInt64(DECL_SIZE(field), true) :
      TD.getTypeAllocSizeInBits(Init->getType());
    uint64_t LastBit = FirstBit + BitWidth;

    // Set the bits occupied by the field to the initial value.
    Layout.AddInterval(FieldContents::get(FirstBit, LastBit, Init, Folder));
  }

  // Force all fields to begin and end on a byte boundary.  This automagically
  // takes care of bitfields.
  Layout.AlignBoundaries(BITS_PER_UNIT);

  // Determine whether to return a packed struct.  If returning an ordinary
  // struct would result in an initializer that is more aligned than its GCC
  // type then return a packed struct instead.  If a field's alignment would
  // make it start after its desired position then also use a packed struct.
  bool Pack = false;
  unsigned MaxAlign = TYPE_ALIGN(type);
  for (unsigned i = 0, e = Layout.getNumIntervals(); i != e; ++i) {
    FieldContents F = Layout.getInterval(i);
    unsigned First = F.getRange().getFirst();
    Constant *Val = F.extractContents(TD);
    unsigned Alignment = TD.getABITypeAlignment(Val->getType()) * 8;
    if (Alignment > MaxAlign || First % Alignment) {
      Pack = true;
      break;
    }
  }

  // Create the elements that will make up the struct.  As well as the fields
  // themselves there may also be padding elements.
  std::vector<Constant*> Elts;
  Elts.reserve(Layout.getNumIntervals());
  unsigned EndOfPrevious = 0; // Offset of first bit after previous element.
  for (unsigned i = 0, e = Layout.getNumIntervals(); i != e; ++i) {
    FieldContents F = Layout.getInterval(i);
    unsigned First = F.getRange().getFirst();
    Constant *Val = F.extractContents(TD);
    assert(EndOfPrevious <= First && "Previous field too big!");

    // If there is a gap then we may need to fill it with padding.
    if (First > EndOfPrevious) {
      // There is a gap between the end of the previous field and the start of
      // this one.  The alignment of the field contents may mean that it will
      // start at the right offset anyway, but if not then insert padding.
      bool NeedPadding = true;
      if (!Pack) {
        // If the field's alignment will take care of the gap then there is no
        // need for padding.
        unsigned Alignment = TD.getABITypeAlignment(Val->getType()) * 8;
        if (First == (EndOfPrevious + Alignment - 1) / Alignment * Alignment)
          NeedPadding = false;
      }
      if (NeedPadding) {
        // Fill the gap with undefined bytes.
        assert((First - EndOfPrevious) % BITS_PER_UNIT == 0 &&
               "Non-unit field boundaries!");
        unsigned Units = (First - EndOfPrevious) / BITS_PER_UNIT;
        Elts.push_back(UndefValue::get(GetUnitType(Context, Units)));
      }
    }

    // Append the field.
    Elts.push_back(Val);
    EndOfPrevious = First + TD.getTypeAllocSizeInBits(Val->getType());
  }

  // We guarantee that initializers are always at least as big as the LLVM type
  // for the initializer.  If needed, append padding to ensure this.
  if (EndOfPrevious < TypeSize) {
    assert((TypeSize - EndOfPrevious) % BITS_PER_UNIT == 0 &&
           "Non-unit type size?");
    unsigned Units = (TypeSize - EndOfPrevious) / BITS_PER_UNIT;
    Elts.push_back(UndefValue::get(GetUnitType(Context, Units)));
  }

  // Okay, we're done.  Return the computed elements as a constant with the type
  // of exp if possible.
  if (StructType *STy = dyn_cast<StructType>(Ty))
    if (STy->isPacked() == Pack && STy->getNumElements() == Elts.size()) {
      bool EltTypesMatch = true;
      for (unsigned i = 0, e = Elts.size(); i != e; ++i) {
        Type *EltTy = Elts[i]->getType();
        Type *FieldTy = STy->getElementType(i);
        if (EltTy == FieldTy)
          continue;
        // When a recursive record type is converted, some of its pointer fields
        // may be converted to the artifical type {}* to break the recursion. As
        // type converting the field directly gives the proper pointer type, the
        // result is a mismatch between the field and element types.  Fix it up.
        if (EltTy->isPointerTy() && FieldTy->isPointerTy()) {
          Elts[i] = Folder.CreateBitCast(Elts[i], FieldTy);
          continue;
        }
        // Too hard, just give up.
        EltTypesMatch = false;
        break;
      }
      if (EltTypesMatch)
        return ConstantStruct::get(STy, Elts);
    }

  // Otherwise return the computed elements as an anonymous struct.
  return ConstantStruct::getAnon(Context, Elts, Pack);
}

static Constant *ConvertCONSTRUCTOR(tree exp, TargetFolder &Folder) {
  // If the constructor is empty then default initialize all of the components.
  // It is safe to use the LLVM type here as it covers every part of the GCC
  // type that can possibly be default initialized.
  if (CONSTRUCTOR_NELTS(exp) == 0)
    return getDefaultValue(ConvertType(TREE_TYPE(exp)));

  switch (TREE_CODE(TREE_TYPE(exp))) {
  default:
    debug_tree(exp);
    llvm_unreachable("Unknown constructor!");
  case VECTOR_TYPE:
  case ARRAY_TYPE:  return ConvertArrayCONSTRUCTOR(exp, Folder);
  case QUAL_UNION_TYPE:
  case RECORD_TYPE:
  case UNION_TYPE: return ConvertRecordCONSTRUCTOR(exp, Folder);
  }
}

static Constant *ConvertMINUS_EXPR(tree exp, TargetFolder &Folder) {
  Constant *LHS = getAsRegister(TREE_OPERAND(exp, 0), Folder);
  Constant *RHS = getAsRegister(TREE_OPERAND(exp, 1), Folder);
  if (LHS->getType()->getScalarType()->isPointerTy()) {
    Type *PtrIntTy = getDataLayout().getIntPtrType(LHS->getType());
    LHS = Folder.CreatePtrToInt(LHS, PtrIntTy);
    RHS = Folder.CreatePtrToInt(RHS, PtrIntTy);
  }
  return RepresentAsMemory(Folder.CreateSub(LHS, RHS), main_type(exp), Folder);
}

static Constant *ConvertPLUS_EXPR(tree exp, TargetFolder &Folder) {
  Constant *LHS = getAsRegister(TREE_OPERAND(exp, 0), Folder);
  Constant *RHS = getAsRegister(TREE_OPERAND(exp, 1), Folder);
  return RepresentAsMemory(Folder.CreateAdd(LHS, RHS), main_type(exp), Folder);
}

static Constant *ConvertPOINTER_PLUS_EXPR(tree exp, TargetFolder &Folder) {
  Constant *Ptr = getAsRegister(TREE_OPERAND(exp, 0), Folder); // Pointer
  Constant *Idx = getAsRegister(TREE_OPERAND(exp, 1), Folder); // Offset (units)

  // Convert the pointer into an i8* and add the offset to it.
  Ptr = Folder.CreateBitCast(Ptr, GetUnitPointerType(Context));
  Constant *Result = POINTER_TYPE_OVERFLOW_UNDEFINED ?
    Folder.CreateInBoundsGetElementPtr(Ptr, Idx) :
    Folder.CreateGetElementPtr(Ptr, Idx);

  // The result may be of a different pointer type.
  Result = Folder.CreateBitCast(Result, getRegType(TREE_TYPE(exp)));

  return RepresentAsMemory(Result, main_type(exp), Folder);
}

static Constant *ConvertVIEW_CONVERT_EXPR(tree exp, TargetFolder &Folder) {
  // Does not change the bits, only the type they are considered to be.
  return ConvertInitializerImpl(TREE_OPERAND(exp, 0), Folder);
}

/// ConvertInitializerImpl - Implementation of ConvertInitializer.
static Constant *ConvertInitializerImpl(tree exp, TargetFolder &Folder) {
  assert(!isa<CONST_DECL>(exp) && !HAS_RTL_P(exp) &&
         "Cache collision with decl_llvm!");

  // If we already converted the initializer then return the cached copy.
  if (Constant *C = cast_or_null<Constant>(getCachedValue(exp)))
    return C;

  Constant *Init;
  switch (TREE_CODE(exp)) {
  default:
    debug_tree(exp);
    llvm_unreachable("Unknown constant to convert!");
  case COMPLEX_CST:
  case INTEGER_CST:
  case REAL_CST:
  case VECTOR_CST:
    Init = ConvertCST(exp, Folder);
    break;
  case STRING_CST:
    Init = ConvertSTRING_CST(exp, Folder);
    break;
  case ADDR_EXPR:
    Init = ConvertADDR_EXPR(exp, Folder);
    break;
  case CONSTRUCTOR:
    Init = ConvertCONSTRUCTOR(exp, Folder);
    break;
  case CONVERT_EXPR:
  case NOP_EXPR:
    Init = ConvertInitializerWithCast(TREE_OPERAND(exp, 0), main_type(exp),
                                      Folder);
    break;
  case MINUS_EXPR:
    Init = ConvertMINUS_EXPR(exp, Folder);
    break;
  case PLUS_EXPR:
    Init = ConvertPLUS_EXPR(exp, Folder);
    break;
  case POINTER_PLUS_EXPR:
    Init = ConvertPOINTER_PLUS_EXPR(exp, Folder);
    break;
  case VIEW_CONVERT_EXPR:
    Init = ConvertVIEW_CONVERT_EXPR(exp, Folder);
    break;
  }

  // Make the IR easier to read by returning a constant of the expected type if
  // it is safe and efficient to do so.
  if (!isa<AGGREGATE_TYPE>(TREE_TYPE(exp)))
    Init = InterpretAsType(Init, ConvertType(TREE_TYPE(exp)), 0, Folder);

#ifndef NDEBUG
  // Check that the guarantees we make about the returned value actually hold.
  // The initializer should always be at least as big as the constructor's type,
  // and except in the cases of incomplete types or types with variable size the
  // sizes should be the same.
  Type *Ty = ConvertType(TREE_TYPE(exp));
  if (Ty->isSized()) {
    uint64_t InitSize = getDataLayout().getTypeAllocSizeInBits(Init->getType());
    uint64_t TypeSize = getDataLayout().getTypeAllocSizeInBits(Ty);
    if (InitSize < TypeSize) {
      debug_tree(exp);
      llvm_unreachable("Constant too small for type!");
    }
  }
  if (getDataLayout().getABITypeAlignment(Init->getType()) * 8 >
      TYPE_ALIGN(main_type(exp))) {
    debug_tree(exp);
    llvm_unreachable("Constant over aligned!");
  }
#endif

  // Cache the result of converting the initializer since the same tree is often
  // converted multiple times.
  setCachedValue(exp, Init);

  return Init;
}

/// ConvertInitializer - Convert the initial value for a global variable to an
/// equivalent LLVM constant.  Also handles constant constructors.  The type of
/// the returned value may be pretty much anything.  All that is guaranteed is
/// that its alloc size is equal to the size of the initial value and that its
/// alignment is less than or equal to the initial value's GCC type alignment.
/// Note that the GCC type may have variable size or no size, in which case the
/// size is determined by the initial value.  When this happens the size of the
/// initial value may exceed the alloc size of the LLVM memory type generated
/// for the GCC type (see ConvertType); it is never smaller than the alloc size.
Constant *ConvertInitializer(tree exp) {
  TargetFolder Folder(&getDataLayout());
  return ConvertInitializerImpl(exp, Folder);
}


//===----------------------------------------------------------------------===//
//                            ... AddressOf ...
//===----------------------------------------------------------------------===//

/// AddressOfSimpleConstant - Return the address of a simple constant, such as a
/// number or constructor.
static Constant *AddressOfSimpleConstant(tree exp, TargetFolder &Folder) {
  Constant *Init = ConvertInitializerImpl(exp, Folder);

  // Cache the constants to avoid making obvious duplicates that have to be
  // folded by the optimizer.
  static DenseMap<Constant*, GlobalVariable*> CSTCache;
  GlobalVariable *&Slot = CSTCache[Init];
  if (Slot)
    return Slot;

  // Create a new global variable.
  Slot = new GlobalVariable(*TheModule, Init->getType(), true,
                            GlobalVariable::LinkerPrivateLinkage, Init, ".cst");
  unsigned align = TYPE_ALIGN(main_type(exp));
#ifdef CONSTANT_ALIGNMENT
  align = CONSTANT_ALIGNMENT(exp, align);
#endif
  Slot->setAlignment(align);
  // Allow identical constants to be merged if the user allowed it.
  // FIXME: maybe this flag should be set unconditionally, and instead the
  // ConstantMerge pass should be disabled if flag_merge_constants is zero.
  Slot->setUnnamedAddr(flag_merge_constants);

  return Slot;
}

/// AddressOfARRAY_REF - Return the address of an array element or slice.
static Constant *AddressOfARRAY_REF(tree exp, TargetFolder &Folder) {
  tree array = TREE_OPERAND(exp, 0);
  tree index = TREE_OPERAND(exp, 1);
  tree index_type = main_type(index);
  assert(isa<ARRAY_TYPE>(TREE_TYPE(array)) && "Unknown ARRAY_REF!");

  // Check for variable sized reference.
  assert(isSizeCompatible(main_type(main_type(array))) &&
         "Global with variable size?");

  // Get the index into the array as an LLVM integer constant.
  Constant *IndexVal = getAsRegister(index, Folder);

  // Subtract off the lower bound, if any.
  tree lower_bound = array_ref_low_bound(exp);
  if (!integer_zerop(lower_bound)) {
    // Get the lower bound as an LLVM integer constant.
    Constant *LowerBoundVal = getAsRegister(lower_bound, Folder);
    IndexVal = Folder.CreateSub(IndexVal, LowerBoundVal, hasNUW(index_type),
                                hasNSW(index_type));
  }

  // Avoid any assumptions about how the array type is represented in LLVM by
  // doing the GEP on a pointer to the first array element.
  Constant *ArrayAddr = AddressOfImpl(array, Folder);
  Type *EltTy = ConvertType(main_type(main_type(array)));
  ArrayAddr = Folder.CreateBitCast(ArrayAddr, EltTy->getPointerTo());

  return POINTER_TYPE_OVERFLOW_UNDEFINED ?
    Folder.CreateInBoundsGetElementPtr(ArrayAddr, IndexVal) :
    Folder.CreateGetElementPtr(ArrayAddr, IndexVal);
}

/// AddressOfCOMPONENT_REF - Return the address of a field in a record.
static Constant *AddressOfCOMPONENT_REF(tree exp, TargetFolder &Folder) {
  tree field_decl = TREE_OPERAND(exp, 1);

  // Compute the field offset in units from the start of the record.
  Constant *Offset;
  if (TREE_OPERAND(exp, 2)) {
    Offset = getAsRegister(TREE_OPERAND(exp, 2), Folder);
    // At this point the offset is measured in units divided by (exactly)
    // (DECL_OFFSET_ALIGN / BITS_PER_UNIT).  Convert to units.
    unsigned factor = DECL_OFFSET_ALIGN(field_decl) / BITS_PER_UNIT;
    if (factor != 1)
      Offset = Folder.CreateMul(Offset, ConstantInt::get(Offset->getType(),
                                                         factor));
  } else {
    assert(DECL_FIELD_OFFSET(field_decl) && "Field offset not available!");
    Offset = getAsRegister(DECL_FIELD_OFFSET(field_decl), Folder);
  }

  // Here BitStart gives the offset of the field in bits from Offset.
  uint64_t BitStart = getInt64(DECL_FIELD_BIT_OFFSET(field_decl), true);
  // Incorporate as much of it as possible into the pointer computation.
  uint64_t Units = BitStart / BITS_PER_UNIT;
  if (Units > 0) {
    Offset = Folder.CreateAdd(Offset, ConstantInt::get(Offset->getType(),
                                                       Units));
    BitStart -= Units * BITS_PER_UNIT;
    (void)BitStart;
  }
  assert(BitStart == 0 &&
         "It's a bitfield reference or we didn't get to the field!");

  Type *UnitPtrTy = GetUnitPointerType(Context);
  Constant *StructAddr = AddressOfImpl(TREE_OPERAND(exp, 0), Folder);
  Constant *FieldPtr = Folder.CreateBitCast(StructAddr, UnitPtrTy);
  FieldPtr = Folder.CreateInBoundsGetElementPtr(FieldPtr, Offset);

  return FieldPtr;
}

/// AddressOfCOMPOUND_LITERAL_EXPR - Return the address of a compound literal.
static Constant *AddressOfCOMPOUND_LITERAL_EXPR(tree exp, TargetFolder &Folder){
  tree decl = DECL_EXPR_DECL(COMPOUND_LITERAL_EXPR_DECL_EXPR(exp));
  return AddressOfImpl(decl, Folder);
}

/// AddressOfDecl - Return the address of a global.
static Constant *AddressOfDecl(tree exp, TargetFolder &) {
  return cast<Constant>(DEFINITION_LLVM(exp));
}

/// AddressOfINDIRECT_REF - Return the address of a dereference.
static Constant *AddressOfINDIRECT_REF(tree exp, TargetFolder &Folder) {
  // The address is just the dereferenced operand.  Get it as an LLVM constant.
  return getAsRegister(TREE_OPERAND(exp, 0), Folder);
}

/// AddressOfLABEL_DECL - Return the address of a label.
static Constant *AddressOfLABEL_DECL(tree exp, TargetFolder &) {
  extern TreeToLLVM *TheTreeToLLVM;

  assert(TheTreeToLLVM &&
         "taking the address of a label while not compiling the function!");

  // Figure out which function this is for, verify it's the one we're compiling.
  if (DECL_CONTEXT(exp)) {
    assert(isa<FUNCTION_DECL>(DECL_CONTEXT(exp)) &&
           "Address of label in nested function?");
    assert(TheTreeToLLVM->getFUNCTION_DECL() == DECL_CONTEXT(exp) &&
           "Taking the address of a label that isn't in the current fn!?");
  }

  return TheTreeToLLVM->AddressOfLABEL_DECL(exp);
}

#if (GCC_MINOR > 5)
/// AddressOfMEM_REF - Return the address of a memory reference.
static Constant *AddressOfMEM_REF(tree exp, TargetFolder &Folder) {
  // The address is the first operand offset in bytes by the second.
  Constant *Addr = getAsRegister(TREE_OPERAND(exp, 0), Folder);
  if (integer_zerop(TREE_OPERAND(exp, 1)))
    return Addr;

  // Convert to a byte pointer and displace by the offset.
  Addr = Folder.CreateBitCast(Addr, GetUnitPointerType(Context));
  APInt Delta = getAPIntValue(TREE_OPERAND(exp, 1));
  Constant *Offset = ConstantInt::get(Context, Delta);
  // The address is always inside the referenced object, so "inbounds".
  return Folder.CreateInBoundsGetElementPtr(Addr, Offset);
}
#endif

/// AddressOfImpl - Implementation of AddressOf.
static Constant *AddressOfImpl(tree exp, TargetFolder &Folder) {
  Constant *Addr;

  switch (TREE_CODE(exp)) {
  default:
    debug_tree(exp);
    llvm_unreachable("Unknown constant to take the address of!");
  case COMPLEX_CST:
  case FIXED_CST:
  case INTEGER_CST:
  case REAL_CST:
  case STRING_CST:
  case VECTOR_CST:
    Addr = AddressOfSimpleConstant(exp, Folder);
    break;
  case ARRAY_RANGE_REF:
  case ARRAY_REF:
    Addr = AddressOfARRAY_REF(exp, Folder);
    break;
  case COMPONENT_REF:
    Addr = AddressOfCOMPONENT_REF(exp, Folder);
    break;
  case COMPOUND_LITERAL_EXPR:
    Addr = AddressOfCOMPOUND_LITERAL_EXPR(exp, Folder);
    break;
  case CONSTRUCTOR:
    Addr = AddressOfSimpleConstant(exp, Folder);
    break;
  case CONST_DECL:
  case FUNCTION_DECL:
  case VAR_DECL:
    Addr = AddressOfDecl(exp, Folder);
    break;
  case INDIRECT_REF:
#if (GCC_MINOR < 6)
  case MISALIGNED_INDIRECT_REF:
#endif
    Addr = AddressOfINDIRECT_REF(exp, Folder);
    break;
  case LABEL_DECL:
    Addr = AddressOfLABEL_DECL(exp, Folder);
    break;
#if (GCC_MINOR > 5)
  case MEM_REF:
    Addr = AddressOfMEM_REF(exp, Folder);
    break;
#endif
  }

  // Ensure that the address has the expected type.  It is simpler to do this
  // once here rather than in every AddressOf helper.
  Type *Ty;
  if (isa<VOID_TYPE>(TREE_TYPE(exp)))
    Ty = GetUnitPointerType(Context); // void* -> i8*.
  else
    Ty = ConvertType(TREE_TYPE(exp))->getPointerTo();

  return Folder.CreateBitCast(Addr, Ty);
}

/// AddressOf - Given an expression with a constant address such as a constant,
/// a global variable or a label, returns the address.  The type of the returned
/// is always a pointer type and, as long as 'exp' does not have void type, the
/// type of the pointee is the memory type that corresponds to the type of exp
/// (see ConvertType).
Constant *AddressOf(tree exp) {
  TargetFolder Folder(&getDataLayout());
  return AddressOfImpl(exp, Folder);
}
