//===------- Constants.cpp - Converting and working with constants --------===//
//
// Copyright (C) 2011  Duncan Sands
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
#include "dragonegg/Constants.h"
#include "dragonegg/Internals.h"
#include "dragonegg/Trees.h"
#include "dragonegg/ADT/IntervalList.h"
#include "dragonegg/ADT/Range.h"
extern "C" {
#include "dragonegg/cache.h"
}

// LLVM headers
#include "llvm/GlobalVariable.h"
#include "llvm/LLVMContext.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetFolder.h"
#include "llvm/Target/TargetData.h"

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
#include "tm_p.h"
}

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
    return R.getWidth();
  }

  /// getFirst - Return the position of the first bit in the range.
  unsigned getFirst() const {
    return R.getFirst();
  }

  /// getLast - Return the position of the last bit defining the range.
  unsigned getLast() const {
    return R.getLast();
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
  const Type *ExtTy = IntegerType::get(Context, r.getWidth());
  // If the slice contains no bits then every bit of the extension is zero.
  if (empty())
    return BitSlice(r, Constant::getNullValue(ExtTy));
  // Extend the contents to the new type.
  Constant *C = Folder.CreateZExtOrBitCast(Contents, ExtTy);
  // Position the old contents correctly inside the new contents.
  unsigned deltaFirst = R.getFirst() - r.getFirst();
  unsigned deltaLast = r.getLast() - R.getLast();
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
  const Type *RetTy = IntegerType::get(Context, r.getWidth());
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
  unsigned deltaFirst = r.getFirst() - R.getFirst();
  unsigned deltaLast = R.getLast() - r.getLast();
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
  const Type *RedTy = IntegerType::get(Context, r.getWidth());
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
  const Type *Ty = C->getType();
  int StoreSize = getTargetData().getTypeStoreSizeInBits(Ty);
  R = R.Meet(SignedRange(0, StoreSize));

  // Quick exit if it is clear that there are no bits in the range.
  if (R.empty())
    return BitSlice();
  assert(StoreSize > 0 && "Empty range not eliminated?");

  switch (Ty->getTypeID()) {
  default:
    DieAbjectly("Unsupported type!");
  case Type::PointerTyID: {
    // Cast to an integer with the same number of bits and return that.
    const IntegerType *IntTy = getTargetData().getIntPtrType(Context);
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
    const IntegerType *IntTy = IntegerType::get(Context, BitWidth);
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
    const ArrayType *ATy = cast<ArrayType>(Ty);
    const Type *EltTy = ATy->getElementType();
    const unsigned Stride = getTargetData().getTypeAllocSizeInBits(EltTy);
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
      // Extract the element.
      Constant *Elt = Folder.CreateExtractValue(C, i);
      // View it as a bunch of bits.
      BitSlice EltBits = ViewAsBits(Elt, StrideRange, Folder);
      // Add to the already known bits.
      Bits.Merge(EltBits.Displace(i * Stride), Folder);
    }
    return Bits;
  }

  case Type::StructTyID: {
    const StructType *STy = cast<StructType>(Ty);
    const StructLayout *SL = getTargetData().getStructLayout(STy);
    // Fields with indices in [FirstIdx, LastIdx) overlap the range.
    unsigned FirstIdx = SL->getElementContainingOffset((R.getFirst()+7)/8);
    unsigned LastIdx = 1 + SL->getElementContainingOffset((R.getLast()+6)/8);
    // Visit all fields that overlap the requested range, accumulating their
    // bits in Bits.
    BitSlice Bits;
    for (unsigned i = FirstIdx; i < LastIdx; ++i) {
      // Extract the field.
      Constant *Field = Folder.CreateExtractValue(C, i);
      // View it as a bunch of bits.
      const Type *FieldTy = Field->getType();
      unsigned FieldStoreSize = getTargetData().getTypeStoreSizeInBits(FieldTy);
      BitSlice FieldBits = ViewAsBits(Field, SignedRange(0, FieldStoreSize),
                                      Folder);
      // Add to the already known bits.
      Bits.Merge(FieldBits.Displace(SL->getElementOffset(i)*8), Folder);
    }
    return Bits;
  }

  case Type::VectorTyID: {
    const VectorType *VTy = cast<VectorType>(Ty);
    const Type *EltTy = VTy->getElementType();
    const unsigned Stride = getTargetData().getTypeAllocSizeInBits(EltTy);
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
      // Extract the element.
      ConstantInt *Idx = ConstantInt::get(Type::getInt32Ty(Context), i);
      Constant *Elt = Folder.CreateExtractElement(C, Idx);
      // View it as a bunch of bits.
      BitSlice EltBits = ViewAsBits(Elt, StrideRange, Folder);
      // Add to the already known bits.
      Bits.Merge(EltBits.Displace(i * Stride), Folder);
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
static Constant *InterpretAsType(Constant *C, const Type* Ty, int StartingBit,
                                 TargetFolder &Folder) {
  if (C->getType() == Ty)
    return C;

  switch (Ty->getTypeID()) {
  default:
    DieAbjectly("Unsupported type!");
  case Type::IntegerTyID: {
    unsigned BitWidth = Ty->getPrimitiveSizeInBits();
    unsigned StoreSize = getTargetData().getTypeStoreSizeInBits(Ty);
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
    const IntegerType *IntTy = getTargetData().getIntPtrType(Context);
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
    const IntegerType *IntTy = IntegerType::get(Context, BitWidth);
    return Folder.CreateBitCast(InterpretAsType(C, IntTy, StartingBit, Folder),
                                Ty);
  }

  case Type::ArrayTyID: {
    // Interpret each array element in turn.
    const ArrayType *ATy = cast<ArrayType>(Ty);
    const Type *EltTy = ATy->getElementType();
    const unsigned Stride = getTargetData().getTypeAllocSizeInBits(EltTy);
    const unsigned NumElts = ATy->getNumElements();
    std::vector<Constant*> Vals(NumElts);
    for (unsigned i = 0; i != NumElts; ++i)
      Vals[i] = InterpretAsType(C, EltTy, StartingBit + i*Stride, Folder);
    return ConstantArray::get(ATy, Vals); // TODO: Use ArrayRef constructor.
  }

  case Type::StructTyID: {
    // Interpret each struct field in turn.
    const StructType *STy = cast<StructType>(Ty);
    const StructLayout *SL = getTargetData().getStructLayout(STy);
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
    const VectorType *VTy = cast<VectorType>(Ty);
    const Type *EltTy = VTy->getElementType();
    const unsigned Stride = getTargetData().getTypeAllocSizeInBits(EltTy);
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
    DieAbjectly("Unknown register type!", type);

  case BOOLEAN_TYPE:
  case ENUMERAL_TYPE:
  case INTEGER_TYPE: {
    // For integral types, extract an integer with size equal to the type size,
    // then truncate down to the precision.  For example, when extracting a bool
    // this probably first loads out an i8 or i32 which is then truncated to i1.
    // This roundabout approach means we get the right result on both little and
    // big endian machines.
    uint64_t Size = getInt64(TYPE_SIZE(type), true);
    const Type *MemTy = IntegerType::get(Context, Size);
    C = InterpretAsType(C, MemTy, StartingBit, Folder);
    return Folder.CreateTruncOrBitCast(C, getRegType(type));
  }

  case COMPLEX_TYPE: {
    tree elt_type = TREE_TYPE(type);
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
    tree elt_type = TREE_TYPE(type);
    unsigned NumElts = TYPE_VECTOR_SUBPARTS(type);
    unsigned Stride = GET_MODE_BITSIZE(TYPE_MODE(elt_type));
    SmallVector<Constant*, 16> Vals(NumElts);
    const IntegerType *IntPtrTy = getTargetData().getIntPtrType(Context);
    for (unsigned i = 0; i != NumElts; ++i) {
      Vals[i] = ExtractRegisterFromConstantImpl(C, elt_type,
                                                StartingBit+i*Stride, Folder);
      // LLVM does not support vectors of pointers, so turn any pointers into
      // integers.
      if (isa<PointerType>(Vals[i]->getType()))
        Vals[i] = Folder.CreatePtrToInt(Vals[i], IntPtrTy);
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
  TargetFolder Folder(&getTargetData());
  return ExtractRegisterFromConstantImpl(C, type, StartingByte, Folder);
}


//===----------------------------------------------------------------------===//
//                       ... ConvertInitializer ...
//===----------------------------------------------------------------------===//

/// getAsRegister - Turn the given GCC scalar constant into an LLVM constant of
/// register type.
static Constant *getAsRegister(tree exp, TargetFolder &Folder) {
  Constant *C = ConvertInitializerImpl(exp, Folder);
  return ExtractRegisterFromConstantImpl(C, TREE_TYPE(exp), 0, Folder);
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
    DieAbjectly("Unknown register type!", type);

  case BOOLEAN_TYPE:
  case ENUMERAL_TYPE:
  case INTEGER_TYPE: {
    // For integral types extend to an integer with size equal to the type size.
    // For example, when inserting a bool this probably extends it to an i8 or
    // to an i32.  This approach means we get the right result on both little
    // and big endian machines.
    uint64_t Size = getInt64(TYPE_SIZE(type), true);
    const Type *MemTy = IntegerType::get(Context, Size);
    // We can extend in any way, but get nicer IR by respecting signedness.
    bool isSigned = !TYPE_UNSIGNED(type);
    Result = isSigned ? Folder.CreateSExtOrBitCast(C, MemTy) :
      Folder.CreateZExtOrBitCast(C, MemTy);
    break;
  }

  case COMPLEX_TYPE: {
    tree elt_type = TREE_TYPE(type);
    unsigned Idx[2] = {0, 1};
    Constant *Real = Folder.CreateExtractValue(C, Idx[0]);
    Constant *Imag = Folder.CreateExtractValue(C, Idx[1]);
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
    tree elt_type = TREE_TYPE(type);
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
/// and the initializer are scalar.  This is convenient for making explicit the
/// implicit scalar casts that GCC allows in "assignments" such as initializing
/// a record field.
static Constant *ConvertInitializerWithCast(tree exp, tree type,
                                            TargetFolder &Folder) {
  // Convert the initializer.
  Constant *C = ConvertInitializerImpl(exp, Folder);

  // If no cast is needed, or it would not be a scalar cast, then just return
  // the initializer as is.
  if (type == TREE_TYPE(exp) || AGGREGATE_TYPE_P(TREE_TYPE(exp)) ||
      AGGREGATE_TYPE_P(type))
    return C;
  // No cast is needed if the LLVM types are the same.  This occurs often since
  // many different GCC types usually map to the same LLVM type.
  const Type *SrcTy = getRegType(TREE_TYPE(exp));
  const Type *DestTy = getRegType(type);
  if (SrcTy == DestTy)
    return C;

  // Ensure that the initializer has a sensible type.  Note that it would be
  // wrong to just interpret the constant as being of type DestTy here since
  // that would not perform a value extension (adding extra zeros or sign bits
  // when casting to a larger integer type for example): any extra bits would
  // wrongly get an undefined value instead.
  C = ExtractRegisterFromConstantImpl(C, TREE_TYPE(exp), 0, Folder);

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
  const tree type = TREE_TYPE(exp);
  unsigned SizeInChars = (TREE_INT_CST_LOW(TYPE_SIZE(type)) + CHAR_BIT - 1) /
    CHAR_BIT;
  // Encode the constant in Buffer in target format.
  std::vector<unsigned char> Buffer(SizeInChars);
  unsigned CharsWritten = native_encode_expr(exp, &Buffer[0], SizeInChars);
  assert(CharsWritten == SizeInChars && "Failed to fully encode expression!");
  (void)CharsWritten; // Avoid unused variable warning when assertions disabled.
  // Turn it into an LLVM byte array.
  return ConstantArray::get(Context, StringRef((char *)&Buffer[0], SizeInChars),
                            /*AddNull*/false);
}

static Constant *ConvertSTRING_CST(tree exp, TargetFolder &) {
  // TODO: Enhance GCC's native_encode_expr to handle arbitrary strings and not
  // just those with a byte component type; then ConvertCST can handle strings.
  const ArrayType *StrTy = cast<ArrayType>(ConvertType(TREE_TYPE(exp)));
  const Type *ElTy = StrTy->getElementType();

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
    assert(0 && "Unknown character type!");
  }

  unsigned LenInElts = Len /
          TREE_INT_CST_LOW(TYPE_SIZE_UNIT(TREE_TYPE(TREE_TYPE(exp))));
  unsigned ConstantSize = StrTy->getNumElements();

  if (LenInElts != ConstantSize) {
    // If this is a variable sized array type, set the length to LenInElts.
    if (ConstantSize == 0) {
      tree Domain = TYPE_DOMAIN(TREE_TYPE(exp));
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
  const TargetData &TD = getTargetData();

  tree init_type = TREE_TYPE(exp);
  const Type *InitTy = ConvertType(init_type);

  tree elt_type = TREE_TYPE(init_type);
  const Type *EltTy = ConvertType(elt_type);

  // Check that the element type has a known, constant size.
  assert(isSequentialCompatible(init_type) && "Variable sized array element!");
  uint64_t EltSize = TD.getTypeAllocSizeInBits(EltTy);

  /// Elts - The initial values to use for the array elements.  A null entry
  /// means that the corresponding array element should be default initialized.
  std::vector<Constant*> Elts;

  // Resize to the number of array elements if known.  This ensures that every
  // element will be at least default initialized even if no initial value is
  // given for it.
  uint64_t TypeElts = TREE_CODE(init_type) == ARRAY_TYPE ?
    ArrayLengthOf(init_type) : TYPE_VECTOR_SUBPARTS(init_type);
  if (TypeElts != NO_LENGTH)
    Elts.resize(TypeElts);

  // If GCC indices into the array need adjusting to make them zero indexed then
  // record here the value to subtract off.
  tree lower_bnd = NULL_TREE;
  if (TREE_CODE(init_type) == ARRAY_TYPE && TYPE_DOMAIN(init_type) &&
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
      Constant *Elts[] = {
        Val, UndefValue::get(GetUnitType(Context, Units))
      };
      
      Val = ConstantStruct::getAnon(Elts);
    }

    // Get the index position of the element within the array.  Note that this
    // can be NULL_TREE, which means that it belongs in the next available slot.
    tree index = elt_index;

    // The first and last elements to fill in, inclusive.
    unsigned FirstIndex, LastIndex;
    if (!index) {
      LastIndex = FirstIndex = NextIndex;
    } else if (TREE_CODE(index) == RANGE_EXPR) {
      tree first = TREE_OPERAND(index, 0);
      tree last  = TREE_OPERAND(index, 1);

      // Subtract off the lower bound if any to ensure indices start from zero.
      if (lower_bnd != NULL_TREE) {
        first = fold_build2(MINUS_EXPR, TREE_TYPE(first), first, lower_bnd);
        last = fold_build2(MINUS_EXPR, TREE_TYPE(last), last, lower_bnd);
      }

      assert(host_integerp(first, 1) && host_integerp(last, 1) &&
             "Unknown range_expr!");
      FirstIndex = tree_low_cst(first, 1);
      LastIndex = tree_low_cst(last, 1);
    } else {
      // Subtract off the lower bound if any to ensure indices start from zero.
      if (lower_bnd != NULL_TREE)
        index = fold_build2(MINUS_EXPR, TREE_TYPE(index), index, lower_bnd);
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
  bool UseStruct = false;
  const Type *ActualEltTy = Elts[0]->getType();
  unsigned MaxAlign = TD.getABITypeAlignment(ActualEltTy);
  for (unsigned i = 1; i != NumElts; ++i)
    if (Elts[i]->getType() != ActualEltTy) {
      MaxAlign = std::max(TD.getABITypeAlignment(Elts[i]->getType()), MaxAlign);
      UseStruct = true;
    }

  // If any elements are more aligned than the GCC type then we need to return a
  // packed struct.  This can happen if the user forced a small alignment on the
  // array type.
  bool Pack = MaxAlign * 8 > TYPE_ALIGN(TREE_TYPE(exp));

  // We guarantee that initializers are always at least as big as the LLVM type
  // for the initializer.  If needed, append padding to ensure this.
  uint64_t TypeSize = TD.getTypeAllocSizeInBits(InitTy);
  if (NumElts * EltSize < TypeSize) {
    unsigned PadBits = TypeSize - NumElts * EltSize;
    assert(PadBits % BITS_PER_UNIT == 0 && "Non-unit type size?");
    unsigned Units = PadBits / BITS_PER_UNIT;
    Elts.push_back(UndefValue::get(GetUnitType(Context, Units)));
    UseStruct = true;
  }

  // Return as a struct if the contents are not homogeneous.
  if (UseStruct || Pack)
    return ConstantStruct::getAnon(Context, Elts, Pack);

  // Make the IR more pleasant by returning as a vector if the GCC type was a
  // vector.  However this is only correct if the initial values had the same
  // type as the vector element type, rather than some random other type.
  return ActualEltTy == EltTy && TREE_CODE(init_type) == VECTOR_TYPE ?
    ConstantVector::get(Elts) :
    ConstantArray::get(ArrayType::get(ActualEltTy, Elts.size()), Elts);
}

/// FieldContents - A constant restricted to a range of bits.  Any part of the
/// constant outside of the range is discarded.  The range may be bigger than
/// the constant in which case any extra bits have an undefined value.
namespace {

class FieldContents {
  SignedRange R; // The range of bits occupied by the constant.
  Constant *C;   // The constant.  May be null if the range is empty.
  int Starts;    // The first bit of the constant is positioned at this offset.
  TargetFolder &Folder;

  FieldContents(SignedRange r, Constant *c, int starts, TargetFolder &folder)
    : R(r), C(c), Starts(starts), Folder(folder) {
    assert((R.empty() || C) && "Need constant when range not empty!");
  }

  /// getAsBits - Return the bits in the range as an integer (or null if the
  /// range is empty).
  Constant *getAsBits() const {
    if (R.empty())
      return 0;
    const Type *IntTy = IntegerType::get(Context, R.getWidth());
    return InterpretAsType(C, IntTy, R.getFirst() - Starts, Folder);
  }

  /// isSafeToReturnContentsDirectly - Return whether the current value for the
  /// constant properly represents the bits in the range and so can be handed to
  /// the user as is.
  bool isSafeToReturnContentsDirectly(const TargetData &TD) const {
    // If there is no constant (allowed when the range is empty) then one needs
    // to be created.
    if (!C)
      return false;
    // If the first bit of the constant is not the first bit of the range then
    // it needs to be displaced before being passed to the user.
    if (!R.empty() && R.getFirst() != Starts)
      return false;
    // If the constant is wider than the range then it needs to be truncated
    // before being passed to the user.
    const Type *Ty = C->getType();
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
  Constant *extractContents(const TargetData &TD) {
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
    assert(R.getWidth() % BITS_PER_UNIT == 0 && "Boundaries not aligned?");
    unsigned Units = R.getWidth() / BITS_PER_UNIT;
    // Turn the contents into a bunch of bits.  Remember the returned value as
    // an optimization in case we are called again.
    // TODO: If the contents only need to be truncated and have struct or array
    // type then we could try to do the truncation by dropping or modifying the
    // last elements of the constant, maybe yielding something less horrible.
    C = getAsBits();
    Starts = R.getFirst();
    if (isSafeToReturnContentsDirectly(TD))
      return C;
    // The integer type used to hold the bits was too big (for example an i24
    // typically occupies 32 bits so is too big for a range of 24 bits).  Turn
    // it into an array of bytes instead.
    C = InterpretAsType(C, GetUnitType(Context, Units), 0, Folder);
    assert(isSafeToReturnContentsDirectly(TD) && "Unit over aligned?");
    return C;
  }
};

} // Unnamed namespace.

/// JoinWith - Form the union of this field with another field (which must be
/// disjoint from this one).  After this the range will be the convex hull of
/// the ranges of the two fields.
void FieldContents::JoinWith(const FieldContents &S) {
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
  const TargetData &TD = getTargetData();
  uint64_t TypeSize = TD.getTypeAllocSizeInBits(ConvertType(TREE_TYPE(exp)));

  // Ensure that fields without an initial value are default initialized by
  // explicitly setting the starting value for all fields to be zero.  If an
  // initial value is supplied for a field then the value will overwrite and
  // replace the zero starting value later.
  if (flag_default_initialize_globals) {
    for (tree field = TYPE_FIELDS(TREE_TYPE(exp)); field;
         field = TREE_CHAIN(field)) {
      assert(TREE_CODE(field) == FIELD_DECL && "Lang data not freed?");
      // If the field has variable or unknown position then it cannot be default
      // initialized - skip it.
      if (!OffsetIsLLVMCompatible(field))
        continue;
      uint64_t FirstBit = getFieldOffsetInBits(field);
      assert(FirstBit <= TypeSize && "Field off end of type!");
      // Determine the width of the field.
      uint64_t BitWidth;
      const Type *FieldTy = ConvertType(TREE_TYPE(field));
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
  next_field = TYPE_FIELDS(TREE_TYPE(exp));
  FOR_EACH_CONSTRUCTOR_ELT(CONSTRUCTOR_ELTS(exp), ix, field, value) {
    if (!field) {
      // Move on to the next FIELD_DECL, skipping contained methods, types etc.
      field = next_field;
      while (1) {
        assert(field && "Fell off end of record!");
        if (TREE_CODE(field) == FIELD_DECL) break;
        field = TREE_CHAIN(field);
      }
    }
    next_field = TREE_CHAIN(field);

    assert(TREE_CODE(field) == FIELD_DECL && "Initial value not for a field!");
    assert(OffsetIsLLVMCompatible(field) && "Field position not known!");
    // Turn the initial value for this field into an LLVM constant.
    Constant *Init = ConvertInitializerWithCast(value, TREE_TYPE(field),
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
  unsigned MaxAlign = TYPE_ALIGN(TREE_TYPE(exp));
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

  // Okay, we're done, return the computed elements.
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
    DieAbjectly("Unknown constructor!", exp);
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
  return RepresentAsMemory(Folder.CreateSub(LHS, RHS), TREE_TYPE(exp), Folder);
}

static Constant *ConvertPLUS_EXPR(tree exp, TargetFolder &Folder) {
  Constant *LHS = getAsRegister(TREE_OPERAND(exp, 0), Folder);
  Constant *RHS = getAsRegister(TREE_OPERAND(exp, 1), Folder);
  return RepresentAsMemory(Folder.CreateAdd(LHS, RHS), TREE_TYPE(exp), Folder);
}

static Constant *ConvertPOINTER_PLUS_EXPR(tree exp, TargetFolder &Folder) {
  Constant *Ptr = getAsRegister(TREE_OPERAND(exp, 0), Folder); // Pointer
  Constant *Idx = getAsRegister(TREE_OPERAND(exp, 1), Folder); // Offset (units)

  // Convert the pointer into an i8* and add the offset to it.
  Ptr = Folder.CreateBitCast(Ptr, GetUnitPointerType(Context));
  Constant *Result = POINTER_TYPE_OVERFLOW_UNDEFINED ?
    Folder.CreateInBoundsGetElementPtr(Ptr, &Idx, 1) :
    Folder.CreateGetElementPtr(Ptr, &Idx, 1);

  // The result may be of a different pointer type.
  Result = Folder.CreateBitCast(Result, getRegType(TREE_TYPE(exp)));

  return RepresentAsMemory(Result, TREE_TYPE(exp), Folder);
}

static Constant *ConvertVIEW_CONVERT_EXPR(tree exp, TargetFolder &Folder) {
  // Does not change the bits, only the type they are considered to be.
  return ConvertInitializerImpl(TREE_OPERAND(exp, 0), Folder);
}

/// ConvertInitializerImpl - Implementation of ConvertInitializer.
static Constant *ConvertInitializerImpl(tree exp, TargetFolder &Folder) {
  // If we already converted the initializer then return the cached copy.
  if (Constant *C = (Constant *)llvm_get_cached(exp))
    return C;

  Constant *Init;
  switch (TREE_CODE(exp)) {
  default:
    DieAbjectly("Unknown constant to convert!", exp);
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
    Init = ConvertInitializerWithCast(TREE_OPERAND(exp, 0), TREE_TYPE(exp),
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

  // Make the IR easier to read by converting the bunch of bytes returned by
  // ConvertCST into a less surprising type when it is safe to do so.
  if (!AGGREGATE_TYPE_P(TREE_TYPE(exp)))
    Init = InterpretAsType(Init, ConvertType(TREE_TYPE(exp)), 0, Folder);

#ifndef NDEBUG
  // Check that the guarantees we make about the returned value actually hold.
  // The initializer should always be at least as big as the constructor's type,
  // and except in the cases of incomplete types or types with variable size the
  // sizes should be the same.
  const Type *Ty = ConvertType(TREE_TYPE(exp));
  if (Ty->isSized()) {
    uint64_t InitSize = getTargetData().getTypeAllocSizeInBits(Init->getType());
    uint64_t TypeSize = getTargetData().getTypeAllocSizeInBits(Ty);
    if (InitSize < TypeSize)
      DieAbjectly("Constant too small for type!", exp);
    if (isInt64(TREE_TYPE(exp), true) && InitSize != TypeSize)
      DieAbjectly("Constant too big for type!", exp);
  }
  if (getTargetData().getABITypeAlignment(Init->getType()) * 8 >
      TYPE_ALIGN(TREE_TYPE(exp)))
    DieAbjectly("Constant over aligned!", exp);
#endif

  // Cache the result of converting the initializer since the same tree is often
  // converted multiple times.
  llvm_set_cached(exp, Init);

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
  TargetFolder Folder(&getTargetData());
  return ConvertInitializerImpl(exp, Folder);
}


//===----------------------------------------------------------------------===//
//                            ... AddressOf ...
//===----------------------------------------------------------------------===//

/// AddressOfCST - Return the address of a simple constant, eg a of number.
static Constant *AddressOfCST(tree exp, TargetFolder &Folder) {
  Constant *Init = ConvertInitializerImpl(exp, Folder);

  // Cache the constants to avoid making obvious duplicates that have to be
  // folded by the optimizer.
  static DenseMap<Constant*, GlobalVariable*> CSTCache;
  GlobalVariable *&Slot = CSTCache[Init];
  if (Slot)
    return Slot;

  // Create a new global variable.
  Slot = new GlobalVariable(*TheModule, Init->getType(), true,
                            GlobalVariable::PrivateLinkage, Init, ".cst");
  unsigned align = TYPE_ALIGN(TREE_TYPE(exp));
#ifdef CONSTANT_ALIGNMENT
  align = CONSTANT_ALIGNMENT(exp, align);
#endif
  Slot->setAlignment(align);

  return Slot;
}

/// AddressOfARRAY_REF - Return the address of an array element or slice.
static Constant *AddressOfARRAY_REF(tree exp, TargetFolder &Folder) {
  tree array = TREE_OPERAND(exp, 0);
  tree index = TREE_OPERAND(exp, 1);
  tree index_type = TREE_TYPE(index);
  assert(TREE_CODE(TREE_TYPE(array)) == ARRAY_TYPE && "Unknown ARRAY_REF!");

  // Check for variable sized reference.
  assert(isSequentialCompatible(TREE_TYPE(array)) &&
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
  const Type *EltTy = ConvertType(TREE_TYPE(TREE_TYPE(array)));
  ArrayAddr = Folder.CreateBitCast(ArrayAddr, EltTy->getPointerTo());

  return POINTER_TYPE_OVERFLOW_UNDEFINED ?
    Folder.CreateInBoundsGetElementPtr(ArrayAddr, &IndexVal, 1) :
    Folder.CreateGetElementPtr(ArrayAddr, &IndexVal, 1);
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
  }
  assert(BitStart == 0 &&
         "It's a bitfield reference or we didn't get to the field!");

  const Type *UnitPtrTy = GetUnitPointerType(Context);
  Constant *StructAddr = AddressOfImpl(TREE_OPERAND(exp, 0), Folder);
  Constant *FieldPtr = Folder.CreateBitCast(StructAddr, UnitPtrTy);
  FieldPtr = Folder.CreateInBoundsGetElementPtr(FieldPtr, &Offset, 1);

  return FieldPtr;
}

/// AddressOfDecl - Return the address of a global.
static Constant *AddressOfDecl(tree exp, TargetFolder &) {
  return cast<GlobalValue>(DEFINITION_LLVM(exp));
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
    assert(TREE_CODE(DECL_CONTEXT(exp)) == FUNCTION_DECL &&
           "Address of label in nested function?");
    assert(TheTreeToLLVM->getFUNCTION_DECL() == DECL_CONTEXT(exp) &&
           "Taking the address of a label that isn't in the current fn!?");
  }

  return TheTreeToLLVM->AddressOfLABEL_DECL(exp);
}

/// AddressOfImpl - Implementation of AddressOf.
static Constant *AddressOfImpl(tree exp, TargetFolder &Folder) {
  Constant *Addr;

  switch (TREE_CODE(exp)) {
  default:
    DieAbjectly("Unknown constant to take the address of!", exp);
  case COMPLEX_CST:
  case FIXED_CST:
  case INTEGER_CST:
  case REAL_CST:
  case STRING_CST:
  case VECTOR_CST:
    Addr = AddressOfCST(exp, Folder);
    break;
  case ARRAY_RANGE_REF:
  case ARRAY_REF:
    Addr = AddressOfARRAY_REF(exp, Folder);
    break;
  case COMPONENT_REF:
    Addr = AddressOfCOMPONENT_REF(exp, Folder);
    break;
  case COMPOUND_LITERAL_EXPR: // FIXME: not gimple - defined by C front-end
    Addr = AddressOfImpl(DECL_EXPR_DECL(TREE_OPERAND(exp, 0)), Folder);
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
  }

  // Ensure that the address has the expected type.  It is simpler to do this
  // once here rather than in every AddressOf helper.
  const Type *Ty;
  if (VOID_TYPE_P(TREE_TYPE(exp)))
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
  TargetFolder Folder(&getTargetData());
  return AddressOfImpl(exp, Folder);
}
