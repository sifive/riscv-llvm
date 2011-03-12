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
#include "Constants.h"
#include "Internals.h"
#include "Trees.h"

// LLVM headers
#include "llvm/GlobalVariable.h"
#include "llvm/LLVMContext.h"
#include "llvm/Support/Host.h"
#include "llvm/Target/TargetData.h"

// System headers
#include <gmp.h>
#include <map>

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

/// EncodeExpr - Write the given expression into Buffer as it would appear in
/// memory on the target (the buffer is resized to contain exactly the bytes
/// written).  Return the number of bytes written; this can also be obtained
/// by querying the buffer's size.
/// The following kinds of expressions are currently supported: INTEGER_CST,
/// REAL_CST, COMPLEX_CST, VECTOR_CST, STRING_CST.
static unsigned EncodeExpr(tree exp, SmallVectorImpl<unsigned char> &Buffer) {
  const tree type = TREE_TYPE(exp);
  unsigned SizeInBytes = (TREE_INT_CST_LOW(TYPE_SIZE(type)) + 7) / 8;
  Buffer.resize(SizeInBytes);
  unsigned BytesWritten = native_encode_expr(exp, &Buffer[0], SizeInBytes);
  assert(BytesWritten == SizeInBytes && "Failed to fully encode expression!");
  return BytesWritten;
}

static Constant *ConvertINTEGER_CST(tree exp) {
  const Type *Ty = ConvertType(TREE_TYPE(exp));

  // Handle i128 specially.
  if (const IntegerType *IT = dyn_cast<IntegerType>(Ty)) {
    if (IT->getBitWidth() == 128) {
      // GCC only supports i128 on 64-bit systems.
      assert(HOST_BITS_PER_WIDE_INT == 64 &&
             "i128 only supported on 64-bit system");
      uint64_t Bits[] = { TREE_INT_CST_LOW(exp), TREE_INT_CST_HIGH(exp) };
      return ConstantInt::get(Context, APInt(128, 2, Bits));
    }
  }

  // Build the value as a ulong constant, then constant fold it to the right
  // type.  This handles overflow and other things appropriately.
  uint64_t IntValue = getINTEGER_CSTVal(exp);
  ConstantInt *C = ConstantInt::get(Type::getInt64Ty(Context), IntValue);
  // The destination type can be a pointer, integer or floating point
  // so we need a generalized cast here
  Instruction::CastOps opcode = CastInst::getCastOpcode(C, false, Ty,
      !TYPE_UNSIGNED(TREE_TYPE(exp)));
  return TheFolder->CreateCast(opcode, C, Ty);
}

static Constant *ConvertREAL_CST(tree exp) {
  // TODO: Test new implementation on a big-endian machine.

  // Encode the constant in Buffer in target format.
  SmallVector<unsigned char, 16> Buffer;
  EncodeExpr(exp, Buffer);

  // Discard any alignment padding, which we assume comes at the end.
  unsigned Precision = TYPE_PRECISION(TREE_TYPE(exp));
  assert((Precision & 7) == 0 && "Unsupported real number precision!");
  Buffer.resize(Precision / 8);

  // We are going to view the buffer as an array of APInt words.  Ensure that
  // the buffer contains a whole number of words by extending it if necessary.
  unsigned Words = (Precision + integerPartWidth - 1) / integerPartWidth;
  // On a little-endian machine extend the buffer by adding bytes to the end.
  Buffer.resize(Words * (integerPartWidth / 8));
  // On a big-endian machine extend the buffer by adding bytes to the beginning.
  if (BYTES_BIG_ENDIAN)
    std::copy_backward(Buffer.begin(), Buffer.begin() + Precision / 8,
                       Buffer.end());

  // Ensure that the least significant word comes first: we are going to make an
  // APInt, and the APInt constructor wants the least significant word first.
  integerPart *Parts = (integerPart *)&Buffer[0];
  if (BYTES_BIG_ENDIAN)
    std::reverse(Parts, Parts + Words);

  bool isPPC_FP128 = ConvertType(TREE_TYPE(exp))->isPPC_FP128Ty();
  if (isPPC_FP128) {
    // This type is actually a pair of doubles in disguise.  They turn up the
    // wrong way round here, so flip them.
    assert(FLOAT_WORDS_BIG_ENDIAN && "PPC not big endian!");
    assert(Words == 2 && Precision == 128 && "Strange size for PPC_FP128!");
    std::swap(Parts[0], Parts[1]);
  }

  // Form an APInt from the buffer, an APFloat from the APInt, and the desired
  // floating point constant from the APFloat, phew!
  const APInt &I = APInt(Precision, Words, Parts);
  return ConstantFP::get(Context, APFloat(I, !isPPC_FP128));
}

static Constant *ConvertVECTOR_CST(tree exp) {
  if (!TREE_VECTOR_CST_ELTS(exp))
    return Constant::getNullValue(ConvertType(TREE_TYPE(exp)));

  std::vector<Constant*> Elts;
  for (tree elt = TREE_VECTOR_CST_ELTS(exp); elt; elt = TREE_CHAIN(elt))
    Elts.push_back(ConvertConstant(TREE_VALUE(elt)));

  // The vector should be zero filled if insufficient elements are provided.
  if (Elts.size() < TYPE_VECTOR_SUBPARTS(TREE_TYPE(exp))) {
    tree EltType = TREE_TYPE(TREE_TYPE(exp));
    Constant *Zero = Constant::getNullValue(ConvertType(EltType));
    while (Elts.size() < TYPE_VECTOR_SUBPARTS(TREE_TYPE(exp)))
      Elts.push_back(Zero);
  }

  return ConstantVector::get(Elts);
}

static Constant *ConvertSTRING_CST(tree exp) {
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

static Constant *ConvertCOMPLEX_CST(tree exp) {
  Constant *Elts[2] = {
    ConvertConstant(TREE_REALPART(exp)),
    ConvertConstant(TREE_IMAGPART(exp))
  };
  return ConstantStruct::get(Context, Elts, 2, false);
}

static Constant *ConvertNOP_EXPR(tree exp) {
  Constant *Elt = ConvertConstant(TREE_OPERAND(exp, 0));
  const Type *Ty = ConvertType(TREE_TYPE(exp));
  bool EltIsSigned = !TYPE_UNSIGNED(TREE_TYPE(TREE_OPERAND(exp, 0)));
  bool TyIsSigned = !TYPE_UNSIGNED(TREE_TYPE(exp));

  // If this is a structure-to-structure cast, just return the uncasted value.
  if (!Elt->getType()->isSingleValueType() || !Ty->isSingleValueType())
    return Elt;

  // Elt and Ty can be integer, float or pointer here: need generalized cast
  Instruction::CastOps opcode = CastInst::getCastOpcode(Elt, EltIsSigned,
                                                        Ty, TyIsSigned);
  return TheFolder->CreateCast(opcode, Elt, Ty);
}

static Constant *ConvertCONVERT_EXPR(tree exp) {
  Constant *Elt = ConvertConstant(TREE_OPERAND(exp, 0));
  bool EltIsSigned = !TYPE_UNSIGNED(TREE_TYPE(TREE_OPERAND(exp, 0)));
  const Type *Ty = ConvertType(TREE_TYPE(exp));
  bool TyIsSigned = !TYPE_UNSIGNED(TREE_TYPE(exp));
  Instruction::CastOps opcode = CastInst::getCastOpcode(Elt, EltIsSigned, Ty,
                                                        TyIsSigned);
  return TheFolder->CreateCast(opcode, Elt, Ty);
}

static Constant *ConvertPOINTER_PLUS_EXPR(tree exp) {
  Constant *Ptr = ConvertConstant(TREE_OPERAND(exp, 0)); // The pointer.
  Constant *Idx = ConvertConstant(TREE_OPERAND(exp, 1)); // The offset in bytes.

  // Convert the pointer into an i8* and add the offset to it.
  Ptr = TheFolder->CreateBitCast(Ptr, Type::getInt8PtrTy(Context));
  Constant *GEP = POINTER_TYPE_OVERFLOW_UNDEFINED ?
    TheFolder->CreateInBoundsGetElementPtr(Ptr, &Idx, 1) :
    TheFolder->CreateGetElementPtr(Ptr, &Idx, 1);

  // The result may be of a different pointer type.
  return TheFolder->CreateBitCast(GEP, ConvertType(TREE_TYPE(exp)));
}

static Constant *ConvertBinOp_CST(tree exp) {
  Constant *LHS = ConvertConstant(TREE_OPERAND(exp, 0));
  bool LHSIsSigned = !TYPE_UNSIGNED(TREE_TYPE(TREE_OPERAND(exp,0)));
  Constant *RHS = ConvertConstant(TREE_OPERAND(exp, 1));
  bool RHSIsSigned = !TYPE_UNSIGNED(TREE_TYPE(TREE_OPERAND(exp,1)));
  Instruction::CastOps opcode;
  if (LHS->getType()->isPointerTy()) {
    const Type *IntPtrTy = getTargetData().getIntPtrType(Context);
    opcode = CastInst::getCastOpcode(LHS, LHSIsSigned, IntPtrTy, false);
    LHS = TheFolder->CreateCast(opcode, LHS, IntPtrTy);
    opcode = CastInst::getCastOpcode(RHS, RHSIsSigned, IntPtrTy, false);
    RHS = TheFolder->CreateCast(opcode, RHS, IntPtrTy);
  }

  Constant *Result;
  switch (TREE_CODE(exp)) {
  default: assert(0 && "Unexpected case!");
  case PLUS_EXPR:   Result = TheFolder->CreateAdd(LHS, RHS); break;
  case MINUS_EXPR:  Result = TheFolder->CreateSub(LHS, RHS); break;
  }

  const Type *Ty = ConvertType(TREE_TYPE(exp));
  bool TyIsSigned = !TYPE_UNSIGNED(TREE_TYPE(exp));
  opcode = CastInst::getCastOpcode(Result, LHSIsSigned, Ty, TyIsSigned);
  return TheFolder->CreateCast(opcode, Result, Ty);
}

static Constant *ConvertArrayCONSTRUCTOR(tree exp) {
  // Vectors are like arrays, but the domain is stored via an array
  // type indirectly.

  // If we have a lower bound for the range of the type, get it.
  tree init_type = TREE_TYPE(exp);
  tree min_element = size_zero_node;
  std::vector<Constant*> ResultElts;

  if (TREE_CODE(init_type) == VECTOR_TYPE) {
    ResultElts.resize(TYPE_VECTOR_SUBPARTS(init_type));
  } else {
    assert(TREE_CODE(init_type) == ARRAY_TYPE && "Unknown type for init");
    tree Domain = TYPE_DOMAIN(init_type);
    if (Domain && TYPE_MIN_VALUE(Domain))
      min_element = fold_convert(sizetype, TYPE_MIN_VALUE(Domain));

    if (Domain && TYPE_MAX_VALUE(Domain)) {
      tree max_element = fold_convert(sizetype, TYPE_MAX_VALUE(Domain));
      tree size = size_binop (MINUS_EXPR, max_element, min_element);
      size = size_binop (PLUS_EXPR, size, size_one_node);

      if (host_integerp(size, 1))
        ResultElts.resize(tree_low_cst(size, 1));
    }
  }

  unsigned NextFieldToFill = 0;
  unsigned HOST_WIDE_INT ix;
  tree elt_index, elt_value;
  Constant *SomeVal = 0;
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (exp), ix, elt_index, elt_value) {
    // Find and decode the constructor's value.
    Constant *Val = ConvertConstant(elt_value);
    SomeVal = Val;

    // Get the index position of the element within the array.  Note that this
    // can be NULL_TREE, which means that it belongs in the next available slot.
    tree index = elt_index;

    // The first and last field to fill in, inclusive.
    unsigned FieldOffset, FieldLastOffset;
    if (index && TREE_CODE(index) == RANGE_EXPR) {
      tree first = fold_convert (sizetype, TREE_OPERAND(index, 0));
      tree last  = fold_convert (sizetype, TREE_OPERAND(index, 1));

      first = size_binop (MINUS_EXPR, first, min_element);
      last  = size_binop (MINUS_EXPR, last, min_element);

      assert(host_integerp(first, 1) && host_integerp(last, 1) &&
             "Unknown range_expr!");
      FieldOffset     = tree_low_cst(first, 1);
      FieldLastOffset = tree_low_cst(last, 1);
    } else if (index) {
      index = size_binop (MINUS_EXPR, fold_convert (sizetype, index),
                          min_element);
      assert(host_integerp(index, 1));
      FieldOffset = tree_low_cst(index, 1);
      FieldLastOffset = FieldOffset;
    } else {
      FieldOffset = NextFieldToFill;
      FieldLastOffset = FieldOffset;
    }

    // Process all of the elements in the range.
    for (--FieldOffset; FieldOffset != FieldLastOffset; ) {
      ++FieldOffset;
      if (FieldOffset == ResultElts.size())
        ResultElts.push_back(Val);
      else {
        if (FieldOffset >= ResultElts.size())
          ResultElts.resize(FieldOffset+1);
        ResultElts[FieldOffset] = Val;
      }

      NextFieldToFill = FieldOffset+1;
    }
  }

  // Zero length array.
  if (ResultElts.empty())
    return Constant::getNullValue(ConvertType(TREE_TYPE(exp)));
  assert(SomeVal && "If we had some initializer, we should have some value!");

  // Do a post-pass over all of the elements.  We're taking care of two things
  // here:
  //   #1. If any elements did not have initializers specified, provide them
  //       with a null init.
  //   #2. If any of the elements have different types, return a struct instead
  //       of an array.  This can occur in cases where we have an array of
  //       unions, and the various unions had different pieces init'd.
  const Type *ElTy = SomeVal->getType();
  Constant *Filler = Constant::getNullValue(ElTy);
  bool AllEltsSameType = true;
  for (unsigned i = 0, e = ResultElts.size(); i != e; ++i) {
    if (ResultElts[i] == 0)
      ResultElts[i] = Filler;
    else if (ResultElts[i]->getType() != ElTy)
      AllEltsSameType = false;
  }

  if (TREE_CODE(init_type) == VECTOR_TYPE) {
    assert(AllEltsSameType && "Vector of heterogeneous element types?");
    return ConstantVector::get(ResultElts);
  }

  Constant *Res = AllEltsSameType ?
    ConstantArray::get(ArrayType::get(ElTy, ResultElts.size()), ResultElts) :
    ConstantStruct::get(Context, ResultElts, false);

  // If the array does not require extra padding, return it.
  const Type *InitType = ConvertType(init_type);
  uint64_t ExpectedBits = getTargetData().getTypeAllocSizeInBits(InitType);
  uint64_t FoundBits = getTargetData().getTypeAllocSizeInBits(Res->getType());
  // The initializer may be bigger than the type if init_type is variable sized
  // or has no size (in which case the size is determined by the initial value).
  if (ExpectedBits <= FoundBits)
    return Res;

  // Wrap the array in a struct with padding at the end.
  Constant *PadElts[2];
  PadElts[0] = Res;
  PadElts[1] = UndefValue::get(ArrayType::get(Type::getInt8Ty(Context),
                                              (ExpectedBits - FoundBits) / 8));
  return ConstantStruct::get(Context, PadElts, 2, false);
}


namespace {
/// ConstantLayoutInfo - A helper class used by ConvertRecordCONSTRUCTOR to
/// lay out struct inits.
struct ConstantLayoutInfo {
  const TargetData &TD;

  /// ResultElts - The initializer elements so far.
  std::vector<Constant*> ResultElts;

  /// StructIsPacked - This is set to true if we find out that we have to emit
  /// the ConstantStruct as a Packed LLVM struct type (because the LLVM
  /// alignment rules would prevent laying out the struct correctly).
  bool StructIsPacked;

  /// NextFieldByteStart - This field indicates the *byte* that the next field
  /// will start at.  Put another way, this is the size of the struct as
  /// currently laid out, but without any tail padding considered.
  uint64_t NextFieldByteStart;

  /// MaxLLVMFieldAlignment - This is the largest alignment of any IR field,
  /// which is the alignment that the ConstantStruct will get.
  unsigned MaxLLVMFieldAlignment;


  ConstantLayoutInfo(const TargetData &TD) : TD(TD) {
    StructIsPacked = false;
    NextFieldByteStart = 0;
    MaxLLVMFieldAlignment = 1;
  }

  void ConvertToPacked();
  void AddFieldToRecordConstant(Constant *Val, uint64_t GCCFieldOffsetInBits);
  void AddBitFieldToRecordConstant(ConstantInt *Val,
                                   uint64_t GCCFieldOffsetInBits);
  void HandleTailPadding(uint64_t GCCStructBitSize);
};

}

/// ConvertToPacked - Given a partially constructed initializer for a LLVM
/// struct constant, change it to make all the implicit padding between elements
/// be fully explicit.
void ConstantLayoutInfo::ConvertToPacked() {
  assert(!StructIsPacked && "Struct is already packed");
  uint64_t EltOffs = 0;
  for (unsigned i = 0, e = ResultElts.size(); i != e; ++i) {
    Constant *Val = ResultElts[i];

    // Check to see if this element has an alignment that would cause it to get
    // offset.  If so, insert explicit padding for the offset.
    unsigned ValAlign = TD.getABITypeAlignment(Val->getType());
    uint64_t AlignedEltOffs = TargetData::RoundUpAlignment(EltOffs, ValAlign);

    // If the alignment doesn't affect the element offset, then the value is ok.
    // Accept the field and keep moving.
    if (AlignedEltOffs == EltOffs) {
      EltOffs += TD.getTypeAllocSize(Val->getType());
      continue;
    }

    // Otherwise, there is padding here.  Insert explicit zeros.
    const Type *PadTy = Type::getInt8Ty(Context);
    if (AlignedEltOffs-EltOffs != 1)
      PadTy = ArrayType::get(PadTy, AlignedEltOffs-EltOffs);
    ResultElts.insert(ResultElts.begin()+i,
                      Constant::getNullValue(PadTy));

    // The padding is now element "i" and just bumped us up to "AlignedEltOffs".
    EltOffs = AlignedEltOffs;
    ++e;  // One extra element to scan.
  }

  // Packed now!
  MaxLLVMFieldAlignment = 1;
  StructIsPacked = true;
}


/// AddFieldToRecordConstant - As ConvertRecordCONSTRUCTOR builds up an LLVM
/// constant to represent a GCC CONSTRUCTOR node, it calls this method to add
/// fields.  The design of this is that it adds leading/trailing padding as
/// needed to make the piece fit together and honor the GCC layout.  This does
/// not handle bitfields.
///
/// The arguments are:
///   Val: The value to add to the struct, with a size that matches the size of
///        the corresponding GCC field.
///   GCCFieldOffsetInBits: The offset that we have to put Val in the result.
///
void ConstantLayoutInfo::
AddFieldToRecordConstant(Constant *Val, uint64_t GCCFieldOffsetInBits) {
  // Figure out how to add this non-bitfield value to our constant struct so
  // that it ends up at the right offset.  There are four cases we have to
  // think about:
  //   1. We may be able to just slap it onto the end of our struct and have
  //      everything be ok.
  //   2. We may have to insert explicit padding into the LLVM struct to get
  //      the initializer over into the right space.  This is needed when the
  //      GCC field has a larger alignment than the LLVM field.
  //   3. The LLVM field may be too far over and we may be forced to convert
  //      this to an LLVM packed struct.  This is required when the LLVM
  //      alignment is larger than the GCC alignment.
  //   4. We may have a bitfield that needs to be merged into a previous
  //      field.
  // Start by determining which case we have by looking at where LLVM and GCC
  // would place the field.

  // Verified that we haven't already laid out bytes that will overlap with
  // this new field.
  assert(NextFieldByteStart*8 <= GCCFieldOffsetInBits &&
         "Overlapping LLVM fields!");

  // Compute the offset the field would get if we just stuck 'Val' onto the
  // end of our structure right now.  It is NextFieldByteStart rounded up to
  // the LLVM alignment of Val's type.
  unsigned ValLLVMAlign = 1;

  if (!StructIsPacked) { // Packed structs ignore the alignment of members.
    ValLLVMAlign = TD.getABITypeAlignment(Val->getType());
    MaxLLVMFieldAlignment = std::max(MaxLLVMFieldAlignment, ValLLVMAlign);
  }

  // LLVMNaturalByteOffset - This is where LLVM would drop the field if we
  // slap it onto the end of the struct.
  uint64_t LLVMNaturalByteOffset
    = TargetData::RoundUpAlignment(NextFieldByteStart, ValLLVMAlign);

  // If adding the LLVM field would push it over too far, then we must have a
  // case that requires the LLVM struct to be packed.  Do it now if so.
  if (LLVMNaturalByteOffset*8 > GCCFieldOffsetInBits) {
    // Switch to packed.
    ConvertToPacked();
    assert(NextFieldByteStart*8 <= GCCFieldOffsetInBits &&
           "Packing didn't fix the problem!");

    // Recurse to add the field after converting to packed.
    return AddFieldToRecordConstant(Val, GCCFieldOffsetInBits);
  }

  // If the LLVM offset is not large enough, we need to insert explicit
  // padding in the LLVM struct between the fields.
  if (LLVMNaturalByteOffset*8 < GCCFieldOffsetInBits) {
    // Insert enough padding to fully fill in the hole.  Insert padding from
    // NextFieldByteStart (not LLVMNaturalByteOffset) because the padding will
    // not get the same alignment as "Val".
    const Type *FillTy = Type::getInt8Ty(Context);
    if (GCCFieldOffsetInBits/8-NextFieldByteStart != 1)
      FillTy = ArrayType::get(FillTy,
                              GCCFieldOffsetInBits/8-NextFieldByteStart);
    ResultElts.push_back(Constant::getNullValue(FillTy));

    NextFieldByteStart = GCCFieldOffsetInBits/8;

    // Recurse to add the field.  This handles the case when the LLVM struct
    // needs to be converted to packed after inserting tail padding.
    return AddFieldToRecordConstant(Val, GCCFieldOffsetInBits);
  }

  // Slap 'Val' onto the end of our ConstantStruct, it must be known to land
  // at the right offset now.
  assert(LLVMNaturalByteOffset*8 == GCCFieldOffsetInBits);
  ResultElts.push_back(Val);
  NextFieldByteStart = LLVMNaturalByteOffset;
  NextFieldByteStart += TD.getTypeAllocSize(Val->getType());
}

/// AddBitFieldToRecordConstant - Bitfields can span multiple LLVM fields and
/// have other annoying properties, thus requiring extra layout rules.  This
/// routine handles the extra complexity and then forwards to
/// AddFieldToRecordConstant.
void ConstantLayoutInfo::
AddBitFieldToRecordConstant(ConstantInt *ValC, uint64_t GCCFieldOffsetInBits) {
  // If the GCC field starts after our current LLVM field then there must have
  // been an anonymous bitfield or other thing that shoved it over.  No matter,
  // just insert some i8 padding until there are bits to fill in.
  while (GCCFieldOffsetInBits > NextFieldByteStart*8) {
    ResultElts.push_back(ConstantInt::get(Type::getInt8Ty(Context), 0));
    ++NextFieldByteStart;
  }

  // If the field is a bitfield, it could partially go in a previously
  // laid out structure member, and may add elements to the end of the currently
  // laid out structure.
  //
  // Since bitfields can only partially overlap other bitfields, because we
  // always emit components of bitfields as i8, and because we never emit tail
  // padding until we know it exists, this boils down to merging pieces of the
  // bitfield values into i8's.  This is also simplified by the fact that
  // bitfields can only be initialized by ConstantInts.  An interesting case is
  // sharing of tail padding in C++ structures.  Because this can only happen
  // in inheritance cases, and those are non-POD, we should never see them here.

  // First handle any part of Val that overlaps an already laid out field by
  // merging it into it.  By the above invariants, we know that it is an i8 that
  // we are merging into.  Note that we may be inserting *all* of Val into the
  // previous field.
  if (GCCFieldOffsetInBits < NextFieldByteStart*8) {
    unsigned ValBitSize = ValC->getBitWidth();
    assert(!ResultElts.empty() && "Bitfield starts before first element?");
    assert(ResultElts.back()->getType()->isIntegerTy(8) &&
           isa<ConstantInt>(ResultElts.back()) &&
           "Merging bitfield with non-bitfield value?");
    assert(NextFieldByteStart*8 - GCCFieldOffsetInBits < 8 &&
           "Bitfield overlaps backwards more than one field?");

    // Figure out how many bits can fit into the previous field given the
    // starting point in that field.
    unsigned BitsInPreviousField =
      unsigned(NextFieldByteStart*8 - GCCFieldOffsetInBits);
    assert(BitsInPreviousField != 0 && "Previous field should not be null!");

    // Split the bits that will be inserted into the previous element out of
    // Val into a new constant.  If Val is completely contained in the previous
    // element, this sets Val to null, otherwise we shrink Val to contain the
    // bits to insert in the next element.
    APInt ValForPrevField(ValC->getValue());
    if (BitsInPreviousField >= ValBitSize) {
      // The whole field fits into the previous field.
      ValC = 0;
    } else if (!BYTES_BIG_ENDIAN) {
      // Little endian, take bits from the bottom of the field value.
      ValForPrevField = ValForPrevField.trunc(BitsInPreviousField);
      APInt Tmp = ValC->getValue();
      Tmp = Tmp.lshr(BitsInPreviousField);
      Tmp = Tmp.trunc(ValBitSize-BitsInPreviousField);
      ValC = ConstantInt::get(Context, Tmp);
    } else {
      // Big endian, take bits from the top of the field value.
      ValForPrevField = ValForPrevField.lshr(ValBitSize-BitsInPreviousField);
      ValForPrevField = ValForPrevField.trunc(BitsInPreviousField);

      APInt Tmp = ValC->getValue();
      Tmp = Tmp.trunc(ValBitSize-BitsInPreviousField);
      ValC = ConstantInt::get(Context, Tmp);
    }

    // Okay, we're going to insert ValForPrevField into the previous i8, extend
    // it and shift into place.
    ValForPrevField = ValForPrevField.zext(8);
    if (!BYTES_BIG_ENDIAN) {
      ValForPrevField = ValForPrevField.shl(8-BitsInPreviousField);
    } else {
      // On big endian, if the entire field fits into the remaining space, shift
      // over to not take part of the next field's bits.
      if (BitsInPreviousField > ValBitSize)
        ValForPrevField = ValForPrevField.shl(BitsInPreviousField-ValBitSize);
    }

    // "or" in the previous value and install it.
    const APInt &LastElt = cast<ConstantInt>(ResultElts.back())->getValue();
    ResultElts.back() = ConstantInt::get(Context, ValForPrevField | LastElt);

    // If the whole bit-field fit into the previous field, we're done.
    if (ValC == 0) return;
    GCCFieldOffsetInBits = NextFieldByteStart*8;
  }

  APInt Val = ValC->getValue();

  // Okay, we know that we're plopping bytes onto the end of the struct.
  // Iterate while there is stuff to do.
  while (1) {
    ConstantInt *ValToAppend;
    if (Val.getBitWidth() > 8) {
      if (!BYTES_BIG_ENDIAN) {
        // Little endian lays out low bits first.
        APInt Tmp = Val.trunc(8);
        ValToAppend = ConstantInt::get(Context, Tmp);

        Val = Val.lshr(8);
      } else {
        // Big endian lays out high bits first.
        APInt Tmp = Val.lshr(Val.getBitWidth()-8).trunc(8);
        ValToAppend = ConstantInt::get(Context, Tmp);
      }
    } else if (Val.getBitWidth() == 8) {
      ValToAppend = ConstantInt::get(Context, Val);
    } else {
      APInt Tmp = Val.zext(8);

      if (BYTES_BIG_ENDIAN)
        Tmp = Tmp << 8-Val.getBitWidth();
      ValToAppend = ConstantInt::get(Context, Tmp);
    }

    ResultElts.push_back(ValToAppend);
    ++NextFieldByteStart;

    if (Val.getBitWidth() <= 8)
      break;
    Val = Val.trunc(Val.getBitWidth()-8);
  }
}


/// HandleTailPadding - Check to see if the struct fields, as laid out so far,
/// will be large enough to make the generated constant struct have the right
/// size.  If not, add explicit tail padding.  If rounding up based on the LLVM
/// IR alignment would make the struct too large, convert it to a packed LLVM
/// struct.
void ConstantLayoutInfo::HandleTailPadding(uint64_t GCCStructBitSize) {
  uint64_t GCCStructSize = (GCCStructBitSize+7)/8;
  uint64_t LLVMNaturalSize =
    TargetData::RoundUpAlignment(NextFieldByteStart, MaxLLVMFieldAlignment);

  // If the total size of the laid out data is within the size of the GCC type
  // but the rounded-up size (including the tail padding induced by LLVM
  // alignment) is too big, convert to a packed struct type.  We don't do this
  // if the size of the laid out fields is too large because initializers like
  //
  //    struct X { int A; char C[]; } x = { 4, "foo" };
  //
  // can occur and no amount of packing will help.
  if (NextFieldByteStart <= GCCStructSize &&   // Not flexible init case.
      LLVMNaturalSize > GCCStructSize) {       // Tail pad will overflow type.
    assert(!StructIsPacked && "LLVM Struct type overflow!");

    // Switch to packed.
    ConvertToPacked();
    LLVMNaturalSize = NextFieldByteStart;

    // Verify that packing solved the problem.
    assert(LLVMNaturalSize <= GCCStructSize &&
           "Oversized should be handled by packing");
  }

  // If the LLVM Size is too small, add some tail padding to fill it in.
  if (LLVMNaturalSize < GCCStructSize) {
    const Type *FillTy = Type::getInt8Ty(Context);
    if (GCCStructSize - NextFieldByteStart != 1)
      FillTy = ArrayType::get(FillTy, GCCStructSize - NextFieldByteStart);
    ResultElts.push_back(Constant::getNullValue(FillTy));
    NextFieldByteStart = GCCStructSize;

    // At this point, we know that our struct should have the right size.
    // However, if the size of the struct is not a multiple of the largest
    // element alignment, the rounding could bump up the struct more.  In this
    // case, we have to convert the struct to being packed.
    LLVMNaturalSize =
      TargetData::RoundUpAlignment(NextFieldByteStart, MaxLLVMFieldAlignment);

    // If the alignment will make the struct too big, convert it to being
    // packed.
    if (LLVMNaturalSize > GCCStructSize) {
      assert(!StructIsPacked && "LLVM Struct type overflow!");
      ConvertToPacked();
    }
  }
}

static Constant *ConvertRecordCONSTRUCTOR(tree exp) {
  ConstantLayoutInfo LayoutInfo(getTargetData());

  tree NextField = TYPE_FIELDS(TREE_TYPE(exp));
  unsigned HOST_WIDE_INT CtorIndex;
  tree FieldValue;
  tree Field; // The FIELD_DECL for the field.
  FOR_EACH_CONSTRUCTOR_ELT(CONSTRUCTOR_ELTS(exp), CtorIndex, Field, FieldValue){
    // If an explicit field is specified, use it.
    if (Field == 0) {
      Field = NextField;
      // Advance to the next FIELD_DECL, skipping over other structure members
      // (e.g. enums).
      while (1) {
        assert(Field && "Fell off end of record!");
        if (TREE_CODE(Field) == FIELD_DECL) break;
        Field = TREE_CHAIN(Field);
      }
    }

    // Decode the field's value.
    Constant *Val = ConvertConstant(FieldValue);

    // GCCFieldOffsetInBits is where GCC is telling us to put the current field.
    uint64_t GCCFieldOffsetInBits = getFieldOffsetInBits(Field);
    NextField = TREE_CHAIN(Field);

    uint64_t FieldSizeInBits = 0;
    if (DECL_SIZE(Field))
      FieldSizeInBits = getInt64(DECL_SIZE(Field), true);
    uint64_t ValueSizeInBits = Val->getType()->getPrimitiveSizeInBits();
    ConstantInt *ValC = dyn_cast<ConstantInt>(Val);
    if (ValC && ValC->isZero() && DECL_SIZE(Field)) {
      // G++ has various bugs handling {} initializers where it doesn't
      // synthesize a zero node of the right type.  Instead of figuring out G++,
      // just hack around it by special casing zero and allowing it to be the
      // wrong size.
      if (ValueSizeInBits != FieldSizeInBits) {
        APInt ValAsInt = ValC->getValue();
        ValC = ConstantInt::get(Context, ValueSizeInBits < FieldSizeInBits ?
                                         ValAsInt.zext(FieldSizeInBits) :
                                         ValAsInt.trunc(FieldSizeInBits));
        ValueSizeInBits = FieldSizeInBits;
        Val = ValC;
      }
    }

    // If this is a non-bitfield value, just slap it onto the end of the struct
    // with the appropriate padding etc.  If it is a bitfield, we have more
    // processing to do.
    if (!isBitfield(Field))
      LayoutInfo.AddFieldToRecordConstant(Val, GCCFieldOffsetInBits);
    else {
      // Bitfields can only be initialized with constants (integer constant
      // expressions).
      assert(ValC);
      assert(DECL_SIZE(Field));
      assert(ValueSizeInBits >= FieldSizeInBits &&
             "disagreement between LLVM and GCC on bitfield size");
      if (ValueSizeInBits != FieldSizeInBits) {
        // Fields are allowed to be smaller than their type.  Simply discard
        // the unwanted upper bits in the field value.
        APInt ValAsInt = ValC->getValue();
        ValC = ConstantInt::get(Context, ValAsInt.trunc(FieldSizeInBits));
      }
      LayoutInfo.AddBitFieldToRecordConstant(ValC, GCCFieldOffsetInBits);
    }
  }

  // Check to see if the struct fields, as laid out so far, will be large enough
  // to make the generated constant struct have the right size.  If not, add
  // explicit tail padding.  If rounding up based on the LLVM IR alignment would
  // make the struct too large, convert it to a packed LLVM struct.
  tree StructTypeSizeTree = TYPE_SIZE(TREE_TYPE(exp));
  if (StructTypeSizeTree && TREE_CODE(StructTypeSizeTree) == INTEGER_CST)
    LayoutInfo.HandleTailPadding(getInt64(StructTypeSizeTree, true));

  // Okay, we're done, return the computed elements.
  return ConstantStruct::get(Context, LayoutInfo.ResultElts,
                             LayoutInfo.StructIsPacked);
}

static Constant *ConvertUnionCONSTRUCTOR(tree exp) {
  assert(!VEC_empty(constructor_elt, CONSTRUCTOR_ELTS(exp))
         && "Union CONSTRUCTOR has no elements? Zero?");

  VEC(constructor_elt, gc) *elt = CONSTRUCTOR_ELTS(exp);
  assert(VEC_length(constructor_elt, elt) == 1
         && "Union CONSTRUCTOR with multiple elements?");

  ConstantLayoutInfo LayoutInfo(getTargetData());

  // Convert the constant itself.
  Constant *Val = ConvertConstant(VEC_index(constructor_elt, elt, 0)->value);

  // Unions are initialized using the first member field.  Find it.
  tree Field = TYPE_FIELDS(TREE_TYPE(exp));
  assert(Field && "cannot initialize union with no fields");
  while (TREE_CODE(Field) != FIELD_DECL) {
    Field = TREE_CHAIN(Field);
    assert(Field && "cannot initialize union with no fields");
  }

  // If this is a non-bitfield value, just slap it onto the end of the struct
  // with the appropriate padding etc.  If it is a bitfield, we have more
  // processing to do.
  if (!isBitfield(Field))
    LayoutInfo.AddFieldToRecordConstant(Val, 0);
  else {
    // Bitfields can only be initialized with constants (integer constant
    // expressions).
    ConstantInt *ValC = cast<ConstantInt>(Val);
    uint64_t FieldSizeInBits = getInt64(DECL_SIZE(Field), true);
    uint64_t ValueSizeInBits = Val->getType()->getPrimitiveSizeInBits();

    assert(ValueSizeInBits >= FieldSizeInBits &&
           "disagreement between LLVM and GCC on bitfield size");
    if (ValueSizeInBits != FieldSizeInBits) {
      // Fields are allowed to be smaller than their type.  Simply discard
      // the unwanted upper bits in the field value.
      APInt ValAsInt = ValC->getValue();
      ValC = ConstantInt::get(Context, ValAsInt.trunc(FieldSizeInBits));
    }
    LayoutInfo.AddBitFieldToRecordConstant(ValC, 0);
  }

  // If the union has a fixed size, and if the value we converted isn't large
  // enough to fill all the bits, add a zero initialized array at the end to pad
  // it out.
  tree UnionTypeSizeTree = TYPE_SIZE(TREE_TYPE(exp));
  if (UnionTypeSizeTree && TREE_CODE(UnionTypeSizeTree) == INTEGER_CST)
    LayoutInfo.HandleTailPadding(getInt64(UnionTypeSizeTree, true));

  return ConstantStruct::get(Context, LayoutInfo.ResultElts,
                             LayoutInfo.StructIsPacked);
}

static Constant *ConvertCONSTRUCTOR(tree exp) {
  // Please note, that we can have empty ctor, even if array is non-trivial (has
  // nonzero number of entries). This situation is typical for static ctors,
  // when array is filled during program initialization.
  if (CONSTRUCTOR_ELTS(exp) == 0 ||
      VEC_length(constructor_elt, CONSTRUCTOR_ELTS(exp)) == 0)  // All zeros?
    return Constant::getNullValue(ConvertType(TREE_TYPE(exp)));

  switch (TREE_CODE(TREE_TYPE(exp))) {
  default:
    debug_tree(exp);
    assert(0 && "Unknown ctor!");
  case VECTOR_TYPE:
  case ARRAY_TYPE:  return ConvertArrayCONSTRUCTOR(exp);
  case RECORD_TYPE: return ConvertRecordCONSTRUCTOR(exp);
  case QUAL_UNION_TYPE:
  case UNION_TYPE:  return ConvertUnionCONSTRUCTOR(exp);
  }
}

Constant *ConvertConstant(tree exp) {
  assert(TREE_CONSTANT(exp) && "Isn't a constant!");
  switch (TREE_CODE(exp)) {
  case FDESC_EXPR:    // Needed on itanium
  default:
    debug_tree(exp);
    assert(0 && "Unknown constant to convert!");
    abort();
  case INTEGER_CST:   return ConvertINTEGER_CST(exp);
  case REAL_CST:      return ConvertREAL_CST(exp);
  case VECTOR_CST:    return ConvertVECTOR_CST(exp);
  case STRING_CST:    return ConvertSTRING_CST(exp);
  case COMPLEX_CST:   return ConvertCOMPLEX_CST(exp);
  case NOP_EXPR:      return ConvertNOP_EXPR(exp);
  case CONVERT_EXPR:  return ConvertCONVERT_EXPR(exp);
  case PLUS_EXPR:
  case MINUS_EXPR:    return ConvertBinOp_CST(exp);
  case CONSTRUCTOR:   return ConvertCONSTRUCTOR(exp);
  case VIEW_CONVERT_EXPR: return ConvertConstant(TREE_OPERAND(exp, 0));
  case POINTER_PLUS_EXPR: return ConvertPOINTER_PLUS_EXPR(exp);
  case ADDR_EXPR:
    return TheFolder->CreateBitCast(AddressOf(TREE_OPERAND(exp, 0)),
                                    ConvertType(TREE_TYPE(exp)));
  }
}

/// get_constant_alignment - Return the alignment of constant EXP in bits.
static unsigned int
get_constant_alignment (tree exp)
{
    unsigned int align = TYPE_ALIGN (TREE_TYPE (exp));
#ifdef CONSTANT_ALIGNMENT
      align = CONSTANT_ALIGNMENT (exp, align);
#endif
        return align;
}

static Constant *AddressOfDecl(tree exp) {
  GlobalValue *Val = cast<GlobalValue>(DEFINITION_LLVM(exp));

  // The type of the global value output for exp need not match that of exp.
  // For example if the global's initializer has a different type to the global
  // itself (allowed in GCC but not in LLVM) then the global is changed to have
  // the type of the initializer.  Correct for this now.
  const Type *Ty = ConvertType(TREE_TYPE(exp));
  if (Ty->isVoidTy()) Ty = Type::getInt8Ty(Context);  // void* -> i8*.

  return TheFolder->CreateBitCast(Val, Ty->getPointerTo());
}

/// AddressOfLABEL_DECL - Someone took the address of a label.
static Constant *AddressOfLABEL_DECL(tree exp) {
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

static Constant *AddressOfCOMPLEX_CST(tree exp) {
  Constant *Init = ConvertCOMPLEX_CST(exp);

  // Cache the constants to avoid making obvious duplicates that have to be
  // folded by the optimizer.
  static std::map<Constant*, GlobalVariable*> ComplexCSTCache;
  GlobalVariable *&Slot = ComplexCSTCache[Init];
  if (Slot) return Slot;

  // Create a new complex global.
  Slot = new GlobalVariable(*TheModule, Init->getType(), true,
                            GlobalVariable::PrivateLinkage, Init, ".cpx");
  Slot->setAlignment(get_constant_alignment(exp) / 8);

  return Slot;
}

static Constant *AddressOfREAL_CST(tree exp) {
  Constant *Init = ConvertREAL_CST(exp);

  // Cache the constants to avoid making obvious duplicates that have to be
  // folded by the optimizer.
  static std::map<Constant*, GlobalVariable*> RealCSTCache;
  GlobalVariable *&Slot = RealCSTCache[Init];
  if (Slot) return Slot;

  // Create a new real global.
  Slot = new GlobalVariable(*TheModule, Init->getType(), true,
                            GlobalVariable::PrivateLinkage, Init, ".rl");
  Slot->setAlignment(get_constant_alignment(exp) / 8);

  return Slot;
}

static Constant *AddressOfSTRING_CST(tree exp) {
  Constant *Init = ConvertSTRING_CST(exp);

  GlobalVariable **SlotP = 0;

  // Cache the string constants to avoid making obvious duplicate strings that
  // have to be folded by the optimizer.
  static std::map<Constant*, GlobalVariable*> StringCSTCache;
  GlobalVariable *&Slot = StringCSTCache[Init];
  if (Slot) return Slot;
  SlotP = &Slot;

  // Create a new string global.
  GlobalVariable *GV = new GlobalVariable(*TheModule, Init->getType(), true,
                                          GlobalVariable::PrivateLinkage, Init,
                                          ".str");
  GV->setAlignment(get_constant_alignment(exp) / 8);

  if (SlotP) *SlotP = GV;
  return GV;
}

static Constant *AddressOfARRAY_REF(tree exp) {
  tree Array = TREE_OPERAND(exp, 0);
  tree Index = TREE_OPERAND(exp, 1);
  tree IndexType = TREE_TYPE(Index);
  assert(TREE_CODE(TREE_TYPE(Array)) == ARRAY_TYPE && "Unknown ARRAY_REF!");

  // Check for variable sized reference.
  // FIXME: add support for array types where the size doesn't fit into 64 bits
  assert(isSequentialCompatible(TREE_TYPE(Array)) &&
         "Global with variable size?");

  // First subtract the lower bound, if any, in the type of the index.
  Constant *IndexVal = ConvertConstant(Index);
  tree LowerBound = array_ref_low_bound(exp);
  if (!integer_zerop(LowerBound))
    IndexVal = TheFolder->CreateSub(IndexVal, ConvertConstant(LowerBound),
                                    hasNUW(TREE_TYPE(Index)),
                                    hasNSW(TREE_TYPE(Index)));

  const Type *IntPtrTy = getTargetData().getIntPtrType(Context);
  IndexVal = TheFolder->CreateIntCast(IndexVal, IntPtrTy,
                                      /*isSigned*/!TYPE_UNSIGNED(IndexType));

  // Avoid any assumptions about how the array type is represented in LLVM by
  // doing the GEP on a pointer to the first array element.
  Constant *ArrayAddr = AddressOf(Array);
  const Type *EltTy = ConvertType(TREE_TYPE(TREE_TYPE(Array)));
  ArrayAddr = TheFolder->CreateBitCast(ArrayAddr, EltTy->getPointerTo());

  return POINTER_TYPE_OVERFLOW_UNDEFINED ?
    TheFolder->CreateInBoundsGetElementPtr(ArrayAddr, &IndexVal, 1) :
    TheFolder->CreateGetElementPtr(ArrayAddr, &IndexVal, 1);
}

static Constant *AddressOfCOMPONENT_REF(tree exp) {
  Constant *StructAddrLV = AddressOf(TREE_OPERAND(exp, 0));

  tree FieldDecl = TREE_OPERAND(exp, 1);
  const Type *StructTy = ConvertType(DECL_CONTEXT(FieldDecl));

  StructAddrLV = TheFolder->CreateBitCast(StructAddrLV,
                                          StructTy->getPointerTo());
  const Type *FieldTy = ConvertType(TREE_TYPE(FieldDecl));

  // BitStart - This is the actual offset of the field from the start of the
  // struct, in bits.  For bitfields this may be on a non-byte boundary.
  unsigned BitStart;
  Constant *FieldPtr;

  // If the GCC field directly corresponds to an LLVM field, handle it.
  unsigned MemberIndex = GetFieldIndex(FieldDecl, StructTy);
  if (MemberIndex < INT_MAX) {
    // Get a pointer to the byte in which the GCC field starts.
    Constant *Ops[] = {
      Constant::getNullValue(Type::getInt32Ty(Context)),
      ConstantInt::get(Type::getInt32Ty(Context), MemberIndex)
    };
    FieldPtr = TheFolder->CreateInBoundsGetElementPtr(StructAddrLV, Ops, 2);
    // Within that byte, the bit at which the GCC field starts.
    BitStart = TREE_INT_CST_LOW(DECL_FIELD_BIT_OFFSET(TREE_OPERAND(exp, 1)));
    BitStart &= 7;
  } else {
    // Offset will hold the field offset in octets.
    Constant *Offset;

    assert(!(BITS_PER_UNIT & 7) && "Unit size not a multiple of 8 bits!");
    if (TREE_OPERAND(exp, 2)) {
      Offset = ConvertConstant(TREE_OPERAND(exp, 2));
      // At this point the offset is measured in units divided by (exactly)
      // (DECL_OFFSET_ALIGN / BITS_PER_UNIT).  Convert to octets.
      unsigned factor = DECL_OFFSET_ALIGN(FieldDecl) / 8;
      if (factor != 1)
        Offset = TheFolder->CreateMul(Offset,
                                      ConstantInt::get(Offset->getType(),
                                                       factor));
    } else {
      assert(DECL_FIELD_OFFSET(FieldDecl) && "Field offset not available!");
      Offset = ConvertConstant(DECL_FIELD_OFFSET(FieldDecl));
      // At this point the offset is measured in units.  Convert to octets.
      unsigned factor = BITS_PER_UNIT / 8;
      if (factor != 1)
        Offset = TheFolder->CreateMul(Offset,
                                      ConstantInt::get(Offset->getType(),
                                                       factor));
    }

    // Here BitStart gives the offset of the field in bits from Offset.
    BitStart = getInt64(DECL_FIELD_BIT_OFFSET(FieldDecl), true);
    // Incorporate as much of it as possible into the pointer computation.
    unsigned ByteOffset = BitStart/8;
    if (ByteOffset > 0) {
      Offset = TheFolder->CreateAdd(Offset,
                                    ConstantInt::get(Offset->getType(),
                                                     ByteOffset));
      BitStart -= ByteOffset*8;
    }

    const Type *BytePtrTy = Type::getInt8PtrTy(Context);
    FieldPtr = TheFolder->CreateBitCast(StructAddrLV, BytePtrTy);
    FieldPtr = TheFolder->CreateInBoundsGetElementPtr(FieldPtr, &Offset, 1);
    FieldPtr = TheFolder->CreateBitCast(FieldPtr, FieldTy->getPointerTo());
  }

  // Make sure we return a pointer to the right type.
  const Type *EltTy = ConvertType(TREE_TYPE(exp));
  FieldPtr = TheFolder->CreateBitCast(FieldPtr, EltTy->getPointerTo());

  assert(BitStart == 0 &&
         "It's a bitfield reference or we didn't get to the field!");
  return FieldPtr;
}

Constant *AddressOf(tree exp) {
  Constant *LV;

  switch (TREE_CODE(exp)) {
  default:
    debug_tree(exp);
    assert(0 && "Unknown constant lvalue to convert!");
    abort();
  case FUNCTION_DECL:
  case CONST_DECL:
  case VAR_DECL:
    LV = AddressOfDecl(exp);
    break;
  case LABEL_DECL:
    LV = AddressOfLABEL_DECL(exp);
    break;
  case COMPLEX_CST:
    LV = AddressOfCOMPLEX_CST(exp);
    break;
  case REAL_CST:
    LV = AddressOfREAL_CST(exp);
    break;
  case STRING_CST:
    LV = AddressOfSTRING_CST(exp);
    break;
  case COMPONENT_REF:
    LV = AddressOfCOMPONENT_REF(exp);
    break;
  case ARRAY_RANGE_REF:
  case ARRAY_REF:
    LV = AddressOfARRAY_REF(exp);
    break;
  case INDIRECT_REF:
    // The lvalue is just the address.
    LV = ConvertConstant(TREE_OPERAND(exp, 0));
    break;
  case COMPOUND_LITERAL_EXPR: // FIXME: not gimple - defined by C front-end
    /* This used to read
       return AddressOf(COMPOUND_LITERAL_EXPR_DECL(exp));
       but gcc warns about that and there doesn't seem to be any way to stop it
       with casts or the like.  The following is equivalent with no checking
       (since we know TREE_CODE(exp) is COMPOUND_LITERAL_EXPR the checking
       doesn't accomplish anything anyway). */
    LV = AddressOf(DECL_EXPR_DECL (TREE_OPERAND (exp, 0)));
    break;
  }

  // Check that the type of the lvalue is indeed that of a pointer to the tree
  // node.  Since LLVM has no void* type, don't insist that void* be converted
  // to a specific LLVM type.
  assert(isa<PointerType>(LV->getType()) &&
         (VOID_TYPE_P(TREE_TYPE(exp)) ||
          cast<PointerType>(LV->getType())->getElementType() ==
          ConvertType(TREE_TYPE(exp))) && "Constant LValue has wrong type!");

  return LV;
}
