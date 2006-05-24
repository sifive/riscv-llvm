//===-- ARMISelDAGToDAG.cpp - A dag to dag inst selector for ARM ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by Chris Lattner and is distributed under
// the University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines an instruction selector for the ARM target.
//
//===----------------------------------------------------------------------===//

#include "ARM.h"
#include "ARMTargetMachine.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Intrinsics.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/SSARegMap.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Support/Debug.h"
#include <iostream>
#include <set>
using namespace llvm;

namespace ARMISD {
  enum {
    FIRST_NUMBER = ISD::BUILTIN_OP_END+ARM::INSTRUCTION_LIST_END,
    RET_FLAG
  };
}

namespace {
  class ARMTargetLowering : public TargetLowering {
  public:
    ARMTargetLowering(TargetMachine &TM);
    virtual SDOperand LowerOperation(SDOperand Op, SelectionDAG &DAG);

    virtual std::pair<SDOperand, SDOperand>
      LowerCallTo(SDOperand Chain, const Type *RetTy, bool isVarArg,
                  unsigned CC,
                  bool isTailCall, SDOperand Callee, ArgListTy &Args,
                  SelectionDAG &DAG);

  };

}

ARMTargetLowering::ARMTargetLowering(TargetMachine &TM)
  : TargetLowering(TM) {
  setOperationAction(ISD::RET, MVT::Other, Custom);
}

std::pair<SDOperand, SDOperand>
ARMTargetLowering::LowerCallTo(SDOperand Chain, const Type *RetTy,
                                 bool isVarArg, unsigned CC,
                                 bool isTailCall, SDOperand Callee,
                                 ArgListTy &Args, SelectionDAG &DAG) {
  assert(0 && "Not implemented");
  abort();
}

static SDOperand LowerRET(SDOperand Op, SelectionDAG &DAG) {
  SDOperand Copy;
  switch(Op.getNumOperands()) {
  default:
    assert(0 && "Do not know how to return this many arguments!");
    abort();
  case 1:
    return SDOperand(); // ret void is legal
  case 2:
    Copy = DAG.getCopyToReg(Op.getOperand(0), ARM::R0, Op.getOperand(1), SDOperand());
    break;
  }

  return DAG.getNode(ARMISD::RET_FLAG, MVT::Other, Copy, Copy.getValue(1));
}

static SDOperand LowerFORMAL_ARGUMENTS(SDOperand Op, SelectionDAG &DAG) {
  MachineFunction &MF = DAG.getMachineFunction();
  SSARegMap *RegMap = MF.getSSARegMap();
  std::vector<SDOperand> ArgValues;
  SDOperand Root = Op.getOperand(0);

  unsigned reg_idx = 0;
  unsigned num_regs = 4;

  static const unsigned REGS[] = {
    ARM::R0, ARM::R1, ARM::R2, ARM::R3
  };

  for (unsigned ArgNo = 0, e = Op.Val->getNumValues()-1; ArgNo != e; ++ArgNo) {
    SDOperand ArgVal;

    MVT::ValueType ObjectVT = Op.getValue(ArgNo).getValueType();
    assert (ObjectVT == MVT::i32);

    assert(reg_idx < num_regs);
    unsigned VReg = RegMap->createVirtualRegister(&ARM::IntRegsRegClass);
    MF.addLiveIn(REGS[reg_idx], VReg);
    ArgVal = DAG.getCopyFromReg(Root, VReg, MVT::i32);
    ++reg_idx;

    ArgValues.push_back(ArgVal);
  }

  bool isVarArg = cast<ConstantSDNode>(Op.getOperand(2))->getValue() != 0;
  assert(!isVarArg);

  ArgValues.push_back(Root);

  // Return the new list of results.
  std::vector<MVT::ValueType> RetVT(Op.Val->value_begin(),
                                    Op.Val->value_end());
  return DAG.getNode(ISD::MERGE_VALUES, RetVT, ArgValues);
}

SDOperand ARMTargetLowering::LowerOperation(SDOperand Op, SelectionDAG &DAG) {
  switch (Op.getOpcode()) {
  default:
    assert(0 && "Should not custom lower this!");
    abort();
  case ISD::FORMAL_ARGUMENTS:
    return LowerFORMAL_ARGUMENTS(Op, DAG);
  case ISD::RET:
    return LowerRET(Op, DAG);
  }
}

//===----------------------------------------------------------------------===//
// Instruction Selector Implementation
//===----------------------------------------------------------------------===//

//===--------------------------------------------------------------------===//
/// ARMDAGToDAGISel - ARM specific code to select ARM machine
/// instructions for SelectionDAG operations.
///
namespace {
class ARMDAGToDAGISel : public SelectionDAGISel {
  ARMTargetLowering Lowering;

public:
  ARMDAGToDAGISel(TargetMachine &TM)
    : SelectionDAGISel(Lowering), Lowering(TM) {
  }

  void Select(SDOperand &Result, SDOperand Op);
  virtual void InstructionSelectBasicBlock(SelectionDAG &DAG);

  // Include the pieces autogenerated from the target description.
#include "ARMGenDAGISel.inc"
};

void ARMDAGToDAGISel::InstructionSelectBasicBlock(SelectionDAG &DAG) {
  DEBUG(BB->dump());

  DAG.setRoot(SelectRoot(DAG.getRoot()));
  CodeGenMap.clear();
  HandleMap.clear();
  ReplaceMap.clear();
  DAG.RemoveDeadNodes();

  ScheduleAndEmitDAG(DAG);
}

void ARMDAGToDAGISel::Select(SDOperand &Result, SDOperand Op) {
  SelectCode(Result, Op);
}

}  // end anonymous namespace

/// createARMISelDag - This pass converts a legalized DAG into a
/// ARM-specific DAG, ready for instruction scheduling.
///
FunctionPass *llvm::createARMISelDag(TargetMachine &TM) {
  return new ARMDAGToDAGISel(TM);
}
