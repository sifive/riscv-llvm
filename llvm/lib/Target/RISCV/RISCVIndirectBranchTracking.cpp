//===---- RISCVIndirectBranchTracking.cpp - Enables CET IBT mechanism -----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "RISCV.h"
#include "RISCVInstrInfo.h"
#include "RISCVSubtarget.h"
#include "RISCVTargetMachine.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"

using namespace llvm;

namespace {
class RISCVIndirectBranchTrackingPass : public MachineFunctionPass {
public:
  RISCVIndirectBranchTrackingPass() : MachineFunctionPass(ID) {}

  StringRef getPassName() const override {
    return "RISCV Indirect Branch Tracking";
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

private:
  static char ID;
  Align LpadAlign = Align(4);
};

} // end anonymous namespace

char RISCVIndirectBranchTrackingPass::ID = 0;

FunctionPass *llvm::createRISCVIndirectBranchTrackingPass() {
  return new RISCVIndirectBranchTrackingPass();
}

static void emitLpad(MachineBasicBlock &MBB, const RISCVInstrInfo *TII) {
  auto I = MBB.begin();
  BuildMI(MBB, I, MBB.findDebugLoc(I), TII->get(RISCV::AUIPC), RISCV::X0)
    .addImm(1);
}

bool
RISCVIndirectBranchTrackingPass::runOnMachineFunction(MachineFunction &MF) {
  const auto &Subtarget = MF.getSubtarget<RISCVSubtarget>();
  const RISCVInstrInfo *TII = Subtarget.getInstrInfo();
  if (!Subtarget.hasStdExtZicfilp())
    return false;

  bool Changed = false;
  for (MachineBasicBlock &MBB : MF) {
    if (&MBB == &MF.front()) {
      Function &F = MF.getFunction();
      if (F.hasAddressTaken() || !F.hasLocalLinkage()) {
        emitLpad(MBB, TII);
        if (MF.getAlignment() < LpadAlign)
          MF.setAlignment(LpadAlign);
        Changed = true;
      }
      continue;
    }

    if (MBB.hasAddressTaken()) {
      emitLpad(MBB, TII);
      if (MBB.getAlignment() < LpadAlign)
        MBB.setAlignment(LpadAlign);
      Changed = true;
    }
  }

  return Changed;
}
