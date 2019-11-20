//===-- RISCVInstrInfoV.td - RISC-V 'V' instructions -------*- tablegen -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file describes the RISC-V instructions from the standard 'V',
// Vector instruction set extension.
//
//===----------------------------------------------------------------------===//

include "RISCVInstrFormatsV.td"

//===----------------------------------------------------------------------===//
// Operand and SDNode transformation definitions.
//===----------------------------------------------------------------------===//

def VTypeIAsmOperand : AsmOperandClass {
  let Name = "VTypeI";
  let ParserMethod = "parseVTypeIAsmOperand";
  let DiagnosticType = "InvalidVTypeIAsmOperand";
}

def VTypeIOp : Operand<XLenVT> {
  let ParserMatchClass = VTypeIAsmOperand;
  let PrintMethod = "printVTypeI";
  let DecoderMethod = "decodeUImmOperand<11>";
}

def VRAsmOperand : AsmOperandClass {
  let Name = "RVVRegOpOperand";
  let RenderMethod = "addRegOperands";
  let PredicateMethod = "isReg";
  let ParserMethod = "parseRegister";
}

def VRRegOp: RegisterOperand<VR> {
  let ParserMatchClass = VRAsmOperand;
  let PrintMethod = "printOperand";
}

def VRMaskAsmOperand : AsmOperandClass {
  let Name = "RVVMaskRegOpOperand";
  let RenderMethod = "addRegOperands";
  let PredicateMethod = "isV0Reg";
  let ParserMethod = "parseMaskRegister";
  let IsOptional = 1;
  let DefaultMethod = "defaultRVVMaskRegOp";
  let DiagnosticType = "InvalidVRMaskAsmOperand";
}

def VRMaskRegOp: RegisterOperand<VRV0> {
  let ParserMatchClass = VRMaskAsmOperand;
  let PrintMethod = "printVRMaskOp";
  let EncoderMethod = "getVRMaskOp";
  let DecoderMethod = "decodeVRMaskOp";
}

def simm5 : Operand<XLenVT>, ImmLeaf<XLenVT, [{return isInt<5>(Imm);}]> {
  let ParserMatchClass = SImmAsmOperand<5>;
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeSImmOperand<5>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isInt<5>(Imm);
    return MCOp.isBareSymbolRef();
  }];
}

//===----------------------------------------------------------------------===//
// Instruction class templates
//===----------------------------------------------------------------------===//

let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in {
// load vd, (rs1), vm
class VUnitStrideLoad<RISCVMOP mop, RISCVLSUMOP lumop, RISCVWidth width,
                        string opcodestr>
    : RVInstVLU<0b000, mop, lumop, width, (outs VRRegOp:$vd),
                (ins GPR:$rs1, VRMaskRegOp:$vm), opcodestr, "$vd, (${rs1})${vm}">;

// load vd, (rs1), rs2, vm
class VStridedLoad<RISCVMOP mop, RISCVWidth width, string opcodestr>
    : RVInstVLS<0b000, mop, width, (outs VRRegOp:$vd),
                (ins GPR:$rs1, GPR:$rs2, VRMaskRegOp:$vm), opcodestr,
                "$vd, (${rs1}), $rs2${vm}">;

// load vd, (rs1), vs2, vm
class VIndexedLoad<RISCVMOP mop, RISCVWidth width, string opcodestr>
    : RVInstVLX<0b000, mop, width, (outs VRRegOp:$vd),
                (ins GPR:$rs1, VRRegOp:$vs2, VRMaskRegOp:$vm), opcodestr,
                "$vd, (${rs1}), $vs2${vm}">;
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in {
// store vd, vs3, (rs1), vm
class VUnitStrideStore<RISCVMOP mop, RISCVLSUMOP sumop, RISCVWidth width,
                         string opcodestr>
    : RVInstVSU<0b000, mop, sumop, width, (outs),
                (ins VRRegOp:$vs3, GPR:$rs1, VRMaskRegOp:$vm), opcodestr,
                "$vs3, (${rs1})${vm}">;

// store vd, vs3, (rs1), rs2, vm
class VStridedStore<RISCVMOP mop, RISCVWidth width, string opcodestr>
    : RVInstVSS<0b000, mop, width, (outs),
                (ins VRRegOp:$vs3, GPR:$rs1, GPR:$rs2, VRMaskRegOp:$vm),
                opcodestr, "$vs3, (${rs1}), $rs2${vm}">;

// store vd, vs3, (rs1), vs2, vm
class VIndexedStore<RISCVMOP mop, RISCVWidth width, string opcodestr>
    : RVInstVSX<0b000, mop, width, (outs),
                (ins VRRegOp:$vs3, GPR:$rs1, VRRegOp:$vs2, VRMaskRegOp:$vm),
                opcodestr, "$vs3, (${rs1}), $vs2${vm}">;
}

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
// op vd, vs2, vs1, vm
class VALUVV<bits<6> funct6, RISCVVFormat opv, string opcodestr>
    : RVInstVV<funct6, opv, (outs VRRegOp:$vd),
                (ins VRRegOp:$vs2, VRRegOp:$vs1, VRMaskRegOp:$vm),
                opcodestr, "$vd, $vs2, $vs1${vm}">;

// op vd, vs2, vs1, v0 (without mask, use v0 as carry input)
class VALUmVV<bits<6> funct6, RISCVVFormat opv, string opcodestr>
    : RVInstVV<funct6, opv, (outs VRRegOp:$vd),
                (ins VRRegOp:$vs2, VRRegOp:$vs1, VRV0:$v0),
                opcodestr, "$vd, $vs2, $vs1, v0"> {
  let vm = 0;
}

// op vd, vs1, vs2, vm (reverse the order of vs1 and vs2)
class VALUrVV<bits<6> funct6, RISCVVFormat opv, string opcodestr>
    : RVInstVV<funct6, opv, (outs VRRegOp:$vd),
                (ins VRRegOp:$vs1, VRRegOp:$vs2, VRMaskRegOp:$vm),
                opcodestr, "$vd, $vs1, $vs2${vm}">;

// op vd, vs1, vs2
class VALUVVNoVm<bits<6> funct6, RISCVVFormat opv, string opcodestr>
    : RVInstVV<funct6, opv, (outs VRRegOp:$vd),
               (ins VRRegOp:$vs2, VRRegOp:$vs1),
               opcodestr, "$vd, $vs2, $vs1"> {
  let vm = 1;
}

// op vd, vs2, rs1, vm
class VALUVX<bits<6> funct6, RISCVVFormat opv, string opcodestr>
    : RVInstVX<funct6, opv, (outs VRRegOp:$vd),
                (ins VRRegOp:$vs2, GPR:$rs1, VRMaskRegOp:$vm),
                opcodestr, "$vd, $vs2, $rs1${vm}">;

// op vd, vs2, rs1, v0 (without mask, use v0 as carry input)
class VALUmVX<bits<6> funct6, RISCVVFormat opv, string opcodestr>
    : RVInstVX<funct6, opv, (outs VRRegOp:$vd),
                (ins VRRegOp:$vs2, GPR:$rs1, VRV0:$v0),
                opcodestr, "$vd, $vs2, $rs1, v0"> {
  let vm = 0;
}

// op vd, rs1, vs2, vm (reverse the order of rs1 and vs2)
class VALUrVX<bits<6> funct6, RISCVVFormat opv, string opcodestr>
    : RVInstVX<funct6, opv, (outs VRRegOp:$vd),
                (ins GPR:$rs1, VRRegOp:$vs2, VRMaskRegOp:$vm),
                opcodestr, "$vd, $rs1, $vs2${vm}">;

// op vd, vs1, vs2
class VALUVXNoVm<bits<6> funct6, RISCVVFormat opv, string opcodestr>
    : RVInstVX<funct6, opv, (outs VRRegOp:$vd),
               (ins VRRegOp:$vs2, GPR:$rs1),
               opcodestr, "$vd, $vs2, $rs1"> {
  let vm = 1;
}

// op vd, vs2, imm, vm
class VALUVI<bits<6> funct6, string opcodestr, Operand optype = simm5>
    : RVInstIVI<funct6, (outs VRRegOp:$vd),
                (ins VRRegOp:$vs2, optype:$imm, VRMaskRegOp:$vm),
                opcodestr, "$vd, $vs2, $imm${vm}">;

// op vd, vs2, imm, v0 (without mask, use v0 as carry input)
class VALUmVI<bits<6> funct6, string opcodestr, Operand optype = simm5>
    : RVInstIVI<funct6, (outs VRRegOp:$vd),
                (ins VRRegOp:$vs2, optype:$imm, VRV0:$v0),
                opcodestr, "$vd, $vs2, $imm, v0"> {
  let vm = 0;
}

// op vd, vs2, imm, vm
class VALUVINoVm<bits<6> funct6, string opcodestr, Operand optype = simm5>
    : RVInstIVI<funct6, (outs VRRegOp:$vd),
                (ins VRRegOp:$vs2, optype:$imm),
                opcodestr, "$vd, $vs2, $imm"> {
  let vm = 1;
}

// op vd, vs2, rs1, vm (Float)
class VALUVF<bits<6> funct6, RISCVVFormat opv, string opcodestr>
    : RVInstVX<funct6, opv, (outs VRRegOp:$vd),
                (ins VRRegOp:$vs2, FPR32:$rs1, VRMaskRegOp:$vm),
                opcodestr, "$vd, $vs2, $rs1${vm}">;

// op vd, rs1, vs2, vm (Float) (with mask, reverse the order of rs1 and vs2)
class VALUrVF<bits<6> funct6, RISCVVFormat opv, string opcodestr>
    : RVInstVX<funct6, opv, (outs VRRegOp:$vd),
                (ins FPR32:$rs1, VRRegOp:$vs2, VRMaskRegOp:$vm),
                opcodestr, "$vd, $rs1, $vs2${vm}">;

// op vd, vs2, vm (use vs1 as instruction encoding)
class VALUVs2<bits<6> funct6, bits<5> vs1, RISCVVFormat opv, string opcodestr>
    : RVInstV<funct6, vs1, opv, (outs VRRegOp:$vd),
               (ins VRRegOp:$vs2, VRMaskRegOp:$vm),
               opcodestr, "$vd, $vs2${vm}">;
}

//===----------------------------------------------------------------------===//
// Combination of instruction classes.
// Use these multiclasses to define instructions more easily.
//===----------------------------------------------------------------------===//
multiclass VALU_IV_V_X_I<string opcodestr, bits<6> funct6, Operand optype = simm5, string vw = "v"> {
  def V  : VALUVV<funct6, OPIVV, opcodestr # "." # vw # "v">;
  def X  : VALUVX<funct6, OPIVX, opcodestr # "." # vw # "x">;
  def I  : VALUVI<funct6, opcodestr # "." # vw # "i", optype>;
}

multiclass VALU_IV_V_X<string opcodestr, bits<6> funct6, string vw = "v"> {
  def V  : VALUVV<funct6, OPIVV, opcodestr # "." # vw # "v">;
  def X  : VALUVX<funct6, OPIVX, opcodestr # "." # vw # "x">;
}

multiclass VALUr_IV_V_X<string opcodestr, bits<6> funct6, string vw = "v"> {
  def V : VALUrVV<funct6, OPIVV, opcodestr # "." # vw # "v">;
  def X : VALUrVX<funct6, OPIVX, opcodestr # "." # vw # "x">;
}

multiclass VALU_IV_X_I<string opcodestr, bits<6> funct6, Operand optype = simm5, string vw = "v"> {
  def X  : VALUVX<funct6, OPIVX, opcodestr # "." # vw # "x">;
  def I  : VALUVI<funct6, opcodestr # "." # vw # "i", optype>;
}

multiclass VALU_IV_V<string opcodestr, bits<6> funct6> {
  def _VS  : VALUVV<funct6, OPIVV, opcodestr # ".vs">;
}

multiclass VALUr_IV_X<string opcodestr, bits<6> funct6, string vw = "v"> {
  def X : VALUrVX<funct6, OPIVX, opcodestr # "." # vw # "x">;
}

multiclass VALU_MV_V_X<string opcodestr, bits<6> funct6, string vw = "v"> {
  def V : VALUVV<funct6, OPMVV, opcodestr # "." # vw # "v">;
  def X  : VALUVX<funct6, OPMVX, opcodestr # "." # vw # "x">;
}

multiclass VALU_MV_V<string opcodestr, bits<6> funct6> {
  def _VS : VALUVV<funct6, OPMVV, opcodestr # ".vs">;
}

multiclass VALU_MV_Mask<string opcodestr, bits<6> funct6, string vm = "v"> {
  def M : VALUVVNoVm<funct6, OPMVV, opcodestr # "." # vm # "m">;
}

multiclass VALU_MV_X<string opcodestr, bits<6> funct6, string vw = "v"> {
  def X  : VALUVX<funct6, OPMVX, opcodestr # "." # vw # "x">;
}

multiclass VALUr_MV_V_X<string opcodestr, bits<6> funct6, string vw = "v"> {
  def V : VALUrVV<funct6, OPMVV, opcodestr # "." # vw # "v">;
  def X : VALUrVX<funct6, OPMVX, opcodestr # "." # vw # "x">;
}

multiclass VALUr_MV_X<string opcodestr, bits<6> funct6, string vw = "v"> {
  def X : VALUrVX<funct6, OPMVX, opcodestr # "." # vw # "x">;
}

multiclass VALU_MV_VS2<string opcodestr, bits<6> funct6, bits<5> vs1> {
  def "" : VALUVs2<funct6, vs1, OPMVV, opcodestr>;
}

multiclass VALUm_IV_V_X_I<string opcodestr, bits<6> funct6> {
  def VM : VALUmVV<funct6, OPIVV, opcodestr # ".vvm">;
  def XM : VALUmVX<funct6, OPIVX, opcodestr # ".vxm">;
  def IM : VALUmVI<funct6, opcodestr # ".vim">;
}

multiclass VALUm_IV_V_X<string opcodestr, bits<6> funct6> {
  def VM : VALUmVV<funct6, OPIVV, opcodestr # ".vvm">;
  def XM : VALUmVX<funct6, OPIVX, opcodestr # ".vxm">;
}

multiclass VALUNoVm_IV_V_X_I<string opcodestr, bits<6> funct6, Operand optype = simm5> {
  def V : VALUVVNoVm<funct6, OPIVV, opcodestr # ".vv">;
  def X : VALUVXNoVm<funct6, OPIVX, opcodestr # ".vx">;
  def I : VALUVINoVm<funct6, opcodestr # ".vi", optype>;
}

multiclass VALUNoVm_IV_V_X<string opcodestr, bits<6> funct6> {
  def V : VALUVVNoVm<funct6, OPIVV, opcodestr # ".vv">;
  def X : VALUVXNoVm<funct6, OPIVX, opcodestr # ".vx">;
}

multiclass VALU_FV_V_F<string opcodestr, bits<6> funct6, string vw = "v"> {
  def V : VALUVV<funct6, OPFVV, opcodestr # "." # vw # "v">;
  def F : VALUVF<funct6, OPFVF, opcodestr # "." # vw # "f">;
}

multiclass VALU_FV_F<string opcodestr, bits<6> funct6, string vw = "v"> {
  def F : VALUVF<funct6, OPFVF, opcodestr # "." # vw # "f">;
}

multiclass VALUr_FV_V_F<string opcodestr, bits<6> funct6, string vw = "v"> {
  def V : VALUrVV<funct6, OPFVV, opcodestr # "." # vw # "v">;
  def F : VALUrVF<funct6, OPFVF, opcodestr # "." # vw # "f">;
}

multiclass VALU_FV_V<string opcodestr, bits<6> funct6> {
  def _VS : VALUVV<funct6, OPFVV, opcodestr # ".vs">;
}

multiclass VALU_FV_VS2<string opcodestr, bits<6> funct6, bits<5> vs1> {
  def "" : VALUVs2<funct6, vs1, OPFVV, opcodestr>;
}

//===----------------------------------------------------------------------===//
// Instructions
//===----------------------------------------------------------------------===//

let Predicates = [HasStdExtV] in {

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
def VSETVLI : RVInstSetVLi<(outs GPR:$rd), (ins GPR:$rs1, VTypeIOp:$vtypei),
                           "vsetvli", "$rd, $rs1, $vtypei">;
def VSETVL : RVInstSetVL<(outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2),
                         "vsetvl", "$rd, $rs1, $rs2">;
}

// Vector Unit-Stride Instructions
def VLB_V : VUnitStrideLoad<MOPLDUnitStrideS, LUMOPUnitStride, LSWidthVByte, "vlb.v">;
def VLH_V : VUnitStrideLoad<MOPLDUnitStrideS, LUMOPUnitStride, LSWidthVHalf, "vlh.v">;
def VLW_V : VUnitStrideLoad<MOPLDUnitStrideS, LUMOPUnitStride, LSWidthVWord, "vlw.v">;

def VLBU_V : VUnitStrideLoad<MOPLDUnitStrideU, LUMOPUnitStride, LSWidthVByte, "vlbu.v">;
def VLHU_V : VUnitStrideLoad<MOPLDUnitStrideU, LUMOPUnitStride, LSWidthVHalf, "vlhu.v">;
def VLWU_V : VUnitStrideLoad<MOPLDUnitStrideU, LUMOPUnitStride, LSWidthVWord, "vlwu.v">;

def VLE_V : VUnitStrideLoad<MOPLDUnitStrideU, LUMOPUnitStride, LSWidthVSEW, "vle.v">;

def VLBFF_V : VUnitStrideLoad<MOPLDUnitStrideS, LUMOPUnitStrideFF, LSWidthVByte, "vlbff.v">;
def VLHFF_V : VUnitStrideLoad<MOPLDUnitStrideS, LUMOPUnitStrideFF, LSWidthVHalf, "vlhff.v">;
def VLWFF_V : VUnitStrideLoad<MOPLDUnitStrideS, LUMOPUnitStrideFF, LSWidthVWord, "vlwff.v">;

def VLBUFF_V : VUnitStrideLoad<MOPLDUnitStrideU, LUMOPUnitStrideFF, LSWidthVByte, "vlbuff.v">;
def VLHUFF_V : VUnitStrideLoad<MOPLDUnitStrideU, LUMOPUnitStrideFF, LSWidthVHalf, "vlhuff.v">;
def VLWUFF_V : VUnitStrideLoad<MOPLDUnitStrideU, LUMOPUnitStrideFF, LSWidthVWord, "vlwuff.v">;

def VLEFF_V : VUnitStrideLoad<MOPLDUnitStrideU, LUMOPUnitStrideFF, LSWidthVSEW, "vleff.v">;

def VSB_V : VUnitStrideStore<MOPSTUnitStride, SUMOPUnitStride, LSWidthVByte, "vsb.v">;
def VSH_V : VUnitStrideStore<MOPSTUnitStride, SUMOPUnitStride, LSWidthVHalf, "vsh.v">;
def VSW_V : VUnitStrideStore<MOPSTUnitStride, SUMOPUnitStride, LSWidthVWord, "vsw.v">;

def VSE_V : VUnitStrideStore<MOPSTUnitStride, SUMOPUnitStride, LSWidthVSEW, "vse.v">;

// Vector Strided Instructions
def VLSB_V : VStridedLoad<MOPLDStridedS, LSWidthVByte, "vlsb.v">;
def VLSH_V : VStridedLoad<MOPLDStridedS, LSWidthVHalf, "vlsh.v">;
def VLSW_V : VStridedLoad<MOPLDStridedS, LSWidthVWord, "vlsw.v">;

def VLSBU_V : VStridedLoad<MOPLDStridedU, LSWidthVByte, "vlsbu.v">;
def VLSHU_V : VStridedLoad<MOPLDStridedU, LSWidthVHalf, "vlshu.v">;
def VLSWU_V : VStridedLoad<MOPLDStridedU, LSWidthVWord, "vlswu.v">;

def VLSE_V : VStridedLoad<MOPLDStridedU, LSWidthVSEW, "vlse.v">;

def VSSB_V : VStridedStore<MOPSTStrided, LSWidthVByte, "vssb.v">;
def VSSH_V : VStridedStore<MOPSTStrided, LSWidthVHalf, "vssh.v">;
def VSSW_V : VStridedStore<MOPSTStrided, LSWidthVWord, "vssw.v">;
def VSSE_V : VStridedStore<MOPSTStrided, LSWidthVSEW, "vsse.v">;

// Vector Indexed Instructions
def VLXB_V : VIndexedLoad<MOPLDIndexedS, LSWidthVByte, "vlxb.v">;
def VLXH_V : VIndexedLoad<MOPLDIndexedS, LSWidthVHalf, "vlxh.v">;
def VLXW_V : VIndexedLoad<MOPLDIndexedS, LSWidthVWord, "vlxw.v">;

def VLXBU_V : VIndexedLoad<MOPLDIndexedU, LSWidthVByte, "vlxbu.v">;
def VLXHU_V : VIndexedLoad<MOPLDIndexedU, LSWidthVHalf, "vlxhu.v">;
def VLXWU_V : VIndexedLoad<MOPLDIndexedU, LSWidthVWord, "vlxwu.v">;

def VLXE_V : VIndexedLoad<MOPLDIndexedU, LSWidthVSEW, "vlxe.v">;

def VSXB_V : VIndexedStore<MOPSTIndexedOrder, LSWidthVByte, "vsxb.v">;
def VSXH_V : VIndexedStore<MOPSTIndexedOrder, LSWidthVHalf, "vsxh.v">;
def VSXW_V : VIndexedStore<MOPSTIndexedOrder, LSWidthVWord, "vsxw.v">;
def VSXE_V : VIndexedStore<MOPSTIndexedOrder, LSWidthVSEW, "vsxe.v">;

def VSUXB_V : VIndexedStore<MOPSTIndexedUnOrd, LSWidthVByte, "vsuxb.v">;
def VSUXH_V : VIndexedStore<MOPSTIndexedUnOrd, LSWidthVHalf, "vsuxh.v">;
def VSUXW_V : VIndexedStore<MOPSTIndexedUnOrd, LSWidthVWord, "vsuxw.v">;
def VSUXE_V : VIndexedStore<MOPSTIndexedUnOrd, LSWidthVSEW, "vsuxe.v">;

// Vector Single-Width Integer Add and Subtract
defm VADD_V : VALU_IV_V_X_I<"vadd", 0b000000>;
defm VSUB_V : VALU_IV_V_X<"vsub", 0b000010>;
defm VRSUB_V: VALU_IV_X_I<"vrsub", 0b000011>;

// Vector Widening Integer Add/Subtract
let Constraints = "@earlyclobber $vd" in {
defm VWADDU_V : VALU_MV_V_X<"vwaddu", 0b110000>;
defm VWSUBU_V : VALU_MV_V_X<"vwsubu", 0b110010>;
defm VWADD_V : VALU_MV_V_X<"vwadd", 0b110001>;
defm VWSUB_V : VALU_MV_V_X<"vwsub", 0b110011>;
}
defm VWADDU_W : VALU_MV_V_X<"vwaddu", 0b110100, "w">;
defm VWSUBU_W : VALU_MV_V_X<"vwsubu", 0b110110, "w">;
defm VWADD_W : VALU_MV_V_X<"vwadd", 0b110101, "w">;
defm VWSUB_W : VALU_MV_V_X<"vwsub", 0b110111, "w">;

// Vector Integer Add-with-Carry / Subtract-with-Borrow Instructions
defm VADC_V : VALUm_IV_V_X_I<"vadc", 0b010000>;
defm VMADC_V : VALUm_IV_V_X_I<"vmadc", 0b010001>;
defm VMADC_V : VALUNoVm_IV_V_X_I<"vmadc", 0b010001>;
defm VSBC_V : VALUm_IV_V_X<"vsbc", 0b010010>;
defm VMSBC_V : VALUm_IV_V_X<"vmsbc", 0b010011>;
defm VMSBC_V : VALUNoVm_IV_V_X<"vmsbc", 0b010011>;

// Vector Bitwise Logical Instructions
defm VAND_V : VALU_IV_V_X_I<"vand", 0b001001>;
defm VOR_V : VALU_IV_V_X_I<"vor", 0b001010>;
defm VXOR_V : VALU_IV_V_X_I<"vxor", 0b001011>;

// Vector Single-Width Bit Shift Instructions
defm VSLL_V : VALU_IV_V_X_I<"vsll", 0b100101, uimm5>;
defm VSRL_V : VALU_IV_V_X_I<"vsrl", 0b101000, uimm5>;
defm VSRA_V : VALU_IV_V_X_I<"vsra", 0b101001, uimm5>;

// Vector Narrowing Integer Right Shift Instructions
defm VNSRL_W : VALU_IV_V_X_I<"vnsrl", 0b101100, uimm5, "w">;
defm VNSRA_W : VALU_IV_V_X_I<"vnsra", 0b101101, uimm5, "w">;

// Vector Integer Comparison Instructions
defm VMSEQ_V : VALU_IV_V_X_I<"vmseq", 0b011000>;
defm VMSNE_V : VALU_IV_V_X_I<"vmsne", 0b011001>;
defm VMSLTU_V : VALU_IV_V_X<"vmsltu", 0b011010>;
defm VMSLT_V : VALU_IV_V_X<"vmslt", 0b011011>;
defm VMSLEU_V : VALU_IV_V_X_I<"vmsleu", 0b011100>;
defm VMSLE_V : VALU_IV_V_X_I<"vmsle", 0b011101>;
defm VMSGTU_V : VALU_IV_X_I<"vmsgtu", 0b011110>;
defm VMSGT_V : VALU_IV_X_I<"vmsgt", 0b011111>;

// Vector Integer Min/Max Instructions
defm VMINU_V : VALU_IV_V_X<"vminu", 0b000100>;
defm VMIN_V : VALU_IV_V_X<"vmin", 0b000101>;
defm VMAXU_V : VALU_IV_V_X<"vmaxu", 0b000110>;
defm VMAX_V : VALU_IV_V_X<"vmax", 0b000111>;

// Vector Single-Width Integer Multiply Instructions
defm VMUL_V : VALU_MV_V_X<"vmul", 0b100101>;
defm VMULH_V: VALU_MV_V_X<"vmulh", 0b100111>;
defm VMULHU_V : VALU_MV_V_X<"vmulhu", 0b100100>;
defm VMULHSU_V : VALU_MV_V_X<"vmulhsu", 0b100110>;

// Vector Integer Divide Instructions
defm VDIVU_V : VALU_MV_V_X<"vdivu", 0b100000>;
defm VDIV_V : VALU_MV_V_X<"vdiv", 0b100001>;
defm VREMU_V : VALU_MV_V_X<"vremu", 0b100010>;
defm VREM_V : VALU_MV_V_X<"vrem", 0b100011>;

// Vector Widening Integer Multiply Instructions
let Constraints = "@earlyclobber $vd" in {
defm VWMUL_V : VALU_MV_V_X<"vwmul", 0b111011>;
defm VWMULU_V : VALU_MV_V_X<"vwmulu", 0b111000>;
defm VWMULSU_V : VALU_MV_V_X<"vwmulsu", 0b111010>;
}

// Vector Single-Width Integer Multiply-Add Instructions
defm VMACC_V : VALUr_MV_V_X<"vmacc", 0b101101>;
defm VNMSAC_V : VALUr_MV_V_X<"vnmsac", 0b101111>;
defm VMADD_V : VALUr_MV_V_X<"vmadd", 0b101001>;
defm VNMSUB_V : VALUr_MV_V_X<"vnmsub", 0b101011>;

// Vector Widening Integer Multiply-Add Instructions
let Constraints = "@earlyclobber $vd" in {
defm VWMACCU_V : VALUr_MV_V_X<"vwmaccu", 0b111100>;
defm VWMACC_V : VALUr_MV_V_X<"vwmacc", 0b111101>;
defm VWMACCSU_V : VALUr_MV_V_X<"vwmaccsu", 0b111111>;
defm VWMACCUS_V : VALUr_MV_X<"vwmaccus", 0b111110>;
}

// Vector Integer Merge Instructions
defm VMERGE : VALUm_IV_V_X_I<"vmerge", 0b010111>;

// Vector Integer Move Instructions
let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
// op vd, vs1
def VMV_V_V : RVInstVV<0b010111, OPIVV, (outs VRRegOp:$vd),
                       (ins VRRegOp:$vs1), "vmv.v.v", "$vd, $vs1"> {
  let vs2 = 0;
  let vm = 1;
}
// op vd, rs1
def VMV_V_X : RVInstVX<0b010111, OPIVX, (outs VRRegOp:$vd),
                       (ins GPR:$rs1), "vmv.v.x", "$vd, $rs1"> {
  let vs2 = 0;
  let vm = 1;
}
// op vd, imm
def VMV_V_I : RVInstIVI<0b010111, (outs VRRegOp:$vd),
                       (ins simm5:$imm), "vmv.v.i", "$vd, $imm"> {
  let vs2 = 0;
  let vm = 1;
}
}

// Vector Fixed-Point Arithmetic Instructions
defm VSADDU_V : VALU_IV_V_X_I<"vsaddu", 0b100000>;
defm VSADD_V : VALU_IV_V_X_I<"vsadd", 0b100001>;
defm VSSUBU_V : VALU_IV_V_X<"vssubu", 0b100010>;
defm VSSUB_V : VALU_IV_V_X<"vssub", 0b100011>;

// Vector Single-Width Averaging Add and Subtract
defm VAADDU_V : VALU_MV_V_X<"vaaddu", 0b001000>;
defm VAADD_V : VALU_MV_V_X<"vaadd", 0b001001>;
defm VASUBU_V : VALU_MV_V_X<"vasubu", 0b001010>;
defm VASUB_V : VALU_MV_V_X<"vasub", 0b001011>;

// Vector Single-Width Fractional Multiply with Rounding and Saturation
defm VSMUL_V : VALU_IV_V_X<"vsmul", 0b100111>;

// Vector Widening Saturating Scaled Multiply-Add
let Constraints = "@earlyclobber $vd" in {
defm VWSMACCU_V : VALUr_IV_V_X<"vwsmaccu", 0b111100>;
defm VWSMACC_V : VALUr_IV_V_X<"vwsmacc", 0b111101>;
defm VWSMACCSU_V : VALUr_IV_V_X<"vwsmaccsu", 0b111111>;
defm VWSMACCUS_V : VALUr_IV_X<"vwsmaccus", 0b111110>;
}

// Vector Single-Width Scaling Shift Instructions
defm VSSRL_V : VALU_IV_V_X_I<"vssrl", 0b101010, uimm5>;
defm VSSRA_V : VALU_IV_V_X_I<"vssra", 0b101011, uimm5>;

// Vector Narrowing Fixed-Point Clip Instructions
defm VNCLIPU_W : VALU_IV_V_X_I<"vnclipu", 0b101110, uimm5, "w">;
defm VNCLIP_W : VALU_IV_V_X_I<"vnclip", 0b101111, uimm5, "w">;

// Vector Single-Width Floating-Point Add/Subtract Instructions
defm VFADD_V : VALU_FV_V_F<"vfadd", 0b000000>;
defm VFSUB_V : VALU_FV_V_F<"vfsub", 0b000010>;
defm VFRSUB_V : VALU_FV_F<"vfrsub", 0b100111>;

// Vector Widening Floating-Point Add/Subtract Instructions
let Constraints = "@earlyclobber $vd" in {
defm VFWADD_V : VALU_FV_V_F<"vfwadd", 0b110000>;
defm VFWSUB_V : VALU_FV_V_F<"vfwsub", 0b110010>;
}

defm VFWADD_W : VALU_FV_V_F<"vfwadd", 0b110100, "w">;
defm VFWSUB_W : VALU_FV_V_F<"vfwsub", 0b110110, "w">;

// Vector Single-Width Floating-Point Multiply/Divide Instructions
defm VFMUL_V : VALU_FV_V_F<"vfmul", 0b100100>;
defm VFDIV_V : VALU_FV_V_F<"vfdiv", 0b100000>;
defm VFRDIV_V : VALU_FV_F<"vfrdiv", 0b100001>;

// Vector Widening Floating-Point Multiply
let Constraints = "@earlyclobber $vd" in {
defm VFWMUL_V : VALU_FV_V_F<"vfwmul", 0b111000>;
}

// Vector Single-Width Floating-Point Fused Multiply-Add Instructions
defm VFMACC_V: VALUr_FV_V_F<"vfmacc", 0b101100>;
defm VFNMACC_V : VALUr_FV_V_F<"vfnmacc", 0b101101>;
defm VFMSAC_V : VALUr_FV_V_F<"vfmsac", 0b101110>;
defm VFNMSAC_V : VALUr_FV_V_F<"vfnmsac", 0b101111>;
defm VFMADD_V : VALUr_FV_V_F<"vfmadd", 0b101000>;
defm VFNMADD_V : VALUr_FV_V_F<"vfnmadd", 0b101001>;
defm VFMSUB_V : VALUr_FV_V_F<"vfmsub", 0b101010>;
defm VFNMSUB_V : VALUr_FV_V_F<"vfnmsub", 0b101011>;

// Vector Widening Floating-Point Fused Multiply-Add Instructions
let Constraints = "@earlyclobber $vd" in {
defm VFWMACC_V : VALUr_FV_V_F<"vfwmacc", 0b111100>;
defm VFWNMACC_V : VALUr_FV_V_F<"vfwnmacc", 0b111101>;
defm VFWMSAC_V : VALUr_FV_V_F<"vfwmsac", 0b111110>;
defm VFWNMSAC_V : VALUr_FV_V_F<"vfwnmsac", 0b111111>;
}

// Vector Floating-Point Square-Root Instruction
defm VFSQRT_V : VALU_FV_VS2<"vfsqrt.v", 0b100011, 0b00000>;

// Vector Floating-Point MIN/MAX Instructions
defm VFMIN_V : VALU_FV_V_F<"vfmin", 0b000100>;
defm VFMAX_V : VALU_FV_V_F<"vfmax", 0b000110>;

// Vector Floating-Point Sign-Injection Instructions
defm VFSGNJ_V : VALU_FV_V_F<"vfsgnj", 0b001000>;
defm VFSGNJN_V : VALU_FV_V_F<"vfsgnjn", 0b001001>;
defm VFSGNJX_V : VALU_FV_V_F<"vfsgnjx", 0b001010>;

// Vector Floating-Point Compare Instructions
defm VMFEQ_V : VALU_FV_V_F<"vmfeq", 0b011000>;
defm VMFNE_V : VALU_FV_V_F<"vmfne", 0b011100>;
defm VMFLT_V : VALU_FV_V_F<"vmflt", 0b011011>;
defm VMFLE_V : VALU_FV_V_F<"vmfle", 0b011001>;
defm VMFGT_V : VALU_FV_F<"vmfgt", 0b011101>;
defm VMFGE_V : VALU_FV_F<"vmfge", 0b011111>;

// Vector Floating-Point Classify Instruction
defm VFCLASS_V : VALU_FV_VS2<"vfclass.v", 0b100011, 0b10000>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
// Vector Floating-Point Merge Instruction
def VFMERGE_VFM : RVInstVX<0b010111, OPFVF, (outs VRRegOp:$vd),
                           (ins VRRegOp:$vs2, FPR32:$rs1, VRV0:$v0),
                           "vfmerge.vfm", "$vd, $vs2, $rs1, v0"> {
  let vm = 0;
}

// Vector Floating-Point Move Instruction
def VFMV_V_F : RVInstVX<0b010111, OPFVF, (outs VRRegOp:$vd),
                       (ins FPR32:$rs1), "vfmv.v.f", "$vd, $rs1"> {
  let vs2 = 0;
  let vm = 1;
}
}

// Single-Width Floating-Point/Integer Type-Convert Instructions
defm VFCVT_XU_F_V : VALU_FV_VS2<"vfcvt.xu.f.v", 0b100010, 0b00000>;
defm VFCVT_X_F_V : VALU_FV_VS2<"vfcvt.x.f.v", 0b100010, 0b00001>;
defm VFCVT_F_XU_V : VALU_FV_VS2<"vfcvt.f.xu.v", 0b100010, 0b00010>;
defm VFCVT_F_X_V : VALU_FV_VS2<"vfcvt.f.x.v", 0b100010, 0b00011>;

// Widening Floating-Point/Integer Type-Convert Instructions
let Constraints = "@earlyclobber $vd" in {
defm VFWCVT_XU_F_V : VALU_FV_VS2<"vfwcvt.xu.f.v", 0b100010, 0b01000>;
defm VFWCVT_X_F_V : VALU_FV_VS2<"vfwcvt.x.f.v", 0b100010, 0b01001>;
defm VFWCVT_F_XU_V : VALU_FV_VS2<"vfwcvt.f.xu.v", 0b100010, 0b01010>;
defm VFWCVT_F_X_V : VALU_FV_VS2<"vfwcvt.f.x.v", 0b100010, 0b01011>;
defm VFWCVT_F_F_V : VALU_FV_VS2<"vfwcvt.f.f.v", 0b100010, 0b01100>;
}

// Narrowing Floating-Point/Integer Type-Convert Instructions
defm VFNCVT_XU_F_W : VALU_FV_VS2<"vfncvt.xu.f.w", 0b100010, 0b10000>;
defm VFNCVT_X_F_W : VALU_FV_VS2<"vfncvt.x.f.w", 0b100010, 0b10001>;
defm VFNCVT_F_XU_W : VALU_FV_VS2<"vfncvt.f.xu.w", 0b100010, 0b10010>;
defm VFNCVT_F_X_W : VALU_FV_VS2<"vfncvt.f.x.w", 0b100010, 0b10011>;
defm VFNCVT_F_F_W : VALU_FV_VS2<"vfncvt.f.f.w", 0b100010, 0b10100>;
defm VFNCVT_ROD_F_F_W : VALU_FV_VS2<"vfncvt.rod.f.f.w", 0b100010, 0b10101>;

// Vector Single-Width Integer Reduction Instructions
defm VREDSUM : VALU_MV_V<"vredsum", 0b000000>;
defm VREDMAXU : VALU_MV_V<"vredmaxu", 0b000110>;
defm VREDMAX : VALU_MV_V<"vredmax", 0b000111>;
defm VREDMINU : VALU_MV_V<"vredminu", 0b000100>;
defm VREDMIN : VALU_MV_V<"vredmin", 0b000101>;
defm VREDAND : VALU_MV_V<"vredand", 0b000001>;
defm VREDOR : VALU_MV_V<"vredor", 0b000010>;
defm VREDXOR : VALU_MV_V<"vredxor", 0b000011>;

// Vector Widening Integer Reduction Instructions
defm VWREDSUMU : VALU_IV_V<"vwredsumu", 0b110000>;
defm VWREDSUM : VALU_IV_V<"vwredsum", 0b110001>;

// Vector Single-Width Floating-Point Reduction Instructions
defm VFREDOSUM : VALU_FV_V<"vfredosum", 0b000011>;
defm VFREDSUM : VALU_FV_V<"vfredsum", 0b000001>;
defm VFREDMAX : VALU_FV_V<"vfredmax", 0b000111>;
defm VFREDMIN : VALU_FV_V<"vfredmin", 0b000101>;

// Vector Widening Floating-Point Reduction Instructions
defm VFWREDOSUM : VALU_FV_V<"vfwredosum", 0b110011>;
defm VFWREDSUM : VALU_FV_V<"vfwredsum", 0b110001>;

// Vector Mask-Register Logical Instructions
defm VMAND_M : VALU_MV_Mask<"vmand", 0b011001, "m">;
defm VMNAND_M : VALU_MV_Mask<"vmnand", 0b011101, "m">;
defm VMANDNOT_M : VALU_MV_Mask<"vmandnot", 0b011000, "m">;
defm VMXOR_M : VALU_MV_Mask<"vmxor", 0b011011, "m">;
defm VMOR_M : VALU_MV_Mask<"vmor", 0b011010, "m">;
defm VMNOR_M : VALU_MV_Mask<"vmnor", 0b011110, "m">;
defm VMORNOT_M : VALU_MV_Mask<"vmornot", 0b011100, "m">;
defm VMXNOR_M : VALU_MV_Mask<"vmxnor", 0b011111, "m">;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
// Vector mask population count vpopc
def VPOPC_M : RVInstV<0b010000, 0b10000, OPMVV, (outs GPR:$vd),
                        (ins VRRegOp:$vs2, VRMaskRegOp:$vm),
                        "vpopc.m", "$vd, $vs2${vm}">;

// vfirst find-first-set mask bit
def VFIRST_M : RVInstV<0b010000, 0b10001, OPMVV, (outs GPR:$vd),
                        (ins VRRegOp:$vs2, VRMaskRegOp:$vm),
                        "vfirst.m", "$vd, $vs2${vm}">;
}

// vmsbf.m set-before-first mask bit
defm VMSBF_M : VALU_MV_VS2<"vmsbf.m", 0b010100, 0b00001>;

// vmsif.m set-including-first mask bit
defm VMSIF_M : VALU_MV_VS2<"vmsif.m", 0b010100, 0b00011>;

// vmsof.m set-only-first mask bit
defm VMSOF_M : VALU_MV_VS2<"vmsof.m", 0b010100, 0b00010>;

// Vector Iota Instruction
let Constraints = "@earlyclobber $vd" in {
defm VIOTA_M : VALU_MV_VS2<"viota.m", 0b010100, 0b10000>;
}

// Vector Element Index Instruction
let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in {
def VID_V : RVInstV<0b010100, 0b10001, OPMVV, (outs VRRegOp:$vd),
                      (ins VRMaskRegOp:$vm), "vid.v", "$vd${vm}"> {
  let vs2 = 0;
}

// Integer Scalar Move Instructions
let vm = 1 in {
def VMV_X_S : RVInstV<0b010000, 0b00000, OPMVV, (outs GPR:$vd),
                      (ins VRRegOp:$vs2), "vmv.x.s", "$vd, $vs2">;
def VMV_S_X : RVInstV2<0b010000, 0b00000, OPMVX, (outs VRRegOp:$vd),
                      (ins GPR:$rs1), "vmv.s.x", "$vd, $rs1">;

// Floating-Point Scalar Move Instructions
def VFMV_F_S : RVInstV<0b010000, 0b00000, OPFVV, (outs FPR32:$vd),
                      (ins VRRegOp:$vs2), "vfmv.f.s", "$vd, $vs2">;
def VFMV_S_F : RVInstV2<0b010000, 0b00000, OPFVF, (outs VRRegOp:$vd),
                      (ins FPR32:$rs1), "vfmv.s.f", "$vd, $rs1">;
}
}

// Vector Slide Instructions
let Constraints = "@earlyclobber $vd" in {
defm VSLIDEUP_V : VALU_IV_X_I<"vslideup", 0b001110, uimm5>;
}
defm VSLIDEDOWN_V : VALU_IV_X_I<"vslidedown", 0b001111, uimm5>;

let Constraints = "@earlyclobber $vd" in {
defm VSLIDE1UP_V : VALU_MV_X<"vslide1up", 0b001110>;
}
defm VSLIDE1DOWN_V : VALU_MV_X<"vslide1down", 0b001111>;

// Vector Register Gather Instruction
let Constraints = "@earlyclobber $vd" in {
defm VRGATHER_V : VALU_IV_V_X_I<"vrgather", 0b001100, uimm5>;
}

// Vector Compress Instruction
let Constraints = "@earlyclobber $vd" in {
defm VCOMPRESS_V : VALU_MV_Mask<"vcompress", 0b010111>;
}

} // Predicates = [HasStdExtV]
