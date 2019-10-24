// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vadd.vv v1, v3, v2, v0.t
// CHECK-INST: vadd.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 00 <unknown>

vadd.vv v1, v3, v2
// CHECK-INST: vadd.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 02 <unknown>

vadd.vx v1, v3, a0, v0.t
// CHECK-INST: vadd.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 00 <unknown>

vadd.vx v1, v3, a0
// CHECK-INST: vadd.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 02 <unknown>

vadd.vi v1, v3, 15, v0.t
// CHECK-INST: vadd.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 00 <unknown>

vadd.vi v1, v3, 15
// CHECK-INST: vadd.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 02 <unknown>

vwaddu.vv v1, v3, v2, v0.t
// CHECK-INST: vwaddu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xc0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 c0 <unknown>

vwaddu.vv v1, v3, v2
// CHECK-INST: vwaddu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0xc2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 c2 <unknown>

vwaddu.vx v1, v3, a0, v0.t
// CHECK-INST: vwaddu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xc0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 c0 <unknown>

vwaddu.vx v1, v3, a0
// CHECK-INST: vwaddu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0xc2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 c2 <unknown>

vwadd.vv v1, v3, v2, v0.t
// CHECK-INST: vwadd.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xc4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 c4 <unknown>

vwadd.vv v1, v3, v2
// CHECK-INST: vwadd.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0xc6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 c6 <unknown>

vwadd.vx v1, v3, a0, v0.t
// CHECK-INST: vwadd.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xc4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 c4 <unknown>

vwadd.vx v1, v3, a0
// CHECK-INST: vwadd.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0xc6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 c6 <unknown>

vwaddu.wv v1, v3, v2, v0.t
// CHECK-INST: vwaddu.wv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xd0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 d0 <unknown>

vwaddu.wv v1, v3, v2
// CHECK-INST: vwaddu.wv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0xd2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 d2 <unknown>

vwaddu.wx v1, v3, a0, v0.t
// CHECK-INST: vwaddu.wx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xd0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 d0 <unknown>

vwaddu.wx v1, v3, a0
// CHECK-INST: vwaddu.wx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0xd2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 d2 <unknown>

vwadd.wv v1, v3, v2, v0.t
// CHECK-INST: vwadd.wv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xd4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 d4 <unknown>

vwadd.wv v1, v3, v2
// CHECK-INST: vwadd.wv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0xd6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 d6 <unknown>

vwadd.wx v1, v3, a0, v0.t
// CHECK-INST: vwadd.wx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xd4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 d4 <unknown>

vwadd.wx v1, v3, a0
// CHECK-INST: vwadd.wx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0xd6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 d6 <unknown>

vadc.vvm v1, v3, v2, v0
// CHECK-INST: vadc.vvm v1, v3, v2, v0
// CHECK-ENCODING: [0xd7,0x00,0x31,0x40]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 40 <unknown>

vadc.vxm v1, v3, a0, v0
// CHECK-INST: vadc.vxm v1, v3, a0, v0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x40]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 40 <unknown>

vadc.vim v1, v3, 15, v0
// CHECK-INST: vadc.vim v1, v3, 15, v0
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x40]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 40 <unknown>

vmadc.vvm v1, v3, v2, v0
// CHECK-INST: vmadc.vvm v1, v3, v2, v0
// CHECK-ENCODING: [0xd7,0x00,0x31,0x44]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 44 <unknown>

vmadc.vxm v1, v3, a0, v0
// CHECK-INST: vmadc.vxm v1, v3, a0, v0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x44]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 44 <unknown>

vmadc.vim v1, v3, 15, v0
// CHECK-INST: vmadc.vim v1, v3, 15, v0
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x44]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 44 <unknown>

vmadc.vv v1, v3, v2
// CHECK-INST: vmadc.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x46]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 46 <unknown>

vmadc.vx v1, v3, a0
// CHECK-INST: vmadc.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x46]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 46 <unknown>

vmadc.vi v1, v3, 15
// CHECK-INST: vmadc.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x46]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 46 <unknown>

vsaddu.vv v1, v3, v2, v0.t
// CHECK-INST: vsaddu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x80]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 80 <unknown>

vsaddu.vv v1, v3, v2
// CHECK-INST: vsaddu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x82]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 82 <unknown>

vsaddu.vx v1, v3, a0, v0.t
// CHECK-INST: vsaddu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x80]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 80 <unknown>

vsaddu.vx v1, v3, a0
// CHECK-INST: vsaddu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x82]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 82 <unknown>

vsaddu.vi v1, v3, 15, v0.t
// CHECK-INST: vsaddu.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x80]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 80 <unknown>

vsaddu.vi v1, v3, 15
// CHECK-INST: vsaddu.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x82]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 82 <unknown>

vsadd.vv v1, v3, v2, v0.t
// CHECK-INST: vsadd.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x84]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 84 <unknown>

vsadd.vv v1, v3, v2
// CHECK-INST: vsadd.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x86]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 86 <unknown>

vsadd.vx v1, v3, a0, v0.t
// CHECK-INST: vsadd.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x84]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 84 <unknown>

vsadd.vx v1, v3, a0
// CHECK-INST: vsadd.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x86]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 86 <unknown>

vsadd.vi v1, v3, 15, v0.t
// CHECK-INST: vsadd.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x84]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 84 <unknown>

vsadd.vi v1, v3, 15
// CHECK-INST: vsadd.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x86]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 86 <unknown>

vaadd.vv v1, v3, v2, v0.t
// CHECK-INST: vaadd.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x90]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 90 <unknown>

vaadd.vv v1, v3, v2
// CHECK-INST: vaadd.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x92]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 92 <unknown>

vaadd.vx v1, v3, a0, v0.t
// CHECK-INST: vaadd.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x90]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 90 <unknown>

vaadd.vx v1, v3, a0
// CHECK-INST: vaadd.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x92]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 92 <unknown>

vaadd.vi v1, v3, 15, v0.t
// CHECK-INST: vaadd.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x90]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 90 <unknown>

vaadd.vi v1, v3, 15
// CHECK-INST: vaadd.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x92]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 92 <unknown>

vfadd.vv v1, v3, v2, v0.t
// CHECK-INST: vfadd.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 00 <unknown>

vfadd.vv v1, v3, v2
// CHECK-INST: vfadd.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 02 <unknown>

vfadd.vf v1, v3, fa0, v0.t
// CHECK-INST: vfadd.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 00 <unknown>

vfadd.vf v1, v3, fa0
// CHECK-INST: vfadd.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 02 <unknown>

vfwadd.vv v1, v3, v2, v0.t
// CHECK-INST: vfwadd.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xc0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 c0 <unknown>

vfwadd.vv v1, v3, v2
// CHECK-INST: vfwadd.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0xc2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 c2 <unknown>

vfwadd.vf v1, v3, fa0, v0.t
// CHECK-INST: vfwadd.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xc0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 c0 <unknown>

vfwadd.vf v1, v3, fa0
// CHECK-INST: vfwadd.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0xc2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 c2 <unknown>
