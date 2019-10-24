// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vsub.vv v1, v3, v2, v0.t
// CHECK-INST: vsub.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 08 <unknown>

vsub.vv v1, v3, v2
// CHECK-INST: vsub.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 0a <unknown>

vsub.vx v1, v3, a0, v0.t
// CHECK-INST: vsub.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 08 <unknown>

vsub.vx v1, v3, a0
// CHECK-INST: vsub.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 0a <unknown>

vrsub.vx v1, v3, a0, v0.t
// CHECK-INST: vrsub.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 0c <unknown>

vrsub.vx v1, v3, a0
// CHECK-INST: vrsub.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 0e <unknown>

vrsub.vi v1, v3, 15, v0.t
// CHECK-INST: vrsub.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 0c <unknown>

vrsub.vi v1, v3, 15
// CHECK-INST: vrsub.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 0e <unknown>

vwsubu.vv v1, v3, v2, v0.t
// CHECK-INST: vwsubu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xc8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 c8 <unknown>

vwsubu.vv v1, v3, v2
// CHECK-INST: vwsubu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0xca]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 ca <unknown>

vwsubu.vx v1, v3, a0, v0.t
// CHECK-INST: vwsubu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xc8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 c8 <unknown>

vwsubu.vx v1, v3, a0
// CHECK-INST: vwsubu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0xca]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 ca <unknown>

vwsub.vv v1, v3, v2, v0.t
// CHECK-INST: vwsub.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xcc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 cc <unknown>

vwsub.vv v1, v3, v2
// CHECK-INST: vwsub.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0xce]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 ce <unknown>

vwsub.vx v1, v3, a0, v0.t
// CHECK-INST: vwsub.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xcc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 cc <unknown>

vwsub.vx v1, v3, a0
// CHECK-INST: vwsub.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0xce]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 ce <unknown>

vwsubu.wv v1, v3, v2, v0.t
// CHECK-INST: vwsubu.wv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xd8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 d8 <unknown>

vwsubu.wv v1, v3, v2
// CHECK-INST: vwsubu.wv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0xda]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 da <unknown>

vwsubu.wx v1, v3, a0, v0.t
// CHECK-INST: vwsubu.wx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xd8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 d8 <unknown>

vwsubu.wx v1, v3, a0
// CHECK-INST: vwsubu.wx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0xda]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 da <unknown>

vwsub.wv v1, v3, v2, v0.t
// CHECK-INST: vwsub.wv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xdc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 dc <unknown>

vwsub.wv v1, v3, v2
// CHECK-INST: vwsub.wv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0xde]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 de <unknown>

vwsub.wx v1, v3, a0, v0.t
// CHECK-INST: vwsub.wx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xdc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 dc <unknown>

vwsub.wx v1, v3, a0
// CHECK-INST: vwsub.wx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0xde]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 de <unknown>

vsbc.vvm v1, v3, v2, v0
// CHECK-INST: vsbc.vvm v1, v3, v2, v0
// CHECK-ENCODING: [0xd7,0x00,0x31,0x48]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 48 <unknown>

vsbc.vxm v1, v3, a0, v0
// CHECK-INST: vsbc.vxm v1, v3, a0, v0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x48]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 48 <unknown>

vmsbc.vvm v1, v3, v2, v0
// CHECK-INST: vmsbc.vvm v1, v3, v2, v0
// CHECK-ENCODING: [0xd7,0x00,0x31,0x4c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 4c <unknown>

vmsbc.vxm v1, v3, a0, v0
// CHECK-INST: vmsbc.vxm v1, v3, a0, v0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x4c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 4c <unknown>

vmsbc.vv v1, v3, v2
// CHECK-INST: vmsbc.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x4e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 4e <unknown>

vmsbc.vx v1, v3, a0
// CHECK-INST: vmsbc.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x4e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 4e <unknown>

vssubu.vv v1, v3, v2, v0.t
// CHECK-INST: vssubu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 88 <unknown>

vssubu.vv v1, v3, v2
// CHECK-INST: vssubu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 8a <unknown>

vssubu.vx v1, v3, a0, v0.t
// CHECK-INST: vssubu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 88 <unknown>

vssubu.vx v1, v3, a0
// CHECK-INST: vssubu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 8a <unknown>

vssub.vv v1, v3, v2, v0.t
// CHECK-INST: vssub.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x8c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 8c <unknown>

vssub.vv v1, v3, v2
// CHECK-INST: vssub.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x8e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 8e <unknown>

vssub.vx v1, v3, a0, v0.t
// CHECK-INST: vssub.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x8c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 8c <unknown>

vssub.vx v1, v3, a0
// CHECK-INST: vssub.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x8e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 8e <unknown>

vasub.vv v1, v3, v2, v0.t
// CHECK-INST: vasub.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x98]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 98 <unknown>

vasub.vv v1, v3, v2
// CHECK-INST: vasub.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x9a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 9a <unknown>

vasub.vx v1, v3, a0, v0.t
// CHECK-INST: vasub.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x98]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 98 <unknown>

vasub.vx v1, v3, a0
// CHECK-INST: vasub.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x9a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 9a <unknown>

vfsub.vv v1, v3, v2, v0.t
// CHECK-INST: vfsub.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 08 <unknown>

vfsub.vv v1, v3, v2
// CHECK-INST: vfsub.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 0a <unknown>

vfsub.vf v1, v3, fa0, v0.t
// CHECK-INST: vfsub.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 08 <unknown>

vfsub.vf v1, v3, fa0
// CHECK-INST: vfsub.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 0a <unknown>

vfrsub.vf v1, v3, fa0, v0.t
// CHECK-INST: vfrsub.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x9c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 9c <unknown>

vfrsub.vf v1, v3, fa0
// CHECK-INST: vfrsub.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x9e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 9e <unknown>

vfwsub.vv v1, v3, v2, v0.t
// CHECK-INST: vfwsub.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xc8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 c8 <unknown>

vfwsub.vv v1, v3, v2
// CHECK-INST: vfwsub.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0xca]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 ca <unknown>

vfwsub.vf v1, v3, fa0, v0.t
// CHECK-INST: vfwsub.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xc8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 c8 <unknown>

vfwsub.vf v1, v3, fa0
// CHECK-INST: vfwsub.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0xca]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 ca <unknown>
