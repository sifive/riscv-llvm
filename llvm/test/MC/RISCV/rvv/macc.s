// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vmacc.vv v1, v2, v3, v0.t
// CHECK-INST: vmacc.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xb4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 b4 <unknown>

vmacc.vv v1, v2, v3
// CHECK-INST: vmacc.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x20,0x31,0xb6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 b6 <unknown>

vmacc.vx v1, a0, v3, v0.t
// CHECK-INST: vmacc.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xb4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 b4 <unknown>

vmacc.vx v1, a0, v3
// CHECK-INST: vmacc.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x60,0x35,0xb6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 b6 <unknown>

vnmsac.vv v1, v2, v3, v0.t
// CHECK-INST: vnmsac.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xbc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 bc <unknown>

vnmsac.vv v1, v2, v3
// CHECK-INST: vnmsac.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x20,0x31,0xbe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 be <unknown>

vnmsac.vx v1, a0, v3, v0.t
// CHECK-INST: vnmsac.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xbc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 bc <unknown>

vnmsac.vx v1, a0, v3
// CHECK-INST: vnmsac.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x60,0x35,0xbe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 be <unknown>

vmadd.vv v1, v2, v3, v0.t
// CHECK-INST: vmadd.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xa4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 a4 <unknown>

vmadd.vv v1, v2, v3
// CHECK-INST: vmadd.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x20,0x31,0xa6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 a6 <unknown>

vmadd.vx v1, a0, v3, v0.t
// CHECK-INST: vmadd.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xa4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 a4 <unknown>

vmadd.vx v1, a0, v3
// CHECK-INST: vmadd.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x60,0x35,0xa6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 a6 <unknown>

vnmsub.vv v1, v2, v3, v0.t
// CHECK-INST: vnmsub.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xac]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 ac <unknown>

vnmsub.vv v1, v2, v3
// CHECK-INST: vnmsub.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x20,0x31,0xae]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 ae <unknown>

vnmsub.vx v1, a0, v3, v0.t
// CHECK-INST: vnmsub.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xac]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 ac <unknown>

vnmsub.vx v1, a0, v3
// CHECK-INST: vnmsub.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x60,0x35,0xae]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 ae <unknown>

vwmaccu.vv v1, v2, v3, v0.t
// CHECK-INST: vwmaccu.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xf0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 f0 <unknown>

vwmaccu.vv v1, v2, v3
// CHECK-INST: vwmaccu.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x20,0x31,0xf2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 f2 <unknown>

vwmaccu.vx v1, a0, v3, v0.t
// CHECK-INST: vwmaccu.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xf0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 f0 <unknown>

vwmaccu.vx v1, a0, v3
// CHECK-INST: vwmaccu.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x60,0x35,0xf2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 f2 <unknown>

vwmacc.vv v1, v2, v3, v0.t
// CHECK-INST: vwmacc.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xf4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 f4 <unknown>

vwmacc.vv v1, v2, v3
// CHECK-INST: vwmacc.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x20,0x31,0xf6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 f6 <unknown>

vwmacc.vx v1, a0, v3, v0.t
// CHECK-INST: vwmacc.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xf4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 f4 <unknown>

vwmacc.vx v1, a0, v3
// CHECK-INST: vwmacc.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x60,0x35,0xf6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 f6 <unknown>

vwmaccsu.vv v1, v2, v3, v0.t
// CHECK-INST: vwmaccsu.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xfc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 fc <unknown>

vwmaccsu.vv v1, v2, v3
// CHECK-INST: vwmaccsu.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x20,0x31,0xfe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 fe <unknown>

vwmaccsu.vx v1, a0, v3, v0.t
// CHECK-INST: vwmaccsu.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xfc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 fc <unknown>

vwmaccsu.vx v1, a0, v3
// CHECK-INST: vwmaccsu.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x60,0x35,0xfe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 fe <unknown>

vwmaccus.vx v1, a0, v3, v0.t
// CHECK-INST: vwmaccus.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xf8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 f8 <unknown>

vwmaccus.vx v1, a0, v3
// CHECK-INST: vwmaccus.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x60,0x35,0xfa]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 fa <unknown>

vwsmaccu.vv v1, v2, v3, v0.t
// CHECK-INST: vwsmaccu.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xf0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 f0 <unknown>

vwsmaccu.vv v1, v2, v3
// CHECK-INST: vwsmaccu.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x00,0x31,0xf2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 f2 <unknown>

vwsmaccu.vx v1, a0, v3, v0.t
// CHECK-INST: vwsmaccu.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xf0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 f0 <unknown>

vwsmaccu.vx v1, a0, v3
// CHECK-INST: vwsmaccu.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x40,0x35,0xf2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 f2 <unknown>

vwsmacc.vv v1, v2, v3, v0.t
// CHECK-INST: vwsmacc.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xf4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 f4 <unknown>

vwsmacc.vv v1, v2, v3
// CHECK-INST: vwsmacc.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x00,0x31,0xf6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 f6 <unknown>

vwsmacc.vx v1, a0, v3, v0.t
// CHECK-INST: vwsmacc.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xf4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 f4 <unknown>

vwsmacc.vx v1, a0, v3
// CHECK-INST: vwsmacc.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x40,0x35,0xf6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 f6 <unknown>

vwsmaccsu.vv v1, v2, v3, v0.t
// CHECK-INST: vwsmaccsu.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xfc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 fc <unknown>

vwsmaccsu.vv v1, v2, v3
// CHECK-INST: vwsmaccsu.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x00,0x31,0xfe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 fe <unknown>

vwsmaccsu.vx v1, a0, v3, v0.t
// CHECK-INST: vwsmaccsu.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xfc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 fc <unknown>

vwsmaccsu.vx v1, a0, v3
// CHECK-INST: vwsmaccsu.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x40,0x35,0xfe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 fe <unknown>

vwsmaccus.vx v1, a0, v3, v0.t
// CHECK-INST: vwsmaccus.vx v1, a0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xf8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 f8 <unknown>

vwsmaccus.vx v1, a0, v3
// CHECK-INST: vwsmaccus.vx v1, a0, v3
// CHECK-ENCODING: [0xd7,0x40,0x35,0xfa]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 fa <unknown>

vfmacc.vv v1, v2, v3, v0.t
// CHECK-INST: vfmacc.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xb0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 b0 <unknown>

vfmacc.vv v1, v2, v3
// CHECK-INST: vfmacc.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xb2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 b2 <unknown>

vfmacc.vf v1, fa0, v3, v0.t
// CHECK-INST: vfmacc.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xb0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 b0 <unknown>

vfmacc.vf v1, fa0, v3
// CHECK-INST: vfmacc.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xb2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 b2 <unknown>

vfnmacc.vv v1, v2, v3, v0.t
// CHECK-INST: vfnmacc.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xb4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 b4 <unknown>

vfnmacc.vv v1, v2, v3
// CHECK-INST: vfnmacc.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xb6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 b6 <unknown>

vfnmacc.vf v1, fa0, v3, v0.t
// CHECK-INST: vfnmacc.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xb4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 b4 <unknown>

vfnmacc.vf v1, fa0, v3
// CHECK-INST: vfnmacc.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xb6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 b6 <unknown>

vfmsac.vv v1, v2, v3, v0.t
// CHECK-INST: vfmsac.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xb8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 b8 <unknown>

vfmsac.vv v1, v2, v3
// CHECK-INST: vfmsac.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xba]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 ba <unknown>

vfmsac.vf v1, fa0, v3, v0.t
// CHECK-INST: vfmsac.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xb8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 b8 <unknown>

vfmsac.vf v1, fa0, v3
// CHECK-INST: vfmsac.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xba]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 ba <unknown>

vfnmsac.vv v1, v2, v3, v0.t
// CHECK-INST: vfnmsac.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xbc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 bc <unknown>

vfnmsac.vv v1, v2, v3
// CHECK-INST: vfnmsac.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xbe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 be <unknown>

vfnmsac.vf v1, fa0, v3, v0.t
// CHECK-INST: vfnmsac.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xbc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 bc <unknown>

vfnmsac.vf v1, fa0, v3
// CHECK-INST: vfnmsac.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xbe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 be <unknown>

vfmadd.vv v1, v2, v3, v0.t
// CHECK-INST: vfmadd.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xa0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 a0 <unknown>

vfmadd.vv v1, v2, v3
// CHECK-INST: vfmadd.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xa2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 a2 <unknown>

vfmadd.vf v1, fa0, v3, v0.t
// CHECK-INST: vfmadd.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xa0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 a0 <unknown>

vfmadd.vf v1, fa0, v3
// CHECK-INST: vfmadd.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xa2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 a2 <unknown>

vfnmadd.vv v1, v2, v3, v0.t
// CHECK-INST: vfnmadd.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xa4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 a4 <unknown>

vfnmadd.vv v1, v2, v3
// CHECK-INST: vfnmadd.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xa6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 a6 <unknown>

vfnmadd.vf v1, fa0, v3, v0.t
// CHECK-INST: vfnmadd.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xa4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 a4 <unknown>

vfnmadd.vf v1, fa0, v3
// CHECK-INST: vfnmadd.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xa6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 a6 <unknown>

vfmsub.vv v1, v2, v3, v0.t
// CHECK-INST: vfmsub.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xa8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 a8 <unknown>

vfmsub.vv v1, v2, v3
// CHECK-INST: vfmsub.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xaa]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 aa <unknown>

vfmsub.vf v1, fa0, v3, v0.t
// CHECK-INST: vfmsub.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xa8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 a8 <unknown>

vfmsub.vf v1, fa0, v3
// CHECK-INST: vfmsub.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xaa]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 aa <unknown>

vfnmsub.vv v1, v2, v3, v0.t
// CHECK-INST: vfnmsub.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xac]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 ac <unknown>

vfnmsub.vv v1, v2, v3
// CHECK-INST: vfnmsub.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xae]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 ae <unknown>

vfnmsub.vf v1, fa0, v3, v0.t
// CHECK-INST: vfnmsub.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xac]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 ac <unknown>

vfnmsub.vf v1, fa0, v3
// CHECK-INST: vfnmsub.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xae]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 ae <unknown>

vfwmacc.vv v1, v2, v3, v0.t
// CHECK-INST: vfwmacc.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xf0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 f0 <unknown>

vfwmacc.vv v1, v2, v3
// CHECK-INST: vfwmacc.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xf2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 f2 <unknown>

vfwmacc.vf v1, fa0, v3, v0.t
// CHECK-INST: vfwmacc.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xf0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 f0 <unknown>

vfwmacc.vf v1, fa0, v3
// CHECK-INST: vfwmacc.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xf2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 f2 <unknown>

vfwnmacc.vv v1, v2, v3, v0.t
// CHECK-INST: vfwnmacc.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xf4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 f4 <unknown>

vfwnmacc.vv v1, v2, v3
// CHECK-INST: vfwnmacc.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xf6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 f6 <unknown>

vfwnmacc.vf v1, fa0, v3, v0.t
// CHECK-INST: vfwnmacc.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xf4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 f4 <unknown>

vfwnmacc.vf v1, fa0, v3
// CHECK-INST: vfwnmacc.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xf6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 f6 <unknown>

vfwmsac.vv v1, v2, v3, v0.t
// CHECK-INST: vfwmsac.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xf8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 f8 <unknown>

vfwmsac.vv v1, v2, v3
// CHECK-INST: vfwmsac.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xfa]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 fa <unknown>

vfwmsac.vf v1, fa0, v3, v0.t
// CHECK-INST: vfwmsac.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xf8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 f8 <unknown>

vfwmsac.vf v1, fa0, v3
// CHECK-INST: vfwmsac.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xfa]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 fa <unknown>

vfwnmsac.vv v1, v2, v3, v0.t
// CHECK-INST: vfwnmsac.vv v1, v2, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xfc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 fc <unknown>

vfwnmsac.vv v1, v2, v3
// CHECK-INST: vfwnmsac.vv v1, v2, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0xfe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 fe <unknown>

vfwnmsac.vf v1, fa0, v3, v0.t
// CHECK-INST: vfwnmsac.vf v1, fa0, v3, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xfc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 fc <unknown>

vfwnmsac.vf v1, fa0, v3
// CHECK-INST: vfwnmsac.vf v1, fa0, v3
// CHECK-ENCODING: [0xd7,0x50,0x35,0xfe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 fe <unknown>
