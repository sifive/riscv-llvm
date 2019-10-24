// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vsll.vv v1, v3, v2, v0.t
// CHECK-INST: vsll.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x94]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 94 <unknown>

vsll.vv v1, v3, v2
// CHECK-INST: vsll.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x96]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 96 <unknown>

vsll.vx v1, v3, a0, v0.t
// CHECK-INST: vsll.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x94]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 94 <unknown>

vsll.vx v1, v3, a0
// CHECK-INST: vsll.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x96]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 96 <unknown>

vsll.vi v1, v3, 31, v0.t
// CHECK-INST: vsll.vi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0x94]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f 94 <unknown>

vsll.vi v1, v3, 31
// CHECK-INST: vsll.vi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0x96]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f 96 <unknown>

vsrl.vv v1, v3, v2, v0.t
// CHECK-INST: vsrl.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xa0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 a0 <unknown>

vsrl.vv v1, v3, v2
// CHECK-INST: vsrl.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0xa2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 a2 <unknown>

vsrl.vx v1, v3, a0, v0.t
// CHECK-INST: vsrl.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xa0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 a0 <unknown>

vsrl.vx v1, v3, a0
// CHECK-INST: vsrl.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0xa2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 a2 <unknown>

vsrl.vi v1, v3, 31, v0.t
// CHECK-INST: vsrl.vi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xa0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f a0 <unknown>

vsrl.vi v1, v3, 31
// CHECK-INST: vsrl.vi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xa2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f a2 <unknown>

vsra.vv v1, v3, v2, v0.t
// CHECK-INST: vsra.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xa4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 a4 <unknown>

vsra.vv v1, v3, v2
// CHECK-INST: vsra.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0xa6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 a6 <unknown>

vsra.vx v1, v3, a0, v0.t
// CHECK-INST: vsra.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xa4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 a4 <unknown>

vsra.vx v1, v3, a0
// CHECK-INST: vsra.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0xa6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 a6 <unknown>

vsra.vi v1, v3, 31, v0.t
// CHECK-INST: vsra.vi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xa4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f a4 <unknown>

vsra.vi v1, v3, 31
// CHECK-INST: vsra.vi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xa6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f a6 <unknown>

vnsrl.wv v1, v3, v2, v0.t
// CHECK-INST: vnsrl.wv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xb0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 b0 <unknown>

vnsrl.wv v1, v3, v2
// CHECK-INST: vnsrl.wv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0xb2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 b2 <unknown>

vnsrl.wx v1, v3, a0, v0.t
// CHECK-INST: vnsrl.wx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xb0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 b0 <unknown>

vnsrl.wx v1, v3, a0
// CHECK-INST: vnsrl.wx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0xb2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 b2 <unknown>

vnsrl.wi v1, v3, 31, v0.t
// CHECK-INST: vnsrl.wi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xb0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f b0 <unknown>

vnsrl.wi v1, v3, 31
// CHECK-INST: vnsrl.wi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xb2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f b2 <unknown>

vnsra.wv v1, v3, v2, v0.t
// CHECK-INST: vnsra.wv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xb4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 b4 <unknown>

vnsra.wv v1, v3, v2
// CHECK-INST: vnsra.wv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0xb6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 b6 <unknown>

vnsra.wx v1, v3, a0, v0.t
// CHECK-INST: vnsra.wx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xb4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 b4 <unknown>

vnsra.wx v1, v3, a0
// CHECK-INST: vnsra.wx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0xb6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 b6 <unknown>

vnsra.wi v1, v3, 31, v0.t
// CHECK-INST: vnsra.wi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xb4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f b4 <unknown>

vnsra.wi v1, v3, 31
// CHECK-INST: vnsra.wi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xb6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f b6 <unknown>

vssrl.vv v1, v3, v2, v0.t
// CHECK-INST: vssrl.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xa8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 a8 <unknown>

vssrl.vv v1, v3, v2
// CHECK-INST: vssrl.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0xaa]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 aa <unknown>

vssrl.vx v1, v3, a0, v0.t
// CHECK-INST: vssrl.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xa8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 a8 <unknown>

vssrl.vx v1, v3, a0
// CHECK-INST: vssrl.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0xaa]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 aa <unknown>

vssrl.vi v1, v3, 31, v0.t
// CHECK-INST: vssrl.vi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xa8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f a8 <unknown>

vssrl.vi v1, v3, 31
// CHECK-INST: vssrl.vi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xaa]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f aa <unknown>

vssra.vv v1, v3, v2, v0.t
// CHECK-INST: vssra.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xac]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 ac <unknown>

vssra.vv v1, v3, v2
// CHECK-INST: vssra.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0xae]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 ae <unknown>

vssra.vx v1, v3, a0, v0.t
// CHECK-INST: vssra.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xac]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 ac <unknown>

vssra.vx v1, v3, a0
// CHECK-INST: vssra.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0xae]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 ae <unknown>

vssra.vi v1, v3, 31, v0.t
// CHECK-INST: vssra.vi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xac]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f ac <unknown>

vssra.vi v1, v3, 31
// CHECK-INST: vssra.vi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xae]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f ae <unknown>
