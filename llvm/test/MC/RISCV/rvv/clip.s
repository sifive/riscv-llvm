// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vnclipu.wv v1, v3, v2, v0.t
// CHECK-INST: vnclipu.wv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xb8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 b8 <unknown>

vnclipu.wv v1, v3, v2
// CHECK-INST: vnclipu.wv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0xba]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 ba <unknown>

vnclipu.wx v1, v3, a0, v0.t
// CHECK-INST: vnclipu.wx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xb8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 b8 <unknown>

vnclipu.wx v1, v3, a0
// CHECK-INST: vnclipu.wx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0xba]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 ba <unknown>

vnclipu.wi v1, v3, 31, v0.t
// CHECK-INST: vnclipu.wi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xb8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f b8 <unknown>

vnclipu.wi v1, v3, 31
// CHECK-INST: vnclipu.wi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xba]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f ba <unknown>

vnclip.wv v1, v3, v2, v0.t
// CHECK-INST: vnclip.wv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xbc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 bc <unknown>

vnclip.wv v1, v3, v2
// CHECK-INST: vnclip.wv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0xbe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 be <unknown>

vnclip.wx v1, v3, a0, v0.t
// CHECK-INST: vnclip.wx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0xbc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 bc <unknown>

vnclip.wx v1, v3, a0
// CHECK-INST: vnclip.wx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0xbe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 be <unknown>

vnclip.wi v1, v3, 31, v0.t
// CHECK-INST: vnclip.wi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xbc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f bc <unknown>

vnclip.wi v1, v3, 31
// CHECK-INST: vnclip.wi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0xbe]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f be <unknown>
