// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vfsgnj.vv v1, v3, v2, v0.t
// CHECK-INST: vfsgnj.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x20]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 20 <unknown>

vfsgnj.vv v1, v3, v2
// CHECK-INST: vfsgnj.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x22]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 22 <unknown>

vfsgnj.vf v1, v3, fa0, v0.t
// CHECK-INST: vfsgnj.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x20]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 20 <unknown>

vfsgnj.vf v1, v3, fa0
// CHECK-INST: vfsgnj.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x22]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 22 <unknown>

vfsgnjn.vv v1, v3, v2, v0.t
// CHECK-INST: vfsgnjn.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x24]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 24 <unknown>

vfsgnjn.vv v1, v3, v2
// CHECK-INST: vfsgnjn.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x26]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 26 <unknown>

vfsgnjn.vf v1, v3, fa0, v0.t
// CHECK-INST: vfsgnjn.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x24]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 24 <unknown>

vfsgnjn.vf v1, v3, fa0
// CHECK-INST: vfsgnjn.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x26]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 26 <unknown>

vfsgnjx.vv v1, v3, v2, v0.t
// CHECK-INST: vfsgnjx.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x28]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 28 <unknown>

vfsgnjx.vv v1, v3, v2
// CHECK-INST: vfsgnjx.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x2a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 2a <unknown>

vfsgnjx.vf v1, v3, fa0, v0.t
// CHECK-INST: vfsgnjx.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x28]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 28 <unknown>

vfsgnjx.vf v1, v3, fa0
// CHECK-INST: vfsgnjx.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x2a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 2a <unknown>
