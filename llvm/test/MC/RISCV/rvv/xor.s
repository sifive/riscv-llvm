// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vxor.vv v1, v3, v2, v0.t
// CHECK-INST: vxor.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x2c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 2c <unknown>

vxor.vv v1, v3, v2
// CHECK-INST: vxor.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x2e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 2e <unknown>

vxor.vx v1, v3, a0, v0.t
// CHECK-INST: vxor.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x2c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 2c <unknown>

vxor.vx v1, v3, a0
// CHECK-INST: vxor.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x2e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 2e <unknown>

vxor.vi v1, v3, 15, v0.t
// CHECK-INST: vxor.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x2c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 2c <unknown>

vxor.vi v1, v3, 15
// CHECK-INST: vxor.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x2e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 2e <unknown>
