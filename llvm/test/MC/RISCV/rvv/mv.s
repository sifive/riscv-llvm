// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vmv.v.v v8, v20
// CHECK-INST: vmv.v.v v8, v20
// CHECK-ENCODING: [0x57,0x04,0x0a,0x5e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 0a 5e <unknown>

vmv.v.x v8, a0
// CHECK-INST: vmv.v.x v8, a0
// CHECK-ENCODING: [0x57,0x44,0x05,0x5e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 05 5e <unknown>

vmv.v.i v8, 15
// CHECK-INST: vmv.v.i v8, 15
// CHECK-ENCODING: [0x57,0xb4,0x07,0x5e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 07 5e <unknown>

vmv.x.s a2, v4
// CHECK-INST: vmv.x.s a2, v4
// CHECK-ENCODING: [0x57,0x26,0x40,0x42]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 26 40 42 <unknown>

vmv.s.x v8, a0
// CHECK-INST: vmv.s.x v8, a0
// CHECK-ENCODING: [0x57,0x64,0x05,0x42]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 64 05 42 <unknown>

vfmv.v.f v8, fa0
// CHECK-INST: vfmv.v.f v8, fa0
// CHECK-ENCODING: [0x57,0x54,0x05,0x5e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 05 5e <unknown>

vfmv.f.s fa0, v4
// CHECK-INST: vfmv.f.s fa0, v4
// CHECK-ENCODING: [0x57,0x15,0x40,0x42]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 15 40 42 <unknown>

vfmv.s.f v8, fa0
// CHECK-INST: vfmv.s.f v8, fa0
// CHECK-ENCODING: [0x57,0x54,0x05,0x42]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 05 42 <unknown>

