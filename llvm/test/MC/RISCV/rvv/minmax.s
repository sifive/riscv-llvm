// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vminu.vv v1, v3, v2, v0.t
// CHECK-INST: vminu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x10]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 10 <unknown>

vminu.vv v1, v3, v2
// CHECK-INST: vminu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x12]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 12 <unknown>

vminu.vx v1, v3, a0, v0.t
// CHECK-INST: vminu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x10]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 10 <unknown>

vminu.vx v1, v3, a0
// CHECK-INST: vminu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x12]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 12 <unknown>

vmin.vv v1, v3, v2, v0.t
// CHECK-INST: vmin.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x14]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 14 <unknown>

vmin.vv v1, v3, v2
// CHECK-INST: vmin.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x16]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 16 <unknown>

vmin.vx v1, v3, a0, v0.t
// CHECK-INST: vmin.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x14]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 14 <unknown>

vmin.vx v1, v3, a0
// CHECK-INST: vmin.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x16]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 16 <unknown>

vmaxu.vv v1, v3, v2, v0.t
// CHECK-INST: vmaxu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x18]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 18 <unknown>

vmaxu.vv v1, v3, v2
// CHECK-INST: vmaxu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x1a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 1a <unknown>

vmaxu.vx v1, v3, a0, v0.t
// CHECK-INST: vmaxu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x18]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 18 <unknown>

vmaxu.vx v1, v3, a0
// CHECK-INST: vmaxu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x1a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 1a <unknown>

vmax.vv v1, v3, v2, v0.t
// CHECK-INST: vmax.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x1c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 1c <unknown>

vmax.vv v1, v3, v2
// CHECK-INST: vmax.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x1e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 1e <unknown>

vmax.vx v1, v3, a0, v0.t
// CHECK-INST: vmax.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x1c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 1c <unknown>

vmax.vx v1, v3, a0
// CHECK-INST: vmax.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x1e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 1e <unknown>

vfmin.vv v1, v3, v2, v0.t
// CHECK-INST: vfmin.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x10]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 10 <unknown>

vfmin.vv v1, v3, v2
// CHECK-INST: vfmin.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x12]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 12 <unknown>

vfmin.vf v1, v3, fa0, v0.t
// CHECK-INST: vfmin.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x10]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 10 <unknown>

vfmin.vf v1, v3, fa0
// CHECK-INST: vfmin.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x12]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 12 <unknown>

vfmax.vv v1, v3, v2, v0.t
// CHECK-INST: vfmax.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x18]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 18 <unknown>

vfmax.vv v1, v3, v2
// CHECK-INST: vfmax.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x1a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 1a <unknown>

vfmax.vf v1, v3, fa0, v0.t
// CHECK-INST: vfmax.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x18]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 18 <unknown>

vfmax.vf v1, v3, fa0
// CHECK-INST: vfmax.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x1a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 1a <unknown>
