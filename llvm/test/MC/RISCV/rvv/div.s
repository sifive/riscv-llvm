// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vdivu.vv v1, v3, v2, v0.t
// CHECK-INST: vdivu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x80]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 80 <unknown>

vdivu.vv v1, v3, v2
// CHECK-INST: vdivu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x82]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 82 <unknown>

vdivu.vx v1, v3, a0, v0.t
// CHECK-INST: vdivu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0x80]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 80 <unknown>

vdivu.vx v1, v3, a0
// CHECK-INST: vdivu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0x82]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 82 <unknown>

vdiv.vv v1, v3, v2, v0.t
// CHECK-INST: vdiv.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x84]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 84 <unknown>

vdiv.vv v1, v3, v2
// CHECK-INST: vdiv.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x86]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 86 <unknown>

vdiv.vx v1, v3, a0, v0.t
// CHECK-INST: vdiv.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0x84]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 84 <unknown>

vdiv.vx v1, v3, a0
// CHECK-INST: vdiv.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0x86]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 86 <unknown>

vremu.vv v1, v3, v2, v0.t
// CHECK-INST: vremu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 88 <unknown>

vremu.vv v1, v3, v2
// CHECK-INST: vremu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 8a <unknown>

vremu.vx v1, v3, a0, v0.t
// CHECK-INST: vremu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 88 <unknown>

vremu.vx v1, v3, a0
// CHECK-INST: vremu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 8a <unknown>

vrem.vv v1, v3, v2, v0.t
// CHECK-INST: vrem.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x8c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 8c <unknown>

vrem.vv v1, v3, v2
// CHECK-INST: vrem.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x8e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 8e <unknown>

vrem.vx v1, v3, a0, v0.t
// CHECK-INST: vrem.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0x8c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 8c <unknown>

vrem.vx v1, v3, a0
// CHECK-INST: vrem.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0x8e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 8e <unknown>

vfdiv.vv v1, v3, v2, v0.t
// CHECK-INST: vfdiv.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x80]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 80 <unknown>

vfdiv.vv v1, v3, v2
// CHECK-INST: vfdiv.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x82]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 82 <unknown>

vfdiv.vf v1, v3, fa0, v0.t
// CHECK-INST: vfdiv.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x80]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 80 <unknown>

vfdiv.vf v1, v3, fa0
// CHECK-INST: vfdiv.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x82]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 82 <unknown>

vfrdiv.vf v1, v3, fa0, v0.t
// CHECK-INST: vfrdiv.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x84]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 84 <unknown>

vfrdiv.vf v1, v3, fa0
// CHECK-INST: vfrdiv.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x86]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 86 <unknown>
