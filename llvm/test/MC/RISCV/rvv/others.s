// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vmerge.vvm v1, v3, v2, v0
// CHECK-INST: vmerge.vvm v1, v3, v2, v0
// CHECK-ENCODING: [0xd7,0x00,0x31,0x5c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 5c <unknown>

vmerge.vxm v1, v3, a0, v0
// CHECK-INST: vmerge.vxm v1, v3, a0, v0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x5c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 5c <unknown>

vmerge.vim v1, v3, 15, v0
// CHECK-INST: vmerge.vim v1, v3, 15, v0
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x5c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 5c <unknown>

vslideup.vx v1, v3, a0, v0.t
// CHECK-INST: vslideup.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x38]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 38 <unknown>

vslideup.vx v1, v3, a0
// CHECK-INST: vslideup.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x3a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 3a <unknown>

vslideup.vi v1, v3, 31, v0.t
// CHECK-INST: vslideup.vi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0x38]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f 38 <unknown>

vslideup.vi v1, v3, 31
// CHECK-INST: vslideup.vi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0x3a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f 3a <unknown>

vslidedown.vx v1, v3, a0, v0.t
// CHECK-INST: vslidedown.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x3c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 3c <unknown>

vslidedown.vx v1, v3, a0
// CHECK-INST: vslidedown.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x3e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 3e <unknown>

vslidedown.vi v1, v3, 31, v0.t
// CHECK-INST: vslidedown.vi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0x3c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f 3c <unknown>

vslidedown.vi v1, v3, 31
// CHECK-INST: vslidedown.vi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0x3e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f 3e <unknown>

vslide1up.vx v1, v3, a0, v0.t
// CHECK-INST: vslide1up.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0x38]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 38 <unknown>

vslide1up.vx v1, v3, a0
// CHECK-INST: vslide1up.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0x3a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 3a <unknown>

vslide1down.vx v1, v3, a0, v0.t
// CHECK-INST: vslide1down.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0x3c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 3c <unknown>

vslide1down.vx v1, v3, a0
// CHECK-INST: vslide1down.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0x3e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 3e <unknown>

vrgather.vv v1, v3, v2, v0.t
// CHECK-INST: vrgather.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x30]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 30 <unknown>

vrgather.vv v1, v3, v2
// CHECK-INST: vrgather.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x32]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 32 <unknown>

vrgather.vx v1, v3, a0, v0.t
// CHECK-INST: vrgather.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x30]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 30 <unknown>

vrgather.vx v1, v3, a0
// CHECK-INST: vrgather.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x32]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 32 <unknown>

vrgather.vi v1, v3, 31, v0.t
// CHECK-INST: vrgather.vi v1, v3, 31, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0x30]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f 30 <unknown>

vrgather.vi v1, v3, 31
// CHECK-INST: vrgather.vi v1, v3, 31
// CHECK-ENCODING: [0xd7,0xb0,0x3f,0x32]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 3f 32 <unknown>

vcompress.vm v1, v3, v2
// CHECK-INST: vcompress.vm v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x5e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 5e <unknown>

vfsqrt.v v1, v3, v0.t
// CHECK-INST: vfsqrt.v v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x30,0x8c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 30 8c <unknown>

vfsqrt.v v1, v3
// CHECK-INST: vfsqrt.v v1, v3
// CHECK-ENCODING: [0xd7,0x10,0x30,0x8e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 30 8e <unknown>

vfclass.v v1, v3, v0.t
// CHECK-INST: vfclass.v v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x38,0x8c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 38 8c <unknown>

vfclass.v v1, v3
// CHECK-INST: vfclass.v v1, v3
// CHECK-ENCODING: [0xd7,0x10,0x38,0x8e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 38 8e <unknown>

vfmerge.vfm v1, v3, fa0, v0
// CHECK-INST: vfmerge.vfm v1, v3, fa0, v0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x5c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 5c <unknown>
