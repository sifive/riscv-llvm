// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vfcvt.xu.f.v v1, v3, v0.t
// CHECK-INST: vfcvt.xu.f.v v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x30,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 30 88 <unknown>

vfcvt.xu.f.v v1, v3
// CHECK-INST: vfcvt.xu.f.v v1, v3
// CHECK-ENCODING: [0xd7,0x10,0x30,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 30 8a <unknown>

vfcvt.x.f.v v1, v3, v0.t
// CHECK-INST: vfcvt.x.f.v v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x90,0x30,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 30 88 <unknown>

vfcvt.x.f.v v1, v3
// CHECK-INST: vfcvt.x.f.v v1, v3
// CHECK-ENCODING: [0xd7,0x90,0x30,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 30 8a <unknown>

vfcvt.f.xu.v v1, v3, v0.t
// CHECK-INST: vfcvt.f.xu.v v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 88 <unknown>

vfcvt.f.xu.v v1, v3
// CHECK-INST: vfcvt.f.xu.v v1, v3
// CHECK-ENCODING: [0xd7,0x10,0x31,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 8a <unknown>

vfcvt.f.x.v v1, v3, v0.t
// CHECK-INST: vfcvt.f.x.v v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x90,0x31,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 31 88 <unknown>

vfcvt.f.x.v v1, v3
// CHECK-INST: vfcvt.f.x.v v1, v3
// CHECK-ENCODING: [0xd7,0x90,0x31,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 31 8a <unknown>

vfwcvt.xu.f.v v1, v3, v0.t
// CHECK-INST: vfwcvt.xu.f.v v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x34,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 34 88 <unknown>

vfwcvt.xu.f.v v1, v3
// CHECK-INST: vfwcvt.xu.f.v v1, v3
// CHECK-ENCODING: [0xd7,0x10,0x34,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 34 8a <unknown>

vfwcvt.x.f.v v1, v3, v0.t
// CHECK-INST: vfwcvt.x.f.v v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x90,0x34,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 34 88 <unknown>

vfwcvt.x.f.v v1, v3
// CHECK-INST: vfwcvt.x.f.v v1, v3
// CHECK-ENCODING: [0xd7,0x90,0x34,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 34 8a <unknown>

vfwcvt.f.xu.v v1, v3, v0.t
// CHECK-INST: vfwcvt.f.xu.v v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x35,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 35 88 <unknown>

vfwcvt.f.xu.v v1, v3
// CHECK-INST: vfwcvt.f.xu.v v1, v3
// CHECK-ENCODING: [0xd7,0x10,0x35,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 35 8a <unknown>

vfwcvt.f.x.v v1, v3, v0.t
// CHECK-INST: vfwcvt.f.x.v v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x90,0x35,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 35 88 <unknown>

vfwcvt.f.x.v v1, v3
// CHECK-INST: vfwcvt.f.x.v v1, v3
// CHECK-ENCODING: [0xd7,0x90,0x35,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 35 8a <unknown>

vfwcvt.f.f.v v1, v3, v0.t
// CHECK-INST: vfwcvt.f.f.v v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x36,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 36 88 <unknown>

vfwcvt.f.f.v v1, v3
// CHECK-INST: vfwcvt.f.f.v v1, v3
// CHECK-ENCODING: [0xd7,0x10,0x36,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 36 8a <unknown>

vfncvt.xu.f.w v1, v3, v0.t
// CHECK-INST: vfncvt.xu.f.w v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x38,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 38 88 <unknown>

vfncvt.xu.f.w v1, v3
// CHECK-INST: vfncvt.xu.f.w v1, v3
// CHECK-ENCODING: [0xd7,0x10,0x38,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 38 8a <unknown>

vfncvt.x.f.w v1, v3, v0.t
// CHECK-INST: vfncvt.x.f.w v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x90,0x38,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 38 88 <unknown>

vfncvt.x.f.w v1, v3
// CHECK-INST: vfncvt.x.f.w v1, v3
// CHECK-ENCODING: [0xd7,0x90,0x38,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 38 8a <unknown>

vfncvt.f.xu.w v1, v3, v0.t
// CHECK-INST: vfncvt.f.xu.w v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x39,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 39 88 <unknown>

vfncvt.f.xu.w v1, v3
// CHECK-INST: vfncvt.f.xu.w v1, v3
// CHECK-ENCODING: [0xd7,0x10,0x39,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 39 8a <unknown>

vfncvt.f.x.w v1, v3, v0.t
// CHECK-INST: vfncvt.f.x.w v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x90,0x39,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 39 88 <unknown>

vfncvt.f.x.w v1, v3
// CHECK-INST: vfncvt.f.x.w v1, v3
// CHECK-ENCODING: [0xd7,0x90,0x39,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 39 8a <unknown>

vfncvt.f.f.w v1, v3, v0.t
// CHECK-INST: vfncvt.f.f.w v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x3a,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 3a 88 <unknown>

vfncvt.f.f.w v1, v3
// CHECK-INST: vfncvt.f.f.w v1, v3
// CHECK-ENCODING: [0xd7,0x10,0x3a,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 3a 8a <unknown>

vfncvt.rod.f.f.w v1, v3, v0.t
// CHECK-INST: vfncvt.rod.f.f.w v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x90,0x3a,0x88]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 3a 88 <unknown>

vfncvt.rod.f.f.w v1, v3
// CHECK-INST: vfncvt.rod.f.f.w v1, v3
// CHECK-ENCODING: [0xd7,0x90,0x3a,0x8a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 90 3a 8a <unknown>
