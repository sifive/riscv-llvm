// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vmul.vv v1, v3, v2, v0.t
// CHECK-INST: vmul.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x94]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 94 <unknown>

vmul.vv v1, v3, v2
// CHECK-INST: vmul.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x96]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 96 <unknown>

vmul.vx v1, v3, a0, v0.t
// CHECK-INST: vmul.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0x94]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 94 <unknown>

vmul.vx v1, v3, a0
// CHECK-INST: vmul.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0x96]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 96 <unknown>

vmulh.vv v1, v3, v2, v0.t
// CHECK-INST: vmulh.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x9c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 9c <unknown>

vmulh.vv v1, v3, v2
// CHECK-INST: vmulh.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x9e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 9e <unknown>

vmulh.vx v1, v3, a0, v0.t
// CHECK-INST: vmulh.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0x9c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 9c <unknown>

vmulh.vx v1, v3, a0
// CHECK-INST: vmulh.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0x9e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 9e <unknown>

vmulhu.vv v1, v3, v2, v0.t
// CHECK-INST: vmulhu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x90]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 90 <unknown>

vmulhu.vv v1, v3, v2
// CHECK-INST: vmulhu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x92]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 92 <unknown>

vmulhu.vx v1, v3, a0, v0.t
// CHECK-INST: vmulhu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0x90]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 90 <unknown>

vmulhu.vx v1, v3, a0
// CHECK-INST: vmulhu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0x92]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 92 <unknown>

vmulhsu.vv v1, v3, v2, v0.t
// CHECK-INST: vmulhsu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x98]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 98 <unknown>

vmulhsu.vv v1, v3, v2
// CHECK-INST: vmulhsu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x9a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 9a <unknown>

vmulhsu.vx v1, v3, a0, v0.t
// CHECK-INST: vmulhsu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0x98]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 98 <unknown>

vmulhsu.vx v1, v3, a0
// CHECK-INST: vmulhsu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0x9a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 9a <unknown>

vwmul.vv v1, v3, v2, v0.t
// CHECK-INST: vwmul.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xec]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 ec <unknown>

vwmul.vv v1, v3, v2
// CHECK-INST: vwmul.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0xee]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 ee <unknown>

vwmul.vx v1, v3, a0, v0.t
// CHECK-INST: vwmul.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xec]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 ec <unknown>

vwmul.vx v1, v3, a0
// CHECK-INST: vwmul.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0xee]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 ee <unknown>

vwmulu.vv v1, v3, v2, v0.t
// CHECK-INST: vwmulu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xe0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 e0 <unknown>

vwmulu.vv v1, v3, v2
// CHECK-INST: vwmulu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0xe2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 e2 <unknown>

vwmulu.vx v1, v3, a0, v0.t
// CHECK-INST: vwmulu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xe0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 e0 <unknown>

vwmulu.vx v1, v3, a0
// CHECK-INST: vwmulu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0xe2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 e2 <unknown>

vwmulsu.vv v1, v3, v2, v0.t
// CHECK-INST: vwmulsu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0xe8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 e8 <unknown>

vwmulsu.vv v1, v3, v2
// CHECK-INST: vwmulsu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0xea]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 ea <unknown>

vwmulsu.vx v1, v3, a0, v0.t
// CHECK-INST: vwmulsu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x60,0x35,0xe8]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 e8 <unknown>

vwmulsu.vx v1, v3, a0
// CHECK-INST: vwmulsu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x60,0x35,0xea]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 60 35 ea <unknown>

vsmul.vv v1, v3, v2, v0.t
// CHECK-INST: vsmul.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x9c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 9c <unknown>

vsmul.vv v1, v3, v2
// CHECK-INST: vsmul.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x9e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 9e <unknown>

vsmul.vx v1, v3, a0, v0.t
// CHECK-INST: vsmul.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x9c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 9c <unknown>

vsmul.vx v1, v3, a0
// CHECK-INST: vsmul.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x9e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 9e <unknown>

vfmul.vv v1, v3, v2, v0.t
// CHECK-INST: vfmul.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x90]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 90 <unknown>

vfmul.vv v1, v3, v2
// CHECK-INST: vfmul.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x92]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 92 <unknown>

vfmul.vf v1, v3, fa0, v0.t
// CHECK-INST: vfmul.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x90]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 90 <unknown>

vfmul.vf v1, v3, fa0
// CHECK-INST: vfmul.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x92]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 92 <unknown>

vfwmul.vv v1, v3, v2, v0.t
// CHECK-INST: vfwmul.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xe0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 e0 <unknown>

vfwmul.vv v1, v3, v2
// CHECK-INST: vfwmul.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0xe2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 e2 <unknown>

vfwmul.vf v1, v3, fa0, v0.t
// CHECK-INST: vfwmul.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0xe0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 e0 <unknown>

vfwmul.vf v1, v3, fa0
// CHECK-INST: vfwmul.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0xe2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 e2 <unknown>
