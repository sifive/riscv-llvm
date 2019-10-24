// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vsetvli a2, a0, e32,m4
// CHECK-INST: vsetvli a2, a0, e32,m4
// CHECK-ENCODING: [0x57,0x76,0xa5,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 76 a5 00 <unknown>

vsetvli a2, a0, e32
// CHECK-INST: vsetvli a2, a0, e32,m1
// CHECK-ENCODING: [0x57,0x76,0x85,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 76 85 00 <unknown>

vsetvl a2, a0, a1
// CHECK-INST: vsetvl a2, a0, a1
// CHECK-ENCODING: [0x57,0x76,0xb5,0x80]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 76 b5 80 <unknown>
