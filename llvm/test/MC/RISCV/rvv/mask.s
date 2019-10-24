// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vmand.mm v1, v3, v2
// CHECK-INST: vmand.mm v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x66]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 66 <unknown>

vmnand.mm v1, v3, v2
// CHECK-INST: vmnand.mm v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x76]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 76 <unknown>

vmandnot.mm v1, v3, v2
// CHECK-INST: vmandnot.mm v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 62 <unknown>

vmxor.mm v1, v3, v2
// CHECK-INST: vmxor.mm v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x6e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 6e <unknown>

vmor.mm v1, v3, v2
// CHECK-INST: vmor.mm v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x6a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 6a <unknown>

vmnor.mm v1, v3, v2
// CHECK-INST: vmnor.mm v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x7a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 7a <unknown>

vmornot.mm v1, v3, v2
// CHECK-INST: vmornot.mm v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x72]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 72 <unknown>

vmxnor.mm v1, v3, v2
// CHECK-INST: vmxnor.mm v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x7e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 7e <unknown>

vpopc.m a2, v3, v0.t
// CHECK-INST: vpopc.m a2, v3, v0.t
// CHECK-ENCODING: [0x57,0x26,0x38,0x40]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 26 38 40 <unknown>

vpopc.m a2, v3
// CHECK-INST: vpopc.m a2, v3
// CHECK-ENCODING: [0x57,0x26,0x38,0x42]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 26 38 42 <unknown>

vfirst.m a2, v3, v0.t
// CHECK-INST: vfirst.m a2, v3, v0.t
// CHECK-ENCODING: [0x57,0xa6,0x38,0x40]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 a6 38 40 <unknown>

vfirst.m a2, v3
// CHECK-INST: vfirst.m a2, v3
// CHECK-ENCODING: [0x57,0xa6,0x38,0x42]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 a6 38 42 <unknown>

vmsbf.m v1, v3, v0.t
// CHECK-INST: vmsbf.m v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0xa0,0x30,0x50]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 a0 30 50 <unknown>

vmsbf.m v1, v3
// CHECK-INST: vmsbf.m v1, v3
// CHECK-ENCODING: [0xd7,0xa0,0x30,0x52]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 a0 30 52 <unknown>

vmsif.m v1, v3, v0.t
// CHECK-INST: vmsif.m v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0xa0,0x31,0x50]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 a0 31 50 <unknown>

vmsif.m v1, v3
// CHECK-INST: vmsif.m v1, v3
// CHECK-ENCODING: [0xd7,0xa0,0x31,0x52]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 a0 31 52 <unknown>

vmsof.m v1, v3, v0.t
// CHECK-INST: vmsof.m v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x50]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 50 <unknown>

vmsof.m v1, v3
// CHECK-INST: vmsof.m v1, v3
// CHECK-ENCODING: [0xd7,0x20,0x31,0x52]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 52 <unknown>

viota.m v1, v3, v0.t
// CHECK-INST: viota.m v1, v3, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x38,0x50]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 38 50 <unknown>

viota.m v1, v3
// CHECK-INST: viota.m v1, v3
// CHECK-ENCODING: [0xd7,0x20,0x38,0x52]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 38 52 <unknown>

vid.v v1, v0.t
// CHECK-INST: vid.v v1, v0.t
// CHECK-ENCODING: [0xd7,0xa0,0x08,0x50]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 a0 08 50 <unknown>

vid.v v1
// CHECK-INST: vid.v v1
// CHECK-ENCODING: [0xd7,0xa0,0x08,0x52]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 a0 08 52 <unknown>
