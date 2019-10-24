// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vmseq.vv v1, v3, v2, v0.t
// CHECK-INST: vmseq.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x60]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 60 <unknown>

vmseq.vv v1, v3, v2
// CHECK-INST: vmseq.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 62 <unknown>

vmseq.vx v1, v3, a0, v0.t
// CHECK-INST: vmseq.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x60]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 60 <unknown>

vmseq.vx v1, v3, a0
// CHECK-INST: vmseq.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 62 <unknown>

vmseq.vi v1, v3, 15, v0.t
// CHECK-INST: vmseq.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x60]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 60 <unknown>

vmseq.vi v1, v3, 15
// CHECK-INST: vmseq.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 62 <unknown>

vmsne.vv v1, v3, v2, v0.t
// CHECK-INST: vmsne.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x64]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 64 <unknown>

vmsne.vv v1, v3, v2
// CHECK-INST: vmsne.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x66]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 66 <unknown>

vmsne.vx v1, v3, a0, v0.t
// CHECK-INST: vmsne.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x64]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 64 <unknown>

vmsne.vx v1, v3, a0
// CHECK-INST: vmsne.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x66]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 66 <unknown>

vmsne.vi v1, v3, 15, v0.t
// CHECK-INST: vmsne.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x64]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 64 <unknown>

vmsne.vi v1, v3, 15
// CHECK-INST: vmsne.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x66]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 66 <unknown>

vmsltu.vv v1, v3, v2, v0.t
// CHECK-INST: vmsltu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x68]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 68 <unknown>

vmsltu.vv v1, v3, v2
// CHECK-INST: vmsltu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x6a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 6a <unknown>

vmsltu.vx v1, v3, a0, v0.t
// CHECK-INST: vmsltu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x68]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 68 <unknown>

vmsltu.vx v1, v3, a0
// CHECK-INST: vmsltu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x6a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 6a <unknown>

vmslt.vv v1, v3, v2, v0.t
// CHECK-INST: vmslt.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x6c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 6c <unknown>

vmslt.vv v1, v3, v2
// CHECK-INST: vmslt.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x6e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 6e <unknown>

vmslt.vx v1, v3, a0, v0.t
// CHECK-INST: vmslt.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x6c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 6c <unknown>

vmslt.vx v1, v3, a0
// CHECK-INST: vmslt.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x6e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 6e <unknown>

vmsleu.vv v1, v3, v2, v0.t
// CHECK-INST: vmsleu.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x70]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 70 <unknown>

vmsleu.vv v1, v3, v2
// CHECK-INST: vmsleu.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x72]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 72 <unknown>

vmsleu.vx v1, v3, a0, v0.t
// CHECK-INST: vmsleu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x70]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 70 <unknown>

vmsleu.vx v1, v3, a0
// CHECK-INST: vmsleu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x72]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 72 <unknown>

vmsleu.vi v1, v3, 15, v0.t
// CHECK-INST: vmsleu.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x70]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 70 <unknown>

vmsleu.vi v1, v3, 15
// CHECK-INST: vmsleu.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x72]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 72 <unknown>

vmsle.vv v1, v3, v2, v0.t
// CHECK-INST: vmsle.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0x74]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 74 <unknown>

vmsle.vv v1, v3, v2
// CHECK-INST: vmsle.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0x76]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 76 <unknown>

vmsle.vx v1, v3, a0, v0.t
// CHECK-INST: vmsle.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x74]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 74 <unknown>

vmsle.vx v1, v3, a0
// CHECK-INST: vmsle.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x76]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 76 <unknown>

vmsle.vi v1, v3, 15, v0.t
// CHECK-INST: vmsle.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x74]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 74 <unknown>

vmsle.vi v1, v3, 15
// CHECK-INST: vmsle.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x76]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 76 <unknown>

vmsgtu.vx v1, v3, a0, v0.t
// CHECK-INST: vmsgtu.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x78]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 78 <unknown>

vmsgtu.vx v1, v3, a0
// CHECK-INST: vmsgtu.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x7a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 7a <unknown>

vmsgtu.vi v1, v3, 15, v0.t
// CHECK-INST: vmsgtu.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x78]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 78 <unknown>

vmsgtu.vi v1, v3, 15
// CHECK-INST: vmsgtu.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x7a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 7a <unknown>

vmsgt.vx v1, v3, a0, v0.t
// CHECK-INST: vmsgt.vx v1, v3, a0, v0.t
// CHECK-ENCODING: [0xd7,0x40,0x35,0x7c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 7c <unknown>

vmsgt.vx v1, v3, a0
// CHECK-INST: vmsgt.vx v1, v3, a0
// CHECK-ENCODING: [0xd7,0x40,0x35,0x7e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 40 35 7e <unknown>

vmsgt.vi v1, v3, 15, v0.t
// CHECK-INST: vmsgt.vi v1, v3, 15, v0.t
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x7c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 7c <unknown>

vmsgt.vi v1, v3, 15
// CHECK-INST: vmsgt.vi v1, v3, 15
// CHECK-ENCODING: [0xd7,0xb0,0x37,0x7e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 b0 37 7e <unknown>

vmfeq.vv v1, v3, v2, v0.t
// CHECK-INST: vmfeq.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x60]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 60 <unknown>

vmfeq.vv v1, v3, v2
// CHECK-INST: vmfeq.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 62 <unknown>

vmfeq.vf v1, v3, fa0, v0.t
// CHECK-INST: vmfeq.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x60]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 60 <unknown>

vmfeq.vf v1, v3, fa0
// CHECK-INST: vmfeq.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 62 <unknown>

vmfne.vv v1, v3, v2, v0.t
// CHECK-INST: vmfne.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x70]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 70 <unknown>

vmfne.vv v1, v3, v2
// CHECK-INST: vmfne.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x72]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 72 <unknown>

vmfne.vf v1, v3, fa0, v0.t
// CHECK-INST: vmfne.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x70]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 70 <unknown>

vmfne.vf v1, v3, fa0
// CHECK-INST: vmfne.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x72]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 72 <unknown>

vmflt.vv v1, v3, v2, v0.t
// CHECK-INST: vmflt.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x6c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 6c <unknown>

vmflt.vv v1, v3, v2
// CHECK-INST: vmflt.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x6e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 6e <unknown>

vmflt.vf v1, v3, fa0, v0.t
// CHECK-INST: vmflt.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x6c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 6c <unknown>

vmflt.vf v1, v3, fa0
// CHECK-INST: vmflt.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x6e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 6e <unknown>

vmfle.vv v1, v3, v2, v0.t
// CHECK-INST: vmfle.vv v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x64]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 64 <unknown>

vmfle.vv v1, v3, v2
// CHECK-INST: vmfle.vv v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x66]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 66 <unknown>

vmfle.vf v1, v3, fa0, v0.t
// CHECK-INST: vmfle.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x64]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 64 <unknown>

vmfle.vf v1, v3, fa0
// CHECK-INST: vmfle.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x66]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 66 <unknown>

vmfgt.vf v1, v3, fa0, v0.t
// CHECK-INST: vmfgt.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x74]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 74 <unknown>

vmfgt.vf v1, v3, fa0
// CHECK-INST: vmfgt.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x76]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 76 <unknown>

vmfge.vf v1, v3, fa0, v0.t
// CHECK-INST: vmfge.vf v1, v3, fa0, v0.t
// CHECK-ENCODING: [0xd7,0x50,0x35,0x7c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 7c <unknown>

vmfge.vf v1, v3, fa0
// CHECK-INST: vmfge.vf v1, v3, fa0
// CHECK-ENCODING: [0xd7,0x50,0x35,0x7e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 50 35 7e <unknown>
