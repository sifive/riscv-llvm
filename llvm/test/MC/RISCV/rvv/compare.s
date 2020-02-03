// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vmseq.vv v8, v4, v20, v0.t
// CHECK-INST: vmseq.vv v8, v4, v20, v0.t
// CHECK-ENCODING: [0x57,0x04,0x4a,0x60]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 60 <unknown>

vmseq.vv v8, v4, v20
// CHECK-INST: vmseq.vv v8, v4, v20
// CHECK-ENCODING: [0x57,0x04,0x4a,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 62 <unknown>

vmseq.vx v8, v4, a0, v0.t
// CHECK-INST: vmseq.vx v8, v4, a0, v0.t
// CHECK-ENCODING: [0x57,0x44,0x45,0x60]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 60 <unknown>

vmseq.vx v8, v4, a0
// CHECK-INST: vmseq.vx v8, v4, a0
// CHECK-ENCODING: [0x57,0x44,0x45,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 62 <unknown>

vmseq.vi v8, v4, 15, v0.t
// CHECK-INST: vmseq.vi v8, v4, 15, v0.t
// CHECK-ENCODING: [0x57,0xb4,0x47,0x60]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 60 <unknown>

vmseq.vi v8, v4, 15
// CHECK-INST: vmseq.vi v8, v4, 15
// CHECK-ENCODING: [0x57,0xb4,0x47,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 62 <unknown>

vmsne.vv v8, v4, v20, v0.t
// CHECK-INST: vmsne.vv v8, v4, v20, v0.t
// CHECK-ENCODING: [0x57,0x04,0x4a,0x64]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 64 <unknown>

vmsne.vv v8, v4, v20
// CHECK-INST: vmsne.vv v8, v4, v20
// CHECK-ENCODING: [0x57,0x04,0x4a,0x66]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 66 <unknown>

vmsne.vx v8, v4, a0, v0.t
// CHECK-INST: vmsne.vx v8, v4, a0, v0.t
// CHECK-ENCODING: [0x57,0x44,0x45,0x64]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 64 <unknown>

vmsne.vx v8, v4, a0
// CHECK-INST: vmsne.vx v8, v4, a0
// CHECK-ENCODING: [0x57,0x44,0x45,0x66]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 66 <unknown>

vmsne.vi v8, v4, 15, v0.t
// CHECK-INST: vmsne.vi v8, v4, 15, v0.t
// CHECK-ENCODING: [0x57,0xb4,0x47,0x64]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 64 <unknown>

vmsne.vi v8, v4, 15
// CHECK-INST: vmsne.vi v8, v4, 15
// CHECK-ENCODING: [0x57,0xb4,0x47,0x66]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 66 <unknown>

vmsltu.vv v8, v4, v20, v0.t
// CHECK-INST: vmsltu.vv v8, v4, v20, v0.t
// CHECK-ENCODING: [0x57,0x04,0x4a,0x68]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 68 <unknown>

vmsltu.vv v8, v4, v20
// CHECK-INST: vmsltu.vv v8, v4, v20
// CHECK-ENCODING: [0x57,0x04,0x4a,0x6a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 6a <unknown>

vmsltu.vx v8, v4, a0, v0.t
// CHECK-INST: vmsltu.vx v8, v4, a0, v0.t
// CHECK-ENCODING: [0x57,0x44,0x45,0x68]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 68 <unknown>

vmsltu.vx v8, v4, a0
// CHECK-INST: vmsltu.vx v8, v4, a0
// CHECK-ENCODING: [0x57,0x44,0x45,0x6a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 6a <unknown>

vmslt.vv v8, v4, v20, v0.t
// CHECK-INST: vmslt.vv v8, v4, v20, v0.t
// CHECK-ENCODING: [0x57,0x04,0x4a,0x6c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 6c <unknown>

vmslt.vv v8, v4, v20
// CHECK-INST: vmslt.vv v8, v4, v20
// CHECK-ENCODING: [0x57,0x04,0x4a,0x6e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 6e <unknown>

vmslt.vx v8, v4, a0, v0.t
// CHECK-INST: vmslt.vx v8, v4, a0, v0.t
// CHECK-ENCODING: [0x57,0x44,0x45,0x6c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 6c <unknown>

vmslt.vx v8, v4, a0
// CHECK-INST: vmslt.vx v8, v4, a0
// CHECK-ENCODING: [0x57,0x44,0x45,0x6e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 6e <unknown>

vmsleu.vv v8, v4, v20, v0.t
// CHECK-INST: vmsleu.vv v8, v4, v20, v0.t
// CHECK-ENCODING: [0x57,0x04,0x4a,0x70]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 70 <unknown>

vmsleu.vv v8, v4, v20
// CHECK-INST: vmsleu.vv v8, v4, v20
// CHECK-ENCODING: [0x57,0x04,0x4a,0x72]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 72 <unknown>

vmsleu.vx v8, v4, a0, v0.t
// CHECK-INST: vmsleu.vx v8, v4, a0, v0.t
// CHECK-ENCODING: [0x57,0x44,0x45,0x70]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 70 <unknown>

vmsleu.vx v8, v4, a0
// CHECK-INST: vmsleu.vx v8, v4, a0
// CHECK-ENCODING: [0x57,0x44,0x45,0x72]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 72 <unknown>

vmsleu.vi v8, v4, 15, v0.t
// CHECK-INST: vmsleu.vi v8, v4, 15, v0.t
// CHECK-ENCODING: [0x57,0xb4,0x47,0x70]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 70 <unknown>

vmsleu.vi v8, v4, 15
// CHECK-INST: vmsleu.vi v8, v4, 15
// CHECK-ENCODING: [0x57,0xb4,0x47,0x72]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 72 <unknown>

vmsle.vv v8, v4, v20, v0.t
// CHECK-INST: vmsle.vv v8, v4, v20, v0.t
// CHECK-ENCODING: [0x57,0x04,0x4a,0x74]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 74 <unknown>

vmsle.vv v8, v4, v20
// CHECK-INST: vmsle.vv v8, v4, v20
// CHECK-ENCODING: [0x57,0x04,0x4a,0x76]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 04 4a 76 <unknown>

vmsle.vx v8, v4, a0, v0.t
// CHECK-INST: vmsle.vx v8, v4, a0, v0.t
// CHECK-ENCODING: [0x57,0x44,0x45,0x74]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 74 <unknown>

vmsle.vx v8, v4, a0
// CHECK-INST: vmsle.vx v8, v4, a0
// CHECK-ENCODING: [0x57,0x44,0x45,0x76]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 76 <unknown>

vmsle.vi v8, v4, 15, v0.t
// CHECK-INST: vmsle.vi v8, v4, 15, v0.t
// CHECK-ENCODING: [0x57,0xb4,0x47,0x74]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 74 <unknown>

vmsle.vi v8, v4, 15
// CHECK-INST: vmsle.vi v8, v4, 15
// CHECK-ENCODING: [0x57,0xb4,0x47,0x76]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 76 <unknown>

vmsgtu.vx v8, v4, a0, v0.t
// CHECK-INST: vmsgtu.vx v8, v4, a0, v0.t
// CHECK-ENCODING: [0x57,0x44,0x45,0x78]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 78 <unknown>

vmsgtu.vx v8, v4, a0
// CHECK-INST: vmsgtu.vx v8, v4, a0
// CHECK-ENCODING: [0x57,0x44,0x45,0x7a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 7a <unknown>

vmsgtu.vi v8, v4, 15, v0.t
// CHECK-INST: vmsgtu.vi v8, v4, 15, v0.t
// CHECK-ENCODING: [0x57,0xb4,0x47,0x78]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 78 <unknown>

vmsgtu.vi v8, v4, 15
// CHECK-INST: vmsgtu.vi v8, v4, 15
// CHECK-ENCODING: [0x57,0xb4,0x47,0x7a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 7a <unknown>

vmsgt.vx v8, v4, a0, v0.t
// CHECK-INST: vmsgt.vx v8, v4, a0, v0.t
// CHECK-ENCODING: [0x57,0x44,0x45,0x7c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 7c <unknown>

vmsgt.vx v8, v4, a0
// CHECK-INST: vmsgt.vx v8, v4, a0
// CHECK-ENCODING: [0x57,0x44,0x45,0x7e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 44 45 7e <unknown>

vmsgt.vi v8, v4, 15, v0.t
// CHECK-INST: vmsgt.vi v8, v4, 15, v0.t
// CHECK-ENCODING: [0x57,0xb4,0x47,0x7c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 7c <unknown>

vmsgt.vi v8, v4, 15
// CHECK-INST: vmsgt.vi v8, v4, 15
// CHECK-ENCODING: [0x57,0xb4,0x47,0x7e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 b4 47 7e <unknown>

vmfeq.vv v8, v4, v20, v0.t
// CHECK-INST: vmfeq.vv v8, v4, v20, v0.t
// CHECK-ENCODING: [0x57,0x14,0x4a,0x60]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 14 4a 60 <unknown>

vmfeq.vv v8, v4, v20
// CHECK-INST: vmfeq.vv v8, v4, v20
// CHECK-ENCODING: [0x57,0x14,0x4a,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 14 4a 62 <unknown>

vmfeq.vf v8, v4, fa0, v0.t
// CHECK-INST: vmfeq.vf v8, v4, fa0, v0.t
// CHECK-ENCODING: [0x57,0x54,0x45,0x60]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 60 <unknown>

vmfeq.vf v8, v4, fa0
// CHECK-INST: vmfeq.vf v8, v4, fa0
// CHECK-ENCODING: [0x57,0x54,0x45,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 62 <unknown>

vmfne.vv v8, v4, v20, v0.t
// CHECK-INST: vmfne.vv v8, v4, v20, v0.t
// CHECK-ENCODING: [0x57,0x14,0x4a,0x70]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 14 4a 70 <unknown>

vmfne.vv v8, v4, v20
// CHECK-INST: vmfne.vv v8, v4, v20
// CHECK-ENCODING: [0x57,0x14,0x4a,0x72]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 14 4a 72 <unknown>

vmfne.vf v8, v4, fa0, v0.t
// CHECK-INST: vmfne.vf v8, v4, fa0, v0.t
// CHECK-ENCODING: [0x57,0x54,0x45,0x70]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 70 <unknown>

vmfne.vf v8, v4, fa0
// CHECK-INST: vmfne.vf v8, v4, fa0
// CHECK-ENCODING: [0x57,0x54,0x45,0x72]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 72 <unknown>

vmflt.vv v8, v4, v20, v0.t
// CHECK-INST: vmflt.vv v8, v4, v20, v0.t
// CHECK-ENCODING: [0x57,0x14,0x4a,0x6c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 14 4a 6c <unknown>

vmflt.vv v8, v4, v20
// CHECK-INST: vmflt.vv v8, v4, v20
// CHECK-ENCODING: [0x57,0x14,0x4a,0x6e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 14 4a 6e <unknown>

vmflt.vf v8, v4, fa0, v0.t
// CHECK-INST: vmflt.vf v8, v4, fa0, v0.t
// CHECK-ENCODING: [0x57,0x54,0x45,0x6c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 6c <unknown>

vmflt.vf v8, v4, fa0
// CHECK-INST: vmflt.vf v8, v4, fa0
// CHECK-ENCODING: [0x57,0x54,0x45,0x6e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 6e <unknown>

vmfle.vv v8, v4, v20, v0.t
// CHECK-INST: vmfle.vv v8, v4, v20, v0.t
// CHECK-ENCODING: [0x57,0x14,0x4a,0x64]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 14 4a 64 <unknown>

vmfle.vv v8, v4, v20
// CHECK-INST: vmfle.vv v8, v4, v20
// CHECK-ENCODING: [0x57,0x14,0x4a,0x66]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 14 4a 66 <unknown>

vmfle.vf v8, v4, fa0, v0.t
// CHECK-INST: vmfle.vf v8, v4, fa0, v0.t
// CHECK-ENCODING: [0x57,0x54,0x45,0x64]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 64 <unknown>

vmfle.vf v8, v4, fa0
// CHECK-INST: vmfle.vf v8, v4, fa0
// CHECK-ENCODING: [0x57,0x54,0x45,0x66]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 66 <unknown>

vmfgt.vf v8, v4, fa0, v0.t
// CHECK-INST: vmfgt.vf v8, v4, fa0, v0.t
// CHECK-ENCODING: [0x57,0x54,0x45,0x74]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 74 <unknown>

vmfgt.vf v8, v4, fa0
// CHECK-INST: vmfgt.vf v8, v4, fa0
// CHECK-ENCODING: [0x57,0x54,0x45,0x76]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 76 <unknown>

vmfge.vf v8, v4, fa0, v0.t
// CHECK-INST: vmfge.vf v8, v4, fa0, v0.t
// CHECK-ENCODING: [0x57,0x54,0x45,0x7c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 7c <unknown>

vmfge.vf v8, v4, fa0
// CHECK-INST: vmfge.vf v8, v4, fa0
// CHECK-ENCODING: [0x57,0x54,0x45,0x7e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 57 54 45 7e <unknown>

