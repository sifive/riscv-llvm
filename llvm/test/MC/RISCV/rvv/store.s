// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vsb.v v4, (a0), v0.t
// CHECK-INST: vsb.v v4, (a0), v0.t
// CHECK-ENCODING: [0x27,0x02,0x05,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 02 05 00 <unknown>

vsb.v v4, (a0)
// CHECK-INST: vsb.v v4, (a0)
// CHECK-ENCODING: [0x27,0x02,0x05,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 02 05 02 <unknown>

vsh.v v4, (a0), v0.t
// CHECK-INST: vsh.v v4, (a0), v0.t
// CHECK-ENCODING: [0x27,0x52,0x05,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 52 05 00 <unknown>

vsh.v v4, (a0)
// CHECK-INST: vsh.v v4, (a0)
// CHECK-ENCODING: [0x27,0x52,0x05,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 52 05 02 <unknown>

vsw.v v4, (a0), v0.t
// CHECK-INST: vsw.v v4, (a0), v0.t
// CHECK-ENCODING: [0x27,0x62,0x05,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 62 05 00 <unknown>

vsw.v v4, (a0)
// CHECK-INST: vsw.v v4, (a0)
// CHECK-ENCODING: [0x27,0x62,0x05,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 62 05 02 <unknown>

vse.v v4, (a0), v0.t
// CHECK-INST: vse.v v4, (a0), v0.t
// CHECK-ENCODING: [0x27,0x72,0x05,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 72 05 00 <unknown>

vse.v v4, (a0)
// CHECK-INST: vse.v v4, (a0)
// CHECK-ENCODING: [0x27,0x72,0x05,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 72 05 02 <unknown>

vssb.v v4, (a0), a1, v0.t
// CHECK-INST: vssb.v v4, (a0), a1, v0.t
// CHECK-ENCODING: [0x27,0x02,0xb5,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 02 b5 08 <unknown>

vssb.v v4, (a0), a1
// CHECK-INST: vssb.v v4, (a0), a1
// CHECK-ENCODING: [0x27,0x02,0xb5,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 02 b5 0a <unknown>

vssh.v v4, (a0), a1, v0.t
// CHECK-INST: vssh.v v4, (a0), a1, v0.t
// CHECK-ENCODING: [0x27,0x52,0xb5,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 52 b5 08 <unknown>

vssh.v v4, (a0), a1
// CHECK-INST: vssh.v v4, (a0), a1
// CHECK-ENCODING: [0x27,0x52,0xb5,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 52 b5 0a <unknown>

vssw.v v4, (a0), a1, v0.t
// CHECK-INST: vssw.v v4, (a0), a1, v0.t
// CHECK-ENCODING: [0x27,0x62,0xb5,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 62 b5 08 <unknown>

vssw.v v4, (a0), a1
// CHECK-INST: vssw.v v4, (a0), a1
// CHECK-ENCODING: [0x27,0x62,0xb5,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 62 b5 0a <unknown>

vsse.v v4, (a0), a1, v0.t
// CHECK-INST: vsse.v v4, (a0), a1, v0.t
// CHECK-ENCODING: [0x27,0x72,0xb5,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 72 b5 08 <unknown>

vsse.v v4, (a0), a1
// CHECK-INST: vsse.v v4, (a0), a1
// CHECK-ENCODING: [0x27,0x72,0xb5,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 72 b5 0a <unknown>

vsxb.v v4, (a0), v3, v0.t
// CHECK-INST: vsxb.v v4, (a0), v3, v0.t
// CHECK-ENCODING: [0x27,0x02,0x35,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 02 35 0c <unknown>

vsxb.v v4, (a0), v3
// CHECK-INST: vsxb.v v4, (a0), v3
// CHECK-ENCODING: [0x27,0x02,0x35,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 02 35 0e <unknown>

vsxh.v v4, (a0), v3, v0.t
// CHECK-INST: vsxh.v v4, (a0), v3, v0.t
// CHECK-ENCODING: [0x27,0x52,0x35,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 52 35 0c <unknown>

vsxh.v v4, (a0), v3
// CHECK-INST: vsxh.v v4, (a0), v3
// CHECK-ENCODING: [0x27,0x52,0x35,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 52 35 0e <unknown>

vsxw.v v4, (a0), v3, v0.t
// CHECK-INST: vsxw.v v4, (a0), v3, v0.t
// CHECK-ENCODING: [0x27,0x62,0x35,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 62 35 0c <unknown>

vsxw.v v4, (a0), v3
// CHECK-INST: vsxw.v v4, (a0), v3
// CHECK-ENCODING: [0x27,0x62,0x35,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 62 35 0e <unknown>

vsxe.v v4, (a0), v3, v0.t
// CHECK-INST: vsxe.v v4, (a0), v3, v0.t
// CHECK-ENCODING: [0x27,0x72,0x35,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 72 35 0c <unknown>

vsxe.v v4, (a0), v3
// CHECK-INST: vsxe.v v4, (a0), v3
// CHECK-ENCODING: [0x27,0x72,0x35,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 72 35 0e <unknown>

vsuxb.v v4, (a0), v3, v0.t
// CHECK-INST: vsuxb.v v4, (a0), v3, v0.t
// CHECK-ENCODING: [0x27,0x02,0x35,0x1c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 02 35 1c <unknown>

vsuxb.v v4, (a0), v3
// CHECK-INST: vsuxb.v v4, (a0), v3
// CHECK-ENCODING: [0x27,0x02,0x35,0x1e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 02 35 1e <unknown>

vsuxh.v v4, (a0), v3, v0.t
// CHECK-INST: vsuxh.v v4, (a0), v3, v0.t
// CHECK-ENCODING: [0x27,0x52,0x35,0x1c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 52 35 1c <unknown>

vsuxh.v v4, (a0), v3
// CHECK-INST: vsuxh.v v4, (a0), v3
// CHECK-ENCODING: [0x27,0x52,0x35,0x1e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 52 35 1e <unknown>

vsuxw.v v4, (a0), v3, v0.t
// CHECK-INST: vsuxw.v v4, (a0), v3, v0.t
// CHECK-ENCODING: [0x27,0x62,0x35,0x1c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 62 35 1c <unknown>

vsuxw.v v4, (a0), v3
// CHECK-INST: vsuxw.v v4, (a0), v3
// CHECK-ENCODING: [0x27,0x62,0x35,0x1e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 62 35 1e <unknown>

vsuxe.v v4, (a0), v3, v0.t
// CHECK-INST: vsuxe.v v4, (a0), v3, v0.t
// CHECK-ENCODING: [0x27,0x72,0x35,0x1c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 72 35 1c <unknown>

vsuxe.v v4, (a0), v3
// CHECK-INST: vsuxe.v v4, (a0), v3
// CHECK-ENCODING: [0x27,0x72,0x35,0x1e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 27 72 35 1e <unknown>

vs1r.v v1, (a0)
// CHECK-INST: vs1r.v v1, (a0)
// CHECK-ENCODING: [0xa7,0x70,0x85,0x22]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: a7 70 85 22 <unknown>

vs2r.v v1, (a0)
// CHECK-INST: vs2r.v v1, (a0)
// CHECK-ENCODING: [0xa7,0x70,0x85,0x42]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: a7 70 85 42 <unknown>

vs3r.v v1, (a0)
// CHECK-INST: vs3r.v v1, (a0)
// CHECK-ENCODING: [0xa7,0x70,0x85,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: a7 70 85 62 <unknown>

vs4r.v v1, (a0)
// CHECK-INST: vs4r.v v1, (a0)
// CHECK-ENCODING: [0xa7,0x70,0x85,0x82]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: a7 70 85 82 <unknown>

vs5r.v v1, (a0)
// CHECK-INST: vs5r.v v1, (a0)
// CHECK-ENCODING: [0xa7,0x70,0x85,0xa2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: a7 70 85 a2 <unknown>

vs6r.v v1, (a0)
// CHECK-INST: vs6r.v v1, (a0)
// CHECK-ENCODING: [0xa7,0x70,0x85,0xc2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: a7 70 85 c2 <unknown>

vs7r.v v1, (a0)
// CHECK-INST: vs7r.v v1, (a0)
// CHECK-ENCODING: [0xa7,0x70,0x85,0xe2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: a7 70 85 e2 <unknown>
