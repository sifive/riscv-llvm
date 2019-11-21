// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vlb.v v1, (a0), v0.t
// CHECK-INST: vlb.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x00,0x05,0x10]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 05 10 <unknown>

vlb.v v1, (a0)
// CHECK-INST: vlb.v v1, (a0)
// CHECK-ENCODING: [0x87,0x00,0x05,0x12]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 05 12 <unknown>

vlh.v v1, (a0), v0.t
// CHECK-INST: vlh.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x50,0x05,0x10]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 05 10 <unknown>

vlh.v v1, (a0)
// CHECK-INST: vlh.v v1, (a0)
// CHECK-ENCODING: [0x87,0x50,0x05,0x12]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 05 12 <unknown>

vlw.v v1, (a0), v0.t
// CHECK-INST: vlw.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x60,0x05,0x10]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 05 10 <unknown>

vlw.v v1, (a0)
// CHECK-INST: vlw.v v1, (a0)
// CHECK-ENCODING: [0x87,0x60,0x05,0x12]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 05 12 <unknown>

vlbu.v v1, (a0), v0.t
// CHECK-INST: vlbu.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x00,0x05,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 05 00 <unknown>

vlbu.v v1, (a0)
// CHECK-INST: vlbu.v v1, (a0)
// CHECK-ENCODING: [0x87,0x00,0x05,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 05 02 <unknown>

vlhu.v v1, (a0), v0.t
// CHECK-INST: vlhu.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x50,0x05,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 05 00 <unknown>

vlhu.v v1, (a0)
// CHECK-INST: vlhu.v v1, (a0)
// CHECK-ENCODING: [0x87,0x50,0x05,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 05 02 <unknown>

vlwu.v v1, (a0), v0.t
// CHECK-INST: vlwu.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x60,0x05,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 05 00 <unknown>

vlwu.v v1, (a0)
// CHECK-INST: vlwu.v v1, (a0)
// CHECK-ENCODING: [0x87,0x60,0x05,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 05 02 <unknown>

vlbff.v v1, (a0), v0.t
// CHECK-INST: vlbff.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x00,0x05,0x11]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 05 11 <unknown>

vlbff.v v1, (a0)
// CHECK-INST: vlbff.v v1, (a0)
// CHECK-ENCODING: [0x87,0x00,0x05,0x13]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 05 13 <unknown>

vlhff.v v1, (a0), v0.t
// CHECK-INST: vlhff.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x50,0x05,0x11]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 05 11 <unknown>

vlhff.v v1, (a0)
// CHECK-INST: vlhff.v v1, (a0)
// CHECK-ENCODING: [0x87,0x50,0x05,0x13]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 05 13 <unknown>

vlwff.v v1, (a0), v0.t
// CHECK-INST: vlwff.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x60,0x05,0x11]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 05 11 <unknown>

vlwff.v v1, (a0)
// CHECK-INST: vlwff.v v1, (a0)
// CHECK-ENCODING: [0x87,0x60,0x05,0x13]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 05 13 <unknown>

vlbuff.v v1, (a0), v0.t
// CHECK-INST: vlbuff.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x00,0x05,0x01]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 05 01 <unknown>

vlbuff.v v1, (a0)
// CHECK-INST: vlbuff.v v1, (a0)
// CHECK-ENCODING: [0x87,0x00,0x05,0x03]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 05 03 <unknown>

vlhuff.v v1, (a0), v0.t
// CHECK-INST: vlhuff.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x50,0x05,0x01]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 05 01 <unknown>

vlhuff.v v1, (a0)
// CHECK-INST: vlhuff.v v1, (a0)
// CHECK-ENCODING: [0x87,0x50,0x05,0x03]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 05 03 <unknown>

vlwuff.v v1, (a0), v0.t
// CHECK-INST: vlwuff.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x60,0x05,0x01]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 05 01 <unknown>

vlwuff.v v1, (a0)
// CHECK-INST: vlwuff.v v1, (a0)
// CHECK-ENCODING: [0x87,0x60,0x05,0x03]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 05 03 <unknown>

vleff.v v1, (a0), v0.t
// CHECK-INST: vleff.v v1, (a0), v0.t
// CHECK-ENCODING: [0x87,0x70,0x05,0x01]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 05 01 <unknown>

vleff.v v1, (a0)
// CHECK-INST: vleff.v v1, (a0)
// CHECK-ENCODING: [0x87,0x70,0x05,0x03]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 05 03 <unknown>

vlsb.v v1, (a0), a1, v0.t
// CHECK-INST: vlsb.v v1, (a0), a1, v0.t
// CHECK-ENCODING: [0x87,0x00,0xb5,0x18]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 b5 18 <unknown>

vlsb.v v1, (a0), a1
// CHECK-INST: vlsb.v v1, (a0), a1
// CHECK-ENCODING: [0x87,0x00,0xb5,0x1a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 b5 1a <unknown>

vlsh.v v1, (a0), a1, v0.t
// CHECK-INST: vlsh.v v1, (a0), a1, v0.t
// CHECK-ENCODING: [0x87,0x50,0xb5,0x18]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 b5 18 <unknown>

vlsh.v v1, (a0), a1
// CHECK-INST: vlsh.v v1, (a0), a1
// CHECK-ENCODING: [0x87,0x50,0xb5,0x1a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 b5 1a <unknown>

vlsw.v v1, (a0), a1, v0.t
// CHECK-INST: vlsw.v v1, (a0), a1, v0.t
// CHECK-ENCODING: [0x87,0x60,0xb5,0x18]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 b5 18 <unknown>

vlsw.v v1, (a0), a1
// CHECK-INST: vlsw.v v1, (a0), a1
// CHECK-ENCODING: [0x87,0x60,0xb5,0x1a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 b5 1a <unknown>

vlsbu.v v1, (a0), a1, v0.t
// CHECK-INST: vlsbu.v v1, (a0), a1, v0.t
// CHECK-ENCODING: [0x87,0x00,0xb5,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 b5 08 <unknown>

vlsbu.v v1, (a0), a1
// CHECK-INST: vlsbu.v v1, (a0), a1
// CHECK-ENCODING: [0x87,0x00,0xb5,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 b5 0a <unknown>

vlshu.v v1, (a0), a1, v0.t
// CHECK-INST: vlshu.v v1, (a0), a1, v0.t
// CHECK-ENCODING: [0x87,0x50,0xb5,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 b5 08 <unknown>

vlshu.v v1, (a0), a1
// CHECK-INST: vlshu.v v1, (a0), a1
// CHECK-ENCODING: [0x87,0x50,0xb5,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 b5 0a <unknown>

vlswu.v v1, (a0), a1, v0.t
// CHECK-INST: vlswu.v v1, (a0), a1, v0.t
// CHECK-ENCODING: [0x87,0x60,0xb5,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 b5 08 <unknown>

vlswu.v v1, (a0), a1
// CHECK-INST: vlswu.v v1, (a0), a1
// CHECK-ENCODING: [0x87,0x60,0xb5,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 b5 0a <unknown>

vlse.v v1, (a0), a1, v0.t
// CHECK-INST: vlse.v v1, (a0), a1, v0.t
// CHECK-ENCODING: [0x87,0x70,0xb5,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 b5 08 <unknown>

vlse.v v1, (a0), a1
// CHECK-INST: vlse.v v1, (a0), a1
// CHECK-ENCODING: [0x87,0x70,0xb5,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 b5 0a <unknown>

vlxb.v v1, (a0), v3, v0.t
// CHECK-INST: vlxb.v v1, (a0), v3, v0.t
// CHECK-ENCODING: [0x87,0x00,0x35,0x1c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 35 1c <unknown>

vlxb.v v1, (a0), v3
// CHECK-INST: vlxb.v v1, (a0), v3
// CHECK-ENCODING: [0x87,0x00,0x35,0x1e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 35 1e <unknown>

vlxh.v v1, (a0), v3, v0.t
// CHECK-INST: vlxh.v v1, (a0), v3, v0.t
// CHECK-ENCODING: [0x87,0x50,0x35,0x1c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 35 1c <unknown>

vlxh.v v1, (a0), v3
// CHECK-INST: vlxh.v v1, (a0), v3
// CHECK-ENCODING: [0x87,0x50,0x35,0x1e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 35 1e <unknown>

vlxw.v v1, (a0), v3, v0.t
// CHECK-INST: vlxw.v v1, (a0), v3, v0.t
// CHECK-ENCODING: [0x87,0x60,0x35,0x1c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 35 1c <unknown>

vlxw.v v1, (a0), v3
// CHECK-INST: vlxw.v v1, (a0), v3
// CHECK-ENCODING: [0x87,0x60,0x35,0x1e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 35 1e <unknown>

vlxbu.v v1, (a0), v3, v0.t
// CHECK-INST: vlxbu.v v1, (a0), v3, v0.t
// CHECK-ENCODING: [0x87,0x00,0x35,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 35 0c <unknown>

vlxbu.v v1, (a0), v3
// CHECK-INST: vlxbu.v v1, (a0), v3
// CHECK-ENCODING: [0x87,0x00,0x35,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 00 35 0e <unknown>

vlxhu.v v1, (a0), v3, v0.t
// CHECK-INST: vlxhu.v v1, (a0), v3, v0.t
// CHECK-ENCODING: [0x87,0x50,0x35,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 35 0c <unknown>

vlxhu.v v1, (a0), v3
// CHECK-INST: vlxhu.v v1, (a0), v3
// CHECK-ENCODING: [0x87,0x50,0x35,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 50 35 0e <unknown>

vlxwu.v v1, (a0), v3, v0.t
// CHECK-INST: vlxwu.v v1, (a0), v3, v0.t
// CHECK-ENCODING: [0x87,0x60,0x35,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 35 0c <unknown>

vlxwu.v v1, (a0), v3
// CHECK-INST: vlxwu.v v1, (a0), v3
// CHECK-ENCODING: [0x87,0x60,0x35,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 60 35 0e <unknown>

vlxe.v v1, (a0), v3, v0.t
// CHECK-INST: vlxe.v v1, (a0), v3, v0.t
// CHECK-ENCODING: [0x87,0x70,0x35,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 35 0c <unknown>

vlxe.v v1, (a0), v3
// CHECK-INST: vlxe.v v1, (a0), v3
// CHECK-ENCODING: [0x87,0x70,0x35,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 35 0e <unknown>

vl1r.v v1, (a0)
// CHECK-INST: vl1r.v v1, (a0)
// CHECK-ENCODING: [0x87,0x70,0x85,0x22]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 85 22 <unknown>

vl2r.v v1, (a0)
// CHECK-INST: vl2r.v v1, (a0)
// CHECK-ENCODING: [0x87,0x70,0x85,0x42]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 85 42 <unknown>

vl3r.v v1, (a0)
// CHECK-INST: vl3r.v v1, (a0)
// CHECK-ENCODING: [0x87,0x70,0x85,0x62]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 85 62 <unknown>

vl4r.v v1, (a0)
// CHECK-INST: vl4r.v v1, (a0)
// CHECK-ENCODING: [0x87,0x70,0x85,0x82]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 85 82 <unknown>

vl5r.v v1, (a0)
// CHECK-INST: vl5r.v v1, (a0)
// CHECK-ENCODING: [0x87,0x70,0x85,0xa2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 85 a2 <unknown>

vl6r.v v1, (a0)
// CHECK-INST: vl6r.v v1, (a0)
// CHECK-ENCODING: [0x87,0x70,0x85,0xc2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 85 c2 <unknown>

vl7r.v v1, (a0)
// CHECK-INST: vl7r.v v1, (a0)
// CHECK-ENCODING: [0x87,0x70,0x85,0xe2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: 87 70 85 e2 <unknown>
