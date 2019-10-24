// RUN: llvm-mc -triple=riscv64 -show-encoding -mattr=+v < %s \
// RUN:        | FileCheck %s --check-prefixes=CHECK-ENCODING,CHECK-INST
// RUN: not llvm-mc -triple=riscv64 -show-encoding < %s 2>&1 \
// RUN:        | FileCheck %s --check-prefix=CHECK-ERROR
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d -mattr=+v - | FileCheck %s --check-prefix=CHECK-INST
// RUN: llvm-mc -triple=riscv64 -filetype=obj -mattr=+v < %s \
// RUN:        | llvm-objdump -d - | FileCheck %s --check-prefix=CHECK-UNKNOWN
vredsum.vs v1, v3, v2, v0.t
// CHECK-INST: vredsum.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x00]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 00 <unknown>

vredsum.vs v1, v3, v2
// CHECK-INST: vredsum.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x02]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 02 <unknown>

vredmaxu.vs v1, v3, v2, v0.t
// CHECK-INST: vredmaxu.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x18]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 18 <unknown>

vredmaxu.vs v1, v3, v2
// CHECK-INST: vredmaxu.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x1a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 1a <unknown>

vredmax.vs v1, v3, v2, v0.t
// CHECK-INST: vredmax.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x1c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 1c <unknown>

vredmax.vs v1, v3, v2
// CHECK-INST: vredmax.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x1e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 1e <unknown>

vredminu.vs v1, v3, v2, v0.t
// CHECK-INST: vredminu.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x10]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 10 <unknown>

vredminu.vs v1, v3, v2
// CHECK-INST: vredminu.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x12]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 12 <unknown>

vredmin.vs v1, v3, v2, v0.t
// CHECK-INST: vredmin.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x14]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 14 <unknown>

vredmin.vs v1, v3, v2
// CHECK-INST: vredmin.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x16]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 16 <unknown>

vredand.vs v1, v3, v2, v0.t
// CHECK-INST: vredand.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x04]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 04 <unknown>

vredand.vs v1, v3, v2
// CHECK-INST: vredand.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x06]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 06 <unknown>

vredor.vs v1, v3, v2, v0.t
// CHECK-INST: vredor.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x08]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 08 <unknown>

vredor.vs v1, v3, v2
// CHECK-INST: vredor.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x0a]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 0a <unknown>

vredxor.vs v1, v3, v2, v0.t
// CHECK-INST: vredxor.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x20,0x31,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 0c <unknown>

vredxor.vs v1, v3, v2
// CHECK-INST: vredxor.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x20,0x31,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 20 31 0e <unknown>

vwredsumu.vs v1, v3, v2, v0.t
// CHECK-INST: vwredsumu.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xc0]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 c0 <unknown>

vwredsumu.vs v1, v3, v2
// CHECK-INST: vwredsumu.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0xc2]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 c2 <unknown>

vwredsum.vs v1, v3, v2, v0.t
// CHECK-INST: vwredsum.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x00,0x31,0xc4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 c4 <unknown>

vwredsum.vs v1, v3, v2
// CHECK-INST: vwredsum.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x00,0x31,0xc6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 00 31 c6 <unknown>

vfredosum.vs v1, v3, v2, v0.t
// CHECK-INST: vfredosum.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x0c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 0c <unknown>

vfredosum.vs v1, v3, v2
// CHECK-INST: vfredosum.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x0e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 0e <unknown>

vfredsum.vs v1, v3, v2, v0.t
// CHECK-INST: vfredsum.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x04]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 04 <unknown>

vfredsum.vs v1, v3, v2
// CHECK-INST: vfredsum.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x06]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 06 <unknown>

vfredmax.vs v1, v3, v2, v0.t
// CHECK-INST: vfredmax.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x1c]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 1c <unknown>

vfredmax.vs v1, v3, v2
// CHECK-INST: vfredmax.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x1e]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 1e <unknown>

vfredmin.vs v1, v3, v2, v0.t
// CHECK-INST: vfredmin.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0x14]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 14 <unknown>

vfredmin.vs v1, v3, v2
// CHECK-INST: vfredmin.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0x16]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 16 <unknown>

vfwredosum.vs v1, v3, v2, v0.t
// CHECK-INST: vfwredosum.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xcc]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 cc <unknown>

vfwredosum.vs v1, v3, v2
// CHECK-INST: vfwredosum.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0xce]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 ce <unknown>

vfwredsum.vs v1, v3, v2, v0.t
// CHECK-INST: vfwredsum.vs v1, v3, v2, v0.t
// CHECK-ENCODING: [0xd7,0x10,0x31,0xc4]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 c4 <unknown>

vfwredsum.vs v1, v3, v2
// CHECK-INST: vfwredsum.vs v1, v3, v2
// CHECK-ENCODING: [0xd7,0x10,0x31,0xc6]
// CHECK-ERROR: instruction use requires an option to be enabled
// CHECK-UNKNOWN: d7 10 31 c6 <unknown>
