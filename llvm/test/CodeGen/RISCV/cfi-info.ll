; RUN: llc -mtriple=riscv32 -verify-machineinstrs < %s | FileCheck %s

define i32 @callee(i32 %a, i64 %b, i32 %c, i32 %d, double %e) {
; CHECK-LABEL: callee:
; CHECK: addi sp, sp, -32
; CHECK: sw ra, 28(sp)
; CHECK: sw s1, 24(sp)
; CHECK: sw s2, 20(sp)
; CHECK: sw s3, 16(sp)
; CHECK: sw s4, 12(sp)
; CHECK: .cfi_def_cfa_offset 32
; CHECK: .cfi_offset 1, -4
; CHECK: .cfi_offset 9, -8
; CHECK: .cfi_offset 18, -12
; CHECK: .cfi_offset 19, -16
; CHECK: .cfi_offset 20, -20
  %b_trunc = trunc i64 %b to i32
  %e_fptosi = fptosi double %e to i32
  %1 = add i32 %a, %b_trunc
  %2 = add i32 %1, %c
  %3 = add i32 %2, %d
  %4 = add i32 %3, %e_fptosi
  ret i32 %4
}
