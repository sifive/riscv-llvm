; RUN: llc -mtriple=riscv32 -mattr=+d -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefix=RV32IFD %s
; XFAIL: *

define double @test(double %a) nounwind {
  ret double %a
}

define i32 @main() nounwind {
entry:
; Note: test succeeds if loading the double from a global rather than calling
; 'test'
  %call = call double @test(double 2.000000e+00)
  %cmp = fcmp olt double %call, 2.400000e-01
  %cmp2 = fcmp ogt double %call, 2.600000e-01
  %or.cond = or i1 %cmp, %cmp2
  br i1 %or.cond, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  call void @abort()
  unreachable

if.end:                                           ; preds = %entry
  call void @exit(i32 0)
  unreachable
}

declare void @abort()

declare void @exit(i32)
