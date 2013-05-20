// RUN: %dragonegg -S %s -o - | FileCheck %s
// XFAIL: gcc-4.8

int qux;
void foo(void) __attribute__ ((alias ("qux")));
// CHECK: @foo = alias i32* @qux
