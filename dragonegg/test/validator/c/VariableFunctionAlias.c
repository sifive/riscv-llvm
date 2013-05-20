// RUN: %dragonegg -S %s -o - | FileCheck %s
// XFAIL: gcc-4.8

void qux(void) { };
extern int foo __attribute__ ((alias ("qux")));
// CHECK: @foo = alias void ()* @qux
