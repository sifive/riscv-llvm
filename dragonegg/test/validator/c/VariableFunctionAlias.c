// RUN: %dragonegg -S %s -o - | FileCheck %s

void qux(void) { };
extern int foo __attribute__ ((alias ("qux")));
// CHECK: @foo = alias void ()* @qux
