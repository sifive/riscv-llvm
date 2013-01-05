// RUN: %dragonegg -S %s -o - | FileCheck %s
// CHECK-NOT: assume_aligned
// XFAIL: gcc-4.5, gcc-4.6
int *foo(int *p) {
  return __builtin_assume_aligned(p, 16);
}
