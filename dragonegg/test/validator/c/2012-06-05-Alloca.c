// RUN: %dragonegg -S %s -o - | FileCheck %s
// PR13025
// XFAIL: gcc-4.5, gcc-4.6

void use(int*);

void foo(int n, int i) {
// CHECK: foo
  int a[n];
// CHECK: alloca i8, i{{.*}}, align 4
  use(&a[i]);
}
