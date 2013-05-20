// RUN: %dragonegg -S %s -o - | FileCheck %s
// XFAIL: gcc-4.7, gcc-4.8

void use(int*);

void foo(int n, int i) {
// CHECK: foo
  int a[n];
// CHECK: alloca i8, i{{.*}}, align 16
  use(&a[i]);
}
