// RUN: %dragonegg -S %s -o - | FileCheck %s
// XFAIL: gcc-4.7

void use(int*);

void foo(int n, int i) {
// CHECK: foo
  int a[n];
// CHECK: alloca i8, i64 {{.*}}, align 16
  use(&a[i]);
}
