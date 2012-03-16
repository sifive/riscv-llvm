// RUN: %dragonegg -S %s -o - | FileCheck %s
// Check that local variables are output in the order that they are declared in.

void foo(int *);

int bar(int x) {
// CHECK: @bar
  int a;
// CHECK: %a = alloca
  int b;
// CHECK: %b = alloca
  int c;
// CHECK: %c = alloca
  foo (&b);
  foo (&c);
  foo (&a);
  return a + b + c;
}
