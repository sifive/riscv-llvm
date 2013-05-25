// RUN: %dragonegg %s -S -Wtrampolines 2>&1 | FileCheck %s
// CHECK: trampoline generated for nested function
// XFAIL: gcc-4.5
void use(int(*)(void));
void f(int i) {
  int k(void) { return i; }
  use(k);
}
