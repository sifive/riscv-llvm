// RUN: %dragonegg %s -S -o - | FileCheck %s
// Only one eprintf should exist in the output
// CHECK-NOT: eprintf1

extern "C" 
void __eprintf();

void foo() {

  __eprintf();
}

void *bar() {
  extern void *__eprintf;
  return &__eprintf;
}
