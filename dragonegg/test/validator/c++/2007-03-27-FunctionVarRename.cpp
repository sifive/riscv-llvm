// RUN: %dragonegg %s -S -o - | not grep eprintf1
// RUN: %dragonegg %s -S -o - | grep eprintf

// Only one eprintf should exist in the output

extern "C" 
void __eprintf();

void foo() {

  __eprintf();
}

void *bar() {
  extern void *__eprintf;
  return &__eprintf;
}
