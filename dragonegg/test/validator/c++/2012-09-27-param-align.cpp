// RUN: %dragonegg -S %s -o - | FileCheck %s
// ABI code was producing naturally aligned i64 loads and stores to memory that
// was not as aligned as an i64.

struct InReg {
  unsigned a;
  unsigned b;
};

void bar(InReg);

void foo(InReg x) {
  bar(x);
// CHECK: store {{.*}}, align
// CHECK: load {{.*}}, align
}
