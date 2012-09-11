// RUN: %dragonegg -S %s -o - | FileCheck %s

struct T {
  T() {}
  ~T() {}
};

T foo() {
  T x;
  return x;
}
// CHECK-NOT: declare
