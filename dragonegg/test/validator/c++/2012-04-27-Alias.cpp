// RUN: %dragonegg -S %s -o - | FileCheck %s

struct T {
  T() {}
  ~T() {}
};

int main() {
  T x;
  return 0;
}
// CHECK-NOT: declare
