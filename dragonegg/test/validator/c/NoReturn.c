// RUN: %dragonegg -S %s -o - | FileCheck %s

void foo() {
  exit(1);
// CHECK-NOT: ret void
}
