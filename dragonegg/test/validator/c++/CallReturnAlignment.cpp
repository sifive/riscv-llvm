// RUN: %dragonegg %s -O2 -march=native -fplugin-arg-dragonegg-llvm-ir-optimize=0 -S -o - | FileCheck %s

struct base {};

struct child1 : base {
  void* P;
};

class child2 : base {
  child1 C;
};

child2 bar();
bool foo() {
// CHECK: foo
  child2 D = bar();
// CHECK-NOT: store {{.*}}, align 16
}
