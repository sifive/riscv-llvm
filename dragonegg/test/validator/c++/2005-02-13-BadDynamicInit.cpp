// RUN: %dragonegg %s -S -o - | FileCheck %s
// This testcase corresponds to PR509
struct Data {
  unsigned *data;
  unsigned array[1];
};

Data shared_null = { shared_null.array };

// CHECK-NOT: llvm.global_ctors
