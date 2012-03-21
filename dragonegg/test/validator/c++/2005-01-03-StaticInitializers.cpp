// RUN: %dragonegg %s -S -o - | FileCheck %s

struct S {
  int  A[2];
};

int XX = (int)(long)&(((struct S*)0)->A[1]);

// CHECK-NOT: llvm.global_ctors
