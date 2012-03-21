// RUN: %dragonegg -S %s -o - | FileCheck %s

void foo(int * __restrict myptr1, int * myptr2) {
// CHECK: @_Z3fooPiS_(i32* noalias %myptr1, i32* %myptr2)
  myptr1[0] = 0;
  myptr2[0] = 0;
}
