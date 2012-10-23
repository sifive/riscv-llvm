// RUN: %dragonegg -S %s -o - -O1 | FileCheck %s

int foo1(int *A, int N) {
// CHECK: @foo1
  return A[N];
// CHECK: getelementptr i32* %A
}

int foo2(int *A, int N) {
// CHECK: @foo2
  return A[2*N];
// CHECK: getelementptr i32* %A
}

int foo3(int *A, int N) {
// CHECK: @foo3
  return A[3*N];
// CHECK: getelementptr i32* %A
}

int fooM(int *A, int M, int N) {
// CHECK: @fooM
  return A[M*N];
// CHECK: getelementptr i32* %A
}
