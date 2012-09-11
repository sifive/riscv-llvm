// RUN: %dragonegg -xc++ %s -S -o - | FileCheck %s

void foo(int*);

struct FOO {
  int X;
};

struct BAR : virtual FOO { BAR(); };

BAR testfn() {
  // CHECK: "alloca point" = bitcast i32 0 to i32
  // CHECK: "ssa point" = bitcast i32 0 to i32
  // CHECK-NOT: cast
  BAR B;
  foo(&B.X);
  return B;
}
