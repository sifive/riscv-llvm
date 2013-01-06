// RUN: %dragonegg -S %s -o - | FileCheck %s
// Check that the MRV code doesn't load from beyond the end of an alloca.
// XFAIL: i386, i486, i586, i686

struct Three { int a; int b; int c; };

struct Three foo(struct Three *p) {
  return *p;
// CHECK: call void @llvm.memcpy
// CHECK-NOT: bitcast
}
