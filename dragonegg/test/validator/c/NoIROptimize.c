// RUN: %dragonegg -S -O3 -fplugin-arg-dragonegg-llvm-ir-optimize=0 -fplugin-arg-dragonegg-debug-pass-arguments %s 2>&1 | FileCheck %s

// CHECK-NOT: inline

inline __attribute__ ((__always_inline__)) void foo(void) {}
void bar(void) { foo(); }
void qaz(void) { bar(); }
