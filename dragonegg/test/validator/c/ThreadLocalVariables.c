// RUN: %dragonegg -fpic -O -S %s -o - | FileCheck %s

__thread __attribute((tls_model("global-dynamic"))) int a;
__thread __attribute((tls_model("local-dynamic"))) int b;
__thread __attribute((tls_model("initial-exec"))) int c;
__thread __attribute((tls_model("local-exec"))) int d;
__thread int e;

// CHECK: @e = thread_local unnamed_addr global i32 0
// CHECK: @d = thread_local(localexec) unnamed_addr global i32 0
// CHECK: @c = thread_local(initialexec) unnamed_addr global i32 0
// CHECK: @b = thread_local(localdynamic) unnamed_addr global i32 0
// CHECK: @a = thread_local unnamed_addr global i32 0
