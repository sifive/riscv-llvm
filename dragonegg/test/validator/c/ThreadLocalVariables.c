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

extern __thread __attribute((tls_model("global-dynamic"))) int f;
extern __thread __attribute((tls_model("local-dynamic"))) int g;
extern __thread __attribute((tls_model("initial-exec"))) int h;
extern __thread __attribute((tls_model("local-exec"))) int i;
extern __thread int j;

// CHECK: @f = external thread_local global i32
// CHECK: @g = external thread_local(localdynamic) global i32
// CHECK: @h = external thread_local(initialexec) global i32
// CHECK: @i = external thread_local(localexec) global i32
// CHECK: @j = external thread_local global i32

int foo(void) {
  return f + g + h + i + j;
}
