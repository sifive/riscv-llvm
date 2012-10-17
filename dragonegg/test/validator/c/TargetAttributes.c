// RUN: %eggdragon -O1 -S %s -o - -msse | FileCheck %s --check-prefix=CHECK-SSE
// RUN: %eggdragon -O1 -S %s -o - -mno-sse | FileCheck %s --check-prefix=CHECK-NOSSE

void bar(char *);

void foo(void) {
  char buf[32] __attribute__ ((aligned (16)));
  memset(buf, 0, sizeof(buf));
// CHECK-SSE: movaps
// CHECK-NOSSE-NOT: xmm
  bar(buf);
}
