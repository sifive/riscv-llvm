// RUN: %dragonegg -fno-builtin -O2 -S %s -o - | FileCheck %s

void foo(char *c, unsigned n) {
  // CHECK-NOT: memset
  while (n--)
    *(c++) = 0;
}
