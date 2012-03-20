// RUN: %dragonegg -S %s -o - | FileCheck %s
// PR3320

void test(volatile int *a) {
    // should be a volatile load.
    a[0];
// CHECK: load volatile
}
