// RUN: %dragonegg -S %s -o - -std=c++11 | FileCheck %s
// PR11742
// This construct is not supported by gcc-4.6 and earlier.
// XFAIL: gcc-4.5, gcc-4.6

struct S { char c = 1; __attribute__((aligned(8))) char d = 2; } s = S();
char k = ((char*)&s)[1] + 1;

// CHECK-NOT: undef
