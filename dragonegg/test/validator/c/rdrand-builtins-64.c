// RUN: %dragonegg -S %s -o - -mrdrnd | FileCheck %s
// XFAIL: gcc-4.5, i386, i486, i586, i686

#include <immintrin.h>

int rdrand64(unsigned long long *p) {
  return _rdrand64_step(p);
// CHECK: @rdrand64
// CHECK: call { i64, i32 } @llvm.x86.rdrand.64
// CHECK: store i64
}
