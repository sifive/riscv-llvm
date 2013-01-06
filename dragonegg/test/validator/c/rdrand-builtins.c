// RUN: %dragonegg -S %s -o - -mrdrnd | FileCheck %s
// XFAIL: gcc-4.5

#include <immintrin.h>

int rdrand16(unsigned short *p) {
  return _rdrand16_step(p);
// CHECK: @rdrand16
// CHECK: call { i16, i32 } @llvm.x86.rdrand.16
// CHECK: store i16
}

int rdrand32(unsigned *p) {
  return _rdrand32_step(p);
// CHECK: @rdrand32
// CHECK: call { i32, i32 } @llvm.x86.rdrand.32
// CHECK: store i32
}
