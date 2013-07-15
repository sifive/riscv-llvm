// RUN: %dragonegg -mfma4 -S %s -o - | FileCheck %s
// XFAIL: gcc-4.5, i386, i486, i586, i686

// CHECK: @fmaftest
// CHECK: call float @llvm.fma.f32
float fmaftest(float a, float b, float c) { return __builtin_fmaf(a, b, c); }

// CHECK: @fmatest
// CHECK: call double @llvm.fma.f64
double fmatest(double a, double b, double c) { return __builtin_fma(a, b, c); }
