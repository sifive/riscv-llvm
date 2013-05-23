// RUN: %dragonegg -S %s -o - | FileCheck %s
// XFAIL: gcc-4.5, gcc-4.6
long int lr(double x) {
  return __builtin_lround(x);
}
long int lrf(float x) {
  return __builtin_lroundf(x);
}
long int lrl(long double x) {
  return __builtin_lroundl(x);
}
int ir(double x) {
  return __builtin_iround(x);
}
int irf(float x) {
  return __builtin_iroundf(x);
}
int irl(long double x) {
  return __builtin_iroundl(x);
}
// CHECK-NOT: builtin
