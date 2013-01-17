#include <stdio.h>

long double rfoof(double x) {
  return __builtin_ia32_rsqrtf(x);
}
int main(void) {
  double x = 2.0;
  x = rfoof(x);
  printf("%g\n", x);
  return 0;
}
// float -> rsqrtss
// double -> rsqrtss
