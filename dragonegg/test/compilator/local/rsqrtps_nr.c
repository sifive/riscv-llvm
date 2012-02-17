#include <stdio.h>

typedef float v4sf __attribute__ ((vector_size (16)));
v4sf rfoo_nr(v4sf v) {
  return __builtin_ia32_rsqrtps_nr(v);
}
//float rfoof(float x) {
//  return __builtin_ia32_rsqrtf(x);
//}
int main(void) {
  v4sf x = { -1.0, 2.0, 9.0, 999.0 };
  x = rfoo_nr(x);
  printf("%g\n", ((float *)&x)[0]);
  printf("%g\n", ((float *)&x)[1]);
  printf("%g\n", ((float *)&x)[2]);
  printf("%g\n", ((float *)&x)[3]);
  return 0;
}
