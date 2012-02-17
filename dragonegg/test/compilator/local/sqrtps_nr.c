typedef float v4sf __attribute__ ((vector_size (16)));
v4sf foo_nr(v4sf v) {
  return __builtin_ia32_sqrtps_nr(v);
}
