// RUN: %dragonegg -S -o /dev/null %s -msse
// PR12664

typedef float v4sf __attribute__ ((vector_size (16)));
v4sf foo(v4sf l, v4sf r) {
  return __builtin_ia32_copysignps (l, r);
}
