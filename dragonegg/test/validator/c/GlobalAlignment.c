// RUN: %dragonegg -S -o - %s | FileCheck %s

struct s {
  char *a, *b, *c;
};

// CHECK: @s1 = {{.*}}, align 8
struct s s1 __attribute__((aligned(8))) = { 0, 0, 0 };
