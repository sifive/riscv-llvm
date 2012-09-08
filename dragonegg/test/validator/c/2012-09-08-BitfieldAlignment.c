// RUN: %dragonegg -S %s -o - | FileCheck %s
// Check that bitfields are aligned properly.

struct __attribute__ ((__packed__)) __attribute__ ((aligned (4))) Foo {
    int aligned;
    unsigned char aligned4_a : 3;
    unsigned char aligned4_b : 3;
    unsigned char aligned4_c : 3;
    unsigned char aligned1_a : 3;
    unsigned char aligned1_b : 3;
    unsigned char aligned1_c : 3;
    unsigned char aligned2 : 3;
};

extern void baz(struct Foo *);

void bar() {
  struct Foo foo;
  foo.aligned4_a = 7;
// CHECK: load i8* {{.*}}, align 4
// CHECK: store i8 {{.*}}, align 4
  foo.aligned4_b = 7;
// CHECK: load i8* {{.*}}, align 4
// CHECK: store i8 {{.*}}, align 4
  foo.aligned4_c = 7;
// CHECK: load i16* {{.*}}, align 4
// CHECK: store i16 {{.*}}, align 4
  foo.aligned1_a = 7;
// CHECK: load i8* {{.*}}, align 1
// CHECK: store i8 {{.*}}, align 1
  foo.aligned1_b = 7;
// CHECK: load i8* {{.*}}, align 1
// CHECK: store i8 {{.*}}, align 1
  foo.aligned1_c = 7;
// CHECK: load i16* {{.*}}, align 1
// CHECK: store i16 {{.*}}, align 1
  foo.aligned2 = 7;
// CHECK: load i8* {{.*}}, align 2
// CHECK: store i8 {{.*}}, align 2
  baz(&foo);
}
