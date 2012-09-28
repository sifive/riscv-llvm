// RUN: %dragonegg -S %s -o - | FileCheck %s
// Overaligned load of field bf.

struct S {
  char s;
  char bf : 3;                                     
};

void bar(S &);

void foo(S &rhs) {
  if (rhs.bf == 0)
    bar(rhs);
// CHECK: load i8* {{.*}}, align 1
}
