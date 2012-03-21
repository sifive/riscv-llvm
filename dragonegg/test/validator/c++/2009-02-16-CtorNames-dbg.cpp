// RUN: %dragonegg -S -g %s -o - | FileCheck %s
class A {
  int i;
public:
  A() { i = 0; }
 ~A() { i = 42; }
// CHECK: A::~A
};

A a;

