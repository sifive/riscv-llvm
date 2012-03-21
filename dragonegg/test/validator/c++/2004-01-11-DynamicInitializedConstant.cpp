// RUN: %dragonegg -xc++ -S -o - %s | FileCheck %s

extern int X;
const int Y = X;
const int* foo() { return &Y; }
// CHECK-NOT: constant
