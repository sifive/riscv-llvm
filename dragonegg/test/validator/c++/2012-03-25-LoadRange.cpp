// RUN: %dragonegg -S -o - %s | FileCheck %s

bool foo(bool *p) {
  return *p;
// CHECK: load i8* {{.*}} !range !0
}
// CHECK: !0 = metadata !{i8 0, i8 2}                       
