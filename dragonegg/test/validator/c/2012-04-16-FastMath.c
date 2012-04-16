// RUN: %dragonegg -ffast-math -S %s -o - | FileCheck %s

double add(double x, double y) {
// CHECK: @add
  return x + y;
// CHECK: fadd double %{{.}}, %{{.}}, !fpmath !0
}
// CHECK: !0 = metadata !{metadata !"fast"}
