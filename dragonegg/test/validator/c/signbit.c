// RUN: %dragonegg -S -o - %s | FileCheck %s

int sbf(float x) {
  // CHECK: @sbf
  // CHECK: bitcast float {{.*}} to i32
  // CHECK: icmp slt i32 {{.*}}, 0
  // CHECK: zext i1 {{.*}} to i32
  return __builtin_signbitf(x);
}

int sbd(double x) {
  // CHECK: @sbd
  // CHECK: bitcast double {{.*}} to i64
  // CHECK: icmp slt i64 {{.*}}, 0
  // CHECK: zext i1 {{.*}} to i32
  return __builtin_signbit(x);
}
