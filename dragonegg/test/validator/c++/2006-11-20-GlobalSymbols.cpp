// RUN: %eggdragon -O0 -g -S %s -fplugin-arg-dragonegg-emit-ir -o - | FileCheck %s
/// PR1013
// Check to make sure debug symbols use the correct name for globals and
// functions.

// CHECK: @"\01f\01oo" = unnamed_addr global i32 0
// CHECK: metadata !"foo", metadata !"\01f\01oo",

int foo __asm__("f\001oo");

int bar() {
  return foo;
}
