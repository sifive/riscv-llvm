// RUN: %eggdragon -O0 -g -S %s -fplugin-arg-dragonegg-emit-ir -o - | FileCheck %s
/// PR1013
// Check to make sure debug symbols use the correct name for globals and
// functions.

// CHECK: @"\01f\01oo" = unnamed_addr global i32 0
// CHECK: metadata !{metadata !"0x34\00foo\00foo\00\01f\01oo\009\000\001", metadata !6, metadata !6, metadata !9, i32* @"\01f\01oo", null}

int foo __asm__("f\001oo");

int bar() {
  return foo;
}
