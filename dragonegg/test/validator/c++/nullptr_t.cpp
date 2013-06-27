// RUN: %dragonegg -std=c++0x -g -S -o - %s | FileCheck %s
// XFAIL: gcc-4.5

// CHECK: [ DW_TAG_unspecified_type ] [decltype(nullptr)]
decltype(nullptr) f() {
  return nullptr;
}
