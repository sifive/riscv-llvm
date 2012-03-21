// RUN: %dragonegg -xc++ -S -o - %s | FileCheck %s

struct AccessFlags {
  void strlen();
};

void AccessFlags::strlen() { }
// CHECK: define void @_ZN11AccessFlags6strlenEv
