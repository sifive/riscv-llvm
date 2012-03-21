// RUN: %dragonegg -S %s -o - | FileCheck %s

class foo {
  int member[4];
  
  void bar(int * a);
  
};

void foo::bar(int * a) __restrict {
// CHECK: @_ZN3foo3barEPi(%struct.foo* noalias %this, i32* %a)
  member[3] = *a;
}
