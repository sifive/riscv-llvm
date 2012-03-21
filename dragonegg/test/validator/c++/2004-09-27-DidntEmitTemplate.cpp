// RUN: %dragonegg -xc++ %s -S -o - | FileCheck %s

// This is a testcase for LLVM PR445, which was a problem where the 
// instantiation of callDefaultCtor was not being emitted correctly.

struct Pass {};

template<typename PassName>
Pass *callDefaultCtor() { return new Pass(); }
// CHECK: define linkonce_odr %struct.Pass* @_Z15callDefaultCtorI4PassEPS0_v

void foo(Pass *(*C)());

struct basic_string {
  bool empty() const { return true; }
};


bool foo2(basic_string &X) {
  return X.empty();
}
void baz() { foo(callDefaultCtor<Pass>); }
