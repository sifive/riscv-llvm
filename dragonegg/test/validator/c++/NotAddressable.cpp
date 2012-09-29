// RUN: %dragonegg %s -S -o - | FileCheck %s

void a(int);

struct Foo {
  Foo();
  ~Foo();
  virtual void foo();
};

Foo::Foo() { a(1); }
// CHECK: define void @_ZN3FooC2Ev{{.*}}unnamed_addr
Foo::~Foo() { a(1); }
// CHECK: define void @_ZN3FooD2Ev{{.*}}unnamed_addr
void Foo::foo() { a(1); }

Foo f;
