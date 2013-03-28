// RUN: %dragonegg -S -g %s

struct X { };

struct AnyPtrMem {
  template<typename Class, typename T>
  operator T Class::*() const
  {
    T x = 0;
    return 0;
  }
};

void test_deduce_ptrmem_with_qual(AnyPtrMem apm) {
  const float X::* pm = apm;
}

struct A {
  typedef A* (A::*MemberPointer);
  A *m_ptr;
};

void foo() {
  A::MemberPointer member = &A::m_ptr;
}
