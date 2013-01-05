// RUN: %dragonegg -xc++ -S -std=c++0x %s
// XFAIL: gcc-4.5
// PR14777
#include <initializer_list>

struct Foo
{
  Foo(std::initializer_list<Foo>) { };

  template<class T> Foo(T t) { T u(t); };

private:
  union Data
  {
    Data() : null(nullptr) {}

    std::nullptr_t null;
  } u_;
};

int main()
{
  Foo f = { {} };
}
