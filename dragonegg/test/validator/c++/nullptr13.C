// RUN: %dragonegg -xc++ -S -std=c++0x -o /dev/null %s
// XFAIL: gcc-4.5
// PR14777
#include <typeinfo>

#define assert_true(b) do { char c[2 * bool(b) - 1]; } while(0)

void fun()
{
  typeid(nullptr);
  const decltype(nullptr) mynull = 0;
  typeid(mynull);
  assert_true(typeid(nullptr) == typeid(mynull));
}
