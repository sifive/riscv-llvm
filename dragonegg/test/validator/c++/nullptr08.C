// RUN: %dragonegg -xc++ -S -std=c++0x -o /dev/null %s
// XFAIL: gcc-4.5
// PR14777
#define assert_true(b) do { char c[2 * bool(b) - 1]; } while(0)

void fun()
{
  assert_true(nullptr ? false : true);
  decltype(nullptr) mynull = 0;
  assert_true(mynull ? false : true);
}
