// RUN: %dragonegg -xc++ -S -std=c++0x -o /dev/null %s
// XFAIL: gcc-4.5
// PR14777
void f1(const char*, ...) __attribute__((format(printf, 1, 2)));
void f2(const char*) __attribute__((nonnull));
void f3(const char*, ...) __attribute__((sentinel));

void f()
{
  f1("%p", nullptr);
  f2(nullptr);
  f3("x", "y", __null);
  f3("x", "y", nullptr);
  decltype(nullptr) mynull = 0;
  f1("%p", mynull);
  f2(mynull);
  f3("x", "y", mynull);
}
