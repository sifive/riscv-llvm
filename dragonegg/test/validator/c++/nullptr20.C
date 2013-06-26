// RUN: %dragonegg -xc++ -S -std=c++0x -o /dev/null %s
// XFAIL: gcc-4.5
// PR14777
#include <cstdio>
#include <cstring>

int main()
{
  char buf1[64];
  char buf2[64];
  char buf3[64];

  std::sprintf(buf1, "%p", (void*)0);
  std::sprintf(buf2, "%p", nullptr);
  decltype(nullptr) mynull = 0;
  std::sprintf(buf3, "%p", nullptr);
  return std::strcmp(buf1, buf2) != 0 || std::strcmp(buf1, buf3) != 0;
}
