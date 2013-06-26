// RUN: %dragonegg -xc++ -S -std=c++0x -o /dev/null %s
// XFAIL: gcc-4.5
// PR14777
void f(decltype(nullptr) &__restrict np) { }
