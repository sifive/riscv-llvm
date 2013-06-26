// RUN: %dragonegg -xc++ -S -std=c++0x -o /dev/null %s
// XFAIL: gcc-4.5
// PR14777
class F { };

typedef void (F::*pmf)();

const pmf pmf1 = nullptr;
const pmf pmf2 = __null;
const pmf pmf3 = 0;
decltype(nullptr) mynull = 0;
const pmf pmf4 = mynull;
