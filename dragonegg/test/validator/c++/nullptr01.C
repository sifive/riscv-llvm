// RUN: %dragonegg -xc++ -S -std=c++0x -o /dev/null %s
// XFAIL: gcc-4.5
// PR14777
char* const cp1 = nullptr;
char* const cp2 = __null;
char* const cp3 = 0;
decltype(nullptr) mynull = 0;
char* const cp4 = mynull;
