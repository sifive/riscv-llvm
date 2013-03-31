// RUN: %dragonegg -S %s

typedef void (*ft)(void);

void foo(ft f) {
  ((void(*)(int))f)(0);
}
