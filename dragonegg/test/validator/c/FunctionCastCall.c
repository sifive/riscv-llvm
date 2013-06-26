// RUN: %dragonegg -S -o /dev/null %s

typedef void (*ft)(void);

void foo(ft f) {
  ((void(*)(int))f)(0);
}
