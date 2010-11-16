// RUN: %clang_cc1 -fsyntax-only -verify %s
struct S {
  static void f(const char*, ...) __attribute__((format(printf, 1, 2)));
  static const char* f2(const char*) __attribute__((format_arg(1)));

  // GCC has a hidden 'this' argument in member functions which is why
  // the format argument is argument 2 here.
  void g(const char*, ...) __attribute__((format(printf, 2, 3)));
  const char* g2(const char*) __attribute__((format_arg(2)));

  void h(const char*, ...) __attribute__((format(printf, 1, 4))); // \
      expected-error{{implicit this argument as the format string}}
  void h2(const char*, ...) __attribute__((format(printf, 2, 1))); // \
      expected-error{{out of bounds}}
  const char* h3(const char*) __attribute__((format_arg(1))); // \
      expected-error{{invalid for the implicit this argument}}
};

// PR5521
struct A { void a(const char*,...) __attribute((format(printf,2,3))); };
void b(A x) {
  x.a("%d", 3);
}
