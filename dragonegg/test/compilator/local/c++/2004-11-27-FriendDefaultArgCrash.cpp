// PR447

namespace nm {
  struct str {
    friend int foo(int arg = 0);
  };
}
