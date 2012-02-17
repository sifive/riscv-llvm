struct R {
  long a;
  long b;
};

struct R f(long a, long b) {
  struct R A;
  A.a = a;
  A.b = b;
  return A;
}
