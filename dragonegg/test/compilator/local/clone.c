static __attribute__((noinline)) int g(int i, int j) {
   if (j != 0) return 0;
   return i;
}
int f(int i) { return g(i, 0); }
