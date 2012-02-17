void use(void(*)(void));
int g(int);

int f(int x) {
  int y;

  void k(void) {
    y = g(y);
  }

  void l(void) {
    use(k);
    y = g(y);
  }

  y = g(x);
  use(k);
  use(l);
  return y;
}
