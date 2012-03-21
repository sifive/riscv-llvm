#include <new>
typedef double Ty[4];

void foo(Ty *XX) {
  new(XX) Ty();
}
