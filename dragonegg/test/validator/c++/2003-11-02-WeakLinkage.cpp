// RUN: %dragonegg -xc++ -S -o - %s | FileCheck %s
// The template should compile to linkonce linkage, not weak linkage.

template<class T>
void thefunc();

template<class T>
inline void thefunc() {}
// CHECK: linkonce_odr void @_Z7thefuncIiEvv

void test() {
  thefunc<int>();
}

