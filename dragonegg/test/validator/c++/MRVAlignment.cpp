// RUN: %dragonegg %s -S -o - | FileCheck %s
// XFAIL: i386, i486, i586, i686

struct P {
  char a;
  unsigned b;
  unsigned c;
  unsigned d;
};

void bar(const P&);
P qaz();
void foo() {
  bar(qaz());
// CHECK: store i64 %mrv_gr2, {{.*}}, align 1
}
