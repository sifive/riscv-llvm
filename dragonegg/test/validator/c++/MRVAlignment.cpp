// RUN: %dragonegg %s -S -o - | FileCheck %s

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
