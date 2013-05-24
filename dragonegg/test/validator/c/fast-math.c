// RUN: %dragonegg -S %s -o - | FileCheck -check-prefix=DEFAULT %s
// RUN: %dragonegg -S %s -o - -ffast-math | FileCheck -check-prefix=FASTMATH %s
// RUN: %dragonegg -S %s -o - -ffinite-math-only | FileCheck -check-prefix=FINITEMATHONLY %s
// RUN: %dragonegg -S %s -o - -fno-signed-zeros | FileCheck -check-prefix=NOSIGNEDZEROS %s

double fm(double x, double y) {
  return x+y;
// DEFAULT: fadd double
// FASTMATH: fadd fast
// FINITEMATHONLY: fadd nnan ninf
// NOSIGNEDZEROS: fadd nsz
}
