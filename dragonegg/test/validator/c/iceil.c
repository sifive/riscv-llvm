// RUN: %dragonegg -S %s -o - -O2 -lm -ffast-math | FileCheck %s
// PR14270

// CHECK-NOT: builtin

#include <math.h>

int iceilf (float a) { return (int) ceil (a); }
int iceil (double a) { return (int) ceil (a); }
int iceill (long double a) { return (int) ceil (a); }

int ifloorf (float a) { return (int) floor (a); }
int ifloor (double a) { return (int) floor (a); }
int ifloorl (long double a) { return (int) floor (a); }
