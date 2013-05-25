// RUN: %dragonegg -S %s -o - -O3 -msse | FileCheck -check-prefix=VON %s
// RUN: %dragonegg -S %s -o - -O3 -msse -fno-tree-vectorize | FileCheck -check-prefix=VOFF %s
// VON: fadd <
// VOFF-NOT: fadd <

void bar(float *A, float* B, float K) {
  int i;
  for (i = 0; i < 64; ++i)
    A[i] *= B[i] + K;
}
