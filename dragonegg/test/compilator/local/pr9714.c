void lr_states (int n, double W[], double *pW[]) {
  int i;
  for (i = 0; i < n; ++i)
    pW[i] = &W[i];
}
