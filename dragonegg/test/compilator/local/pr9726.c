void output_vtk (int nx1, int nx2, int big_end, double **input, float *output) {
  int i, j;
  for (j = 0; j < nx2; j++)
    for (i = 0; i < nx1; i++)
      output[i] = (float) input[j][i];
  use(output);
}
