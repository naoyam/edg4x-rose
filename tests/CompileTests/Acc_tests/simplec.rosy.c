/* simplec.c */

void f0(int n,double *a,double *b)
{
#pragma acc kernels loop
{
    for (int i = 0; i < n - 1; i++) {
      b[i] = a[i] * 2.0;
    }
  }
}

void f1(int n,double *a,double *b,double *c)
{
  double t;
#pragma acc data copyin (a[0:n], b[0:n]), copy (c[0:n])
{
#pragma acc kernels
{
      for (int j = 0; j < n - 1; j++) {
        for (int i = 0; i < n - 1; i++) {
          t = 0.0;
          for (int k = 0; k < n - 1; k++) {
            t = t + a[i + n * k] * b[k + n * j];
          }
          c[i + n * j] = t;
        }
      }
    }
  }
}
