// RUN: %dragonegg -S %s

void *buf[20];

sub2 (void)
{
  __builtin_longjmp (buf, 1);
}
