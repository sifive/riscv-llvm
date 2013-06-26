// RUN: %dragonegg -S -o /dev/null %s

void *buf[20];

sub2 (void)
{
  __builtin_longjmp (buf, 1);
}

double
bar (int arg)
{
  foo (arg);
  __builtin_return (__builtin_apply ((void (*) ()) foo,
				     __builtin_apply_args (), 16));
}
