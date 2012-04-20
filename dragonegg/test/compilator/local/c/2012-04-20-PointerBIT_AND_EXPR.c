testvacld (int n, ...)
{
  __builtin_va_list ap;
  __builtin_va_start (ap, n);
  _Complex long double t =
    __builtin_va_arg (ap, _Complex long double);
  __builtin_va_end (ap);
}
