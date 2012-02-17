typedef long double float_t;
typedef long double double_t;
extern double acos (double __x) __attribute__ ((__nothrow__)); extern double __acos (double __x) __attribute__ ((__nothrow__));
extern double asin (double __x) __attribute__ ((__nothrow__)); extern double __asin (double __x) __attribute__ ((__nothrow__));
extern double atan (double __x) __attribute__ ((__nothrow__)); extern double __atan (double __x) __attribute__ ((__nothrow__));
extern double atan2 (double __y, double __x) __attribute__ ((__nothrow__)); extern double __atan2 (double __y, double __x) __attribute__ ((__nothrow__));
extern double cos (double __x) __attribute__ ((__nothrow__)); extern double __cos (double __x) __attribute__ ((__nothrow__));
extern double sin (double __x) __attribute__ ((__nothrow__)); extern double __sin (double __x) __attribute__ ((__nothrow__));
extern double tan (double __x) __attribute__ ((__nothrow__)); extern double __tan (double __x) __attribute__ ((__nothrow__));
extern double cosh (double __x) __attribute__ ((__nothrow__)); extern double __cosh (double __x) __attribute__ ((__nothrow__));
extern double sinh (double __x) __attribute__ ((__nothrow__)); extern double __sinh (double __x) __attribute__ ((__nothrow__));
extern double tanh (double __x) __attribute__ ((__nothrow__)); extern double __tanh (double __x) __attribute__ ((__nothrow__));
extern double acosh (double __x) __attribute__ ((__nothrow__)); extern double __acosh (double __x) __attribute__ ((__nothrow__));
extern double asinh (double __x) __attribute__ ((__nothrow__)); extern double __asinh (double __x) __attribute__ ((__nothrow__));
extern double atanh (double __x) __attribute__ ((__nothrow__)); extern double __atanh (double __x) __attribute__ ((__nothrow__));
extern double exp (double __x) __attribute__ ((__nothrow__)); extern double __exp (double __x) __attribute__ ((__nothrow__));
extern double frexp (double __x, int *__exponent) __attribute__ ((__nothrow__)); extern double __frexp (double __x, int *__exponent) __attribute__ ((__nothrow__));
extern double ldexp (double __x, int __exponent) __attribute__ ((__nothrow__)); extern double __ldexp (double __x, int __exponent) __attribute__ ((__nothrow__));
extern double log (double __x) __attribute__ ((__nothrow__)); extern double __log (double __x) __attribute__ ((__nothrow__));
extern double log10 (double __x) __attribute__ ((__nothrow__)); extern double __log10 (double __x) __attribute__ ((__nothrow__));
extern double modf (double __x, double *__iptr) __attribute__ ((__nothrow__)); extern double __modf (double __x, double *__iptr) __attribute__ ((__nothrow__))
     __attribute__ ((__nonnull__ (2)));
extern double expm1 (double __x) __attribute__ ((__nothrow__)); extern double __expm1 (double __x) __attribute__ ((__nothrow__));
extern double log1p (double __x) __attribute__ ((__nothrow__)); extern double __log1p (double __x) __attribute__ ((__nothrow__));
extern double logb (double __x) __attribute__ ((__nothrow__)); extern double __logb (double __x) __attribute__ ((__nothrow__));
extern double exp2 (double __x) __attribute__ ((__nothrow__)); extern double __exp2 (double __x) __attribute__ ((__nothrow__));
extern double log2 (double __x) __attribute__ ((__nothrow__)); extern double __log2 (double __x) __attribute__ ((__nothrow__));
extern double pow (double __x, double __y) __attribute__ ((__nothrow__)); extern double __pow (double __x, double __y) __attribute__ ((__nothrow__));
extern double sqrt (double __x) __attribute__ ((__nothrow__)); extern double __sqrt (double __x) __attribute__ ((__nothrow__));
extern double hypot (double __x, double __y) __attribute__ ((__nothrow__)); extern double __hypot (double __x, double __y) __attribute__ ((__nothrow__));
extern double cbrt (double __x) __attribute__ ((__nothrow__)); extern double __cbrt (double __x) __attribute__ ((__nothrow__));
extern double ceil (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __ceil (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern double fabs (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __fabs (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern double floor (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __floor (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern double fmod (double __x, double __y) __attribute__ ((__nothrow__)); extern double __fmod (double __x, double __y) __attribute__ ((__nothrow__));
extern int __isinf (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int __finite (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int isinf (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int finite (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern double drem (double __x, double __y) __attribute__ ((__nothrow__)); extern double __drem (double __x, double __y) __attribute__ ((__nothrow__));
extern double significand (double __x) __attribute__ ((__nothrow__)); extern double __significand (double __x) __attribute__ ((__nothrow__));
extern double copysign (double __x, double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __copysign (double __x, double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern double nan (__const char *__tagb) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __nan (__const char *__tagb) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int __isnan (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int isnan (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern double j0 (double) __attribute__ ((__nothrow__)); extern double __j0 (double) __attribute__ ((__nothrow__));
extern double j1 (double) __attribute__ ((__nothrow__)); extern double __j1 (double) __attribute__ ((__nothrow__));
extern double jn (int, double) __attribute__ ((__nothrow__)); extern double __jn (int, double) __attribute__ ((__nothrow__));
extern double y0 (double) __attribute__ ((__nothrow__)); extern double __y0 (double) __attribute__ ((__nothrow__));
extern double y1 (double) __attribute__ ((__nothrow__)); extern double __y1 (double) __attribute__ ((__nothrow__));
extern double yn (int, double) __attribute__ ((__nothrow__)); extern double __yn (int, double) __attribute__ ((__nothrow__));
extern double erf (double) __attribute__ ((__nothrow__)); extern double __erf (double) __attribute__ ((__nothrow__));
extern double erfc (double) __attribute__ ((__nothrow__)); extern double __erfc (double) __attribute__ ((__nothrow__));
extern double lgamma (double) __attribute__ ((__nothrow__)); extern double __lgamma (double) __attribute__ ((__nothrow__));
extern double tgamma (double) __attribute__ ((__nothrow__)); extern double __tgamma (double) __attribute__ ((__nothrow__));
extern double gamma (double) __attribute__ ((__nothrow__)); extern double __gamma (double) __attribute__ ((__nothrow__));
extern double lgamma_r (double, int *__signgamp) __attribute__ ((__nothrow__)); extern double __lgamma_r (double, int *__signgamp) __attribute__ ((__nothrow__));
extern double rint (double __x) __attribute__ ((__nothrow__)); extern double __rint (double __x) __attribute__ ((__nothrow__));
extern double nextafter (double __x, double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __nextafter (double __x, double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern double nexttoward (double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __nexttoward (double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern double remainder (double __x, double __y) __attribute__ ((__nothrow__)); extern double __remainder (double __x, double __y) __attribute__ ((__nothrow__));
extern double scalbn (double __x, int __n) __attribute__ ((__nothrow__)); extern double __scalbn (double __x, int __n) __attribute__ ((__nothrow__));
extern int ilogb (double __x) __attribute__ ((__nothrow__)); extern int __ilogb (double __x) __attribute__ ((__nothrow__));
extern double scalbln (double __x, long int __n) __attribute__ ((__nothrow__)); extern double __scalbln (double __x, long int __n) __attribute__ ((__nothrow__));
extern double nearbyint (double __x) __attribute__ ((__nothrow__)); extern double __nearbyint (double __x) __attribute__ ((__nothrow__));
extern double round (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __round (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern double trunc (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __trunc (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern double remquo (double __x, double __y, int *__quo) __attribute__ ((__nothrow__)); extern double __remquo (double __x, double __y, int *__quo) __attribute__ ((__nothrow__));
extern long int lrint (double __x) __attribute__ ((__nothrow__)); extern long int __lrint (double __x) __attribute__ ((__nothrow__));
extern long long int llrint (double __x) __attribute__ ((__nothrow__)); extern long long int __llrint (double __x) __attribute__ ((__nothrow__));
extern long int lround (double __x) __attribute__ ((__nothrow__)); extern long int __lround (double __x) __attribute__ ((__nothrow__));
extern long long int llround (double __x) __attribute__ ((__nothrow__)); extern long long int __llround (double __x) __attribute__ ((__nothrow__));
extern double fdim (double __x, double __y) __attribute__ ((__nothrow__)); extern double __fdim (double __x, double __y) __attribute__ ((__nothrow__));
extern double fmax (double __x, double __y) __attribute__ ((__nothrow__)); extern double __fmax (double __x, double __y) __attribute__ ((__nothrow__));
extern double fmin (double __x, double __y) __attribute__ ((__nothrow__)); extern double __fmin (double __x, double __y) __attribute__ ((__nothrow__));
extern int __fpclassify (double __value) __attribute__ ((__nothrow__))
     __attribute__ ((__const__));
extern int __signbit (double __value) __attribute__ ((__nothrow__))
     __attribute__ ((__const__));
extern double fma (double __x, double __y, double __z) __attribute__ ((__nothrow__)); extern double __fma (double __x, double __y, double __z) __attribute__ ((__nothrow__));
extern double scalb (double __x, double __n) __attribute__ ((__nothrow__)); extern double __scalb (double __x, double __n) __attribute__ ((__nothrow__));
extern float acosf (float __x) __attribute__ ((__nothrow__)); extern float __acosf (float __x) __attribute__ ((__nothrow__));
extern float asinf (float __x) __attribute__ ((__nothrow__)); extern float __asinf (float __x) __attribute__ ((__nothrow__));
extern float atanf (float __x) __attribute__ ((__nothrow__)); extern float __atanf (float __x) __attribute__ ((__nothrow__));
extern float atan2f (float __y, float __x) __attribute__ ((__nothrow__)); extern float __atan2f (float __y, float __x) __attribute__ ((__nothrow__));
extern float cosf (float __x) __attribute__ ((__nothrow__)); extern float __cosf (float __x) __attribute__ ((__nothrow__));
extern float sinf (float __x) __attribute__ ((__nothrow__)); extern float __sinf (float __x) __attribute__ ((__nothrow__));
extern float tanf (float __x) __attribute__ ((__nothrow__)); extern float __tanf (float __x) __attribute__ ((__nothrow__));
extern float coshf (float __x) __attribute__ ((__nothrow__)); extern float __coshf (float __x) __attribute__ ((__nothrow__));
extern float sinhf (float __x) __attribute__ ((__nothrow__)); extern float __sinhf (float __x) __attribute__ ((__nothrow__));
extern float tanhf (float __x) __attribute__ ((__nothrow__)); extern float __tanhf (float __x) __attribute__ ((__nothrow__));
extern float acoshf (float __x) __attribute__ ((__nothrow__)); extern float __acoshf (float __x) __attribute__ ((__nothrow__));
extern float asinhf (float __x) __attribute__ ((__nothrow__)); extern float __asinhf (float __x) __attribute__ ((__nothrow__));
extern float atanhf (float __x) __attribute__ ((__nothrow__)); extern float __atanhf (float __x) __attribute__ ((__nothrow__));
extern float expf (float __x) __attribute__ ((__nothrow__)); extern float __expf (float __x) __attribute__ ((__nothrow__));
extern float frexpf (float __x, int *__exponent) __attribute__ ((__nothrow__)); extern float __frexpf (float __x, int *__exponent) __attribute__ ((__nothrow__));
extern float ldexpf (float __x, int __exponent) __attribute__ ((__nothrow__)); extern float __ldexpf (float __x, int __exponent) __attribute__ ((__nothrow__));
extern float logf (float __x) __attribute__ ((__nothrow__)); extern float __logf (float __x) __attribute__ ((__nothrow__));
extern float log10f (float __x) __attribute__ ((__nothrow__)); extern float __log10f (float __x) __attribute__ ((__nothrow__));
extern float modff (float __x, float *__iptr) __attribute__ ((__nothrow__)); extern float __modff (float __x, float *__iptr) __attribute__ ((__nothrow__))
     __attribute__ ((__nonnull__ (2)));
extern float expm1f (float __x) __attribute__ ((__nothrow__)); extern float __expm1f (float __x) __attribute__ ((__nothrow__));
extern float log1pf (float __x) __attribute__ ((__nothrow__)); extern float __log1pf (float __x) __attribute__ ((__nothrow__));
extern float logbf (float __x) __attribute__ ((__nothrow__)); extern float __logbf (float __x) __attribute__ ((__nothrow__));
extern float exp2f (float __x) __attribute__ ((__nothrow__)); extern float __exp2f (float __x) __attribute__ ((__nothrow__));
extern float log2f (float __x) __attribute__ ((__nothrow__)); extern float __log2f (float __x) __attribute__ ((__nothrow__));
extern float powf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __powf (float __x, float __y) __attribute__ ((__nothrow__));
extern float sqrtf (float __x) __attribute__ ((__nothrow__)); extern float __sqrtf (float __x) __attribute__ ((__nothrow__));
extern float hypotf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __hypotf (float __x, float __y) __attribute__ ((__nothrow__));
extern float cbrtf (float __x) __attribute__ ((__nothrow__)); extern float __cbrtf (float __x) __attribute__ ((__nothrow__));
extern float ceilf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __ceilf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern float fabsf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __fabsf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern float floorf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __floorf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern float fmodf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __fmodf (float __x, float __y) __attribute__ ((__nothrow__));
extern int __isinff (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int __finitef (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int isinff (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int finitef (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern float dremf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __dremf (float __x, float __y) __attribute__ ((__nothrow__));
extern float significandf (float __x) __attribute__ ((__nothrow__)); extern float __significandf (float __x) __attribute__ ((__nothrow__));
extern float copysignf (float __x, float __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __copysignf (float __x, float __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern float nanf (__const char *__tagb) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __nanf (__const char *__tagb) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int __isnanf (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int isnanf (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern float j0f (float) __attribute__ ((__nothrow__)); extern float __j0f (float) __attribute__ ((__nothrow__));
extern float j1f (float) __attribute__ ((__nothrow__)); extern float __j1f (float) __attribute__ ((__nothrow__));
extern float jnf (int, float) __attribute__ ((__nothrow__)); extern float __jnf (int, float) __attribute__ ((__nothrow__));
extern float y0f (float) __attribute__ ((__nothrow__)); extern float __y0f (float) __attribute__ ((__nothrow__));
extern float y1f (float) __attribute__ ((__nothrow__)); extern float __y1f (float) __attribute__ ((__nothrow__));
extern float ynf (int, float) __attribute__ ((__nothrow__)); extern float __ynf (int, float) __attribute__ ((__nothrow__));
extern float erff (float) __attribute__ ((__nothrow__)); extern float __erff (float) __attribute__ ((__nothrow__));
extern float erfcf (float) __attribute__ ((__nothrow__)); extern float __erfcf (float) __attribute__ ((__nothrow__));
extern float lgammaf (float) __attribute__ ((__nothrow__)); extern float __lgammaf (float) __attribute__ ((__nothrow__));
extern float tgammaf (float) __attribute__ ((__nothrow__)); extern float __tgammaf (float) __attribute__ ((__nothrow__));
extern float gammaf (float) __attribute__ ((__nothrow__)); extern float __gammaf (float) __attribute__ ((__nothrow__));
extern float lgammaf_r (float, int *__signgamp) __attribute__ ((__nothrow__)); extern float __lgammaf_r (float, int *__signgamp) __attribute__ ((__nothrow__));
extern float rintf (float __x) __attribute__ ((__nothrow__)); extern float __rintf (float __x) __attribute__ ((__nothrow__));
extern float nextafterf (float __x, float __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __nextafterf (float __x, float __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern float nexttowardf (float __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __nexttowardf (float __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern float remainderf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __remainderf (float __x, float __y) __attribute__ ((__nothrow__));
extern float scalbnf (float __x, int __n) __attribute__ ((__nothrow__)); extern float __scalbnf (float __x, int __n) __attribute__ ((__nothrow__));
extern int ilogbf (float __x) __attribute__ ((__nothrow__)); extern int __ilogbf (float __x) __attribute__ ((__nothrow__));
extern float scalblnf (float __x, long int __n) __attribute__ ((__nothrow__)); extern float __scalblnf (float __x, long int __n) __attribute__ ((__nothrow__));
extern float nearbyintf (float __x) __attribute__ ((__nothrow__)); extern float __nearbyintf (float __x) __attribute__ ((__nothrow__));
extern float roundf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __roundf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern float truncf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __truncf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern float remquof (float __x, float __y, int *__quo) __attribute__ ((__nothrow__)); extern float __remquof (float __x, float __y, int *__quo) __attribute__ ((__nothrow__));
extern long int lrintf (float __x) __attribute__ ((__nothrow__)); extern long int __lrintf (float __x) __attribute__ ((__nothrow__));
extern long long int llrintf (float __x) __attribute__ ((__nothrow__)); extern long long int __llrintf (float __x) __attribute__ ((__nothrow__));
extern long int lroundf (float __x) __attribute__ ((__nothrow__)); extern long int __lroundf (float __x) __attribute__ ((__nothrow__));
extern long long int llroundf (float __x) __attribute__ ((__nothrow__)); extern long long int __llroundf (float __x) __attribute__ ((__nothrow__));
extern float fdimf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __fdimf (float __x, float __y) __attribute__ ((__nothrow__));
extern float fmaxf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __fmaxf (float __x, float __y) __attribute__ ((__nothrow__));
extern float fminf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __fminf (float __x, float __y) __attribute__ ((__nothrow__));
extern int __fpclassifyf (float __value) __attribute__ ((__nothrow__))
     __attribute__ ((__const__));
extern int __signbitf (float __value) __attribute__ ((__nothrow__))
     __attribute__ ((__const__));
extern float fmaf (float __x, float __y, float __z) __attribute__ ((__nothrow__)); extern float __fmaf (float __x, float __y, float __z) __attribute__ ((__nothrow__));
extern float scalbf (float __x, float __n) __attribute__ ((__nothrow__)); extern float __scalbf (float __x, float __n) __attribute__ ((__nothrow__));
extern long double acosl (long double __x) __attribute__ ((__nothrow__)); extern long double __acosl (long double __x) __attribute__ ((__nothrow__));
extern long double asinl (long double __x) __attribute__ ((__nothrow__)); extern long double __asinl (long double __x) __attribute__ ((__nothrow__));
extern long double atanl (long double __x) __attribute__ ((__nothrow__)); extern long double __atanl (long double __x) __attribute__ ((__nothrow__));
extern long double atan2l (long double __y, long double __x) __attribute__ ((__nothrow__)); extern long double __atan2l (long double __y, long double __x) __attribute__ ((__nothrow__));
extern long double cosl (long double __x) __attribute__ ((__nothrow__)); extern long double __cosl (long double __x) __attribute__ ((__nothrow__));
extern long double sinl (long double __x) __attribute__ ((__nothrow__)); extern long double __sinl (long double __x) __attribute__ ((__nothrow__));
extern long double tanl (long double __x) __attribute__ ((__nothrow__)); extern long double __tanl (long double __x) __attribute__ ((__nothrow__));
extern long double coshl (long double __x) __attribute__ ((__nothrow__)); extern long double __coshl (long double __x) __attribute__ ((__nothrow__));
extern long double sinhl (long double __x) __attribute__ ((__nothrow__)); extern long double __sinhl (long double __x) __attribute__ ((__nothrow__));
extern long double tanhl (long double __x) __attribute__ ((__nothrow__)); extern long double __tanhl (long double __x) __attribute__ ((__nothrow__));
extern long double acoshl (long double __x) __attribute__ ((__nothrow__)); extern long double __acoshl (long double __x) __attribute__ ((__nothrow__));
extern long double asinhl (long double __x) __attribute__ ((__nothrow__)); extern long double __asinhl (long double __x) __attribute__ ((__nothrow__));
extern long double atanhl (long double __x) __attribute__ ((__nothrow__)); extern long double __atanhl (long double __x) __attribute__ ((__nothrow__));
extern long double expl (long double __x) __attribute__ ((__nothrow__)); extern long double __expl (long double __x) __attribute__ ((__nothrow__));
extern long double frexpl (long double __x, int *__exponent) __attribute__ ((__nothrow__)); extern long double __frexpl (long double __x, int *__exponent) __attribute__ ((__nothrow__));
extern long double ldexpl (long double __x, int __exponent) __attribute__ ((__nothrow__)); extern long double __ldexpl (long double __x, int __exponent) __attribute__ ((__nothrow__));
extern long double logl (long double __x) __attribute__ ((__nothrow__)); extern long double __logl (long double __x) __attribute__ ((__nothrow__));
extern long double log10l (long double __x) __attribute__ ((__nothrow__)); extern long double __log10l (long double __x) __attribute__ ((__nothrow__));
extern long double modfl (long double __x, long double *__iptr) __attribute__ ((__nothrow__)); extern long double __modfl (long double __x, long double *__iptr) __attribute__ ((__nothrow__))
     __attribute__ ((__nonnull__ (2)));
extern long double expm1l (long double __x) __attribute__ ((__nothrow__)); extern long double __expm1l (long double __x) __attribute__ ((__nothrow__));
extern long double log1pl (long double __x) __attribute__ ((__nothrow__)); extern long double __log1pl (long double __x) __attribute__ ((__nothrow__));
extern long double logbl (long double __x) __attribute__ ((__nothrow__)); extern long double __logbl (long double __x) __attribute__ ((__nothrow__));
extern long double exp2l (long double __x) __attribute__ ((__nothrow__)); extern long double __exp2l (long double __x) __attribute__ ((__nothrow__));
extern long double log2l (long double __x) __attribute__ ((__nothrow__)); extern long double __log2l (long double __x) __attribute__ ((__nothrow__));
extern long double powl (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __powl (long double __x, long double __y) __attribute__ ((__nothrow__));
extern long double sqrtl (long double __x) __attribute__ ((__nothrow__)); extern long double __sqrtl (long double __x) __attribute__ ((__nothrow__));
extern long double hypotl (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __hypotl (long double __x, long double __y) __attribute__ ((__nothrow__));
extern long double cbrtl (long double __x) __attribute__ ((__nothrow__)); extern long double __cbrtl (long double __x) __attribute__ ((__nothrow__));
extern long double ceill (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __ceill (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long double fabsl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __fabsl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long double floorl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __floorl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long double fmodl (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __fmodl (long double __x, long double __y) __attribute__ ((__nothrow__));
extern int __isinfl (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int __finitel (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int isinfl (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int finitel (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long double dreml (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __dreml (long double __x, long double __y) __attribute__ ((__nothrow__));
extern long double significandl (long double __x) __attribute__ ((__nothrow__)); extern long double __significandl (long double __x) __attribute__ ((__nothrow__));
extern long double copysignl (long double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __copysignl (long double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long double nanl (__const char *__tagb) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __nanl (__const char *__tagb) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int __isnanl (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int isnanl (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long double j0l (long double) __attribute__ ((__nothrow__)); extern long double __j0l (long double) __attribute__ ((__nothrow__));
extern long double j1l (long double) __attribute__ ((__nothrow__)); extern long double __j1l (long double) __attribute__ ((__nothrow__));
extern long double jnl (int, long double) __attribute__ ((__nothrow__)); extern long double __jnl (int, long double) __attribute__ ((__nothrow__));
extern long double y0l (long double) __attribute__ ((__nothrow__)); extern long double __y0l (long double) __attribute__ ((__nothrow__));
extern long double y1l (long double) __attribute__ ((__nothrow__)); extern long double __y1l (long double) __attribute__ ((__nothrow__));
extern long double ynl (int, long double) __attribute__ ((__nothrow__)); extern long double __ynl (int, long double) __attribute__ ((__nothrow__));
extern long double erfl (long double) __attribute__ ((__nothrow__)); extern long double __erfl (long double) __attribute__ ((__nothrow__));
extern long double erfcl (long double) __attribute__ ((__nothrow__)); extern long double __erfcl (long double) __attribute__ ((__nothrow__));
extern long double lgammal (long double) __attribute__ ((__nothrow__)); extern long double __lgammal (long double) __attribute__ ((__nothrow__));
extern long double tgammal (long double) __attribute__ ((__nothrow__)); extern long double __tgammal (long double) __attribute__ ((__nothrow__));
extern long double gammal (long double) __attribute__ ((__nothrow__)); extern long double __gammal (long double) __attribute__ ((__nothrow__));
extern long double lgammal_r (long double, int *__signgamp) __attribute__ ((__nothrow__)); extern long double __lgammal_r (long double, int *__signgamp) __attribute__ ((__nothrow__));
extern long double rintl (long double __x) __attribute__ ((__nothrow__)); extern long double __rintl (long double __x) __attribute__ ((__nothrow__));
extern long double nextafterl (long double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __nextafterl (long double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long double nexttowardl (long double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __nexttowardl (long double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long double remainderl (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __remainderl (long double __x, long double __y) __attribute__ ((__nothrow__));
extern long double scalbnl (long double __x, int __n) __attribute__ ((__nothrow__)); extern long double __scalbnl (long double __x, int __n) __attribute__ ((__nothrow__));
extern int ilogbl (long double __x) __attribute__ ((__nothrow__)); extern int __ilogbl (long double __x) __attribute__ ((__nothrow__));
extern long double scalblnl (long double __x, long int __n) __attribute__ ((__nothrow__)); extern long double __scalblnl (long double __x, long int __n) __attribute__ ((__nothrow__));
extern long double nearbyintl (long double __x) __attribute__ ((__nothrow__)); extern long double __nearbyintl (long double __x) __attribute__ ((__nothrow__));
extern long double roundl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __roundl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long double truncl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __truncl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern long double remquol (long double __x, long double __y, int *__quo) __attribute__ ((__nothrow__)); extern long double __remquol (long double __x, long double __y, int *__quo) __attribute__ ((__nothrow__));
extern long int lrintl (long double __x) __attribute__ ((__nothrow__)); extern long int __lrintl (long double __x) __attribute__ ((__nothrow__));
extern long long int llrintl (long double __x) __attribute__ ((__nothrow__)); extern long long int __llrintl (long double __x) __attribute__ ((__nothrow__));
extern long int lroundl (long double __x) __attribute__ ((__nothrow__)); extern long int __lroundl (long double __x) __attribute__ ((__nothrow__));
extern long long int llroundl (long double __x) __attribute__ ((__nothrow__)); extern long long int __llroundl (long double __x) __attribute__ ((__nothrow__));
extern long double fdiml (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __fdiml (long double __x, long double __y) __attribute__ ((__nothrow__));
extern long double fmaxl (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __fmaxl (long double __x, long double __y) __attribute__ ((__nothrow__));
extern long double fminl (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __fminl (long double __x, long double __y) __attribute__ ((__nothrow__));
extern int __fpclassifyl (long double __value) __attribute__ ((__nothrow__))
     __attribute__ ((__const__));
extern int __signbitl (long double __value) __attribute__ ((__nothrow__))
     __attribute__ ((__const__));
extern long double fmal (long double __x, long double __y, long double __z) __attribute__ ((__nothrow__)); extern long double __fmal (long double __x, long double __y, long double __z) __attribute__ ((__nothrow__));
extern long double scalbl (long double __x, long double __n) __attribute__ ((__nothrow__)); extern long double __scalbl (long double __x, long double __n) __attribute__ ((__nothrow__));
extern int signgam;
enum
  {
    FP_NAN,
    FP_INFINITE,
    FP_ZERO,
    FP_SUBNORMAL,
    FP_NORMAL
  };
typedef enum
{
  _IEEE_ = -1,
  _SVID_,
  _XOPEN_,
  _POSIX_,
  _ISOC_
} _LIB_VERSION_TYPE;
extern _LIB_VERSION_TYPE _LIB_VERSION;
struct exception
  {
    int type;
    char *name;
    double arg1;
    double arg2;
    double retval;
  };
extern int matherr (struct exception *__exc);
extern __inline int
__attribute__ ((__nothrow__)) __signbitf (float __x)
{
  __extension__ union { float __f; int __i; } __u = { __f: __x };
  return __u.__i < 0;
}
extern __inline int
__attribute__ ((__nothrow__)) __signbit (double __x)
{
  __extension__ union { double __d; int __i[2]; } __u = { __d: __x };
  return __u.__i[1] < 0;
}
extern __inline int
__attribute__ ((__nothrow__)) __signbitl (long double __x)
{
  __extension__ union { long double __l; int __i[3]; } __u = { __l: __x };
  return (__u.__i[2] & 0x8000) != 0;
}
extern __inline double __sgn (double) __attribute__ ((__nothrow__)); extern __inline double __attribute__ ((__nothrow__)) __sgn (double __x) { return __x == 0.0 ? 0.0 : (__x > 0.0 ? 1.0 : -1.0); } extern __inline float __sgnf (float) __attribute__ ((__nothrow__)); extern __inline float __attribute__ ((__nothrow__)) __sgnf (float __x) { return __x == 0.0 ? 0.0 : (__x > 0.0 ? 1.0 : -1.0); } extern __inline long double __sgnl (long double) __attribute__ ((__nothrow__)); extern __inline long double __attribute__ ((__nothrow__)) __sgnl (long double __x) { return __x == 0.0 ? 0.0 : (__x > 0.0 ? 1.0 : -1.0); }
extern __inline long double __attribute__ ((__nothrow__)) __atan2l (long double __y, long double __x) { return __builtin_atan2l (__y, __x); }
extern __inline double __attribute__ ((__nothrow__)) fabs (double __x) { return __builtin_fabs (__x); }
extern __inline float __attribute__ ((__nothrow__)) fabsf (float __x) { return __builtin_fabsf (__x); }
extern __inline long double __attribute__ ((__nothrow__)) fabsl (long double __x) { return __builtin_fabsl (__x); }
extern __inline long double __attribute__ ((__nothrow__)) __fabsl (long double __x) { return __builtin_fabsl (__x); }
extern __inline long double __sgn1l (long double) __attribute__ ((__nothrow__)); extern __inline long double __attribute__ ((__nothrow__)) __sgn1l (long double __x) { __extension__ union { long double __xld; unsigned int __xi[3]; } __n = { __xld: __x }; __n.__xi[2] = (__n.__xi[2] & 0x8000) | 0x3fff; __n.__xi[1] = 0x80000000; __n.__xi[0] = 0; return __n.__xld; }
extern __inline double __attribute__ ((__nothrow__)) floor (double __x) { register long double __value; register int __ignore; unsigned short int __cw; unsigned short int __cwtmp; __asm __volatile ("fnstcw %3\n\t" "movzwl %3, %1\n\t" "andl $0xf3ff, %1\n\t" "orl $0x0400, %1\n\t" "movw %w1, %2\n\t" "fldcw %2\n\t" "frndint\n\t" "fldcw %3" : "=t" (__value), "=&q" (__ignore), "=m" (__cwtmp), "=m" (__cw) : "0" (__x)); return __value; } extern __inline float __attribute__ ((__nothrow__)) floorf (float __x) { register long double __value; register int __ignore; unsigned short int __cw; unsigned short int __cwtmp; __asm __volatile ("fnstcw %3\n\t" "movzwl %3, %1\n\t" "andl $0xf3ff, %1\n\t" "orl $0x0400, %1\n\t" "movw %w1, %2\n\t" "fldcw %2\n\t" "frndint\n\t" "fldcw %3" : "=t" (__value), "=&q" (__ignore), "=m" (__cwtmp), "=m" (__cw) : "0" (__x)); return __value; } extern __inline long double __attribute__ ((__nothrow__)) floorl (long double __x) { register long double __value; register int __ignore; unsigned short int __cw; unsigned short int __cwtmp; __asm __volatile ("fnstcw %3\n\t" "movzwl %3, %1\n\t" "andl $0xf3ff, %1\n\t" "orl $0x0400, %1\n\t" "movw %w1, %2\n\t" "fldcw %2\n\t" "frndint\n\t" "fldcw %3" : "=t" (__value), "=&q" (__ignore), "=m" (__cwtmp), "=m" (__cw) : "0" (__x)); return __value; }
extern __inline double __attribute__ ((__nothrow__)) ceil (double __x) { register long double __value; register int __ignore; unsigned short int __cw; unsigned short int __cwtmp; __asm __volatile ("fnstcw %3\n\t" "movzwl %3, %1\n\t" "andl $0xf3ff, %1\n\t" "orl $0x0800, %1\n\t" "movw %w1, %2\n\t" "fldcw %2\n\t" "frndint\n\t" "fldcw %3" : "=t" (__value), "=&q" (__ignore), "=m" (__cwtmp), "=m" (__cw) : "0" (__x)); return __value; } extern __inline float __attribute__ ((__nothrow__)) ceilf (float __x) { register long double __value; register int __ignore; unsigned short int __cw; unsigned short int __cwtmp; __asm __volatile ("fnstcw %3\n\t" "movzwl %3, %1\n\t" "andl $0xf3ff, %1\n\t" "orl $0x0800, %1\n\t" "movw %w1, %2\n\t" "fldcw %2\n\t" "frndint\n\t" "fldcw %3" : "=t" (__value), "=&q" (__ignore), "=m" (__cwtmp), "=m" (__cw) : "0" (__x)); return __value; } extern __inline long double __attribute__ ((__nothrow__)) ceill (long double __x) { register long double __value; register int __ignore; unsigned short int __cw; unsigned short int __cwtmp; __asm __volatile ("fnstcw %3\n\t" "movzwl %3, %1\n\t" "andl $0xf3ff, %1\n\t" "orl $0x0800, %1\n\t" "movw %w1, %2\n\t" "fldcw %2\n\t" "frndint\n\t" "fldcw %3" : "=t" (__value), "=&q" (__ignore), "=m" (__cwtmp), "=m" (__cw) : "0" (__x)); return __value; }
extern __inline long int
__attribute__ ((__nothrow__)) lrintf (float __x)
{
  long int __lrintres; __asm__ __volatile__ ("fistpl %0" : "=m" (__lrintres) : "t" (__x) : "st"); return __lrintres;
}
extern __inline long int
__attribute__ ((__nothrow__)) lrint (double __x)
{
  long int __lrintres; __asm__ __volatile__ ("fistpl %0" : "=m" (__lrintres) : "t" (__x) : "st"); return __lrintres;
}
extern __inline long int
__attribute__ ((__nothrow__)) lrintl (long double __x)
{
  long int __lrintres; __asm__ __volatile__ ("fistpl %0" : "=m" (__lrintres) : "t" (__x) : "st"); return __lrintres;
}
extern __inline long long int
__attribute__ ((__nothrow__)) llrintf (float __x)
{
  long long int __llrintres; __asm__ __volatile__ ("fistpll %0" : "=m" (__llrintres) : "t" (__x) : "st"); return __llrintres;
}
extern __inline long long int
__attribute__ ((__nothrow__)) llrint (double __x)
{
  long long int __llrintres; __asm__ __volatile__ ("fistpll %0" : "=m" (__llrintres) : "t" (__x) : "st"); return __llrintres;
}
extern __inline long long int
__attribute__ ((__nothrow__)) llrintl (long double __x)
{
  long long int __llrintres; __asm__ __volatile__ ("fistpll %0" : "=m" (__llrintres) : "t" (__x) : "st"); return __llrintres;
}
extern __inline int
__attribute__ ((__nothrow__)) __finite (double __x)
{
  return (__extension__
   (((((union { double __d; int __i[2]; }) {__d: __x}).__i[1]
      | 0x800fffffu) + 1) >> 31));
}
typedef unsigned int size_t;
extern void *memcpy (void *__restrict __dest,
       __const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern void *memmove (void *__dest, __const void *__src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern void *memccpy (void *__restrict __dest, __const void *__restrict __src,
        int __c, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern void *memset (void *__s, int __c, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int memcmp (__const void *__s1, __const void *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern void *memchr (__const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern char *strcpy (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *strncpy (char *__restrict __dest,
        __const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *strcat (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *strncat (char *__restrict __dest, __const char *__restrict __src,
        size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int strcmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern int strncmp (__const char *__s1, __const char *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern int strcoll (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern size_t strxfrm (char *__restrict __dest,
         __const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
typedef struct __locale_struct
{
  struct __locale_data *__locales[13];
  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;
  const char *__names[13];
} *__locale_t;
typedef __locale_t locale_t;
extern int strcoll_l (__const char *__s1, __const char *__s2, __locale_t __l)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 3)));
extern size_t strxfrm_l (char *__dest, __const char *__src, size_t __n,
    __locale_t __l) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 4)));
extern char *strdup (__const char *__s)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));
extern char *strndup (__const char *__string, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));
extern char *strchr (__const char *__s, int __c)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern char *strrchr (__const char *__s, int __c)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern size_t strcspn (__const char *__s, __const char *__reject)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern size_t strspn (__const char *__s, __const char *__accept)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *strpbrk (__const char *__s, __const char *__accept)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *strstr (__const char *__haystack, __const char *__needle)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *strtok (char *__restrict __s, __const char *__restrict __delim)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern char *__strtok_r (char *__restrict __s,
    __const char *__restrict __delim,
    char **__restrict __save_ptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));
extern char *strtok_r (char *__restrict __s, __const char *__restrict __delim,
         char **__restrict __save_ptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));
extern size_t strlen (__const char *__s)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern size_t strnlen (__const char *__string, size_t __maxlen)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern char *strerror (int __errnum) __attribute__ ((__nothrow__));
extern int strerror_r (int __errnum, char *__buf, size_t __buflen) __asm__ ("" "__xpg_strerror_r") __attribute__ ((__nothrow__))
                        __attribute__ ((__nonnull__ (2)));
extern char *strerror_l (int __errnum, __locale_t __l) __attribute__ ((__nothrow__));
extern void __bzero (void *__s, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern void bcopy (__const void *__src, void *__dest, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern void bzero (void *__s, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int bcmp (__const void *__s1, __const void *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *index (__const char *__s, int __c)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern char *rindex (__const char *__s, int __c)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern int ffs (int __i) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int strcasecmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern int strncasecmp (__const char *__s1, __const char *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *strsep (char **__restrict __stringp,
       __const char *__restrict __delim)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *strsignal (int __sig) __attribute__ ((__nothrow__));
extern char *__stpcpy (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpcpy (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *__stpncpy (char *__restrict __dest,
   __const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpncpy (char *__restrict __dest,
        __const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
__extension__ typedef signed long long int __int64_t;
__extension__ typedef unsigned long long int __uint64_t;
__extension__ typedef long long int __quad_t;
__extension__ typedef unsigned long long int __u_quad_t;
__extension__ typedef __u_quad_t __dev_t;
__extension__ typedef unsigned int __uid_t;
__extension__ typedef unsigned int __gid_t;
__extension__ typedef unsigned long int __ino_t;
__extension__ typedef __u_quad_t __ino64_t;
__extension__ typedef unsigned int __mode_t;
__extension__ typedef unsigned int __nlink_t;
__extension__ typedef long int __off_t;
__extension__ typedef __quad_t __off64_t;
__extension__ typedef int __pid_t;
__extension__ typedef struct { int __val[2]; } __fsid_t;
__extension__ typedef long int __clock_t;
__extension__ typedef unsigned long int __rlim_t;
__extension__ typedef __u_quad_t __rlim64_t;
__extension__ typedef unsigned int __id_t;
__extension__ typedef long int __time_t;
__extension__ typedef unsigned int __useconds_t;
__extension__ typedef long int __suseconds_t;
__extension__ typedef int __daddr_t;
__extension__ typedef long int __swblk_t;
__extension__ typedef int __key_t;
__extension__ typedef int __clockid_t;
__extension__ typedef void * __timer_t;
__extension__ typedef long int __blksize_t;
__extension__ typedef long int __blkcnt_t;
__extension__ typedef __quad_t __blkcnt64_t;
__extension__ typedef unsigned long int __fsblkcnt_t;
__extension__ typedef __u_quad_t __fsblkcnt64_t;
__extension__ typedef unsigned long int __fsfilcnt_t;
__extension__ typedef __u_quad_t __fsfilcnt64_t;
__extension__ typedef int __ssize_t;
typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;
__extension__ typedef int __intptr_t;
__extension__ typedef unsigned int __socklen_t;
extern void *__rawmemchr (const void *__s, int __c);
extern __inline size_t __strcspn_c1 (__const char *__s, int __reject);
extern __inline size_t
__strcspn_c1 (__const char *__s, int __reject)
{
  register size_t __result = 0;
  while (__s[__result] != '\0' && __s[__result] != __reject)
    ++__result;
  return __result;
}
extern __inline size_t __strcspn_c2 (__const char *__s, int __reject1,
         int __reject2);
extern __inline size_t
__strcspn_c2 (__const char *__s, int __reject1, int __reject2)
{
  register size_t __result = 0;
  while (__s[__result] != '\0' && __s[__result] != __reject1
  && __s[__result] != __reject2)
    ++__result;
  return __result;
}
extern __inline size_t __strcspn_c3 (__const char *__s, int __reject1,
         int __reject2, int __reject3);
extern __inline size_t
__strcspn_c3 (__const char *__s, int __reject1, int __reject2,
       int __reject3)
{
  register size_t __result = 0;
  while (__s[__result] != '\0' && __s[__result] != __reject1
  && __s[__result] != __reject2 && __s[__result] != __reject3)
    ++__result;
  return __result;
}
extern __inline size_t __strspn_c1 (__const char *__s, int __accept);
extern __inline size_t
__strspn_c1 (__const char *__s, int __accept)
{
  register size_t __result = 0;
  while (__s[__result] == __accept)
    ++__result;
  return __result;
}
extern __inline size_t __strspn_c2 (__const char *__s, int __accept1,
        int __accept2);
extern __inline size_t
__strspn_c2 (__const char *__s, int __accept1, int __accept2)
{
  register size_t __result = 0;
  while (__s[__result] == __accept1 || __s[__result] == __accept2)
    ++__result;
  return __result;
}
extern __inline size_t __strspn_c3 (__const char *__s, int __accept1,
        int __accept2, int __accept3);
extern __inline size_t
__strspn_c3 (__const char *__s, int __accept1, int __accept2, int __accept3)
{
  register size_t __result = 0;
  while (__s[__result] == __accept1 || __s[__result] == __accept2
  || __s[__result] == __accept3)
    ++__result;
  return __result;
}
extern __inline char *__strpbrk_c2 (__const char *__s, int __accept1,
         int __accept2);
extern __inline char *
__strpbrk_c2 (__const char *__s, int __accept1, int __accept2)
{
  while (*__s != '\0' && *__s != __accept1 && *__s != __accept2)
    ++__s;
  return *__s == '\0' ? ((void *)0) : (char *) (size_t) __s;
}
extern __inline char *__strpbrk_c3 (__const char *__s, int __accept1,
         int __accept2, int __accept3);
extern __inline char *
__strpbrk_c3 (__const char *__s, int __accept1, int __accept2,
       int __accept3)
{
  while (*__s != '\0' && *__s != __accept1 && *__s != __accept2
  && *__s != __accept3)
    ++__s;
  return *__s == '\0' ? ((void *)0) : (char *) (size_t) __s;
}
extern __inline char *__strtok_r_1c (char *__s, char __sep, char **__nextp);
extern __inline char *
__strtok_r_1c (char *__s, char __sep, char **__nextp)
{
  char *__result;
  if (__s == ((void *)0))
    __s = *__nextp;
  while (*__s == __sep)
    ++__s;
  __result = ((void *)0);
  if (*__s != '\0')
    {
      __result = __s++;
      while (*__s != '\0')
 if (*__s++ == __sep)
   {
     __s[-1] = '\0';
     break;
   }
    }
  *__nextp = __s;
  return __result;
}
extern char *__strsep_g (char **__stringp, __const char *__delim);
extern __inline char *__strsep_1c (char **__s, char __reject);
extern __inline char *
__strsep_1c (char **__s, char __reject)
{
  register char *__retval = *__s;
  if (__retval != ((void *)0) && (*__s = (__extension__ (__builtin_constant_p (__reject) && !__builtin_constant_p (__retval) && (__reject) == '\0' ? (char *) __rawmemchr (__retval, __reject) : __builtin_strchr (__retval, __reject)))) != ((void *)0))
    *(*__s)++ = '\0';
  return __retval;
}
extern __inline char *__strsep_2c (char **__s, char __reject1, char __reject2);
extern __inline char *
__strsep_2c (char **__s, char __reject1, char __reject2)
{
  register char *__retval = *__s;
  if (__retval != ((void *)0))
    {
      register char *__cp = __retval;
      while (1)
 {
   if (*__cp == '\0')
     {
       __cp = ((void *)0);
   break;
     }
   if (*__cp == __reject1 || *__cp == __reject2)
     {
       *__cp++ = '\0';
       break;
     }
   ++__cp;
 }
      *__s = __cp;
    }
  return __retval;
}
extern __inline char *__strsep_3c (char **__s, char __reject1, char __reject2,
       char __reject3);
extern __inline char *
__strsep_3c (char **__s, char __reject1, char __reject2, char __reject3)
{
  register char *__retval = *__s;
  if (__retval != ((void *)0))
    {
      register char *__cp = __retval;
      while (1)
 {
   if (*__cp == '\0')
     {
       __cp = ((void *)0);
   break;
     }
   if (*__cp == __reject1 || *__cp == __reject2 || *__cp == __reject3)
     {
       *__cp++ = '\0';
       break;
     }
   ++__cp;
 }
      *__s = __cp;
    }
  return __retval;
}
extern void *malloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) ;
extern void *calloc (size_t __nmemb, size_t __size)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) ;
extern char *__strdup (__const char *__string) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));
extern char *__strndup (__const char *__string, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));
typedef int ptrdiff_t;
typedef long int wchar_t;
struct _IO_FILE;
typedef struct _IO_FILE FILE;
typedef struct _IO_FILE __FILE;
typedef struct
{
  int __count;
  union
  {
    unsigned int __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;
typedef struct
{
  __off_t __pos;
  __mbstate_t __state;
} _G_fpos_t;
typedef struct
{
  __off64_t __pos;
  __mbstate_t __state;
} _G_fpos64_t;
typedef int _G_int16_t __attribute__ ((__mode__ (__HI__)));
typedef int _G_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int _G_uint16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int _G_uint32_t __attribute__ ((__mode__ (__SI__)));
typedef __builtin_va_list __gnuc_va_list;
struct _IO_jump_t; struct _IO_FILE;
typedef void _IO_lock_t;
struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;
  int _pos;
};
enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};
struct _IO_FILE {
  int _flags;
  char* _IO_read_ptr;
  char* _IO_read_end;
  char* _IO_read_base;
  char* _IO_write_base;
  char* _IO_write_ptr;
  char* _IO_write_end;
  char* _IO_buf_base;
  char* _IO_buf_end;
  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;
  struct _IO_marker *_markers;
  struct _IO_FILE *_chain;
  int _fileno;
  int _flags2;
  __off_t _old_offset;
  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];
  _IO_lock_t *_lock;
  __off64_t _offset;
  void *__pad1;
  void *__pad2;
  void *__pad3;
  void *__pad4;
  size_t __pad5;
  int _mode;
  char _unused2[15 * sizeof (int) - 4 * sizeof (void *) - sizeof (size_t)];
};
typedef struct _IO_FILE _IO_FILE;
struct _IO_FILE_plus;
extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;
typedef __ssize_t __io_read_fn (void *__cookie, char *__buf, size_t __nbytes);
typedef __ssize_t __io_write_fn (void *__cookie, __const char *__buf,
     size_t __n);
typedef int __io_seek_fn (void *__cookie, __off64_t *__pos, int __w);
typedef int __io_close_fn (void *__cookie);
extern int __underflow (_IO_FILE *);
extern int __uflow (_IO_FILE *);
extern int __overflow (_IO_FILE *, int);
extern int _IO_getc (_IO_FILE *__fp);
extern int _IO_putc (int __c, _IO_FILE *__fp);
extern int _IO_feof (_IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_ferror (_IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_peekc_locked (_IO_FILE *__fp);
extern void _IO_flockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern void _IO_funlockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_ftrylockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_vfscanf (_IO_FILE * __restrict, const char * __restrict,
   __gnuc_va_list, int *__restrict);
extern int _IO_vfprintf (_IO_FILE *__restrict, const char *__restrict,
    __gnuc_va_list);
extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t);
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t);
extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int);
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int);
extern void _IO_free_backup_area (_IO_FILE *) __attribute__ ((__nothrow__));
typedef __gnuc_va_list va_list;
typedef __off64_t off_t;
typedef __off64_t off64_t;
typedef __ssize_t ssize_t;
typedef _G_fpos64_t fpos_t;
typedef _G_fpos64_t fpos64_t;
extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;
extern int remove (__const char *__filename) __attribute__ ((__nothrow__));
extern int rename (__const char *__old, __const char *__new) __attribute__ ((__nothrow__));
extern int renameat (int __oldfd, __const char *__old, int __newfd,
       __const char *__new) __attribute__ ((__nothrow__));
extern FILE *tmpfile (void) __asm__ ("" "tmpfile64") ;
extern FILE *tmpfile64 (void) ;
extern char *tmpnam (char *__s) __attribute__ ((__nothrow__)) ;
extern char *tmpnam_r (char *__s) __attribute__ ((__nothrow__)) ;
extern char *tempnam (__const char *__dir, __const char *__pfx)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) ;
extern int fclose (FILE *__stream);
extern int fflush (FILE *__stream);
extern int fflush_unlocked (FILE *__stream);
extern FILE *fopen (__const char *__restrict __filename, __const char *__restrict __modes) __asm__ ("" "fopen64")
  ;
extern FILE *freopen (__const char *__restrict __filename, __const char *__restrict __modes, FILE *__restrict __stream) __asm__ ("" "freopen64")
  ;
extern FILE *fopen64 (__const char *__restrict __filename,
        __const char *__restrict __modes) ;
extern FILE *freopen64 (__const char *__restrict __filename,
   __const char *__restrict __modes,
   FILE *__restrict __stream) ;
extern FILE *fdopen (int __fd, __const char *__modes) __attribute__ ((__nothrow__)) ;
extern FILE *fmemopen (void *__s, size_t __len, __const char *__modes)
  __attribute__ ((__nothrow__)) ;
extern FILE *open_memstream (char **__bufloc, size_t *__sizeloc) __attribute__ ((__nothrow__)) ;
extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) __attribute__ ((__nothrow__));
extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
      int __modes, size_t __n) __attribute__ ((__nothrow__));
extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
         size_t __size) __attribute__ ((__nothrow__));
extern void setlinebuf (FILE *__stream) __attribute__ ((__nothrow__));
extern int fprintf (FILE *__restrict __stream,
      __const char *__restrict __format, ...);
extern int printf (__const char *__restrict __format, ...);
extern int sprintf (char *__restrict __s,
      __const char *__restrict __format, ...) __attribute__ ((__nothrow__));
extern int vfprintf (FILE *__restrict __s, __const char *__restrict __format,
       __gnuc_va_list __arg);
extern int vprintf (__const char *__restrict __format, __gnuc_va_list __arg);
extern int vsprintf (char *__restrict __s, __const char *__restrict __format,
       __gnuc_va_list __arg) __attribute__ ((__nothrow__));
extern int snprintf (char *__restrict __s, size_t __maxlen,
       __const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4)));
extern int vsnprintf (char *__restrict __s, size_t __maxlen,
        __const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 0)));
extern int vdprintf (int __fd, __const char *__restrict __fmt,
       __gnuc_va_list __arg)
     __attribute__ ((__format__ (__printf__, 2, 0)));
extern int dprintf (int __fd, __const char *__restrict __fmt, ...)
     __attribute__ ((__format__ (__printf__, 2, 3)));
extern int fscanf (FILE *__restrict __stream,
     __const char *__restrict __format, ...) ;
extern int scanf (__const char *__restrict __format, ...) ;
extern int sscanf (__const char *__restrict __s,
     __const char *__restrict __format, ...) __attribute__ ((__nothrow__));
extern int fscanf (FILE *__restrict __stream, __const char *__restrict __format, ...) __asm__ ("" "__isoc99_fscanf")
                               ;
extern int scanf (__const char *__restrict __format, ...) __asm__ ("" "__isoc99_scanf")
                              ;
extern int sscanf (__const char *__restrict __s, __const char *__restrict __format, ...) __asm__ ("" "__isoc99_sscanf") __attribute__ ((__nothrow__))
                      ;
extern int vfscanf (FILE *__restrict __s, __const char *__restrict __format,
      __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 2, 0))) ;
extern int vscanf (__const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 1, 0))) ;
extern int vsscanf (__const char *__restrict __s,
      __const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__scanf__, 2, 0)));
extern int vfscanf (FILE *__restrict __s, __const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vfscanf")
     __attribute__ ((__format__ (__scanf__, 2, 0))) ;
extern int vscanf (__const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vscanf")
     __attribute__ ((__format__ (__scanf__, 1, 0))) ;
extern int vsscanf (__const char *__restrict __s, __const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vsscanf") __attribute__ ((__nothrow__))
     __attribute__ ((__format__ (__scanf__, 2, 0)));
extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);
extern int getchar (void);
extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);
extern int fgetc_unlocked (FILE *__stream);
extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);
extern int putchar (int __c);
extern int fputc_unlocked (int __c, FILE *__stream);
extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);
extern int getw (FILE *__stream);
extern int putw (int __w, FILE *__stream);
extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
     ;
extern char *gets (char *__s) ;
extern __ssize_t __getdelim (char **__restrict __lineptr,
          size_t *__restrict __n, int __delimiter,
          FILE *__restrict __stream) ;
extern __ssize_t getdelim (char **__restrict __lineptr,
        size_t *__restrict __n, int __delimiter,
        FILE *__restrict __stream) ;
extern __ssize_t getline (char **__restrict __lineptr,
       size_t *__restrict __n,
       FILE *__restrict __stream) ;
extern int fputs (__const char *__restrict __s, FILE *__restrict __stream);
extern int puts (__const char *__s);
extern int ungetc (int __c, FILE *__stream);
extern size_t fread (void *__restrict __ptr, size_t __size,
       size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite (__const void *__restrict __ptr, size_t __size,
        size_t __n, FILE *__restrict __s) ;
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite_unlocked (__const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream) ;
extern int fseek (FILE *__stream, long int __off, int __whence);
extern long int ftell (FILE *__stream) ;
extern void rewind (FILE *__stream);
extern int fseeko (FILE *__stream, __off64_t __off, int __whence) __asm__ ("" "fseeko64")
                  ;
extern __off64_t ftello (FILE *__stream) __asm__ ("" "ftello64");
extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos) __asm__ ("" "fgetpos64")
                                          ;
extern int fsetpos (FILE *__stream, __const fpos_t *__pos) __asm__ ("" "fsetpos64")
                                                            ;
extern int fseeko64 (FILE *__stream, __off64_t __off, int __whence);
extern __off64_t ftello64 (FILE *__stream) ;
extern int fgetpos64 (FILE *__restrict __stream, fpos64_t *__restrict __pos);
extern int fsetpos64 (FILE *__stream, __const fpos64_t *__pos);
extern void clearerr (FILE *__stream) __attribute__ ((__nothrow__));
extern int feof (FILE *__stream) __attribute__ ((__nothrow__)) ;
extern int ferror (FILE *__stream) __attribute__ ((__nothrow__)) ;
extern void clearerr_unlocked (FILE *__stream) __attribute__ ((__nothrow__));
extern int feof_unlocked (FILE *__stream) __attribute__ ((__nothrow__)) ;
extern int ferror_unlocked (FILE *__stream) __attribute__ ((__nothrow__)) ;
extern void perror (__const char *__s);
extern int sys_nerr;
extern __const char *__const sys_errlist[];
extern int fileno (FILE *__stream) __attribute__ ((__nothrow__)) ;
extern int fileno_unlocked (FILE *__stream) __attribute__ ((__nothrow__)) ;
extern FILE *popen (__const char *__command, __const char *__modes) ;
extern int pclose (FILE *__stream);
extern char *ctermid (char *__s) __attribute__ ((__nothrow__));
extern void flockfile (FILE *__stream) __attribute__ ((__nothrow__));
extern int ftrylockfile (FILE *__stream) __attribute__ ((__nothrow__)) ;
extern void funlockfile (FILE *__stream) __attribute__ ((__nothrow__));
extern __inline int
vprintf (__const char *__restrict __fmt, __gnuc_va_list __arg)
{
  return vfprintf (stdout, __fmt, __arg);
}
extern __inline int
getchar (void)
{
  return _IO_getc (stdin);
}
extern __inline int
fgetc_unlocked (FILE *__fp)
{
  return (__builtin_expect (((__fp)->_IO_read_ptr >= (__fp)->_IO_read_end), 0) ? __uflow (__fp) : *(unsigned char *) (__fp)->_IO_read_ptr++);
}
extern __inline int
getc_unlocked (FILE *__fp)
{
  return (__builtin_expect (((__fp)->_IO_read_ptr >= (__fp)->_IO_read_end), 0) ? __uflow (__fp) : *(unsigned char *) (__fp)->_IO_read_ptr++);
}
extern __inline int
getchar_unlocked (void)
{
  return (__builtin_expect (((stdin)->_IO_read_ptr >= (stdin)->_IO_read_end), 0) ? __uflow (stdin) : *(unsigned char *) (stdin)->_IO_read_ptr++);
}
extern __inline int
putchar (int __c)
{
  return _IO_putc (__c, stdout);
}
extern __inline int
fputc_unlocked (int __c, FILE *__stream)
{
  return (__builtin_expect (((__stream)->_IO_write_ptr >= (__stream)->_IO_write_end), 0) ? __overflow (__stream, (unsigned char) (__c)) : (unsigned char) (*(__stream)->_IO_write_ptr++ = (__c)));
}
extern __inline int
putc_unlocked (int __c, FILE *__stream)
{
  return (__builtin_expect (((__stream)->_IO_write_ptr >= (__stream)->_IO_write_end), 0) ? __overflow (__stream, (unsigned char) (__c)) : (unsigned char) (*(__stream)->_IO_write_ptr++ = (__c)));
}
extern __inline int
putchar_unlocked (int __c)
{
  return (__builtin_expect (((stdout)->_IO_write_ptr >= (stdout)->_IO_write_end), 0) ? __overflow (stdout, (unsigned char) (__c)) : (unsigned char) (*(stdout)->_IO_write_ptr++ = (__c)));
}
extern __inline int
__attribute__ ((__nothrow__)) feof_unlocked (FILE *__stream)
{
  return (((__stream)->_flags & 0x10) != 0);
}
extern __inline int
__attribute__ ((__nothrow__)) ferror_unlocked (FILE *__stream)
{
  return (((__stream)->_flags & 0x20) != 0);
}
typedef signed char int8_t;
typedef short int int16_t;
typedef int int32_t;
__extension__
typedef long long int int64_t;
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
__extension__
typedef unsigned long long int uint64_t;
typedef signed char int_least8_t;
typedef short int int_least16_t;
typedef int int_least32_t;
__extension__
typedef long long int int_least64_t;
typedef unsigned char uint_least8_t;
typedef unsigned short int uint_least16_t;
typedef unsigned int uint_least32_t;
__extension__
typedef unsigned long long int uint_least64_t;
typedef signed char int_fast8_t;
typedef int int_fast16_t;
typedef int int_fast32_t;
__extension__
typedef long long int int_fast64_t;
typedef unsigned char uint_fast8_t;
typedef unsigned int uint_fast16_t;
typedef unsigned int uint_fast32_t;
__extension__
typedef unsigned long long int uint_fast64_t;
typedef int intptr_t;
typedef unsigned int uintptr_t;
__extension__
typedef long long int intmax_t;
__extension__
typedef unsigned long long int uintmax_t;
typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;
typedef __loff_t loff_t;
typedef __ino64_t ino_t;
typedef __ino64_t ino64_t;
typedef __dev_t dev_t;
typedef __gid_t gid_t;
typedef __mode_t mode_t;
typedef __nlink_t nlink_t;
typedef __uid_t uid_t;
typedef __pid_t pid_t;
typedef __id_t id_t;
typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;
typedef __key_t key_t;
typedef __clock_t clock_t;
typedef __time_t time_t;
typedef __clockid_t clockid_t;
typedef __timer_t timer_t;
typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;
typedef unsigned int u_int8_t __attribute__ ((__mode__ (__QI__)));
typedef unsigned int u_int16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int u_int64_t __attribute__ ((__mode__ (__DI__)));
typedef int register_t __attribute__ ((__mode__ (__word__)));
typedef int __sig_atomic_t;
typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;
typedef __sigset_t sigset_t;
struct timespec
  {
    __time_t tv_sec;
    long int tv_nsec;
  };
struct timeval
  {
    __time_t tv_sec;
    __suseconds_t tv_usec;
  };
typedef __suseconds_t suseconds_t;
typedef long int __fd_mask;
typedef struct
  {
    __fd_mask __fds_bits[1024 / (8 * (int) sizeof (__fd_mask))];
  } fd_set;
typedef __fd_mask fd_mask;
extern int select (int __nfds, fd_set *__restrict __readfds,
     fd_set *__restrict __writefds,
     fd_set *__restrict __exceptfds,
     struct timeval *__restrict __timeout);
extern int pselect (int __nfds, fd_set *__restrict __readfds,
      fd_set *__restrict __writefds,
      fd_set *__restrict __exceptfds,
      const struct timespec *__restrict __timeout,
      const __sigset_t *__restrict __sigmask);
__extension__
extern unsigned int gnu_dev_major (unsigned long long int __dev)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));
__extension__
extern unsigned int gnu_dev_minor (unsigned long long int __dev)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));
__extension__
extern unsigned long long int gnu_dev_makedev (unsigned int __major,
            unsigned int __minor)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));
__extension__ extern __inline __attribute__ ((__const__)) unsigned int
__attribute__ ((__nothrow__)) gnu_dev_major (unsigned long long int __dev)
{
  return ((__dev >> 8) & 0xfff) | ((unsigned int) (__dev >> 32) & ~0xfff);
}
__extension__ extern __inline __attribute__ ((__const__)) unsigned int
__attribute__ ((__nothrow__)) gnu_dev_minor (unsigned long long int __dev)
{
  return (__dev & 0xff) | ((unsigned int) (__dev >> 12) & ~0xff);
}
__extension__ extern __inline __attribute__ ((__const__)) unsigned long long int
__attribute__ ((__nothrow__)) gnu_dev_makedev (unsigned int __major, unsigned int __minor)
{
  return ((__minor & 0xff) | ((__major & 0xfff) << 8)
   | (((unsigned long long int) (__minor & ~0xff)) << 12)
   | (((unsigned long long int) (__major & ~0xfff)) << 32));
}
typedef __blksize_t blksize_t;
typedef __blkcnt64_t blkcnt_t;
typedef __fsblkcnt64_t fsblkcnt_t;
typedef __fsfilcnt64_t fsfilcnt_t;
typedef __blkcnt64_t blkcnt64_t;
typedef __fsblkcnt64_t fsblkcnt64_t;
typedef __fsfilcnt64_t fsfilcnt64_t;
typedef unsigned long int pthread_t;
typedef union
{
  char __size[36];
  long int __align;
} pthread_attr_t;
typedef struct __pthread_internal_slist
{
  struct __pthread_internal_slist *__next;
} __pthread_slist_t;
typedef union
{
  struct __pthread_mutex_s
  {
    int __lock;
    unsigned int __count;
    int __owner;
    int __kind;
    unsigned int __nusers;
    __extension__ union
    {
      int __spins;
      __pthread_slist_t __list;
    };
  } __data;
  char __size[24];
  long int __align;
} pthread_mutex_t;
typedef union
{
  char __size[4];
  long int __align;
} pthread_mutexattr_t;
typedef union
{
  struct
  {
    int __lock;
    unsigned int __futex;
    __extension__ unsigned long long int __total_seq;
    __extension__ unsigned long long int __wakeup_seq;
    __extension__ unsigned long long int __woken_seq;
    void *__mutex;
    unsigned int __nwaiters;
    unsigned int __broadcast_seq;
  } __data;
  char __size[48];
  __extension__ long long int __align;
} pthread_cond_t;
typedef union
{
  char __size[4];
  long int __align;
} pthread_condattr_t;
typedef unsigned int pthread_key_t;
typedef int pthread_once_t;
typedef union
{
  struct
  {
    int __lock;
    unsigned int __nr_readers;
    unsigned int __readers_wakeup;
    unsigned int __writer_wakeup;
    unsigned int __nr_readers_queued;
    unsigned int __nr_writers_queued;
    unsigned char __flags;
    unsigned char __shared;
    unsigned char __pad1;
    unsigned char __pad2;
    int __writer;
  } __data;
  char __size[32];
  long int __align;
} pthread_rwlock_t;
typedef union
{
  char __size[8];
  long int __align;
} pthread_rwlockattr_t;
typedef volatile int pthread_spinlock_t;
typedef union
{
  char __size[20];
  long int __align;
} pthread_barrier_t;
typedef union
{
  char __size[4];
  int __align;
} pthread_barrierattr_t;
struct iovec
  {
    void *iov_base;
    size_t iov_len;
  };
extern ssize_t readv (int __fd, __const struct iovec *__iovec, int __count)
  ;
extern ssize_t writev (int __fd, __const struct iovec *__iovec, int __count)
  ;
extern ssize_t preadv (int __fd, __const struct iovec *__iovec, int __count, __off64_t __offset) __asm__ ("" "preadv64")
                     ;
extern ssize_t pwritev (int __fd, __const struct iovec *__iovec, int __count, __off64_t __offset) __asm__ ("" "pwritev64")
                      ;
extern ssize_t preadv64 (int __fd, __const struct iovec *__iovec, int __count,
    __off64_t __offset) ;
extern ssize_t pwritev64 (int __fd, __const struct iovec *__iovec, int __count,
     __off64_t __offset) ;
typedef __socklen_t socklen_t;
enum __socket_type
{
  SOCK_STREAM = 1,
  SOCK_DGRAM = 2,
  SOCK_RAW = 3,
  SOCK_RDM = 4,
  SOCK_SEQPACKET = 5,
  SOCK_DCCP = 6,
  SOCK_PACKET = 10,
  SOCK_CLOEXEC = 02000000,
  SOCK_NONBLOCK = 04000
};
typedef unsigned short int sa_family_t;
struct sockaddr
  {
    sa_family_t sa_family;
    char sa_data[14];
  };
struct sockaddr_storage
  {
    sa_family_t ss_family;
    unsigned long int __ss_align;
    char __ss_padding[(128 - (2 * sizeof (unsigned long int)))];
  };
enum
  {
    MSG_OOB = 0x01,
    MSG_PEEK = 0x02,
    MSG_DONTROUTE = 0x04,
    MSG_CTRUNC = 0x08,
    MSG_PROXY = 0x10,
    MSG_TRUNC = 0x20,
    MSG_DONTWAIT = 0x40,
    MSG_EOR = 0x80,
    MSG_WAITALL = 0x100,
    MSG_FIN = 0x200,
    MSG_SYN = 0x400,
    MSG_CONFIRM = 0x800,
    MSG_RST = 0x1000,
    MSG_ERRQUEUE = 0x2000,
    MSG_NOSIGNAL = 0x4000,
    MSG_MORE = 0x8000,
    MSG_WAITFORONE = 0x10000,
    MSG_CMSG_CLOEXEC = 0x40000000
  };
struct msghdr
  {
    void *msg_name;
    socklen_t msg_namelen;
    struct iovec *msg_iov;
    size_t msg_iovlen;
    void *msg_control;
    size_t msg_controllen;
    int msg_flags;
  };
struct cmsghdr
  {
    size_t cmsg_len;
    int cmsg_level;
    int cmsg_type;
    __extension__ unsigned char __cmsg_data [];
  };
extern struct cmsghdr *__cmsg_nxthdr (struct msghdr *__mhdr,
          struct cmsghdr *__cmsg) __attribute__ ((__nothrow__));
extern __inline struct cmsghdr *
__attribute__ ((__nothrow__)) __cmsg_nxthdr (struct msghdr *__mhdr, struct cmsghdr *__cmsg)
{
  if ((size_t) __cmsg->cmsg_len < sizeof (struct cmsghdr))
    return (struct cmsghdr *) 0;
  __cmsg = (struct cmsghdr *) ((unsigned char *) __cmsg
          + (((__cmsg->cmsg_len) + sizeof (size_t) - 1) & (size_t) ~(sizeof (size_t) - 1)));
  if ((unsigned char *) (__cmsg + 1) > ((unsigned char *) __mhdr->msg_control
     + __mhdr->msg_controllen)
      || ((unsigned char *) __cmsg + (((__cmsg->cmsg_len) + sizeof (size_t) - 1) & (size_t) ~(sizeof (size_t) - 1))
   > ((unsigned char *) __mhdr->msg_control + __mhdr->msg_controllen)))
    return (struct cmsghdr *) 0;
  return __cmsg;
}
enum
  {
    SCM_RIGHTS = 0x01
  };
struct linger
  {
    int l_onoff;
    int l_linger;
  };
extern int recvmmsg (int __fd, struct mmsghdr *__vmessages,
       unsigned int __vlen, int __flags,
       __const struct timespec *__tmo);
extern int sendmmsg (int __fd, struct mmsghdr *__vmessages,
       unsigned int __vlen, int __flags);
struct osockaddr
  {
    unsigned short int sa_family;
    unsigned char sa_data[14];
  };
enum
{
  SHUT_RD = 0,
  SHUT_WR,
  SHUT_RDWR
};
extern int socket (int __domain, int __type, int __protocol) __attribute__ ((__nothrow__));
extern int socketpair (int __domain, int __type, int __protocol,
         int __fds[2]) __attribute__ ((__nothrow__));
extern int bind (int __fd, __const struct sockaddr * __addr, socklen_t __len)
     __attribute__ ((__nothrow__));
extern int getsockname (int __fd, struct sockaddr *__restrict __addr,
   socklen_t *__restrict __len) __attribute__ ((__nothrow__));
extern int connect (int __fd, __const struct sockaddr * __addr, socklen_t __len);
extern int getpeername (int __fd, struct sockaddr *__restrict __addr,
   socklen_t *__restrict __len) __attribute__ ((__nothrow__));
extern ssize_t send (int __fd, __const void *__buf, size_t __n, int __flags);
extern ssize_t recv (int __fd, void *__buf, size_t __n, int __flags);
extern ssize_t sendto (int __fd, __const void *__buf, size_t __n,
         int __flags, __const struct sockaddr * __addr,
         socklen_t __addr_len);
extern ssize_t recvfrom (int __fd, void *__restrict __buf, size_t __n,
    int __flags, struct sockaddr *__restrict __addr,
    socklen_t *__restrict __addr_len);
extern ssize_t sendmsg (int __fd, __const struct msghdr *__message,
   int __flags);
extern ssize_t recvmsg (int __fd, struct msghdr *__message, int __flags);
extern int getsockopt (int __fd, int __level, int __optname,
         void *__restrict __optval,
         socklen_t *__restrict __optlen) __attribute__ ((__nothrow__));
extern int setsockopt (int __fd, int __level, int __optname,
         __const void *__optval, socklen_t __optlen) __attribute__ ((__nothrow__));
extern int listen (int __fd, int __n) __attribute__ ((__nothrow__));
extern int accept (int __fd, struct sockaddr *__restrict __addr,
     socklen_t *__restrict __addr_len);
extern int shutdown (int __fd, int __how) __attribute__ ((__nothrow__));
extern int sockatmark (int __fd) __attribute__ ((__nothrow__));
extern int isfdtype (int __fd, int __fdtype) __attribute__ ((__nothrow__));
enum
  {
    IPPROTO_IP = 0,
    IPPROTO_HOPOPTS = 0,
    IPPROTO_ICMP = 1,
    IPPROTO_IGMP = 2,
    IPPROTO_IPIP = 4,
    IPPROTO_TCP = 6,
    IPPROTO_EGP = 8,
    IPPROTO_PUP = 12,
    IPPROTO_UDP = 17,
    IPPROTO_IDP = 22,
    IPPROTO_TP = 29,
    IPPROTO_DCCP = 33,
    IPPROTO_IPV6 = 41,
    IPPROTO_ROUTING = 43,
    IPPROTO_FRAGMENT = 44,
    IPPROTO_RSVP = 46,
    IPPROTO_GRE = 47,
    IPPROTO_ESP = 50,
    IPPROTO_AH = 51,
    IPPROTO_ICMPV6 = 58,
    IPPROTO_NONE = 59,
    IPPROTO_DSTOPTS = 60,
    IPPROTO_MTP = 92,
    IPPROTO_ENCAP = 98,
    IPPROTO_PIM = 103,
    IPPROTO_COMP = 108,
    IPPROTO_SCTP = 132,
    IPPROTO_UDPLITE = 136,
    IPPROTO_RAW = 255,
    IPPROTO_MAX
  };
typedef uint16_t in_port_t;
enum
  {
    IPPORT_ECHO = 7,
    IPPORT_DISCARD = 9,
    IPPORT_SYSTAT = 11,
    IPPORT_DAYTIME = 13,
    IPPORT_NETSTAT = 15,
    IPPORT_FTP = 21,
    IPPORT_TELNET = 23,
    IPPORT_SMTP = 25,
    IPPORT_TIMESERVER = 37,
    IPPORT_NAMESERVER = 42,
    IPPORT_WHOIS = 43,
    IPPORT_MTP = 57,
    IPPORT_TFTP = 69,
    IPPORT_RJE = 77,
    IPPORT_FINGER = 79,
    IPPORT_TTYLINK = 87,
    IPPORT_SUPDUP = 95,
    IPPORT_EXECSERVER = 512,
    IPPORT_LOGINSERVER = 513,
    IPPORT_CMDSERVER = 514,
    IPPORT_EFSSERVER = 520,
    IPPORT_BIFFUDP = 512,
    IPPORT_WHOSERVER = 513,
    IPPORT_ROUTESERVER = 520,
    IPPORT_RESERVED = 1024,
    IPPORT_USERRESERVED = 5000
  };
typedef uint32_t in_addr_t;
struct in_addr
  {
    in_addr_t s_addr;
  };
struct in6_addr
  {
    union
      {
 uint8_t __u6_addr8[16];
 uint16_t __u6_addr16[8];
 uint32_t __u6_addr32[4];
      } __in6_u;
  };
extern const struct in6_addr in6addr_any;
extern const struct in6_addr in6addr_loopback;
struct sockaddr_in
  {
    sa_family_t sin_family;
    in_port_t sin_port;
    struct in_addr sin_addr;
    unsigned char sin_zero[sizeof (struct sockaddr) -
      (sizeof (unsigned short int)) -
      sizeof (in_port_t) -
      sizeof (struct in_addr)];
  };
struct sockaddr_in6
  {
    sa_family_t sin6_family;
    in_port_t sin6_port;
    uint32_t sin6_flowinfo;
    struct in6_addr sin6_addr;
    uint32_t sin6_scope_id;
  };
struct ip_mreq
  {
    struct in_addr imr_multiaddr;
    struct in_addr imr_interface;
  };
struct ip_mreq_source
  {
    struct in_addr imr_multiaddr;
    struct in_addr imr_interface;
    struct in_addr imr_sourceaddr;
  };
struct ipv6_mreq
  {
    struct in6_addr ipv6mr_multiaddr;
    unsigned int ipv6mr_interface;
  };
struct group_req
  {
    uint32_t gr_interface;
    struct sockaddr_storage gr_group;
  };
struct group_source_req
  {
    uint32_t gsr_interface;
    struct sockaddr_storage gsr_group;
    struct sockaddr_storage gsr_source;
  };
struct ip_msfilter
  {
    struct in_addr imsf_multiaddr;
    struct in_addr imsf_interface;
    uint32_t imsf_fmode;
    uint32_t imsf_numsrc;
    struct in_addr imsf_slist[1];
  };
struct group_filter
  {
    uint32_t gf_interface;
    struct sockaddr_storage gf_group;
    uint32_t gf_fmode;
    uint32_t gf_numsrc;
    struct sockaddr_storage gf_slist[1];
};
struct ip_opts
  {
    struct in_addr ip_dst;
    char ip_opts[40];
  };
struct ip_mreqn
  {
    struct in_addr imr_multiaddr;
    struct in_addr imr_address;
    int imr_ifindex;
  };
struct in_pktinfo
  {
    int ipi_ifindex;
    struct in_addr ipi_spec_dst;
    struct in_addr ipi_addr;
  };
extern uint32_t ntohl (uint32_t __netlong) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern uint16_t ntohs (uint16_t __netshort)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern uint32_t htonl (uint32_t __hostlong)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern uint16_t htons (uint16_t __hostshort)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int bindresvport (int __sockfd, struct sockaddr_in *__sock_in) __attribute__ ((__nothrow__));
extern int bindresvport6 (int __sockfd, struct sockaddr_in6 *__sock_in)
     __attribute__ ((__nothrow__));
 size_t MEM_allocN_len(void *vmemh) __attribute__((warn_unused_result));
 short MEM_freeN(void *vmemh);
 short MEM_testN(void *vmemh);
 void *MEM_dupallocN(void *vmemh) __attribute__((warn_unused_result));
 void *MEM_reallocN(void *vmemh, size_t len) __attribute__((warn_unused_result));
 void *MEM_callocN(size_t len, const char * str) __attribute__((warn_unused_result));
 void *MEM_mallocN(size_t len, const char * str) __attribute__((warn_unused_result));
 void *MEM_mapallocN(size_t len, const char * str) __attribute__((warn_unused_result));
 void MEM_printmemlist_pydict(void);
 void MEM_printmemlist(void);
 void MEM_callbackmemlist(void (*func)(void*));
 void MEM_printmemlist_stats(void);
 void MEM_set_error_callback(void (*func)(const char *));
 int MEM_check_memory_integrity(void);
 void MEM_set_lock_callback(void (*lock)(void), void (*unlock)(void));
 void MEM_set_memory_debug(void);
 uintptr_t MEM_get_memory_in_use(void);
 uintptr_t MEM_get_mapped_memory_in_use(void);
 int MEM_get_memory_blocks_in_use(void);
 void MEM_reset_peak_memory(void);
 uintptr_t MEM_get_peak_memory(void) __attribute__((warn_unused_result));
struct ListBase;
union wait
  {
    int w_status;
    struct
      {
 unsigned int __w_termsig:7;
 unsigned int __w_coredump:1;
 unsigned int __w_retcode:8;
 unsigned int:16;
      } __wait_terminated;
    struct
      {
 unsigned int __w_stopval:8;
 unsigned int __w_stopsig:8;
 unsigned int:16;
      } __wait_stopped;
  };
typedef union
  {
    union wait *__uptr;
    int *__iptr;
  } __WAIT_STATUS __attribute__ ((__transparent_union__));
typedef struct
  {
    int quot;
    int rem;
  } div_t;
typedef struct
  {
    long int quot;
    long int rem;
  } ldiv_t;
__extension__ typedef struct
  {
    long long int quot;
    long long int rem;
  } lldiv_t;
extern size_t __ctype_get_mb_cur_max (void) __attribute__ ((__nothrow__)) ;
extern double atof (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;
extern int atoi (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;
extern long int atol (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;
__extension__ extern long long int atoll (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;
extern double strtod (__const char *__restrict __nptr,
        char **__restrict __endptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
extern float strtof (__const char *__restrict __nptr,
       char **__restrict __endptr) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
extern long double strtold (__const char *__restrict __nptr,
       char **__restrict __endptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
extern long int strtol (__const char *__restrict __nptr,
   char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
extern unsigned long int strtoul (__const char *__restrict __nptr,
      char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
__extension__
extern long long int strtoq (__const char *__restrict __nptr,
        char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
__extension__
extern unsigned long long int strtouq (__const char *__restrict __nptr,
           char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
__extension__
extern long long int strtoll (__const char *__restrict __nptr,
         char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
__extension__
extern unsigned long long int strtoull (__const char *__restrict __nptr,
     char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
extern __inline double
__attribute__ ((__nothrow__)) atof (__const char *__nptr)
{
  return strtod (__nptr, (char **) ((void *)0));
}
extern __inline int
__attribute__ ((__nothrow__)) atoi (__const char *__nptr)
{
  return (int) strtol (__nptr, (char **) ((void *)0), 10);
}
extern __inline long int
__attribute__ ((__nothrow__)) atol (__const char *__nptr)
{
  return strtol (__nptr, (char **) ((void *)0), 10);
}
__extension__ extern __inline long long int
__attribute__ ((__nothrow__)) atoll (__const char *__nptr)
{
  return strtoll (__nptr, (char **) ((void *)0), 10);
}
extern char *l64a (long int __n) __attribute__ ((__nothrow__)) ;
extern long int a64l (__const char *__s)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;
extern long int random (void) __attribute__ ((__nothrow__));
extern void srandom (unsigned int __seed) __attribute__ ((__nothrow__));
extern char *initstate (unsigned int __seed, char *__statebuf,
   size_t __statelen) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern char *setstate (char *__statebuf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
struct random_data
  {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
  };
extern int random_r (struct random_data *__restrict __buf,
       int32_t *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int srandom_r (unsigned int __seed, struct random_data *__buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern int initstate_r (unsigned int __seed, char *__restrict __statebuf,
   size_t __statelen,
   struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 4)));
extern int setstate_r (char *__restrict __statebuf,
         struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int rand (void) __attribute__ ((__nothrow__));
extern void srand (unsigned int __seed) __attribute__ ((__nothrow__));
extern int rand_r (unsigned int *__seed) __attribute__ ((__nothrow__));
extern double drand48 (void) __attribute__ ((__nothrow__));
extern double erand48 (unsigned short int __xsubi[3]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern long int lrand48 (void) __attribute__ ((__nothrow__));
extern long int nrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern long int mrand48 (void) __attribute__ ((__nothrow__));
extern long int jrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern void srand48 (long int __seedval) __attribute__ ((__nothrow__));
extern unsigned short int *seed48 (unsigned short int __seed16v[3])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern void lcong48 (unsigned short int __param[7]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
struct drand48_data
  {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    unsigned long long int __a;
  };
extern int drand48_r (struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int erand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int lrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int nrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int mrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int jrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern int seed48_r (unsigned short int __seed16v[3],
       struct drand48_data *__buffer) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int lcong48_r (unsigned short int __param[7],
        struct drand48_data *__buffer)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern void *realloc (void *__ptr, size_t __size)
     __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
extern void free (void *__ptr) __attribute__ ((__nothrow__));
extern void cfree (void *__ptr) __attribute__ ((__nothrow__));
extern void *alloca (size_t __size) __attribute__ ((__nothrow__));
extern void *valloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) ;
extern int posix_memalign (void **__memptr, size_t __alignment, size_t __size)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
extern void abort (void) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
extern int atexit (void (*__func) (void)) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern void exit (int __status) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
extern void _Exit (int __status) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));
extern char *getenv (__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
extern char *__secure_getenv (__const char *__name)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
extern int putenv (char *__string) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int setenv (__const char *__name, __const char *__value, int __replace)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern int unsetenv (__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int clearenv (void) __attribute__ ((__nothrow__));
extern char *mktemp (char *__template) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
extern int mkstemp (char *__template) __asm__ ("" "mkstemp64")
     __attribute__ ((__nonnull__ (1))) ;
extern int mkstemp64 (char *__template) __attribute__ ((__nonnull__ (1))) ;
extern int mkstemps (char *__template, int __suffixlen) __asm__ ("" "mkstemps64")
                     __attribute__ ((__nonnull__ (1))) ;
extern int mkstemps64 (char *__template, int __suffixlen)
     __attribute__ ((__nonnull__ (1))) ;
extern char *mkdtemp (char *__template) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
extern int system (__const char *__command) ;
extern char *realpath (__const char *__restrict __name,
         char *__restrict __resolved) __attribute__ ((__nothrow__)) ;
typedef int (*__compar_fn_t) (__const void *, __const void *);
extern void *bsearch (__const void *__key, __const void *__base,
        size_t __nmemb, size_t __size, __compar_fn_t __compar)
     __attribute__ ((__nonnull__ (1, 2, 5))) ;
extern void qsort (void *__base, size_t __nmemb, size_t __size,
     __compar_fn_t __compar) __attribute__ ((__nonnull__ (1, 4)));
extern int abs (int __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)) ;
extern long int labs (long int __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)) ;
__extension__ extern long long int llabs (long long int __x)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__)) ;
extern div_t div (int __numer, int __denom)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__)) ;
extern ldiv_t ldiv (long int __numer, long int __denom)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__)) ;
__extension__ extern lldiv_t lldiv (long long int __numer,
        long long int __denom)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__)) ;
extern char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4))) ;
extern char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4))) ;
extern char *gcvt (double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3))) ;
extern char *qecvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qfcvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qgcvt (long double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3))) ;
extern int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int qecvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int qfcvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int mblen (__const char *__s, size_t __n) __attribute__ ((__nothrow__)) ;
extern int mbtowc (wchar_t *__restrict __pwc,
     __const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__)) ;
extern int wctomb (char *__s, wchar_t __wchar) __attribute__ ((__nothrow__)) ;
extern size_t mbstowcs (wchar_t *__restrict __pwcs,
   __const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__));
extern size_t wcstombs (char *__restrict __s,
   __const wchar_t *__restrict __pwcs, size_t __n)
     __attribute__ ((__nothrow__));
extern int rpmatch (__const char *__response) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) ;
extern int getsubopt (char **__restrict __optionp,
        char *__const *__restrict __tokens,
        char **__restrict __valuep)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2, 3))) ;
extern int getloadavg (double __loadavg[], int __nelem)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
typedef struct Link
{
 struct Link *next,*prev;
} Link;
typedef struct LinkData
{
 struct LinkData *next, *prev;
 void *data;
} LinkData;
typedef struct ListBase
{
 void *first, *last;
} ListBase;
void BLI_insertlink(struct ListBase *listbase, void *vprevlink, void *vnewlink);
void *BLI_findlink(const struct ListBase *listbase, int number);
int BLI_findindex(const struct ListBase *listbase, void *vlink);
int BLI_findstringindex(const struct ListBase *listbase, const char *id, const int offset);
void *BLI_findstring(const struct ListBase *listbase, const char *id, const int offset);
void *BLI_findstring_ptr(const struct ListBase *listbase, const char *id, const int offset);
void *BLI_rfindstring(const struct ListBase *listbase, const char *id, const int offset);
void *BLI_rfindstring_ptr(const struct ListBase *listbase, const char *id, const int offset);
void BLI_freelistN(struct ListBase *listbase);
void BLI_addtail(struct ListBase *listbase, void *vlink);
void BLI_remlink(struct ListBase *listbase, void *vlink);
int BLI_remlink_safe(struct ListBase *listbase, void *vlink);
void BLI_addhead(struct ListBase *listbase, void *vlink);
void BLI_insertlinkbefore(struct ListBase *listbase, void *vnextlink, void *vnewlink);
void BLI_insertlinkafter(struct ListBase *listbase, void *vprevlink, void *vnewlink);
void BLI_sortlist(struct ListBase *listbase, int (*cmp)(void *, void *));
void BLI_freelist(struct ListBase *listbase);
int BLI_countlist(const struct ListBase *listbase);
void BLI_freelinkN(struct ListBase *listbase, void *vlink);
void BLI_movelisttolist(struct ListBase *dst, struct ListBase *src);
void BLI_duplicatelist(struct ListBase *dst, const struct ListBase *src);
struct LinkData *BLI_genericNodeN(void *data);
char *BLI_strdup(const char *str);
char *BLI_strdupn(const char *str, const size_t len);
char *BLI_strdupcat(const char *str1, const char *str2);
char *BLI_strncpy(char *dst, const char *src, const size_t maxncpy);
char *BLI_getQuotedStr(const char *str, const char *prefix);
char *BLI_replacestr(char *str, const char *oldText, const char *newText);
size_t BLI_snprintf(char *buffer, size_t len, const char *format, ...)
__attribute__ ((format (printf, 3, 4)))
;
char *BLI_sprintfN(const char *format, ...)
__attribute__ ((format (printf, 1, 2)))
;
size_t BLI_strescape(char *dst, const char *src, const size_t maxlen);
int BLI_strcaseeq(const char *a, const char *b);
char *BLI_strcasestr(const char *s, const char *find);
int BLI_strcasecmp(const char *s1, const char *s2);
int BLI_strncasecmp(const char *s1, const char *s2, size_t len);
int BLI_natstrcmp(const char *s1, const char *s2);
size_t BLI_strnlen(const char *str, size_t maxlen);
void BLI_timestr(double _time, char *str);
void BLI_ascii_strtolower(char *str, int len);
void BLI_ascii_strtoupper(char *str, int len);
char *BLI_strncpy_utf8(char *dst, const char *src, size_t maxncpy);
char *BLI_strncat_utf8(char *dst, const char *src, size_t maxncpy);
int BLI_utf8_invalid_byte(const char *str, int length);
int BLI_utf8_invalid_strip(char *str, int length);
int BLI_str_utf8_size(const char *p);
unsigned int BLI_str_utf8_as_unicode(const char *p);
unsigned int BLI_str_utf8_as_unicode_and_size(const char *p, size_t *index);
unsigned int BLI_str_utf8_as_unicode_step(const char *p, size_t *index);
size_t BLI_str_utf8_from_unicode(unsigned int c, char *outbuf);
char *BLI_str_find_prev_char_utf8(const char *str, const char *p);
char *BLI_str_find_next_char_utf8(const char *p, const char *end);
char *BLI_str_prev_char_utf8(const char *p);
size_t BLI_wstrlen_utf8(const wchar_t *src);
size_t BLI_strlen_utf8(const char *strc);
size_t BLI_strncpy_wchar_as_utf8(char *dst, const wchar_t *src, const size_t maxcpy);
size_t BLI_strncpy_wchar_from_utf8(wchar_t *dst, const char *src, const size_t maxcpy);
struct ListBase;
struct direntry;
const char *BLI_getDefaultDocumentFolder(void);
char *BLI_get_folder(int folder_id, const char *subfolder);
char *BLI_get_folder_create(int folder_id, const char *subfolder);
char *BLI_get_user_folder_notest(int folder_id, const char *subfolder);
char *BLI_get_folder_version(const int id, const int ver, const int do_check);
void BLI_setenv(const char *env, const char *val);
void BLI_setenv_if_new(const char *env, const char* val);
void BLI_make_file_string(const char *relabase, char *string, const char *dir, const char *file);
void BLI_make_exist(char *dir);
void BLI_make_existing_file(const char *name);
void BLI_split_dirfile(const char *string, char *dir, char *file, const size_t dirlen, const size_t filelen);
void BLI_split_dir_part(const char *string, char *dir, const size_t dirlen);
void BLI_split_file_part(const char *string, char *file, const size_t filelen);
void BLI_join_dirfile(char *string, const size_t maxlen, const char *dir, const char *file);
char *BLI_path_basename(char *path);
int BKE_rebase_path(char *abs, size_t abs_len, char *rel, size_t rel_len, const char *base_dir, const char *src_dir, const char *dest_dir);
char *BLI_last_slash(const char *string);
int BLI_add_slash(char *string);
void BLI_del_slash(char *string);
char *BLI_first_slash(char *string);
void BLI_getlastdir(const char* dir, char *last, const size_t maxlen);
int BLI_testextensie(const char *str, const char *ext);
int BLI_testextensie_array(const char *str, const char **ext_array);
int BLI_testextensie_glob(const char *str, const char *ext_fnmatch);
int BLI_replace_extension(char *path, size_t maxlen, const char *ext);
int BLI_ensure_extension(char *path, size_t maxlen, const char *ext);
void BLI_uniquename(struct ListBase *list, void *vlink, const char defname[], char delim, short name_offs, short len);
int BLI_uniquename_cb(int (*unique_check)(void *, const char *), void *arg, const char defname[], char delim, char *name, short name_len);
void BLI_newname(char * name, int add);
int BLI_stringdec(const char *string, char *head, char *start, unsigned short *numlen);
void BLI_stringenc(char *string, const char *head, const char *tail, unsigned short numlen, int pic);
int BLI_split_name_num(char *left, int *nr, const char *name, const char delim);
void BLI_splitdirstring(char *di,char *fi);
void BLI_clean(char *path);
void BLI_cleanup_file(const char *relabase, char *dir);
void BLI_cleanup_dir(const char *relabase, char *dir);
void BLI_cleanup_path(const char *relabase, char *dir);
int BLI_parent_dir(char *path);
int BLI_has_parent(char *path);
int BLI_path_abs(char *path, const char *basepath);
int BLI_path_frame(char *path, int frame, int digits);
int BLI_path_frame_range(char *path, int sta, int end, int digits);
int BLI_path_cwd(char *path);
void BLI_path_rel(char *file, const char *relfile);
void BLI_char_switch(char *string, char from, char to);
void BLI_init_program_path(const char *argv0);
void BLI_init_temporary_dir(char *userdir);
const char *BLI_program_path(void);
const char *BLI_program_dir(void);
const char *BLI_temporary_dir(void);
void BLI_system_temporary_dir(char *dir);
struct stat
  {
    __dev_t st_dev;
    unsigned short int __pad1;
    __ino_t __st_ino;
    __mode_t st_mode;
    __nlink_t st_nlink;
    __uid_t st_uid;
    __gid_t st_gid;
    __dev_t st_rdev;
    unsigned short int __pad2;
    __off64_t st_size;
    __blksize_t st_blksize;
    __blkcnt64_t st_blocks;
    struct timespec st_atim;
    struct timespec st_mtim;
    struct timespec st_ctim;
    __ino64_t st_ino;
  };
struct stat64
  {
    __dev_t st_dev;
    unsigned int __pad1;
    __ino_t __st_ino;
    __mode_t st_mode;
    __nlink_t st_nlink;
    __uid_t st_uid;
    __gid_t st_gid;
    __dev_t st_rdev;
    unsigned int __pad2;
    __off64_t st_size;
    __blksize_t st_blksize;
    __blkcnt64_t st_blocks;
    struct timespec st_atim;
    struct timespec st_mtim;
    struct timespec st_ctim;
    __ino64_t st_ino;
  };
extern int stat (__const char *__restrict __file, struct stat *__restrict __buf) __asm__ ("" "stat64") __attribute__ ((__nothrow__))
     __attribute__ ((__nonnull__ (1, 2)));
extern int fstat (int __fd, struct stat *__buf) __asm__ ("" "fstat64") __attribute__ ((__nothrow__))
     __attribute__ ((__nonnull__ (2)));
extern int stat64 (__const char *__restrict __file,
     struct stat64 *__restrict __buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int fstat64 (int __fd, struct stat64 *__buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern int fstatat (int __fd, __const char *__restrict __file, struct stat *__restrict __buf, int __flag) __asm__ ("" "fstatat64") __attribute__ ((__nothrow__))
                 __attribute__ ((__nonnull__ (2, 3)));
extern int fstatat64 (int __fd, __const char *__restrict __file,
        struct stat64 *__restrict __buf, int __flag)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));
extern int lstat (__const char *__restrict __file, struct stat *__restrict __buf) __asm__ ("" "lstat64") __attribute__ ((__nothrow__))
     __attribute__ ((__nonnull__ (1, 2)));
extern int lstat64 (__const char *__restrict __file,
      struct stat64 *__restrict __buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int chmod (__const char *__file, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int lchmod (__const char *__file, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int fchmod (int __fd, __mode_t __mode) __attribute__ ((__nothrow__));
extern int fchmodat (int __fd, __const char *__file, __mode_t __mode,
       int __flag)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2))) ;
extern __mode_t umask (__mode_t __mask) __attribute__ ((__nothrow__));
extern int mkdir (__const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int mkdirat (int __fd, __const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern int mknod (__const char *__path, __mode_t __mode, __dev_t __dev)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int mknodat (int __fd, __const char *__path, __mode_t __mode,
      __dev_t __dev) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern int mkfifo (__const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int mkfifoat (int __fd, __const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern int utimensat (int __fd, __const char *__path,
        __const struct timespec __times[2],
        int __flags)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern int futimens (int __fd, __const struct timespec __times[2]) __attribute__ ((__nothrow__));
extern int __fxstat (int __ver, int __fildes, struct stat *__stat_buf) __asm__ ("" "__fxstat64") __attribute__ ((__nothrow__))
     __attribute__ ((__nonnull__ (3)));
extern int __xstat (int __ver, __const char *__filename, struct stat *__stat_buf) __asm__ ("" "__xstat64") __attribute__ ((__nothrow__))
     __attribute__ ((__nonnull__ (2, 3)));
extern int __lxstat (int __ver, __const char *__filename, struct stat *__stat_buf) __asm__ ("" "__lxstat64") __attribute__ ((__nothrow__))
     __attribute__ ((__nonnull__ (2, 3)));
extern int __fxstatat (int __ver, int __fildes, __const char *__filename, struct stat *__stat_buf, int __flag) __asm__ ("" "__fxstatat64") __attribute__ ((__nothrow__))
                    __attribute__ ((__nonnull__ (3, 4)));
extern int __fxstat64 (int __ver, int __fildes, struct stat64 *__stat_buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3)));
extern int __xstat64 (int __ver, __const char *__filename,
        struct stat64 *__stat_buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));
extern int __lxstat64 (int __ver, __const char *__filename,
         struct stat64 *__stat_buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));
extern int __fxstatat64 (int __ver, int __fildes, __const char *__filename,
    struct stat64 *__stat_buf, int __flag)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4)));
extern int __xmknod (int __ver, __const char *__path, __mode_t __mode,
       __dev_t *__dev) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 4)));
extern int __xmknodat (int __ver, int __fd, __const char *__path,
         __mode_t __mode, __dev_t *__dev)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 5)));
extern __inline int
__attribute__ ((__nothrow__)) stat (__const char *__path, struct stat *__statbuf)
{
  return __xstat (3, __path, __statbuf);
}
extern __inline int
__attribute__ ((__nothrow__)) lstat (__const char *__path, struct stat *__statbuf)
{
  return __lxstat (3, __path, __statbuf);
}
extern __inline int
__attribute__ ((__nothrow__)) fstat (int __fd, struct stat *__statbuf)
{
  return __fxstat (3, __fd, __statbuf);
}
extern __inline int
__attribute__ ((__nothrow__)) fstatat (int __fd, __const char *__filename, struct stat *__statbuf, int __flag)
{
  return __fxstatat (3, __fd, __filename, __statbuf, __flag);
}
extern __inline int
__attribute__ ((__nothrow__)) mknod (__const char *__path, __mode_t __mode, __dev_t __dev)
{
  return __xmknod (1, __path, __mode, &__dev);
}
extern __inline int
__attribute__ ((__nothrow__)) mknodat (int __fd, __const char *__path, __mode_t __mode, __dev_t __dev)
{
  return __xmknodat (1, __fd, __path, __mode, &__dev);
}
extern __inline int
__attribute__ ((__nothrow__)) stat64 (__const char *__path, struct stat64 *__statbuf)
{
  return __xstat64 (3, __path, __statbuf);
}
extern __inline int
__attribute__ ((__nothrow__)) lstat64 (__const char *__path, struct stat64 *__statbuf)
{
  return __lxstat64 (3, __path, __statbuf);
}
extern __inline int
__attribute__ ((__nothrow__)) fstat64 (int __fd, struct stat64 *__statbuf)
{
  return __fxstat64 (3, __fd, __statbuf);
}
extern __inline int
__attribute__ ((__nothrow__)) fstatat64 (int __fd, __const char *__filename, struct stat64 *__statbuf, int __flag)
{
  return __fxstatat64 (3, __fd, __filename, __statbuf, __flag);
}
struct ImBuf;
struct direntry{
 char *string;
 mode_t type;
 char *relname;
 char *path;
 struct stat s;
 unsigned int flags;
 char size[16];
 char mode1[4];
 char mode2[4];
 char mode3[4];
 char owner[16];
 char time[8];
 char date[16];
 char extra[16];
 void *poin;
 int nr;
 struct ImBuf *image;
 unsigned int selflag;
};
struct dirlink
{
 struct dirlink *next,*prev;
 char *name;
};
int BLI_exists(const char *path);
int BLI_copy(const char *path, const char *to);
int BLI_rename(const char *from, const char *to);
int BLI_delete(const char *path, int dir, int recursive);
int BLI_move(const char *path, const char *to);
int BLI_create_symlink(const char *path, const char *to);
struct direntry;
int BLI_is_dir(const char *path);
int BLI_is_file(const char *path);
void BLI_dir_create_recursive(const char *dir);
double BLI_dir_free_space(const char *dir);
char *BLI_current_working_dir(char *dir, const int maxlen);
unsigned int BLI_dir_contents(const char *dir, struct direntry **filelist);
int BLI_file_is_writable(const char *file);
int BLI_file_touch(const char *file);
int BLI_file_gzip(const char *from, const char *to);
char *BLI_file_ungzip_to_mem(const char *from_file, int *size_r);
size_t BLI_file_descriptor_size(int file);
size_t BLI_file_size(const char *file);
int BLI_file_older(const char *file1, const char *file2);
struct LinkNode *BLI_file_read_as_lines(const char *file);
void BLI_file_free_lines(struct LinkNode *lines);
struct rctf;
struct rcti;
int BLI_rcti_is_empty(struct rcti *rect);
int BLI_rctf_is_empty(struct rctf *rect);
void BLI_init_rctf(struct rctf *rect, float xmin, float xmax, float ymin, float ymax);
void BLI_init_rcti(struct rcti *rect, int xmin, int xmax, int ymin, int ymax);
void BLI_translate_rctf(struct rctf *rect, float x, float y);
void BLI_translate_rcti(struct rcti *rect, int x, int y);
void BLI_resize_rcti(struct rcti *rect, int x, int y);
void BLI_resize_rctf(struct rctf *rect, float x, float y);
int BLI_in_rcti(struct rcti *rect, int x, int y);
int BLI_in_rctf(struct rctf *rect, float x, float y);
int BLI_isect_rctf(struct rctf *src1, struct rctf *src2, struct rctf *dest);
int BLI_isect_rcti(struct rcti *src1, struct rcti *src2, struct rcti *dest);
void BLI_union_rctf(struct rctf *rcta, struct rctf *rctb);
void BLI_union_rcti(struct rcti *rcti1, struct rcti *rcti2);
void BLI_copy_rcti_rctf(struct rcti *tar, const struct rctf *src);
void print_rctf(const char *str, struct rctf *rect);
void print_rcti(const char *str, struct rcti *rect);
float BLI_hnoise(float noisesize, float x, float y, float z);
float BLI_hnoisep(float noisesize, float x, float y, float z);
float BLI_turbulence(float noisesize, float x, float y, float z, int nr);
float BLI_turbulence1(float noisesize, float x, float y, float z, int nr);
float BLI_gNoise(float noisesize, float x, float y, float z, int hard, int noisebasis);
float BLI_gTurbulence(float noisesize, float x, float y, float z, int oct, int hard, int noisebasis);
float mg_fBm(float x, float y, float z, float H, float lacunarity, float octaves, int noisebasis);
float mg_MultiFractal(float x, float y, float z, float H, float lacunarity, float octaves, int noisebasis);
float mg_VLNoise(float x, float y, float z, float distortion, int nbas1, int nbas2);
float mg_HeteroTerrain(float x, float y, float z, float H, float lacunarity, float octaves, float offset, int noisebasis);
float mg_HybridMultiFractal(float x, float y, float z, float H, float lacunarity, float octaves, float offset, float gain, int noisebasis);
float mg_RidgedMultiFractal(float x, float y, float z, float H, float lacunarity, float octaves, float offset, float gain, int noisebasis);
void voronoi(float x, float y, float z, float* da, float* pa, float me, int dtype);
float cellNoise(float x, float y, float z);
void cellNoiseV(float x, float y, float z, float *ca);
typedef struct CustomDataLayer {
 int type;
 int offset;
 int flag;
 int active;
 int active_rnd;
 int active_clone;
 int active_mask;
 char pad[4];
 char name[64];
 void *data;
} CustomDataLayer;
typedef struct CustomDataExternal {
 char filename[1024];
} CustomDataExternal;
typedef struct CustomData {
 CustomDataLayer *layers;
 int typemap[32];
 int totlayer, maxlayer;
 int totsize, pad;
 void *pool;
 CustomDataExternal *external;
} CustomData;
struct Library;
struct FileData;
struct ID;
typedef struct IDPropertyData {
 void *pointer;
 ListBase group;
 int val, val2;
} IDPropertyData;
typedef struct IDProperty {
 struct IDProperty *next, *prev;
 char type, subtype;
 short flag;
 char name[64];
 int saved;
 IDPropertyData data;
 int len;
 int totallen;
} IDProperty;
typedef struct ID {
 void *next, *prev;
 struct ID *newid;
 struct Library *lib;
 char name[66];
 short pad, us;
 short flag;
 int icon_id, pad2;
 IDProperty *properties;
} ID;
typedef struct Library {
 ID id;
 ID *idblock;
 struct FileData *filedata;
 char name[1024];
 char filepath[1024];
 int tot, pad;
 struct Library *parent;
} Library;
enum eIconSizes {
 ICON_SIZE_ICON,
 ICON_SIZE_PREVIEW
};
typedef struct PreviewImage {
 unsigned int w[2];
 unsigned int h[2];
 short changed[2];
 short changed_timestamp[2];
 unsigned int * rect[2];
} PreviewImage;
struct DerivedMesh;
struct Ipo;
struct Key;
struct Material;
struct MVert;
struct MEdge;
struct MFace;
struct MCol;
struct MSticky;
struct Mesh;
struct OcInfo;
struct MPoly;
struct MTexPoly;
struct MLoop;
struct MLoopUV;
struct MLoopCol;
struct Multires;
struct EditMesh;
struct AnimData;
typedef struct Mesh {
 ID id;
 struct AnimData *adt;
 struct BoundBox *bb;
 struct Ipo *ipo __attribute__ ((deprecated));
 struct Key *key;
 struct Material **mat;
 struct MPoly *mpoly;
 struct MTexPoly *mtpoly;
 struct MLoop *mloop;
 struct MLoopUV *mloopuv;
 struct MLoopCol *mloopcol;
 struct MFace *mface;
 struct MTFace *mtface;
 struct TFace *tface;
 struct MVert *mvert;
 struct MEdge *medge;
 struct MDeformVert *dvert;
 struct MCol *mcol;
 struct MSticky *msticky;
 struct Mesh *texcomesh;
 struct MSelect *mselect;
 struct EditMesh *edit_mesh;
 struct CustomData vdata, edata, fdata;
 struct CustomData pdata, ldata;
 int totvert, totedge, totface, totselect;
 int totpoly, totloop;
 int act_face;
 float loc[3];
 float size[3];
 float rot[3];
 short texflag, drawflag;
 short smoothresh, flag;
 short subdiv __attribute__ ((deprecated)), subdivr __attribute__ ((deprecated));
 char subsurftype __attribute__ ((deprecated));
 char editflag;
 short totcol;
 struct Multires *mr __attribute__ ((deprecated));
} Mesh;
typedef struct TFace {
 void *tpage;
 float uv[4][2];
 unsigned int col[4];
 char flag, transp;
 short mode, tile, unwrap;
} TFace;
struct DerivedMesh;
typedef struct EditVert
{
 struct EditVert *next, *prev;
 union {
  struct EditVert *v;
  struct EditEdge *e;
  struct EditFace *f;
  void *p;
  intptr_t l;
  float fp;
  int t;
 } tmp;
 float no[3];
 float co[3];
 short xs, ys;
 unsigned char f, h, f1, f2;
 float bweight;
 short fast;
 int hash;
 int keyindex;
 void *data;
} EditVert;
struct EditEdge;
typedef struct HashEdge {
 struct EditEdge *eed;
 struct HashEdge *next;
} HashEdge;
typedef struct EditEdge
{
 struct EditEdge *next, *prev;
 struct EditVert *v1, *v2;
 union {
  struct EditVert *v;
  struct EditEdge *e;
  struct EditFace *f;
  void *p;
  intptr_t l;
  float fp;
 } tmp;
 short f1, f2;
 unsigned char f, h, dir, seam, sharp;
 float crease;
 float bweight;
 short fast;
 short fgoni;
 HashEdge hash;
 void *data;
} EditEdge;
typedef struct EditFace
{
 struct EditFace *next, *prev;
 struct EditVert *v1, *v2, *v3, *v4;
 struct EditEdge *e1, *e2, *e3, *e4;
 union {
  struct EditVert *v;
  struct EditEdge *e;
  struct EditFace *f;
  void *p;
  intptr_t l;
  float fp;
 } tmp;
 float n[3], cent[3];
 unsigned char flag;
 unsigned char f, f1, h;
 unsigned char fast;
 unsigned char fgonf;
 short mat_nr;
 void *data;
} EditFace;
typedef struct EditSelection
{
 struct EditSelection *next, *prev;
 short type;
 void *data;
} EditSelection;
typedef struct EditMesh
{
 ListBase verts, edges, faces;
 ListBase selected;
 HashEdge *hashedgetab;
 EditVert *allverts, *curvert;
 EditEdge *alledges, *curedge;
 EditFace *allfaces, *curface;
 EditFace *act_face;
 short selectmode;
 short mat_nr;
 int totvert, totedge, totface, totvertsel, totedgesel, totfacesel;
 int shapenr;
 struct DerivedMesh *derivedCage, *derivedFinal;
 int lastDataMask;
 CustomData vdata, edata, fdata;
} EditMesh;
static inline float sqrt3f(float f)
{
 if(f==0.0f) return 0.0f;
 if(f<0) return (float)(-exp(log(-f)/3));
 else return (float)(exp(log(f)/3));
}
static inline double sqrt3d(double d)
{
 if(d==0.0) return 0;
 if(d<0) return -exp(log(-d)/3);
 else return exp(log(d)/3);
}
static inline float saacos(float fac)
{
 if(fac<= -1.0f) return (float)3.14159265358979323846;
 else if(fac>=1.0f) return 0.0;
 else return (float)acos(fac);
}
static inline float saasin(float fac)
{
 if(fac<= -1.0f) return (float)-3.14159265358979323846/2.0f;
 else if(fac>=1.0f) return (float)3.14159265358979323846/2.0f;
 else return (float)asin(fac);
}
static inline float sasqrt(float fac)
{
 if(fac<=0.0f) return 0.0f;
 return (float)sqrt(fac);
}
static inline float saacosf(float fac)
{
 if(fac<= -1.0f) return (float)3.14159265358979323846;
 else if(fac>=1.0f) return 0.0f;
 else return (float)((float)acos(fac));
}
static inline float saasinf(float fac)
{
 if(fac<= -1.0f) return (float)-3.14159265358979323846/2.0f;
 else if(fac>=1.0f) return (float)3.14159265358979323846/2.0f;
 else return (float)((float)asin(fac));
}
static inline float sasqrtf(float fac)
{
 if(fac<=0.0f) return 0.0f;
 return (float)((float)sqrt(fac));
}
static inline float interpf(float target, float origin, float fac)
{
 return (fac*target) + (1.0f-fac)*origin;
}
static inline float shell_angle_to_dist(const float angle)
{
 return (angle < 1.e-8f) ? 1.0f : ((float)fabs(1.0f / ((float)cos(angle))));
}
static inline float power_of_2(float val)
{
 return (float)pow(2.0, ceil(log((double)val) / 0.69314718055994530942));
}
static inline int is_power_of_2_i(int n)
{
 return (n & (n - 1)) == 0;
}
static inline int power_of_2_max_i(int n)
{
 if (is_power_of_2_i(n))
  return n;
 while(!is_power_of_2_i(n))
  n = n & (n - 1);
 return n * 2;
}
static inline int power_of_2_min_i(int n)
{
 while (!is_power_of_2_i(n))
  n = n & (n - 1);
 return n;
}
static inline float minf(float a, float b)
{
 return (a < b)? a: b;
}
static inline float maxf(float a, float b)
{
 return (a > b)? a: b;
}
static inline float signf(float f)
{
 return (f < 0.f)? -1.f: 1.f;
}
static inline float sqrt3f(float f);
static inline double sqrt3d(double d);
static inline float saacosf(float f);
static inline float saasinf(float f);
static inline float sasqrtf(float f);
static inline float saacos(float fac);
static inline float saasin(float fac);
static inline float sasqrt(float fac);
static inline float interpf(float a, float b, float t);
static inline float minf(float a, float b);
static inline float maxf(float a, float b);
static inline float signf(float f);
static inline float power_of_2(float f);
static inline int is_power_of_2_i(int n);
static inline int power_of_2_max_i(int n);
static inline int power_of_2_min_i(int n);
static inline float shell_angle_to_dist(float angle);
double double_round(double x, int ndigits);
void hsv_to_rgb(float h, float s, float v, float *r, float *g, float *b);
void hex_to_rgb(char *hexcol, float *r, float *g, float *b);
void yuv_to_rgb(float y, float u, float v, float *lr, float *lg, float *lb);
void ycc_to_rgb(float y, float cb, float cr, float *lr, float *lg, float *lb, int colorspace);
void xyz_to_rgb(float x, float y, float z, float *r, float *g, float *b, int colorspace);
void cpack_to_rgb(unsigned int col, float *r, float *g, float *b);
void rgb_to_yuv(float r, float g, float b, float *ly, float *lu, float *lv);
void rgb_to_ycc(float r, float g, float b, float *ly, float *lcb, float *lcr, int colorspace);
void rgb_to_hsv(float r, float g, float b, float *lh, float *ls, float *lv);
void rgb_to_hsv_compat(float r, float g, float b, float *lh, float *ls, float *lv);
unsigned int rgb_to_cpack(float r, float g, float b);
unsigned int hsv_to_cpack(float h, float s, float v);
float rgb_to_grayscale(const float rgb[3]);
unsigned char rgb_to_grayscale_byte(const unsigned char rgb[3]);
float rgb_to_luma(const float rgb[3]);
unsigned char rgb_to_luma_byte(const unsigned char rgb[3]);
void gamma_correct(float *c, float gamma);
float rec709_to_linearrgb(float c);
float linearrgb_to_rec709(float c);
float srgb_to_linearrgb(float c);
float linearrgb_to_srgb(float c);
static inline void srgb_to_linearrgb_v3_v3(float linear[3], const float srgb[3]);
static inline void linearrgb_to_srgb_v3_v3(float srgb[3], const float linear[3]);
static inline void srgb_to_linearrgb_v4(float linear[4], const float srgb[4]);
static inline void linearrgb_to_srgb_v4(float srgb[4], const float linear[4]);
static inline void srgb_to_linearrgb_predivide_v4(float linear[4], const float srgb[4]);
static inline void linearrgb_to_srgb_predivide_v4(float srgb[4], const float linear[4]);
static inline void linearrgb_to_srgb_uchar3(unsigned char srgb[3], const float linear[3]);
static inline void linearrgb_to_srgb_uchar4(unsigned char srgb[4], const float linear[4]);
void BLI_init_srgb_conversion(void);
int constrain_rgb(float *r, float *g, float *b);
void minmax_rgb(short c[3]);
void rgb_float_set_hue_float_offset(float * rgb, float hue_offset);
void rgb_byte_set_hue_float_offset(unsigned char * rgb, float hue_offset);
void rgb_uchar_to_float(float col_r[3], const unsigned char col_ub[3]);
void rgba_uchar_to_float(float col_r[4], const unsigned char col_ub[4]);
void rgb_float_to_uchar(unsigned char col_r[3], const float col_f[3]);
void rgba_float_to_uchar(unsigned char col_r[4], const float col_f[4]);
void lift_gamma_gain_to_asc_cdl(float *lift, float *gamma, float *gain, float *offset, float *slope, float *power);
static inline void srgb_to_linearrgb_v3_v3(float linear[3], const float srgb[3])
{
 linear[0] = srgb_to_linearrgb(srgb[0]);
 linear[1] = srgb_to_linearrgb(srgb[1]);
 linear[2] = srgb_to_linearrgb(srgb[2]);
}
static inline void linearrgb_to_srgb_v3_v3(float srgb[3], const float linear[3])
{
 srgb[0] = linearrgb_to_srgb(linear[0]);
 srgb[1] = linearrgb_to_srgb(linear[1]);
 srgb[2] = linearrgb_to_srgb(linear[2]);
}
static inline void srgb_to_linearrgb_v4(float linear[4], const float srgb[4])
{
 srgb_to_linearrgb_v3_v3(linear, srgb);
 linear[3] = srgb[3];
}
static inline void linearrgb_to_srgb_v4(float srgb[4], const float linear[4])
{
 linearrgb_to_srgb_v3_v3(srgb, linear);
 srgb[3] = linear[3];
}
static inline void linearrgb_to_srgb_uchar3(unsigned char srgb[3], const float linear[3])
{
 float srgb_f[3];
 linearrgb_to_srgb_v3_v3(srgb_f, linear);
 { (srgb)[0]= (((srgb_f[0]))<=0.0f)? 0 : ((((srgb_f[0]))>(1.0f-0.5f/255.0f))? 255 : (char)((255.0f*((srgb_f[0])))+0.5f)); (srgb)[1]= (((srgb_f[1]))<=0.0f)? 0 : ((((srgb_f[1]))>(1.0f-0.5f/255.0f))? 255 : (char)((255.0f*((srgb_f[1])))+0.5f)); (srgb)[2]= (((srgb_f[2]))<=0.0f)? 0 : ((((srgb_f[2]))>(1.0f-0.5f/255.0f))? 255 : (char)((255.0f*((srgb_f[2])))+0.5f)); };
}
static inline void linearrgb_to_srgb_uchar4(unsigned char srgb[4], const float linear[4])
{
 float srgb_f[4];
 linearrgb_to_srgb_v4(srgb_f, linear);
 { (srgb)[0]= (((srgb_f[0]))<=0.0f)? 0 : ((((srgb_f[0]))>(1.0f-0.5f/255.0f))? 255 : (char)((255.0f*((srgb_f[0])))+0.5f)); (srgb)[1]= (((srgb_f[1]))<=0.0f)? 0 : ((((srgb_f[1]))>(1.0f-0.5f/255.0f))? 255 : (char)((255.0f*((srgb_f[1])))+0.5f)); (srgb)[2]= (((srgb_f[2]))<=0.0f)? 0 : ((((srgb_f[2]))>(1.0f-0.5f/255.0f))? 255 : (char)((255.0f*((srgb_f[2])))+0.5f)); (srgb)[3]= (((srgb_f[3]))<=0.0f)? 0 : ((((srgb_f[3]))>(1.0f-0.5f/255.0f))? 255 : (char)((255.0f*((srgb_f[3])))+0.5f)); };
}
static inline void srgb_to_linearrgb_predivide_v4(float linear[4], const float srgb[4])
{
 float alpha, inv_alpha;
 if(srgb[3] == 1.0f || srgb[3] == 0.0f) {
  alpha = 1.0f;
  inv_alpha = 1.0f;
 }
 else {
  alpha = srgb[3];
  inv_alpha = 1.0f/alpha;
 }
 linear[0] = srgb_to_linearrgb(srgb[0] * inv_alpha) * alpha;
 linear[1] = srgb_to_linearrgb(srgb[1] * inv_alpha) * alpha;
 linear[2] = srgb_to_linearrgb(srgb[2] * inv_alpha) * alpha;
 linear[3] = srgb[3];
}
static inline void linearrgb_to_srgb_predivide_v4(float srgb[4], const float linear[4])
{
 float alpha, inv_alpha;
 if(linear[3] == 1.0f || linear[3] == 0.0f) {
  alpha = 1.0f;
  inv_alpha = 1.0f;
 }
 else {
  alpha = linear[3];
  inv_alpha = 1.0f/alpha;
 }
 srgb[0] = linearrgb_to_srgb(linear[0] * inv_alpha) * alpha;
 srgb[1] = linearrgb_to_srgb(linear[1] * inv_alpha) * alpha;
 srgb[2] = linearrgb_to_srgb(linear[2] * inv_alpha) * alpha;
 srgb[3] = linear[3];
}
extern float BLI_color_from_srgb_table[256];
extern unsigned short BLI_color_to_srgb_table[0x10000];
static inline unsigned short to_srgb_table_lookup(const float f)
{
 union {
  float f;
  unsigned short us[2];
 } tmp;
 tmp.f = f;
 return BLI_color_to_srgb_table[tmp.us[1]];
}
static inline void linearrgb_to_srgb_ushort4(unsigned short srgb[4], const float linear[4])
{
 srgb[0] = to_srgb_table_lookup(linear[0]);
 srgb[1] = to_srgb_table_lookup(linear[1]);
 srgb[2] = to_srgb_table_lookup(linear[2]);
 srgb[3] = ((linear[3] >= 1.0f-0.5f/65535)? 65535: (linear[3] <= 0.0f)? 0: (unsigned short)(linear[3]*65535.0f + 0.5f));
}
static inline void linearrgb_to_srgb_ushort4_predivide(unsigned short srgb[4], const float linear[4])
{
 float alpha, inv_alpha, t;
 int i;
 if(linear[3] == 1.0f || linear[3] == 0.0f) {
  linearrgb_to_srgb_ushort4(srgb, linear);
  return;
 }
 alpha = linear[3];
 inv_alpha = 1.0f/alpha;
 for(i=0; i<3; ++i) {
  t = linear[i] * inv_alpha;
  srgb[i] = (t < 1.0f)? to_srgb_table_lookup(t) * alpha : ((linearrgb_to_srgb(t) * alpha >= 1.0f-0.5f/65535)? 65535: (linearrgb_to_srgb(t) * alpha <= 0.0f)? 0: (unsigned short)(linearrgb_to_srgb(t) * alpha*65535.0f + 0.5f));
 }
 srgb[3] = ((linear[3] >= 1.0f-0.5f/65535)? 65535: (linear[3] <= 0.0f)? 0: (unsigned short)(linear[3]*65535.0f + 0.5f));
}
static inline void srgb_to_linearrgb_uchar4(float linear[4], const unsigned char srgb[4])
{
 linear[0] = BLI_color_from_srgb_table[srgb[0]];
 linear[1] = BLI_color_from_srgb_table[srgb[1]];
 linear[2] = BLI_color_from_srgb_table[srgb[2]];
 linear[3] = srgb[3] * (1.0f/255.0f);
}
static inline void srgb_to_linearrgb_uchar4_predivide(float linear[4], const unsigned char srgb[4])
{
 float alpha, inv_alpha;
 int i;
 if(srgb[3] == 255 || srgb[3] == 0) {
  srgb_to_linearrgb_uchar4(linear, srgb);
  return;
 }
 alpha = srgb[3] * (1.0f/255.0f);
 inv_alpha = 1.0f/alpha;
 for(i=0; i<3; ++i)
  linear[i] = linearrgb_to_srgb(srgb[i] * inv_alpha) * alpha;
 linear[3] = alpha;
}
void zero_m3(float R[3][3]);
void zero_m4(float R[4][4]);
void unit_m3(float R[3][3]);
void unit_m4(float R[4][4]);
void copy_m3_m3(float R[3][3], float A[3][3]);
void copy_m4_m4(float R[4][4], float A[4][4]);
void copy_m3_m4(float R[3][3], float A[4][4]);
void copy_m4_m3(float R[4][4], float A[3][3]);
void swap_m3m3(float A[3][3], float B[3][3]);
void swap_m4m4(float A[4][4], float B[4][4]);
void add_m3_m3m3(float R[3][3], float A[3][3], float B[3][3]);
void add_m4_m4m4(float R[4][4], float A[4][4], float B[4][4]);
void sub_m3_m3m3(float R[3][3], float A[3][3], float B[3][3]);
void sub_m4_m4m4(float R[4][4], float A[4][4], float B[4][4]);
void mul_m3_m3m3(float R[3][3], float A[3][3], float B[3][3]);
void mul_m4_m3m4(float R[4][4], float A[3][3], float B[4][4]);
void mul_m4_m4m3(float R[4][4], float A[4][4], float B[3][3]);
void mult_m4_m4m4(float R[4][4], float A[4][4], float B[4][4]);
void mult_m3_m3m4(float R[3][3], float A[4][4], float B[3][3]);
void mul_serie_m3(float R[3][3],
 float M1[3][3], float M2[3][3], float M3[3][3], float M4[3][3],
 float M5[3][3], float M6[3][3], float M7[3][3], float M8[3][3]);
void mul_serie_m4(float R[4][4],
 float M1[4][4], float M2[4][4], float M3[4][4], float M4[4][4],
 float M5[4][4], float M6[4][4], float M7[4][4], float M8[4][4]);
void mul_m4_v3(float M[4][4], float r[3]);
void mul_v3_m4v3(float r[3], float M[4][4], const float v[3]);
void mul_mat3_m4_v3(float M[4][4], float r[3]);
void mul_m4_v4(float M[4][4], float r[4]);
void mul_v4_m4v4(float r[4], float M[4][4], float v[4]);
void mul_project_m4_v3(float M[4][4], float vec[3]);
void mul_m3_v3(float M[3][3], float r[3]);
void mul_v3_m3v3(float r[3], float M[3][3], float a[3]);
void mul_transposed_m3_v3(float M[3][3], float r[3]);
void mul_m3_v3_double(float M[3][3], double r[3]);
void mul_m3_fl(float R[3][3], float f);
void mul_m4_fl(float R[4][4], float f);
void mul_mat3_m4_fl(float R[4][4], float f);
int invert_m3(float R[3][3]);
int invert_m3_m3(float R[3][3], float A[3][3]);
int invert_m4(float R[4][4]);
int invert_m4_m4(float R[4][4], float A[4][4]);
void transpose_m3(float R[3][3]);
void transpose_m4(float R[4][4]);
void normalize_m3(float R[3][3]);
void normalize_m3_m3(float R[3][3], float A[3][3]);
void normalize_m4(float R[4][4]);
void normalize_m4_m4(float R[4][4], float A[4][4]);
void orthogonalize_m3(float R[3][3], int axis);
void orthogonalize_m4(float R[4][4], int axis);
int is_orthogonal_m3(float mat[3][3]);
int is_orthogonal_m4(float mat[4][4]);
void adjoint_m3_m3(float R[3][3], float A[3][3]);
void adjoint_m4_m4(float R[4][4], float A[4][4]);
float determinant_m2(
 float a, float b,
 float c, float d);
float determinant_m3(
 float a, float b, float c,
 float d, float e, float f,
 float g, float h, float i);
float determinant_m4(float A[4][4]);
void svd_m4(float U[4][4], float s[4], float V[4][4], float A[4][4]);
void pseudoinverse_m4_m4(float Ainv[4][4], float A[4][4], float epsilon);
void scale_m3_fl(float R[3][3], float scale);
void scale_m4_fl(float R[4][4], float scale);
float mat3_to_scale(float M[3][3]);
float mat4_to_scale(float M[4][4]);
void size_to_mat3(float R[3][3], const float size[3]);
void size_to_mat4(float R[4][4], const float size[3]);
void mat3_to_size(float r[3], float M[3][3]);
void mat4_to_size(float r[3], float M[4][4]);
void translate_m4(float mat[4][4], float tx, float ty, float tz);
void rotate_m4(float mat[4][4], const char axis, const float angle);
void mat3_to_rot_size(float rot[3][3], float size[3], float mat3[][3]);
void mat4_to_loc_rot_size(float loc[3], float rot[3][3], float size[3], float wmat[][4]);
void loc_eul_size_to_mat4(float R[4][4],
 const float loc[3], const float eul[3], const float size[3]);
void loc_eulO_size_to_mat4(float R[4][4],
 const float loc[3], const float eul[3], const float size[3], const short order);
void loc_quat_size_to_mat4(float R[4][4],
 const float loc[3], const float quat[4], const float size[3]);
void loc_axisangle_size_to_mat4(float R[4][4],
 const float loc[3], const float axis[4], const float angle, const float size[3]);
void blend_m3_m3m3(float R[3][3], float A[3][3], float B[3][3], const float t);
void blend_m4_m4m4(float R[4][4], float A[4][4], float B[4][4], const float t);
int is_negative_m3(float mat[3][3]);
int is_negative_m4(float mat[4][4]);
void print_m3(const char *str, float M[3][3]);
void print_m4(const char *str, float M[3][4]);
void unit_axis_angle(float axis[3], float *angle);
void unit_qt(float q[4]);
void copy_qt_qt(float q[4], const float a[4]);
void mul_qt_qtqt(float q[4], const float a[4], const float b[4]);
void mul_qt_v3(const float q[4], float r[3]);
void mul_qt_fl(float q[4], const float f);
void mul_fac_qt_fl(float q[4], const float f);
void sub_qt_qtqt(float q[4], const float a[4], const float b[4]);
void invert_qt(float q[4]);
void invert_qt_qt(float q1[4], const float q2[4]);
void conjugate_qt(float q[4]);
float dot_qtqt(const float a[4], const float b[4]);
float normalize_qt(float q[4]);
float normalize_qt_qt(float q1[4], const float q2[4]);
int is_zero_qt(float q[4]);
void interp_qt_qtqt(float q[4], const float a[4], const float b[4], const float t);
void add_qt_qtqt(float q[4], const float a[4], const float b[4], const float t);
void quat_to_mat3(float mat[3][3], const float q[4]);
void quat_to_mat4(float mat[4][4], const float q[4]);
void mat3_to_quat(float q[4], float mat[3][3]);
void mat4_to_quat(float q[4], float mat[4][4]);
void tri_to_quat(float q[4], const float a[3], const float b[3], const float c[3]);
void vec_to_quat(float q[4], const float vec[3], short axis, const short upflag);
void rotation_between_vecs_to_quat(float q[4], const float v1[3], const float v2[3]);
void rotation_between_quats_to_quat(float q[4], const float q1[4], const float q2[4]);
void mat3_to_quat_is_ok(float q[4], float mat[3][3]);
void print_qt(const char *str, const float q[4]);
void axis_angle_to_quat(float r[4], const float axis[3], float angle);
void axis_angle_to_mat3(float R[3][3], const float axis[3], const float angle);
void axis_angle_to_mat4(float R[4][4], const float axis[3], const float angle);
void quat_to_axis_angle(float axis[3], float *angle, const float q[4]);
void mat3_to_axis_angle(float axis[3], float *angle, float M[3][3]);
void mat4_to_axis_angle(float axis[3], float *angle, float M[4][4]);
void single_axis_angle_to_mat3(float R[3][3], const char axis, const float angle);
void vec_rot_to_quat(float quat[4], const float vec[3], const float phi);
void vec_rot_to_mat3(float mat[3][3], const float vec[3], const float phi);
void vec_rot_to_mat4(float mat[4][4], const float vec[3], const float phi);
void eul_to_quat(float quat[4], const float eul[3]);
void eul_to_mat3(float mat[3][3], const float eul[3]);
void eul_to_mat4(float mat[4][4], const float eul[3]);
void quat_to_eul(float eul[3], const float quat[4]);
void mat3_to_eul(float eul[3], float mat[3][3]);
void mat4_to_eul(float eul[3], float mat[4][4]);
void compatible_eul(float eul[3], const float old[3]);
void mat3_to_compatible_eul(float eul[3], const float old[3], float mat[3][3]);
void rotate_eul(float eul[3], const char axis, const float angle);
typedef enum eEulerRotationOrders {
 EULER_ORDER_DEFAULT = 1,
 EULER_ORDER_XYZ = 1,
 EULER_ORDER_XZY,
 EULER_ORDER_YXZ,
 EULER_ORDER_YZX,
 EULER_ORDER_ZXY,
 EULER_ORDER_ZYX
} eEulerRotationOrders;
void eulO_to_quat(float quat[4], const float eul[3], const short order);
void eulO_to_mat3(float mat[3][3], const float eul[3], const short order);
void eulO_to_mat4(float mat[4][4], const float eul[3], const short order);
void eulO_to_axis_angle(float axis[3], float *angle, const float eul[3], const short order);
void eulO_to_gimbal_axis(float gmat[3][3], const float eul[3], const short order);
void quat_to_eulO(float eul[3], const short order, const float quat[4]);
void mat3_to_eulO(float eul[3], const short order, float mat[3][3]);
void mat4_to_eulO(float eul[3], const short order, float mat[4][4]);
void axis_angle_to_eulO(float eul[3], const short order, const float axis[3], const float angle);
void mat3_to_compatible_eulO(float eul[3], float old[3], short order, float mat[3][3]);
void mat4_to_compatible_eulO(float eul[3], float old[3], short order, float mat[4][4]);
void rotate_eulO(float eul[3], short order, char axis, float angle);
typedef struct DualQuat {
 float quat[4];
 float trans[4];
 float scale[4][4];
 float scale_weight;
} DualQuat;
void copy_dq_dq(DualQuat *r, DualQuat *dq);
void normalize_dq(DualQuat *dq, float totw);
void add_weighted_dq_dq(DualQuat *r, DualQuat *dq, float weight);
void mul_v3m3_dq(float r[3], float R[3][3], DualQuat *dq);
void mat4_to_dquat(DualQuat *r, float base[4][4], float M[4][4]);
void dquat_to_mat4(float R[4][4], DualQuat *dq);
void quat_apply_track(float quat[4], short axis, short upflag);
void vec_apply_track(float vec[3], short axis);
float focallength_to_fov(float focal_length, float sensor);
float fov_to_focallength(float fov, float sensor);
float angle_wrap_rad(float angle);
float angle_wrap_deg(float angle);
static inline void zero_sh(float r[9])
{
 memset(r, 0, sizeof(float)*9);
}
static inline void copy_sh_sh(float r[9], const float a[9])
{
 memcpy(r, a, sizeof(float)*9);
}
static inline void mul_sh_fl(float r[9], const float f)
{
 int i;
 for(i=0; i<9; i++)
  r[i] *= f;
}
static inline void add_sh_shsh(float r[9], const float a[9], const float b[9])
{
 int i;
 for(i=0; i<9; i++)
  r[i]= a[i] + b[i];
}
static inline float dot_shsh(float a[9], float b[9])
{
 float r= 0.0f;
 int i;
 for(i=0; i<9; i++)
  r += a[i]*b[i];
 return r;
}
static inline float diffuse_shv3(float sh[9], const float v[3])
{
 static const float c1 = 0.429043f, c2 = 0.511664f, c3 = 0.743125f;
 static const float c4 = 0.886227f, c5 = 0.247708f;
 float x, y, z, sum;
 x= v[0];
 y= v[1];
 z= v[2];
 sum= c1*sh[8]*(x*x - y*y);
 sum += c3*sh[6]*z*z;
 sum += c4*sh[0];
 sum += -c5*sh[6];
 sum += 2.0f*c1*(sh[4]*x*y + sh[7]*x*z + sh[5]*y*z);
 sum += 2.0f*c2*(sh[3]*x + sh[1]*y + sh[2]*z);
 return sum;
}
static inline void vec_fac_to_sh(float r[9], const float v[3], const float f)
{
 float sh[9], x, y, z;
 x= v[0];
 y= v[1];
 z= v[2];
 sh[0]= 0.282095f;
 sh[1]= 0.488603f*y;
 sh[2]= 0.488603f*z;
 sh[3]= 0.488603f*x;
 sh[4]= 1.092548f*x*y;
 sh[5]= 1.092548f*y*z;
 sh[6]= 0.315392f*(3.0f*z*z - 1.0f);
 sh[7]= 1.092548f*x*z;
 sh[8]= 0.546274f*(x*x - y*y);
 mul_sh_fl(sh, f);
 copy_sh_sh(r, sh);
}
static inline float eval_shv3(float sh[9], const float v[3])
{
 float tmp[9];
 vec_fac_to_sh(tmp, v, 1.0f);
 return dot_shsh(tmp, sh);
}
static inline void madd_sh_shfl(float r[9], const float sh[3], const float f)
{
 float tmp[9];
 copy_sh_sh(tmp, sh);
 mul_sh_fl(tmp, f);
 add_sh_shsh(r, r, tmp);
}
void cent_tri_v3(float r[3], const float a[3], const float b[3], const float c[3]);
void cent_quad_v3(float r[3], const float a[3], const float b[3], const float c[3], const float d[3]);
float normal_tri_v3(float r[3], const float a[3], const float b[3], const float c[3]);
float normal_quad_v3(float r[3], const float a[3], const float b[3], const float c[3], const float d[3]);
float area_tri_v2(const float a[2], const float b[2], const float c[2]);
float area_tri_signed_v2(const float v1[2], const float v2[2], const float v3[2]);
float area_tri_v3(const float a[3], const float b[3], const float c[3]);
float area_quad_v3(const float a[3], const float b[3], const float c[3], const float d[3]);
float area_poly_v3(int nr, float verts[][3], const float normal[3]);
int is_quad_convex_v3(const float *v1, const float *v2, const float *v3, const float *v4);
float dist_to_line_v2(const float p[2], const float l1[2], const float l2[2]);
float dist_to_line_segment_v2(const float p[2], const float l1[2], const float l2[2]);
void closest_to_line_segment_v2(float closest[2], const float p[2], const float l1[2], const float l2[2]);
float dist_to_plane_normalized_v3(const float p[3], const float plane_co[3], const float plane_no_unit[3]);
float dist_to_plane_v3(const float p[3], const float plane_co[3], const float plane_no[3]);
float dist_to_line_segment_v3(const float p[3], const float l1[3], const float l2[3]);
float closest_to_line_v3(float r[3], const float p[3], const float l1[3], const float l2[3]);
float closest_to_line_v2(float r[2], const float p[2], const float l1[2], const float l2[2]);
void closest_to_line_segment_v3(float r[3], const float p[3], const float l1[3], const float l2[3]);
void closest_to_plane_v3(float r[3], const float plane_co[3], const float plane_no_unit[3], const float pt[3]);
float line_point_factor_v3(const float p[3], const float l1[3], const float l2[3]);
float line_point_factor_v2(const float p[2], const float l1[2], const float l2[2]);
int isect_line_line_v2(const float a1[2], const float a2[2], const float b1[2], const float b2[2]);
int isect_line_line_v2_int(const int a1[2], const int a2[2], const int b1[2], const int b2[2]);
int isect_line_sphere_v3(const float l1[3], const float l2[3], const float sp[3], const float r, float r_p1[3], float r_p2[3]);
int isect_line_sphere_v2(const float l1[2], const float l2[2], const float sp[2], const float r, float r_p1[2], float r_p2[2]);
int isect_seg_seg_v2_point(const float v1[2], const float v2[2], const float v3[2], const float v4[2], float vi[2]);
int isect_line_line_v3(const float v1[3], const float v2[3],
                       const float v3[3], const float v4[3],
                       float i1[3], float i2[3]);
int isect_line_line_strict_v3(const float v1[3], const float v2[3],
                              const float v3[3], const float v4[3],
                              float vi[3], float *r_lambda);
int isect_ray_plane_v3(const float p1[3], const float d[3],
                       const float v0[3], const float v1[3], const float v2[3],
                       float *r_lambda, const int clip);
int isect_line_plane_v3(float out[3], const float l1[3], const float l2[3],
                        const float plane_co[3], const float plane_no[3], const short no_flip);
void isect_plane_plane_v3(float r_isect_co[3], float r_isect_no[3],
                          const float plane_a_co[3], const float plane_a_no[3],
                          const float plane_b_co[3], const float plane_b_no[3]);
int isect_line_tri_v3(const float p1[3], const float p2[3],
 const float v0[3], const float v1[3], const float v2[3], float *r_lambda, float r_uv[2]);
int isect_ray_tri_v3(const float p1[3], const float d[3],
 const float v0[3], const float v1[3], const float v2[3], float *r_lambda, float r_uv[2]);
int isect_ray_tri_threshold_v3(const float p1[3], const float d[3],
 const float v0[3], const float v1[3], const float v2[3], float *r_lambda, float r_uv[2], const float threshold);
int isect_ray_tri_epsilon_v3(const float p1[3], const float d[3],
 const float v0[3], const float v1[3], const float v2[3], float *r_lambda, float r_uv[2], const float epsilon);
int isect_point_quad_v2(const float p[2], const float a[2], const float b[2], const float c[2], const float d[2]);
int isect_point_tri_v2(const float v1[2], const float v2[2], const float v3[2], const float pt[2]);
int isect_point_tri_v2_int(const int x1, const int y1, const int x2, const int y2, const int a, const int b);
int isect_point_tri_prism_v3(const float p[3], const float v1[3], const float v2[3], const float v3[3]);
void isect_point_quad_uv_v2(const float v0[2], const float v1[2], const float v2[2], const float v3[2],
                            const float pt[2], float r_uv[2]);
void isect_point_face_uv_v2(const int isquad, const float v0[2], const float v1[2], const float v2[2],
                            const float v3[2], const float pt[2], float r_uv[2]);
int isect_sweeping_sphere_tri_v3(const float p1[3], const float p2[3], const float radius,
 const float v0[3], const float v1[3], const float v2[3], float *r_lambda, float ipoint[3]);
int isect_axial_line_tri_v3(const int axis, const float co1[3], const float co2[3],
 const float v0[3], const float v1[3], const float v2[3], float *r_lambda);
int isect_aabb_aabb_v3(const float min1[3], const float max1[3], const float min2[3], const float max2[3]);
int clip_line_plane(float p1[3], float p2[3], const float plane[4]);
void plot_line_v2v2i(const int p1[2], const int p2[2], int (*callback)(int, int, void *), void *userData);
void interp_weights_face_v3(float w[4],
 const float a[3], const float b[3], const float c[3], const float d[3], const float p[3]);
void interp_weights_poly_v3(float w[], float v[][3], const int n, const float p[3]);
void interp_cubic_v3(float x[3], float v[3],
 const float x1[3], const float v1[3], const float x2[3], const float v2[3], const float t);
int interp_sparse_array(float *array, const int list_size, const float invalid);
void barycentric_transform(float pt_tar[3], float const pt_src[3],
 const float tri_tar_p1[3], const float tri_tar_p2[3], const float tri_tar_p3[3],
 const float tri_src_p1[3], const float tri_src_p2[3], const float tri_src_p3[3]);
void barycentric_weights_v2(const float v1[2], const float v2[2], const float v3[2],
 const float co[2], float w[3]);
void resolve_tri_uv(float r_uv[2], const float st[2], const float st0[2], const float st1[2], const float st2[2]);
void resolve_quad_uv(float uv[2], const float st[2], const float st0[2], const float st1[2], const float st2[2], const float st3[2]);
void lookat_m4(float mat[4][4], float vx, float vy,
 float vz, float px, float py, float pz, float twist);
void polarview_m4(float mat[4][4], float dist, float azimuth,
 float incidence, float twist);
void perspective_m4(float mat[4][4], const float left, const float right,
 const float bottom, const float top, const float nearClip, const float farClip);
void orthographic_m4(float mat[4][4], const float left, const float right,
 const float bottom, const float top, const float nearClip, const float farClip);
void window_translate_m4(float winmat[][4], float perspmat[][4],
 const float x, const float y);
int box_clip_bounds_m4(float boundbox[2][3],
 const float bounds[4], float winmat[4][4]);
void box_minmax_bounds_m4(float min[3], float max[3],
 float boundbox[2][3], float mat[4][4]);
void map_to_tube(float *u, float *v, const float x, const float y, const float z);
void map_to_sphere(float *u, float *v, const float x, const float y, const float z);
void accumulate_vertex_normals(float n1[3], float n2[3], float n3[3],
 float n4[3], const float f_no[3], const float co1[3], const float co2[3],
 const float co3[3], const float co4[3]);
void accumulate_vertex_normals_poly(float **vertnos, float polyno[3],
 float **vertcos, float vdiffs[][3], int nverts);
typedef struct VertexTangent {
 struct VertexTangent *next;
 float tang[3], uv[2];
} VertexTangent;
float *find_vertex_tangent(VertexTangent *vtang, const float uv[2]);
void sum_or_add_vertex_tangent(void *arena, VertexTangent **vtang,
 const float tang[3], const float uv[2]);
void tangent_from_uv(float uv1[2], float uv2[2], float uv3[2],
 float co1[3], float co2[3], float co3[3], float n[3], float tang[3]);
void vcloud_estimate_transform(int list_size, float (*pos)[3], float *weight,
 float (*rpos)[3], float *rweight,
 float lloc[3],float rloc[3],float lrot[3][3],float lscale[3][3]);
static inline void zero_sh(float r[9]);
static inline void copy_sh_sh(float r[9], const float a[9]);
static inline void mul_sh_fl(float r[9], const float f);
static inline void add_sh_shsh(float r[9], const float a[9], const float b[9]);
static inline float eval_shv3(float r[9], const float v[3]);
static inline float diffuse_shv3(float r[9], const float v[3]);
static inline void vec_fac_to_sh(float r[9], const float v[3], const float f);
static inline void madd_sh_shfl(float r[9], const float sh[3], const float f);
float form_factor_hemi_poly(float p[3], float n[3],
 float v1[3], float v2[3], float v3[3], float v4[3]);
void axis_dominant_v3(int *axis_a, int *axis_b, const float axis[3]);
static inline void zero_v2(float r[2])
{
 r[0]= 0.0f;
 r[1]= 0.0f;
}
static inline void zero_v3(float r[3])
{
 r[0]= 0.0f;
 r[1]= 0.0f;
 r[2]= 0.0f;
}
static inline void zero_v4(float r[4])
{
 r[0]= 0.0f;
 r[1]= 0.0f;
 r[2]= 0.0f;
 r[3]= 0.0f;
}
static inline void copy_v2_v2(float r[2], const float a[2])
{
 r[0]= a[0];
 r[1]= a[1];
}
static inline void copy_v3_v3(float r[3], const float a[3])
{
 r[0]= a[0];
 r[1]= a[1];
 r[2]= a[2];
}
static inline void copy_v4_v4(float r[4], const float a[4])
{
 r[0]= a[0];
 r[1]= a[1];
 r[2]= a[2];
 r[3]= a[3];
}
static inline void copy_v2_v2_char(char r[2], const char a[2])
{
 r[0]= a[0];
 r[1]= a[1];
}
static inline void copy_v3_v3_char(char r[3], const char a[3])
{
 r[0]= a[0];
 r[1]= a[1];
 r[2]= a[2];
}
static inline void copy_v4_v4_char(char r[4], const char a[4])
{
 r[0]= a[0];
 r[1]= a[1];
 r[2]= a[2];
 r[3]= a[3];
}
static inline void copy_v2_v2_short(short r[2], const short a[2])
{
 r[0]= a[0];
 r[1]= a[1];
}
static inline void copy_v3_v3_short(short r[3], const short a[3])
{
 r[0]= a[0];
 r[1]= a[1];
 r[2]= a[2];
}
static inline void copy_v4_v4_short(short r[4], const short a[4])
{
 r[0]= a[0];
 r[1]= a[1];
 r[2]= a[2];
 r[3]= a[3];
}
static inline void copy_v2_v2_int(int r[2], const int a[2])
{
 r[0]= a[0];
 r[1]= a[1];
}
static inline void copy_v3_v3_int(int r[3], const int a[3])
{
 r[0]= a[0];
 r[1]= a[1];
 r[2]= a[2];
}
static inline void copy_v4_v4_int(int r[4], const int a[4])
{
 r[0]= a[0];
 r[1]= a[1];
 r[2]= a[2];
 r[3]= a[3];
}
static inline void swap_v2_v2(float a[2], float b[2])
{
 { float sw_ap; sw_ap=(a[0]); (a[0])=(b[0]); (b[0])=sw_ap; };
 { float sw_ap; sw_ap=(a[1]); (a[1])=(b[1]); (b[1])=sw_ap; };
}
static inline void swap_v3_v3(float a[3], float b[3])
{
 { float sw_ap; sw_ap=(a[0]); (a[0])=(b[0]); (b[0])=sw_ap; };
 { float sw_ap; sw_ap=(a[1]); (a[1])=(b[1]); (b[1])=sw_ap; };
 { float sw_ap; sw_ap=(a[2]); (a[2])=(b[2]); (b[2])=sw_ap; };
}
static inline void swap_v4_v4(float a[4], float b[4])
{
 { float sw_ap; sw_ap=(a[0]); (a[0])=(b[0]); (b[0])=sw_ap; };
 { float sw_ap; sw_ap=(a[1]); (a[1])=(b[1]); (b[1])=sw_ap; };
 { float sw_ap; sw_ap=(a[2]); (a[2])=(b[2]); (b[2])=sw_ap; };
 { float sw_ap; sw_ap=(a[3]); (a[3])=(b[3]); (b[3])=sw_ap; };
}
static inline void add_v3_fl(float r[3], float f)
{
 r[0] += f;
 r[1] += f;
 r[2] += f;
}
static inline void add_v4_fl(float r[4], float f)
{
 r[0] += f;
 r[1] += f;
 r[2] += f;
 r[3] += f;
}
static inline void add_v2_v2(float r[2], const float a[2])
{
 r[0] += a[0];
 r[1] += a[1];
}
static inline void add_v2_v2v2(float r[2], const float a[2], const float b[2])
{
 r[0]= a[0] + b[0];
 r[1]= a[1] + b[1];
}
static inline void add_v3_v3(float r[3], const float a[3])
{
 r[0] += a[0];
 r[1] += a[1];
 r[2] += a[2];
}
static inline void add_v3_v3v3(float r[3], const float a[3], const float b[3])
{
 r[0]= a[0] + b[0];
 r[1]= a[1] + b[1];
 r[2]= a[2] + b[2];
}
static inline void sub_v2_v2(float r[2], const float a[2])
{
 r[0] -= a[0];
 r[1] -= a[1];
}
static inline void sub_v2_v2v2(float r[2], const float a[2], const float b[2])
{
 r[0]= a[0] - b[0];
 r[1]= a[1] - b[1];
}
static inline void sub_v3_v3(float r[3], const float a[3])
{
 r[0] -= a[0];
 r[1] -= a[1];
 r[2] -= a[2];
}
static inline void sub_v3_v3v3(float r[3], const float a[3], const float b[3])
{
 r[0]= a[0] - b[0];
 r[1]= a[1] - b[1];
 r[2]= a[2] - b[2];
}
static inline void sub_v4_v4(float r[4], const float a[4])
{
 r[0] -= a[0];
 r[1] -= a[1];
 r[2] -= a[2];
 r[3] -= a[3];
}
static inline void sub_v4_v4v4(float r[4], const float a[4], const float b[4])
{
 r[0]= a[0] - b[0];
 r[1]= a[1] - b[1];
 r[2]= a[2] - b[2];
 r[3]= a[3] - b[3];
}
static inline void mul_v2_fl(float r[2], float f)
{
 r[0]*= f;
 r[1]*= f;
}
static inline void mul_v2_v2fl(float r[2], const float a[2], float f)
{
 r[0]= a[0]*f;
 r[1]= a[1]*f;
}
static inline void mul_v3_fl(float r[3], float f)
{
 r[0] *= f;
 r[1] *= f;
 r[2] *= f;
}
static inline void mul_v3_v3fl(float r[3], const float a[3], float f)
{
 r[0]= a[0]*f;
 r[1]= a[1]*f;
 r[2]= a[2]*f;
}
static inline void mul_v2_v2(float r[2], const float a[2])
{
 r[0] *= a[0];
 r[1] *= a[1];
}
static inline void mul_v3_v3(float r[3], const float a[3])
{
 r[0] *= a[0];
 r[1] *= a[1];
 r[2] *= a[2];
}
static inline void mul_v4_fl(float r[4], float f)
{
 r[0]*= f;
 r[1]*= f;
 r[2]*= f;
 r[3]*= f;
}
static inline void madd_v2_v2fl(float r[2], const float a[2], float f)
{
 r[0] += a[0]*f;
 r[1] += a[1]*f;
}
static inline void madd_v3_v3fl(float r[3], const float a[3], float f)
{
 r[0] += a[0]*f;
 r[1] += a[1]*f;
 r[2] += a[2]*f;
}
static inline void madd_v3_v3v3(float r[3], const float a[3], const float b[3])
{
 r[0] += a[0]*b[0];
 r[1] += a[1]*b[1];
 r[2] += a[2]*b[2];
}
static inline void madd_v2_v2v2fl(float r[2], const float a[2], const float b[2], float f)
{
 r[0] = a[0] + b[0]*f;
 r[1] = a[1] + b[1]*f;
}
static inline void madd_v3_v3v3fl(float r[3], const float a[3], const float b[3], float f)
{
 r[0] = a[0] + b[0]*f;
 r[1] = a[1] + b[1]*f;
 r[2] = a[2] + b[2]*f;
}
static inline void madd_v3_v3v3v3(float r[3], const float a[3], const float b[3], const float c[3])
{
 r[0] = a[0] + b[0]*c[0];
 r[1] = a[1] + b[1]*c[1];
 r[2] = a[2] + b[2]*c[2];
}
static inline void madd_v4_v4fl(float r[4], const float a[4], float f)
{
 r[0] += a[0]*f;
 r[1] += a[1]*f;
 r[2] += a[2]*f;
 r[3] += a[3]*f;
}
static inline void mul_v3_v3v3(float r[3], const float v1[3], const float v2[3])
{
 r[0] = v1[0] * v2[0];
 r[1] = v1[1] * v2[1];
 r[2] = v1[2] * v2[2];
}
static inline void negate_v2(float r[3])
{
 r[0]= -r[0];
 r[1]= -r[1];
}
static inline void negate_v2_v2(float r[2], const float a[2])
{
 r[0]= -a[0];
 r[1]= -a[1];
}
static inline void negate_v3(float r[3])
{
 r[0]= -r[0];
 r[1]= -r[1];
 r[2]= -r[2];
}
static inline void negate_v3_v3(float r[3], const float a[3])
{
 r[0]= -a[0];
 r[1]= -a[1];
 r[2]= -a[2];
}
static inline void negate_v4(float r[4])
{
 r[0]= -r[0];
 r[1]= -r[1];
 r[2]= -r[2];
 r[3]= -r[3];
}
static inline void negate_v4_v4(float r[4], const float a[4])
{
 r[0]= -a[0];
 r[1]= -a[1];
 r[2]= -a[2];
 r[3]= -a[3];
}
static inline float dot_v2v2(const float a[2], const float b[2])
{
 return a[0]*b[0] + a[1]*b[1];
}
static inline float dot_v3v3(const float a[3], const float b[3])
{
 return a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
}
static inline float cross_v2v2(const float a[2], const float b[2])
{
 return a[0]*b[1] - a[1]*b[0];
}
static inline void cross_v3_v3v3(float r[3], const float a[3], const float b[3])
{
 r[0]= a[1]*b[2] - a[2]*b[1];
 r[1]= a[2]*b[0] - a[0]*b[2];
 r[2]= a[0]*b[1] - a[1]*b[0];
}
static inline void star_m3_v3(float rmat[][3], float a[3])
{
 rmat[0][0]= rmat[1][1]= rmat[2][2]= 0.0;
 rmat[0][1]= -a[2];
 rmat[0][2]= a[1];
 rmat[1][0]= a[2];
 rmat[1][2]= -a[0];
 rmat[2][0]= -a[1];
 rmat[2][1]= a[0];
}
static inline float len_squared_v2(const float v[2])
{
 return v[0]*v[0] + v[1]*v[1];
}
static inline float len_v2(const float v[2])
{
 return (float)((float)sqrt(v[0]*v[0] + v[1]*v[1]));
}
static inline float len_v2v2(const float v1[2], const float v2[2])
{
 float x, y;
 x = v1[0]-v2[0];
 y = v1[1]-v2[1];
 return (float)((float)sqrt(x*x+y*y));
}
static inline float len_v3(const float a[3])
{
 return ((float)sqrt(dot_v3v3(a, a)));
}
static inline float len_squared_v2v2(const float a[2], const float b[2])
{
 float d[2];
 sub_v2_v2v2(d, b, a);
 return dot_v2v2(d, d);
}
static inline float len_v3v3(const float a[3], const float b[3])
{
 float d[3];
 sub_v3_v3v3(d, b, a);
 return len_v3(d);
}
static inline float len_squared_v3v3(const float a[3], const float b[3])
{
 float d[3];
 sub_v3_v3v3(d, b, a);
 return dot_v3v3(d, d);
}
static inline float normalize_v2_v2(float r[2], const float a[2])
{
 float d= dot_v2v2(a, a);
 if(d > 1.0e-35f) {
  d= ((float)sqrt(d));
  mul_v2_v2fl(r, a, 1.0f/d);
 } else {
  zero_v2(r);
  d= 0.0f;
 }
 return d;
}
static inline float normalize_v2(float n[2])
{
 return normalize_v2_v2(n, n);
}
static inline float normalize_v3_v3(float r[3], const float a[3])
{
 float d= dot_v3v3(a, a);
 if(d > 1.0e-35f) {
  d= ((float)sqrt(d));
  mul_v3_v3fl(r, a, 1.0f/d);
 }
 else {
  zero_v3(r);
  d= 0.0f;
 }
 return d;
}
static inline double normalize_v3_d(double n[3])
{
 double d= n[0]*n[0] + n[1]*n[1] + n[2]*n[2];
 if(d > 1.0e-35) {
  double mul;
  d= sqrt(d);
  mul = 1.0 / d;
  n[0] *= mul;
  n[1] *= mul;
  n[2] *= mul;
 } else {
  n[0] = n[1] = n[2] = 0;
  d= 0.0;
 }
 return d;
}
static inline float normalize_v3(float n[3])
{
 return normalize_v3_v3(n, n);
}
static inline void normal_short_to_float_v3(float out[3], const short in[3])
{
 out[0] = in[0]*(1.0f/32767.0f);
 out[1] = in[1]*(1.0f/32767.0f);
 out[2] = in[2]*(1.0f/32767.0f);
}
static inline void normal_float_to_short_v3(short out[3], const float in[3])
{
 out[0] = (short)(in[0]*32767.0f);
 out[1] = (short)(in[1]*32767.0f);
 out[2] = (short)(in[2]*32767.0f);
}
static inline int is_zero_v3(const float v[3])
{
 return (v[0] == 0 && v[1] == 0 && v[2] == 0);
}
static inline int is_zero_v4(const float v[4])
{
 return (v[0] == 0 && v[1] == 0 && v[2] == 0 && v[3] == 0);
}
static inline int is_one_v3(const float v[3])
{
 return (v[0] == 1 && v[1] == 1 && v[2] == 1);
}
static inline int equals_v2v2(const float v1[2], const float v2[2])
{
 return ((v1[0]==v2[0]) && (v1[1]==v2[1]));
}
static inline int equals_v3v3(const float v1[3], const float v2[3])
{
 return ((v1[0]==v2[0]) && (v1[1]==v2[1]) && (v1[2]==v2[2]));
}
static inline int equals_v4v4(const float v1[4], const float v2[4])
{
 return ((v1[0]==v2[0]) && (v1[1]==v2[1]) && (v1[2]==v2[2]) && (v1[3]==v2[3]));
}
static inline int compare_v3v3(const float v1[3], const float v2[3], const float limit)
{
 if(((float)fabs(v1[0]-v2[0]))<limit)
  if(((float)fabs(v1[1]-v2[1]))<limit)
   if(((float)fabs(v1[2]-v2[2]))<limit)
    return 1;
 return 0;
}
static inline int compare_len_v3v3(const float v1[3], const float v2[3], const float limit)
{
 float x,y,z;
 x=v1[0]-v2[0];
 y=v1[1]-v2[1];
 z=v1[2]-v2[2];
 return ((x*x + y*y + z*z) < (limit*limit));
}
static inline int compare_v4v4(const float v1[4], const float v2[4], const float limit)
{
 if(((float)fabs(v1[0]-v2[0]))<limit)
  if(((float)fabs(v1[1]-v2[1]))<limit)
   if(((float)fabs(v1[2]-v2[2]))<limit)
    if(((float)fabs(v1[3]-v2[3]))<limit)
     return 1;
 return 0;
}
static inline float line_point_side_v2(const float l1[2], const float l2[2], const float pt[2])
{
 return ((l1[0]-pt[0]) * (l2[1]-pt[1])) -
   ((l2[0]-pt[0]) * (l1[1]-pt[1]));
}
static inline void zero_v2(float r[2]);
static inline void zero_v3(float r[3]);
static inline void zero_v4(float r[4]);
static inline void copy_v2_v2(float r[2], const float a[2]);
static inline void copy_v3_v3(float r[3], const float a[3]);
static inline void copy_v4_v4(float r[4], const float a[4]);
static inline void swap_v2_v2(float a[2], float b[2]);
static inline void swap_v3_v3(float a[3], float b[3]);
static inline void swap_v4_v4(float a[4], float b[4]);
static inline void copy_v2_v2_char(char r[2], const char a[2]);
static inline void copy_v3_v3_char(char r[3], const char a[3]);
static inline void copy_v4_v4_char(char r[4], const char a[4]);
static inline void copy_v2_v2_short(short r[2], const short a[2]);
static inline void copy_v3_v3_short(short r[3], const short a[3]);
static inline void copy_v4_v4_short(short r[4], const short a[4]);
static inline void copy_v2_v2_int(int r[2], const int a[2]);
static inline void copy_v3_v3_int(int r[3], const int a[3]);
static inline void copy_v4_v4_int(int r[4], const int a[4]);
static inline void add_v3_fl(float r[3], float f);
static inline void add_v4_fl(float r[4], float f);
static inline void add_v2_v2(float r[2], const float a[2]);
static inline void add_v2_v2v2(float r[2], const float a[2], const float b[2]);
static inline void add_v3_v3(float r[3], const float a[3]);
static inline void add_v3_v3v3(float r[3], const float a[3], const float b[3]);
static inline void sub_v2_v2(float r[2], const float a[2]);
static inline void sub_v2_v2v2(float r[2], const float a[2], const float b[2]);
static inline void sub_v3_v3(float r[3], const float a[3]);
static inline void sub_v3_v3v3(float r[3], const float a[3], const float b[3]);
static inline void sub_v4_v4(float r[4], const float a[4]);
static inline void sub_v4_v4v4(float r[4], const float a[4], const float b[4]);
static inline void mul_v2_fl(float r[2], float f);
static inline void mul_v2_v2fl(float r[2], const float a[2], float f);
static inline void mul_v3_fl(float r[3], float f);
static inline void mul_v3_v3fl(float r[3], const float a[3], float f);
static inline void mul_v2_v2(float r[2], const float a[2]);
static inline void mul_v3_v3(float r[3], const float a[3]);
static inline void mul_v3_v3v3(float r[3], const float a[3], const float b[3]);
static inline void mul_v4_fl(float r[4], float f);
static inline void madd_v3_v3fl(float r[3], const float a[3], float f);
static inline void madd_v3_v3v3(float r[3], const float a[3], const float b[3]);
static inline void madd_v2_v2v2fl(float r[2], const float a[2], const float b[2], float f);
static inline void madd_v3_v3v3fl(float r[3], const float a[3], const float b[3], float f);
static inline void madd_v3_v3v3v3(float r[3], const float a[3], const float b[3], const float c[3]);
static inline void madd_v4_v4fl(float r[4], const float a[4], float f);
static inline void negate_v2(float r[2]);
static inline void negate_v2_v2(float r[2], const float a[2]);
static inline void negate_v3(float r[3]);
static inline void negate_v3_v3(float r[3], const float a[3]);
static inline void negate_v4(float r[4]);
static inline void negate_v4_v4(float r[4], const float a[3]);
static inline float dot_v2v2(const float a[2], const float b[2]);
static inline float dot_v3v3(const float a[3], const float b[3]);
static inline float cross_v2v2(const float a[2], const float b[2]);
static inline void cross_v3_v3v3(float r[3], const float a[3], const float b[3]);
static inline void star_m3_v3(float rmat[3][3],float a[3]);
static inline float len_squared_v2(const float v[2]);
static inline float len_v2(const float a[2]);
static inline float len_v2v2(const float a[2], const float b[2]);
static inline float len_squared_v2v2(const float a[2], const float b[2]);
static inline float len_v3(const float a[3]);
static inline float len_v3v3(const float a[3], const float b[3]);
static inline float len_squared_v3v3(const float a[3], const float b[3]);
static inline float normalize_v2(float r[2]);
static inline float normalize_v2_v2(float r[2], const float a[2]);
static inline float normalize_v3(float r[3]);
static inline float normalize_v3_v3(float r[3], const float a[3]);
void interp_v2_v2v2(float r[2], const float a[2], const float b[2], const float t);
void interp_v2_v2v2v2(float r[2], const float a[2], const float b[2], const float c[3], const float t[3]);
void interp_v3_v3v3(float r[3], const float a[3], const float b[3], const float t);
void interp_v3_v3v3v3(float p[3], const float v1[3], const float v2[3], const float v3[3], const float w[3]);
void interp_v3_v3v3v3v3(float p[3], const float v1[3], const float v2[3], const float v3[3], const float v4[3], const float w[4]);
void interp_v4_v4v4(float r[4], const float a[4], const float b[4], const float t);
void interp_v4_v4v4v4(float p[4], const float v1[4], const float v2[4], const float v3[4], const float w[3]);
void interp_v4_v4v4v4v4(float p[4], const float v1[4], const float v2[4], const float v3[4], const float v4[4], const float w[4]);
void mid_v3_v3v3(float r[3], const float a[3], const float b[3]);
static inline int is_zero_v3(const float a[3]);
static inline int is_zero_v4(const float a[4]);
static inline int is_one_v3(const float a[3]);
static inline int equals_v2v2(const float v1[2], const float v2[2]);
static inline int equals_v3v3(const float a[3], const float b[3]);
static inline int compare_v3v3(const float a[3], const float b[3], const float limit);
static inline int compare_len_v3v3(const float a[3], const float b[3], const float limit);
static inline int compare_v4v4(const float a[4], const float b[4], const float limit);
static inline int equals_v4v4(const float a[4], const float b[4]);
static inline float line_point_side_v2(const float l1[2], const float l2[2], const float pt[2]);
float angle_v2v2(const float a[2], const float b[2]);
float angle_v2v2v2(const float a[2], const float b[2], const float c[2]);
float angle_normalized_v2v2(const float a[2], const float b[2]);
float angle_v3v3(const float a[3], const float b[3]);
float angle_v3v3v3(const float a[3], const float b[3], const float c[3]);
float angle_normalized_v3v3(const float v1[3], const float v2[3]);
void angle_tri_v3(float angles[3], const float v1[3], const float v2[3], const float v3[3]);
void angle_quad_v3(float angles[4], const float v1[3], const float v2[3], const float v3[3], const float v4[3]);
void angle_poly_v3(float* angles, const float* verts[3], int len);
void project_v2_v2v2(float c[2], const float v1[2], const float v2[2]);
void project_v3_v3v3(float r[3], const float p[3], const float n[3]);
void reflect_v3_v3v3(float r[3], const float v[3], const float n[3]);
void ortho_basis_v3v3_v3(float r1[3], float r2[3], const float a[3]);
void bisect_v3_v3v3v3(float r[3], const float a[3], const float b[3], const float c[3]);
void rotate_v3_v3v3fl(float v[3], const float p[3], const float axis[3], const float angle);
void rotate_normalized_v3_v3v3fl(float v[3], const float p[3], const float axis[3], const float angle);
void print_v2(const char *str, const float a[2]);
void print_v3(const char *str, const float a[3]);
void print_v4(const char *str, const float a[4]);
static inline void normal_short_to_float_v3(float r[3], const short n[3]);
static inline void normal_float_to_short_v3(short r[3], const float n[3]);
void minmax_v3v3_v3(float min[3], float max[3], const float vec[3]);
double dot_vn_vn(const float *array_src_a, const float *array_src_b, const int size);
float normalize_vn_vn(float *array_tar, const float *array_src, const int size);
float normalize_vn(float *array_tar, const int size);
void range_vn_i(int *array_tar, const int size, const int start);
void range_vn_fl(float *array_tar, const int size, const float start, const float step);
void negate_vn(float *array_tar, const int size);
void negate_vn_vn(float *array_tar, const float *array_src, const int size);
void mul_vn_fl(float *array_tar, const int size, const float f);
void mul_vn_vn_fl(float *array_tar, const float *array_src, const int size, const float f);
void add_vn_vn(float *array_tar, const float *array_src, const int size);
void add_vn_vnvn(float *array_tar, const float *array_src_a, const float *array_src_b, const int size);
void sub_vn_vn(float *array_tar, const float *array_src, const int size);
void sub_vn_vnvn(float *array_tar, const float *array_src_a, const float *array_src_b, const int size);
void fill_vn_i(int *array_tar, const int size, const int val);
void fill_vn_fl(float *array_tar, const int size, const float val);
typedef struct vec2s {
 short x, y;
} vec2s;
typedef struct vec2f {
 float x, y;
} vec2f;
typedef struct rcti {
 int xmin, xmax;
 int ymin, ymax;
} rcti;
typedef struct rctf {
 float xmin, xmax;
 float ymin, ymax;
} rctf;
typedef struct View2D {
 rctf tot, cur;
 rcti vert, hor;
 rcti mask;
 float min[2], max[2];
 float minzoom, maxzoom;
 short scroll;
 short scroll_ui;
 short keeptot;
 short keepzoom;
 short keepofs;
 short flag;
 short align;
 short winx, winy;
 short oldwinx, oldwiny;
 short around;
 float *tab_offset;
 int tab_num;
 int tab_cur;
} View2D;
struct PackedFile;
struct Scene;
struct anim;
struct ImBuf;
struct RenderResult;
struct GPUTexture;
typedef struct ImageUser {
 struct Scene *scene;
 int framenr;
 int frames;
 int offset, sfra;
 char fie_ima, cycl;
 char ok, pad;
 short multi_index, layer, pass;
 short flag;
 int pad2;
} ImageUser;
typedef struct Image {
 ID id;
 char name[1024];
 ListBase ibufs;
 struct GPUTexture *gputexture;
 struct anim *anim;
 struct RenderResult *rr;
 struct RenderResult *renders[8];
 short render_slot, last_render_slot;
 short ok, flag;
 short source, type;
 int lastframe;
 short tpageflag, totbind;
 short xrep, yrep;
 short twsta, twend;
 unsigned int bindcode;
 unsigned int *repbind;
 struct PackedFile * packedfile;
 struct PreviewImage * preview;
 float lastupdate;
 int lastused;
 short animspeed;
 short gen_x, gen_y;
 char gen_type, gen_flag;
 float aspx, aspy;
} Image;
struct AnimData;
struct Ipo;
struct PluginTex;
struct ColorBand;
struct EnvMap;
struct Object;
struct Tex;
struct Image;
struct PreviewImage;
struct ImBuf;
struct Ocean;
struct CurveMapping;
typedef struct MTex {
 short texco, mapto, maptoneg, blendtype;
 struct Object *object;
 struct Tex *tex;
 char uvname[64];
 char projx, projy, projz, mapping;
 float ofs[3], size[3], rot;
 short texflag, colormodel, pmapto, pmaptoneg;
 short normapspace, which_output;
 char brush_map_mode, pad[7];
 float r, g, b, k;
 float def_var, rt;
 float colfac, varfac;
 float norfac, dispfac, warpfac;
 float colspecfac, mirrfac, alphafac;
 float difffac, specfac, emitfac, hardfac;
 float raymirrfac, translfac, ambfac;
 float colemitfac, colreflfac, coltransfac;
 float densfac, scatterfac, reflfac;
 float timefac, lengthfac, clumpfac, dampfac;
 float kinkfac, roughfac, padensfac, gravityfac;
 float lifefac, sizefac, ivelfac, fieldfac;
 float shadowfac;
 float zenupfac, zendownfac, blendfac;
} MTex;
typedef unsigned short dna_ushort_fix;
typedef struct PluginTex {
 char name[1024];
 void *handle;
 char *pname;
 char *stnames;
 int stypes;
 int vars;
 void *varstr;
 float *result;
 float *cfra;
 float data[32];
 int (*doit)(void);
 void (*instance_init)(void *);
 void (*callback)(dna_ushort_fix);
 int version, pad;
} PluginTex;
typedef struct CBData {
 float r, g, b, a, pos;
 int cur;
} CBData;
typedef struct ColorBand {
 short flag, tot, cur, ipotype;
 CBData data[32];
} ColorBand;
typedef struct EnvMap {
 struct Object *object;
 struct Image *ima;
 struct ImBuf *cube[6];
 float imat[4][4];
 float obimat[3][3];
 short type, stype;
 float clipsta, clipend;
 float viewscale;
 unsigned int notlay;
 short cuberes, depth;
 int ok, lastframe;
 short recalc, lastsize;
} EnvMap;
typedef struct PointDensity {
 short flag;
 short falloff_type;
 float falloff_softness;
 float radius;
 short source;
 short color_source;
 int totpoints;
 int pdpad;
 struct Object *object;
 int psys;
 short psys_cache_space;
 short ob_cache_space;
 void *point_tree;
 float *point_data;
 float noise_size;
 short noise_depth;
 short noise_influence;
 short noise_basis;
 short pdpad3[3];
 float noise_fac;
 float speed_scale, falloff_speed_scale, pdpad2;
 struct ColorBand *coba;
 struct CurveMapping *falloff_curve;
} PointDensity;
typedef struct VoxelData {
 int resol[3];
 int interp_type;
 short file_format;
 short flag;
 short extend;
 short smoked_type;
 struct Object *object;
 float int_multiplier;
 int still_frame;
 char source_path[1024];
 float *dataset;
 int cachedframe;
 int ok;
} VoxelData;
typedef struct OceanTex {
 struct Object *object;
 char oceanmod[64];
 int output;
 int pad;
} OceanTex;
typedef struct Tex {
 ID id;
 struct AnimData *adt;
 float noisesize, turbul;
 float bright, contrast, saturation, rfac, gfac, bfac;
 float filtersize, pad2;
 float mg_H, mg_lacunarity, mg_octaves, mg_offset, mg_gain;
 float dist_amount, ns_outscale;
 float vn_w1;
 float vn_w2;
 float vn_w3;
 float vn_w4;
 float vn_mexp;
 short vn_distm, vn_coltype;
 short noisedepth, noisetype;
 short noisebasis, noisebasis2;
 short imaflag, flag;
 short type, stype;
 float cropxmin, cropymin, cropxmax, cropymax;
 int texfilter;
 int afmax;
 short xrepeat, yrepeat;
 short extend;
 short fie_ima;
 int len;
 int frames, offset, sfra;
 float checkerdist, nabla;
 float pad1;
 struct ImageUser iuser;
 struct bNodeTree *nodetree;
 struct Ipo *ipo __attribute__ ((deprecated));
 struct Image *ima;
 struct PluginTex *plugin;
 struct ColorBand *coba;
 struct EnvMap *env;
 struct PreviewImage * preview;
 struct PointDensity *pd;
 struct VoxelData *vd;
 struct OceanTex *ot;
 char use_nodes;
 char pad[7];
} Tex;
typedef struct TexMapping {
 float loc[3], rot[3], size[3];
 int flag;
 char projx, projy, projz, mapping;
 int pad;
 float mat[4][4];
 float min[3], max[3];
 struct Object *ob;
} TexMapping;
typedef struct ColorMapping {
 struct ColorBand coba;
 float bright, contrast, saturation;
 int flag;
 float blend_color[3];
 float blend_factor;
 int blend_type, pad[3];
} ColorMapping;
struct ColorBand;
typedef struct uiFont {
 struct uiFont *next, *prev;
 char filename[1024];
 short blf_id;
 short uifont_id;
 short r_to_l;
 short pad;
} uiFont;
typedef struct uiFontStyle {
 short uifont_id;
 short points;
 short kerning;
 char pad[6];
 short italic, bold;
 short shadow;
 short shadx, shady;
 short align;
 float shadowalpha;
 float shadowcolor;
} uiFontStyle;
typedef struct uiStyle {
 struct uiStyle *next, *prev;
 char name[64];
 uiFontStyle paneltitle;
 uiFontStyle grouplabel;
 uiFontStyle widgetlabel;
 uiFontStyle widget;
 float panelzoom;
 short minlabelchars;
 short minwidgetchars;
 short columnspace;
 short templatespace;
 short boxspace;
 short buttonspacex;
 short buttonspacey;
 short panelspace;
 short panelouter;
 short pad;
} uiStyle;
typedef struct uiWidgetColors {
 char outline[4];
 char inner[4];
 char inner_sel[4];
 char item[4];
 char text[4];
 char text_sel[4];
 short shaded;
 short shadetop, shadedown;
 short alpha_check;
} uiWidgetColors;
typedef struct uiWidgetStateColors {
 char inner_anim[4];
 char inner_anim_sel[4];
 char inner_key[4];
 char inner_key_sel[4];
 char inner_driven[4];
 char inner_driven_sel[4];
 float blend, pad;
} uiWidgetStateColors;
typedef struct uiPanelColors {
 char header[4];
 short show_header;
 short pad;
} uiPanelColors;
typedef struct ThemeUI {
 uiWidgetColors wcol_regular, wcol_tool, wcol_text;
 uiWidgetColors wcol_radio, wcol_option, wcol_toggle;
 uiWidgetColors wcol_num, wcol_numslider;
 uiWidgetColors wcol_menu, wcol_pulldown, wcol_menu_back, wcol_menu_item;
 uiWidgetColors wcol_box, wcol_scroll, wcol_progress, wcol_list_item;
 uiWidgetStateColors wcol_state;
 uiPanelColors panel;
 char iconfile[256];
 float icon_alpha;
 float pad;
} ThemeUI;
typedef struct ThemeSpace {
 char back[4];
 char title[4];
 char text[4];
 char text_hi[4];
 char header[4];
 char header_title[4];
 char header_text[4];
 char header_text_hi[4];
 char button[4];
 char button_title[4];
 char button_text[4];
 char button_text_hi[4];
 char list[4];
 char list_title[4];
 char list_text[4];
 char list_text_hi[4];
 char panel[4];
 char panel_title[4];
 char panel_text[4];
 char panel_text_hi[4];
 char shade1[4];
 char shade2[4];
 char hilite[4];
 char grid[4];
 char wire[4], select[4];
 char lamp[4], speaker[4], pad2[4];
 char active[4], group[4], group_active[4], transform[4];
 char vertex[4], vertex_select[4];
 char edge[4], edge_select[4];
 char edge_seam[4], edge_sharp[4], edge_facesel[4], edge_crease[4];
 char face[4], face_select[4];
 char face_dot[4];
 char extra_edge_len[4], extra_face_angle[4], extra_face_area[4], pad3[4];
 char normal[4];
 char vertex_normal[4];
 char bone_solid[4], bone_pose[4];
 char strip[4], strip_select[4];
 char cframe[4];
 char nurb_uline[4], nurb_vline[4];
 char act_spline[4], nurb_sel_uline[4], nurb_sel_vline[4], lastsel_point[4];
 char handle_free[4], handle_auto[4], handle_vect[4], handle_align[4], handle_auto_clamped[4];
 char handle_sel_free[4], handle_sel_auto[4], handle_sel_vect[4], handle_sel_align[4], handle_sel_auto_clamped[4];
 char ds_channel[4], ds_subchannel[4];
 char console_output[4], console_input[4], console_info[4], console_error[4];
 char console_cursor[4];
 char vertex_size, outline_width, facedot_size;
 char noodle_curving;
 char syntaxl[4], syntaxn[4], syntaxb[4];
 char syntaxv[4], syntaxc[4];
 char movie[4], image[4], scene[4], audio[4];
 char effect[4], plugin[4], transition[4], meta[4];
 char editmesh_active[4];
 char handle_vertex[4];
 char handle_vertex_select[4];
 char handle_vertex_size;
 char marker_outline[4], marker[4], act_marker[4], sel_marker[4], dis_marker[4], lock_marker[4];
 char bundle_solid[4];
 char path_before[4], path_after[4];
 char camera_path[4];
 char hpad[7];
 char preview_back[4];
 char preview_stitch_face[4];
 char preview_stitch_edge[4];
 char preview_stitch_vert[4];
 char preview_stitch_stitchable[4];
 char preview_stitch_unstitchable[4];
 char preview_stitch_active[4];
 char match[4];
 char selected_highlight[4];
} ThemeSpace;
typedef struct ThemeWireColor {
 char solid[4];
 char select[4];
 char active[4];
 short flag;
 short pad;
} ThemeWireColor;
typedef struct bTheme {
 struct bTheme *next, *prev;
 char name[32];
 ThemeUI tui;
 ThemeSpace tbuts;
 ThemeSpace tv3d;
 ThemeSpace tfile;
 ThemeSpace tipo;
 ThemeSpace tinfo;
 ThemeSpace tact;
 ThemeSpace tnla;
 ThemeSpace tseq;
 ThemeSpace tima;
 ThemeSpace text;
 ThemeSpace toops;
 ThemeSpace ttime;
 ThemeSpace tnode;
 ThemeSpace tlogic;
 ThemeSpace tuserpref;
 ThemeSpace tconsole;
 ThemeSpace tclip;
 ThemeWireColor tarm[20];
 int active_theme_area, pad;
} bTheme;
typedef struct bAddon {
 struct bAddon *next, *prev;
 char module[64];
} bAddon;
typedef struct SolidLight {
 int flag, pad;
 float col[4], spec[4], vec[4];
} SolidLight;
typedef struct UserDef {
 int flag, dupflag;
 int savetime;
 char tempdir[768];
 char fontdir[768];
 char renderdir[1024];
 char textudir[768];
 char plugtexdir[768];
 char plugseqdir[768];
 char pythondir[768];
 char sounddir[768];
 char image_editor[1024];
 char anim_player[1024];
 int anim_player_preset;
 short v2d_min_gridsize;
 short timecode_style;
 short versions;
 short dbl_click_time;
 int gameflags;
 int wheellinescroll;
 int uiflag, language;
 short userpref, viewzoom;
 int mixbufsize;
 int audiodevice;
 int audiorate;
 int audioformat;
 int audiochannels;
 int scrollback;
 int dpi;
 short encoding;
 short transopts;
 short menuthreshold1, menuthreshold2;
 struct ListBase themes;
 struct ListBase uifonts;
 struct ListBase uistyles;
 struct ListBase keymaps __attribute__ ((deprecated));
 struct ListBase user_keymaps;
 struct ListBase addons;
 char keyconfigstr[64];
 short undosteps;
 short undomemory;
 short gp_manhattendist, gp_euclideandist, gp_eraser;
 short gp_settings;
 short tb_leftmouse, tb_rightmouse;
 struct SolidLight light[3];
 short tw_hotspot, tw_flag, tw_handlesize, tw_size;
 short textimeout,texcollectrate;
 short wmdrawmethod;
 short dragthreshold;
 int memcachelimit;
 int prefetchframes;
 short frameserverport;
 short pad_rot_angle;
 short obcenter_dia;
 short rvisize;
 short rvibright;
 short recent_files;
 short smooth_viewtx;
 short glreslimit;
 short curssize;
 short color_picker_type;
 short ipo_new;
 short keyhandles_new;
 short scrcastfps;
 short scrcastwait;
 short widget_unit;
 short anisotropic_filter;
 short use_16bit_textures, pad8;
 float ndof_sensitivity;
 int ndof_flag;
 float glalphaclip;
 short autokey_mode;
 short autokey_flag;
 short text_render, pad9;
 struct ColorBand coba_weight;
 float sculpt_paint_overlay_col[3];
 short tweak_threshold;
 short pad3;
 char author[80];
 int compute_device_type;
 int compute_device_id;
} UserDef;
extern UserDef U;
struct SpaceLink;
struct Object;
struct Group;
struct GHash;
typedef struct bMotionPathVert {
 float co[3];
 int flag;
} bMotionPathVert;
typedef enum eMotionPathVert_Flag {
 MOTIONPATH_VERT_SEL = (1<<0)
} eMotionPathVert_Flag;
typedef struct bMotionPath {
 bMotionPathVert *points;
 int length;
 int start_frame;
 int end_frame;
 int flag;
} bMotionPath;
typedef enum eMotionPath_Flag {
 MOTIONPATH_FLAG_BHEAD = (1<<0),
 MOTIONPATH_FLAG_EDIT = (1<<1)
} eMotionPath_Flag;
typedef struct bAnimVizSettings {
 int ghost_sf, ghost_ef;
 int ghost_bc, ghost_ac;
 short ghost_type;
 short ghost_step;
 short ghost_flag;
 short recalc;
 short path_type;
 short path_step;
 short path_viewflag;
 short path_bakeflag;
 int path_sf, path_ef;
 int path_bc, path_ac;
} bAnimVizSettings;
typedef enum eAnimViz_RecalcFlags {
 ANIMVIZ_RECALC_PATHS = (1<<0)
} eAnimViz_RecalcFlags;
typedef enum eOnionSkin_Types {
 GHOST_TYPE_NONE = 0,
 GHOST_TYPE_ACFRA,
 GHOST_TYPE_RANGE,
 GHOST_TYPE_KEYS
} eOnionSkin_Types;
typedef enum eOnionSkin_Flag {
 GHOST_FLAG_ONLYSEL = (1<<0)
} eOnionSkin_Flag;
typedef enum eMotionPaths_Types {
 MOTIONPATH_TYPE_RANGE = 0,
 MOTIONPATH_TYPE_ACFRA
} eMotionPath_Types;
typedef enum eMotionPaths_ViewFlag {
 MOTIONPATH_VIEW_FNUMS = (1<<0),
 MOTIONPATH_VIEW_KFRAS = (1<<1),
 MOTIONPATH_VIEW_KFNOS = (1<<2),
 MOTIONPATH_VIEW_KFACT = (1<<3)
} eMotionPath_ViewFlag;
typedef enum eMotionPaths_BakeFlag {
 MOTIONPATH_BAKE_NEEDS_RECALC = (1<<0),
 MOTIONPATH_BAKE_HEADS = (1<<1),
 MOTIONPATH_BAKE_HAS_PATHS = (1<<2)
} eMotionPath_BakeFlag;
typedef struct bPoseChannel {
 struct bPoseChannel *next, *prev;
 IDProperty *prop;
 ListBase constraints;
 char name[64];
 short flag;
 short ikflag;
 short protectflag;
 short agrp_index;
 char constflag;
 char selectflag;
 char pad0[6];
 struct Bone *bone;
 struct bPoseChannel *parent;
 struct bPoseChannel *child;
 struct ListBase iktree;
 struct ListBase siktree;
 bMotionPath *mpath;
 struct Object *custom;
 struct bPoseChannel *custom_tx;
 float loc[3];
 float size[3];
 float eul[3];
 float quat[4];
 float rotAxis[3], rotAngle;
 short rotmode;
 short pad;
 float chan_mat[4][4];
 float pose_mat[4][4];
 float constinv[4][4];
 float pose_head[3];
 float pose_tail[3];
 float limitmin[3], limitmax[3];
 float stiffness[3];
 float ikstretch;
 float ikrotweight;
 float iklinweight;
 void *temp;
} bPoseChannel;
typedef enum ePchan_Flag {
 POSE_LOC = (1<<0),
 POSE_ROT = (1<<1),
 POSE_SIZE = (1<<2),
 POSE_IK_MAT = (1<<3),
 POSE_UNUSED2 = (1<<4),
 POSE_UNUSED3 = (1<<5),
 POSE_UNUSED4 = (1<<6),
 POSE_UNUSED5 = (1<<7),
 POSE_HAS_IK = (1<<8),
 POSE_CHAIN = (1<<9),
 POSE_DONE = (1<<10),
 POSE_KEY = (1<<11),
 POSE_STRIDE = (1<<12),
 POSE_IKTREE = (1<<13),
 POSE_HAS_IKS = (1<<14),
 POSE_IKSPLINE = (1<<15)
} ePchan_Flag;
typedef enum ePchan_ConstFlag {
 PCHAN_HAS_IK = (1<<0),
 PCHAN_HAS_CONST = (1<<1),
 PCHAN_HAS_ACTION = (1<<2),
 PCHAN_HAS_TARGET = (1<<3),
 PCHAN_HAS_STRIDE = (1<<4),
 PCHAN_HAS_SPLINEIK = (1<<5)
} ePchan_ConstFlag;
typedef enum ePchan_IkFlag {
 BONE_IK_NO_XDOF = (1<<0),
 BONE_IK_NO_YDOF = (1<<1),
 BONE_IK_NO_ZDOF = (1<<2),
 BONE_IK_XLIMIT = (1<<3),
 BONE_IK_YLIMIT = (1<<4),
 BONE_IK_ZLIMIT = (1<<5),
 BONE_IK_ROTCTL = (1<<6),
 BONE_IK_LINCTL = (1<<7),
 BONE_IK_NO_XDOF_TEMP = (1<<10),
 BONE_IK_NO_YDOF_TEMP = (1<<11),
 BONE_IK_NO_ZDOF_TEMP = (1<<12)
} ePchan_IkFlag;
typedef enum eRotationModes {
 ROT_MODE_QUAT = 0,
 ROT_MODE_EUL = 1,
 ROT_MODE_XYZ = 1,
 ROT_MODE_XZY,
 ROT_MODE_YXZ,
 ROT_MODE_YZX,
 ROT_MODE_ZXY,
 ROT_MODE_ZYX,
 ROT_MODE_AXISANGLE = -1,
 ROT_MODE_MIN = ROT_MODE_AXISANGLE,
 ROT_MODE_MAX = ROT_MODE_ZYX
} eRotationModes;
typedef struct bPose {
 ListBase chanbase;
 struct GHash *chanhash;
 short flag, pad;
 unsigned int proxy_layer;
 int pad1;
 float ctime;
 float stride_offset[3];
 float cyclic_offset[3];
 ListBase agroups;
 int active_group;
 int iksolver;
 void *ikdata;
 void *ikparam;
 bAnimVizSettings avs;
 char proxy_act_bone[64];
} bPose;
typedef enum ePose_Flags {
 POSE_RECALC = (1<<0),
 POSE_LOCKED = (1<<1),
 POSE_DO_UNLOCK = (1<<2),
 POSE_CONSTRAINTS_TIMEDEPEND = (1<<3),
 POSE_RECALCPATHS = (1<<4),
 POSE_WAS_REBUILT = (1<<5),
 POSE_GAME_ENGINE = (1<<6)
} ePose_Flags;
typedef enum ePose_IKSolverType {
 IKSOLVER_LEGACY = 0,
 IKSOLVER_ITASC
} ePose_IKSolverType;
typedef struct bIKParam {
 int iksolver;
} bIKParam;
typedef struct bItasc {
 int iksolver;
 float precision;
 short numiter;
 short numstep;
 float minstep;
 float maxstep;
 short solver;
 short flag;
 float feedback;
 float maxvel;
 float dampmax;
 float dampeps;
} bItasc;
typedef enum eItasc_Flags {
 ITASC_AUTO_STEP = (1<<0),
 ITASC_INITIAL_REITERATION = (1<<1),
 ITASC_REITERATION = (1<<2),
 ITASC_SIMULATION = (1<<3)
} eItasc_Flags;
typedef enum eItasc_Solver {
 ITASC_SOLVER_SDLS = 0,
 ITASC_SOLVER_DLS
} eItasc_Solver;
typedef struct bActionGroup {
 struct bActionGroup *next, *prev;
 ListBase channels;
 int flag;
 int customCol;
 char name[64];
 ThemeWireColor cs;
} bActionGroup;
typedef enum eActionGroup_Flag {
 AGRP_SELECTED = (1<<0),
 AGRP_ACTIVE = (1<<1),
 AGRP_PROTECTED = (1<<2),
 AGRP_EXPANDED = (1<<3),
 AGRP_MUTED = (1<<4),
 AGRP_NOTVISIBLE = (1<<5),
 AGRP_EXPANDED_G = (1<<6),
 AGRP_TEMP = (1<<30),
 AGRP_MOVED = (1<<31)
} eActionGroup_Flag;
typedef struct bAction {
 ID id;
 ListBase curves;
 ListBase chanbase;
 ListBase groups;
 ListBase markers;
 int flag;
 int active_marker;
 int idroot;
 int pad;
} bAction;
typedef enum eAction_Flags {
 ACT_COLLAPSED = (1<<0),
 ACT_SELECTED = (1<<1),
 ACT_MUTED = (1<<9),
 ACT_PROTECTED = (1<<10),
 ACT_DISABLED = (1<<11)
} eAction_Flags;
typedef struct bDopeSheet {
 ID *source;
 ListBase chanbase;
 struct Group *filter_grp;
 char searchstr[64];
 int filterflag;
 int flag;
 int renameIndex;
 int pad;
} bDopeSheet;
typedef enum eDopeSheet_FilterFlag {
 ADS_FILTER_ONLYSEL = (1<<0),
 ADS_FILTER_ONLYDRIVERS = (1<<1),
 ADS_FILTER_ONLYNLA = (1<<2),
 ADS_FILTER_SELEDIT = (1<<3),
 ADS_FILTER_SUMMARY = (1<<4),
 ADS_FILTER_ONLYOBGROUP = (1<<5),
 ADS_FILTER_NOSHAPEKEYS = (1<<6),
 ADS_FILTER_NOMESH = (1<<7),
 ADS_FILTER_NOOBJ = (1<<8),
 ADS_FILTER_NOLAT = (1<<9),
 ADS_FILTER_NOCAM = (1<<10),
 ADS_FILTER_NOMAT = (1<<11),
 ADS_FILTER_NOLAM = (1<<12),
 ADS_FILTER_NOCUR = (1<<13),
 ADS_FILTER_NOWOR = (1<<14),
 ADS_FILTER_NOSCE = (1<<15),
 ADS_FILTER_NOPART = (1<<16),
 ADS_FILTER_NOMBA = (1<<17),
 ADS_FILTER_NOARM = (1<<18),
 ADS_FILTER_NONTREE = (1<<19),
 ADS_FILTER_NOTEX = (1<<20),
 ADS_FILTER_NOSPK = (1<<21),
 ADS_FILTER_NLA_NOACT = (1<<25),
 ADS_FILTER_INCL_HIDDEN = (1<<26),
 ADS_FILTER_BY_FCU_NAME = (1<<27),
 ADS_FILTER_NOOBDATA = (ADS_FILTER_NOCAM|ADS_FILTER_NOMAT|ADS_FILTER_NOLAM|ADS_FILTER_NOCUR|ADS_FILTER_NOPART|ADS_FILTER_NOARM|ADS_FILTER_NOSPK)
} eDopeSheet_FilterFlag;
typedef enum eDopeSheet_Flag {
 ADS_FLAG_SUMMARY_COLLAPSED = (1<<0),
 ADS_FLAG_SHOW_DBFILTERS = (1<<1)
} eDopeSheet_Flag;
typedef struct SpaceAction {
 struct SpaceLink *next, *prev;
 ListBase regionbase;
 int spacetype;
 float blockscale;
 short blockhandler[8];
 View2D v2d __attribute__ ((deprecated));
 bAction *action;
 bDopeSheet ads;
 char mode, autosnap;
 short flag;
 float timeslide;
} SpaceAction;
typedef enum eSAction_Flag {
 SACTION_MOVING = (1<<0),
 SACTION_SLIDERS = (1<<1),
 SACTION_DRAWTIME = (1<<2),
 SACTION_NOTRANSKEYCULL = (1<<4),
 SACTION_POSEMARKERS_SHOW = (1<<6),
 SACTION_NODRAWGCOLORS = (1<<7),
 SACTION_NODRAWCFRANUM = (1<<8),
 SACTION_TEMP_NEEDCHANSYNC = (1<<9),
 SACTION_NOREALTIMEUPDATES = (1<<10),
 SACTION_MARKERS_MOVE = (1<<11)
} eSAction_Flag;
typedef enum eAnimEdit_Context {
 SACTCONT_ACTION = 0,
 SACTCONT_SHAPEKEY,
 SACTCONT_GPENCIL,
 SACTCONT_DOPESHEET
} eAnimEdit_Context;
typedef enum eAnimEdit_AutoSnap {
 SACTSNAP_OFF = 0,
 SACTSNAP_STEP,
 SACTSNAP_FRAME,
 SACTSNAP_MARKER
} eAnimEdit_AutoSnap;
typedef struct bActionChannel {
 struct bActionChannel *next, *prev;
 bActionGroup *grp;
 struct Ipo *ipo;
 ListBase constraintChannels;
 int flag;
 char name[64];
 int temp;
} bActionChannel;
typedef enum ACHAN_FLAG {
 ACHAN_SELECTED = (1<<0),
 ACHAN_HILIGHTED = (1<<1),
 ACHAN_HIDDEN = (1<<2),
 ACHAN_PROTECTED = (1<<3),
 ACHAN_EXPANDED = (1<<4),
 ACHAN_SHOWIPO = (1<<5),
 ACHAN_SHOWCONS = (1<<6),
 ACHAN_MOVED = (1<<31)
} ACHAN_FLAG;
struct BoundBox;
struct Object;
struct Ipo;
struct Key;
struct Material;
struct VFont;
struct AnimData;
struct SelBox;
struct EditFont;
struct GHash;
typedef struct PathPoint {
 float vec[4];
 float quat[4];
 float radius, weight;
} PathPoint;
typedef struct Path {
 struct PathPoint *data;
 int len;
 float totdist;
} Path;
typedef struct BevList {
 struct BevList *next, *prev;
 int nr, dupe_nr;
 short poly, hole;
} BevList;
typedef struct BevPoint {
 float vec[3], alfa, radius, weight;
 float sina, cosa;
 float dir[3], tan[3], quat[4];
 short split_tag, dupe_tag;
} BevPoint;
typedef struct BezTriple {
 float vec[3][3];
 float alfa, weight, radius;
 short ipo;
 char h1, h2;
 char f1, f2, f3;
 char hide;
} BezTriple;
typedef struct BPoint {
 float vec[4];
 float alfa, weight;
 short f1, hide;
 float radius, pad;
} BPoint;
typedef struct Nurb {
 struct Nurb *next, *prev;
 short type;
 short mat_nr;
 short hide, flag;
 short pntsu, pntsv;
 short resolu, resolv;
 short orderu, orderv;
 short flagu, flagv;
 float *knotsu, *knotsv;
 BPoint *bp;
 BezTriple *bezt;
 short tilt_interp;
 short radius_interp;
 int charidx;
} Nurb;
typedef struct CharInfo {
 short kern;
 short mat_nr;
 char flag;
 char pad;
 short pad2;
} CharInfo;
typedef struct TextBox {
 float x, y, w, h;
} TextBox;
typedef struct EditNurb {
 ListBase nurbs;
 struct GHash *keyindex;
 int shapenr;
 char pad[4];
} EditNurb;
typedef struct Curve {
 ID id;
 struct AnimData *adt;
 struct BoundBox *bb;
 ListBase nurb;
 ListBase disp;
 EditNurb *editnurb;
 struct Object *bevobj, *taperobj, *textoncurve;
 struct Ipo *ipo __attribute__ ((deprecated));
 Path *path;
 struct Key *key;
 struct Material **mat;
 ListBase bev;
 float loc[3];
 float size[3];
 float rot[3];
 short type;
 short texflag;
 short drawflag, twist_mode;
 float twist_smooth, smallcaps_scale;
 int pathlen;
 short pad, totcol;
 short flag, bevresol;
 float width, ext1, ext2;
 short resolu, resolv;
 short resolu_ren, resolv_ren;
 int actnu;
 void *lastsel;
 short len, lines, pos, spacemode;
 float spacing, linedist, shear, fsize, wordspace, ulpos, ulheight;
 float xof, yof;
 float linewidth;
 char *str;
 struct SelBox *selboxes;
 struct EditFont *editfont;
 char family[24];
 struct VFont *vfont;
 struct VFont *vfontb;
 struct VFont *vfonti;
 struct VFont *vfontbi;
 int sepchar;
 float ctime;
 int totbox, actbox;
 struct TextBox *tb;
 int selstart, selend;
 struct CharInfo *strinfo;
 struct CharInfo curinfo;
} Curve;
typedef enum eBezTriple_Handle {
 HD_FREE = 0,
 HD_AUTO,
 HD_VECT,
 HD_ALIGN,
 HD_AUTO_ANIM
} eBezTriple_Handle;
typedef enum eBezTriple_Interpolation {
 BEZT_IPO_CONST = 0,
 BEZT_IPO_LIN,
 BEZT_IPO_BEZ
} eBezTriple_Interpolation;
typedef enum eBezTriple_KeyframeType {
 BEZT_KEYTYPE_KEYFRAME = 0,
 BEZT_KEYTYPE_EXTREME,
 BEZT_KEYTYPE_BREAKDOWN,
 BEZT_KEYTYPE_JITTER,
} eBezTriple_KeyframeType;
typedef struct FModifier {
 struct FModifier *next, *prev;
 void *data;
 void *edata;
 char name[64];
 short type;
 short flag;
 float influence;
 float sfra;
 float efra;
 float blendin;
 float blendout;
} FModifier;
typedef enum eFModifier_Types {
 FMODIFIER_TYPE_NULL = 0,
 FMODIFIER_TYPE_GENERATOR,
 FMODIFIER_TYPE_FN_GENERATOR,
 FMODIFIER_TYPE_ENVELOPE,
 FMODIFIER_TYPE_CYCLES,
 FMODIFIER_TYPE_NOISE,
 FMODIFIER_TYPE_FILTER,
 FMODIFIER_TYPE_PYTHON,
 FMODIFIER_TYPE_LIMITS,
 FMODIFIER_TYPE_STEPPED,
 FMODIFIER_NUM_TYPES
} eFModifier_Types;
typedef enum eFModifier_Flags {
 FMODIFIER_FLAG_DISABLED = (1<<0),
 FMODIFIER_FLAG_EXPANDED = (1<<1),
 FMODIFIER_FLAG_ACTIVE = (1<<2),
 FMODIFIER_FLAG_MUTED = (1<<3),
 FMODIFIER_FLAG_RANGERESTRICT = (1<<4),
 FMODIFIER_FLAG_USEINFLUENCE = (1<<5)
} eFModifier_Flags;
typedef struct FMod_Generator {
 float *coefficients;
 unsigned int arraysize;
 int poly_order;
 int mode;
 int flag;
} FMod_Generator;
typedef enum eFMod_Generator_Modes {
 FCM_GENERATOR_POLYNOMIAL = 0,
 FCM_GENERATOR_POLYNOMIAL_FACTORISED
} eFMod_Generator_Modes;
typedef enum eFMod_Generator_Flags {
 FCM_GENERATOR_ADDITIVE = (1<<0)
} eFMod_Generator_Flags;
typedef struct FMod_FunctionGenerator {
 float amplitude;
 float phase_multiplier;
 float phase_offset;
 float value_offset;
 int type;
 int flag;
} FMod_FunctionGenerator;
typedef enum eFMod_Generator_Functions {
 FCM_GENERATOR_FN_SIN = 0,
 FCM_GENERATOR_FN_COS,
 FCM_GENERATOR_FN_TAN,
 FCM_GENERATOR_FN_SQRT,
 FCM_GENERATOR_FN_LN,
 FCM_GENERATOR_FN_SINC
} eFMod_Generator_Functions;
typedef struct FCM_EnvelopeData {
 float min, max;
 float time;
 short f1;
 short f2;
} FCM_EnvelopeData;
typedef struct FMod_Envelope {
 FCM_EnvelopeData *data;
 int totvert;
 float midval;
 float min, max;
} FMod_Envelope;
typedef struct FMod_Cycles {
 short before_mode;
 short after_mode;
 short before_cycles;
 short after_cycles;
} FMod_Cycles;
typedef enum eFMod_Cycling_Modes {
 FCM_EXTRAPOLATE_NONE = 0,
 FCM_EXTRAPOLATE_CYCLIC,
 FCM_EXTRAPOLATE_CYCLIC_OFFSET,
 FCM_EXTRAPOLATE_MIRROR
} eFMod_Cycling_Modes;
typedef struct FMod_Python {
 struct Text *script;
 IDProperty *prop;
} FMod_Python;
typedef struct FMod_Limits {
 rctf rect;
 int flag;
 int pad;
} FMod_Limits;
typedef enum eFMod_Limit_Flags {
 FCM_LIMIT_XMIN = (1<<0),
 FCM_LIMIT_XMAX = (1<<1),
 FCM_LIMIT_YMIN = (1<<2),
 FCM_LIMIT_YMAX = (1<<3)
} eFMod_Limit_Flags;
typedef struct FMod_Noise {
 float size;
 float strength;
 float phase;
 float pad;
 short depth;
 short modification;
} FMod_Noise;
typedef enum eFMod_Noise_Modifications {
 FCM_NOISE_MODIF_REPLACE = 0,
 FCM_NOISE_MODIF_ADD,
 FCM_NOISE_MODIF_SUBTRACT,
 FCM_NOISE_MODIF_MULTIPLY
} eFMod_Noise_Modifications;
typedef struct FMod_Stepped {
 float step_size;
 float offset;
 float start_frame;
 float end_frame;
 int flag;
} FMod_Stepped;
typedef enum eFMod_Stepped_Flags {
 FCM_STEPPED_NO_BEFORE = (1<<0),
 FCM_STEPPED_NO_AFTER = (1<<1),
} eFMod_Stepped_Flags;
typedef struct DriverTarget {
 ID *id;
 char *rna_path;
 char pchan_name[32];
 short transChan;
 short flag;
 int idtype;
} DriverTarget;
typedef enum eDriverTarget_Flag {
 DTAR_FLAG_STRUCT_REF = (1<<0),
 DTAR_FLAG_ID_OB_ONLY = (1<<1),
 DTAR_FLAG_LOCALSPACE = (1<<2),
 DTAR_FLAG_LOCAL_CONSTS = (1<<3),
} eDriverTarget_Flag;
typedef enum eDriverTarget_TransformChannels {
 DTAR_TRANSCHAN_LOCX = 0,
 DTAR_TRANSCHAN_LOCY,
 DTAR_TRANSCHAN_LOCZ,
 DTAR_TRANSCHAN_ROTX,
 DTAR_TRANSCHAN_ROTY,
 DTAR_TRANSCHAN_ROTZ,
 DTAR_TRANSCHAN_SCALEX,
 DTAR_TRANSCHAN_SCALEY,
 DTAR_TRANSCHAN_SCALEZ,
 MAX_DTAR_TRANSCHAN_TYPES
} eDriverTarget_TransformChannels;
typedef struct DriverVar {
 struct DriverVar *next, *prev;
 char name[64];
 DriverTarget targets[8];
 short num_targets;
 short type;
 float curval;
} DriverVar;
typedef enum eDriverVar_Types {
 DVAR_TYPE_SINGLE_PROP = 0,
 DVAR_TYPE_ROT_DIFF,
 DVAR_TYPE_LOC_DIFF,
 DVAR_TYPE_TRANSFORM_CHAN,
 MAX_DVAR_TYPES
} eDriverVar_Types;
typedef struct ChannelDriver {
 ListBase variables;
 char expression[256];
 void *expr_comp;
 float curval;
 float influence;
 int type;
 int flag;
} ChannelDriver;
typedef enum eDriver_Types {
 DRIVER_TYPE_AVERAGE = 0,
 DRIVER_TYPE_PYTHON,
 DRIVER_TYPE_SUM,
 DRIVER_TYPE_MIN,
 DRIVER_TYPE_MAX
} eDriver_Types;
typedef enum eDriver_Flags {
 DRIVER_FLAG_INVALID = (1<<0),
 DRIVER_FLAG_RECALC = (1<<1),
 DRIVER_FLAG_RECOMPILE = (1<<3),
 DRIVER_FLAG_RENAMEVAR = (1<<4),
 DRIVER_FLAG_SHOWDEBUG = (1<<5)
} eDriver_Flags;
typedef struct FPoint {
 float vec[2];
 int flag;
 int pad;
} FPoint;
typedef struct FCurve {
 struct FCurve *next, *prev;
 bActionGroup *grp;
 ChannelDriver *driver;
 ListBase modifiers;
 BezTriple *bezt;
 FPoint *fpt;
 unsigned int totvert;
 float curval;
 short flag;
 short extend;
 int array_index;
 char *rna_path;
 int color_mode;
 float color[3];
} FCurve;
typedef enum eFCurve_Flags {
 FCURVE_VISIBLE = (1<<0),
 FCURVE_SELECTED = (1<<1),
 FCURVE_ACTIVE = (1<<2),
 FCURVE_PROTECTED = (1<<3),
 FCURVE_MUTED = (1<<4),
 FCURVE_AUTO_HANDLES = (1<<5),
 FCURVE_DISABLED = (1<<10),
 FCURVE_INT_VALUES = (1<<11),
 FCURVE_DISCRETE_VALUES = (1<<12),
 FCURVE_TAGGED = (1<<15)
} eFCurve_Flags;
typedef enum eFCurve_Extend {
 FCURVE_EXTRAPOLATE_CONSTANT = 0,
 FCURVE_EXTRAPOLATE_LINEAR
} eFCurve_Extend;
typedef enum eFCurve_Coloring {
 FCURVE_COLOR_AUTO_RAINBOW = 0,
 FCURVE_COLOR_AUTO_RGB,
 FCURVE_COLOR_CUSTOM
} eFCurve_Coloring;
typedef struct AnimMapPair {
 char from[128];
 char to[128];
} AnimMapPair;
typedef struct AnimMapper {
 struct AnimMapper *next, *prev;
 bAction *target;
 ListBase mappings;
} AnimMapper;
typedef struct NlaStrip {
 struct NlaStrip *next, *prev;
 ListBase strips;
 bAction *act;
 AnimMapper *remap;
 ListBase fcurves;
 ListBase modifiers;
 char name[64];
 float influence;
 float strip_time;
 float start, end;
 float actstart, actend;
 float repeat;
 float scale;
 float blendin, blendout;
 short blendmode;
 short extendmode;
 short pad1;
 short type;
 void *speaker_handle;
 int flag;
 int pad2;
} NlaStrip;
typedef enum eNlaStrip_Blend_Mode {
 NLASTRIP_MODE_REPLACE = 0,
 NLASTRIP_MODE_ADD,
 NLASTRIP_MODE_SUBTRACT,
 NLASTRIP_MODE_MULTIPLY
} eNlaStrip_Blend_Mode;
typedef enum eNlaStrip_Extrapolate_Mode {
 NLASTRIP_EXTEND_HOLD = 0,
 NLASTRIP_EXTEND_HOLD_FORWARD,
 NLASTRIP_EXTEND_NOTHING
} eNlaStrip_Extrapolate_Mode;
typedef enum eNlaStrip_Flag {
 NLASTRIP_FLAG_ACTIVE = (1<<0),
 NLASTRIP_FLAG_SELECT = (1<<1),
 NLASTRIP_FLAG_TWEAKUSER = (1<<4),
 NLASTRIP_FLAG_USR_INFLUENCE = (1<<5),
 NLASTRIP_FLAG_USR_TIME = (1<<6),
 NLASTRIP_FLAG_USR_TIME_CYCLIC = (1<<7),
 NLASTRIP_FLAG_SYNC_LENGTH = (1<<9),
 NLASTRIP_FLAG_AUTO_BLENDS = (1<<10),
 NLASTRIP_FLAG_REVERSE = (1<<11),
 NLASTRIP_FLAG_MUTED = (1<<12),
 NLASTRIP_FLAG_MIRROR = (1<<13),
 NLASTRIP_FLAG_TEMP_META = (1<<30),
 NLASTRIP_FLAG_EDIT_TOUCHED = (1<<31)
} eNlaStrip_Flag;
typedef enum eNlaStrip_Type {
 NLASTRIP_TYPE_CLIP = 0,
 NLASTRIP_TYPE_TRANSITION,
 NLASTRIP_TYPE_META,
 NLASTRIP_TYPE_SOUND
} eNlaStrip_Type;
typedef struct NlaTrack {
 struct NlaTrack *next, *prev;
 ListBase strips;
 int flag;
 int index;
 char name[64];
} NlaTrack;
typedef enum eNlaTrack_Flag {
 NLATRACK_ACTIVE = (1<<0),
 NLATRACK_SELECTED = (1<<1),
 NLATRACK_MUTED = (1<<2),
 NLATRACK_SOLO = (1<<3),
 NLATRACK_PROTECTED = (1<<4),
 NLATRACK_DISABLED = (1<<10)
} eNlaTrack_Flag;
typedef struct KS_Path {
 struct KS_Path *next, *prev;
 ID *id;
 char group[64];
 int idtype;
 short groupmode;
 short pad;
 char *rna_path;
 int array_index;
 short flag;
 short keyingflag;
} KS_Path;
typedef enum eKSP_Settings {
 KSP_FLAG_WHOLE_ARRAY = (1<<0)
} eKSP_Settings;
typedef enum eKSP_Grouping {
 KSP_GROUP_NAMED = 0,
 KSP_GROUP_NONE,
 KSP_GROUP_KSNAME,
 KSP_GROUP_TEMPLATE_ITEM
} eKSP_Grouping;
typedef struct KeyingSet {
 struct KeyingSet *next, *prev;
 ListBase paths;
 char name[64];
 char typeinfo[64];
 short flag;
 short keyingflag;
 int active_path;
} KeyingSet;
typedef enum eKS_Settings {
 KEYINGSET_BUILTIN = (1<<0),
 KEYINGSET_ABSOLUTE = (1<<1)
} eKS_Settings;
typedef enum eInsertKeyFlags {
 INSERTKEY_NEEDED = (1<<0),
 INSERTKEY_MATRIX = (1<<1),
 INSERTKEY_FAST = (1<<2),
 INSERTKEY_FASTR = (1<<3),
 INSERTKEY_REPLACE = (1<<4),
 INSERTKEY_XYZ2RGB = (1<<5)
} eInsertKeyFlags;
typedef struct AnimOverride {
 struct AnimOverride *next, *prev;
 char *rna_path;
 int array_index;
 float value;
} AnimOverride;
typedef struct AnimData {
 bAction *action;
 bAction *tmpact;
 AnimMapper *remap;
 ListBase nla_tracks;
 NlaStrip *actstrip;
 ListBase drivers;
 ListBase overrides;
 int flag;
 int recalc;
 short act_blendmode;
 short act_extendmode;
 float act_influence;
} AnimData;
typedef enum eAnimData_Flag {
 ADT_NLA_SOLO_TRACK = (1<<0),
 ADT_NLA_EVAL_OFF = (1<<1),
 ADT_NLA_EDIT_ON = (1<<2),
 ADT_NLA_EDIT_NOMAP = (1<<3),
 ADT_NLA_SKEYS_COLLAPSED = (1<<4),
 ADT_DRIVERS_COLLAPSED = (1<<10),
 ADT_DRIVERS_DISABLED = (1<<11),
 ADT_UI_SELECTED = (1<<14),
 ADT_UI_ACTIVE = (1<<15),
 ADT_CURVES_NOT_VISIBLE = (1<<16)
} eAnimData_Flag;
typedef enum eAnimData_Recalc {
 ADT_RECALC_DRIVERS = (1<<0),
 ADT_RECALC_ANIM = (1<<1),
 ADT_RECALC_ALL = (ADT_RECALC_DRIVERS|ADT_RECALC_ANIM)
} eAnimData_Recalc;
typedef struct IdAdtTemplate {
 ID id;
 AnimData *adt;
} IdAdtTemplate;
struct AnimData;
struct Ipo;
typedef struct KeyBlock {
 struct KeyBlock *next, *prev;
 float pos;
 float curval;
 short type, adrcode, relative, flag;
 int totelem, pad2;
 void *data;
 float *weights;
 char name[64];
 char vgroup[64];
 float slidermin;
 float slidermax;
} KeyBlock;
typedef struct Key {
 ID id;
 struct AnimData *adt;
 KeyBlock *refkey;
 char elemstr[64];
 int elemsize;
 float curval __attribute__ ((deprecated));
 ListBase block;
 struct Ipo *ipo __attribute__ ((deprecated));
 ID *from;
 short type, totkey;
 short slurph, flag;
} Key;
struct AnimData;
struct BPoint;
struct Ipo;
struct Key;
struct MDeformVert;
typedef struct EditLatt {
 struct Lattice *latt;
 int shapenr;
 char pad[4];
} EditLatt;
typedef struct Lattice {
 ID id;
 struct AnimData *adt;
 short pntsu, pntsv, pntsw, flag;
 short opntsu, opntsv, opntsw, pad2;
 char typeu, typev, typew, pad3;
 int pad;
 float fu, fv, fw, du, dv, dw;
 struct BPoint *def;
 struct Ipo *ipo __attribute__ ((deprecated));
 struct Key *key;
 struct MDeformVert *dvert;
 char vgroup[64];
 float *latticedata;
 float latmat[4][4];
 struct EditLatt *editlatt;
} Lattice;
struct Bone;
struct Image;
typedef struct MFace {
 unsigned int v1, v2, v3, v4;
 short mat_nr;
 char edcode, flag;
} MFace;
typedef struct MEdge {
 unsigned int v1, v2;
 char crease, bweight;
 short flag;
} MEdge;
typedef struct MDeformWeight {
 int def_nr;
 float weight;
} MDeformWeight;
typedef struct MDeformVert {
 struct MDeformWeight *dw;
 int totweight;
 int flag;
} MDeformVert;
typedef struct MVert {
 float co[3];
 short no[3];
 char flag, bweight;
} MVert;
typedef struct MCol {
 char a, r, g, b;
} MCol;
typedef struct MPoly {
 int loopstart;
 int totloop;
 short mat_nr;
 char flag, pad;
} MPoly;
typedef struct MLoop {
 unsigned int v;
 unsigned int e;
} MLoop;
typedef struct MTexPoly {
 struct Image *tpage;
 char flag, transp;
 short mode,tile,unwrap;
} MTexPoly;
typedef struct MLoopUV {
 float uv[2];
} MLoopUV;
typedef struct MLoopCol {
 char a, r, g, b;
} MLoopCol;
typedef struct MSticky {
 float co[2];
} MSticky;
typedef struct MSelect {
 int index;
 int type;
} MSelect;
typedef struct MTFace {
 float uv[4][2];
 struct Image *tpage;
 char flag, transp;
 short mode, tile, unwrap;
} MTFace;
typedef struct MFloatProperty {
 float f;
} MFloatProperty;
typedef struct MIntProperty {
 int i;
} MIntProperty;
typedef struct MStringProperty {
 char s[256];
} MStringProperty;
typedef struct OrigSpaceFace {
 float uv[4][2];
} OrigSpaceFace;
typedef struct MDisps {
 int totdisp;
 char pad[4];
 float (*disps)[3];
} MDisps;
typedef struct MultiresCol {
 float a, r, g, b;
} MultiresCol;
typedef struct MultiresColFace {
 MultiresCol col[4];
} MultiresColFace;
typedef struct MultiresFace {
 unsigned int v[4];
 unsigned int mid;
 char flag, mat_nr, pad[2];
} MultiresFace;
typedef struct MultiresEdge {
 unsigned int v[2];
 unsigned int mid;
} MultiresEdge;
struct MultiresMapNode;
typedef struct MultiresLevel {
 struct MultiresLevel *next, *prev;
 MultiresFace *faces;
 MultiresColFace *colfaces;
 MultiresEdge *edges;
 unsigned int totvert, totface, totedge, pad;
 MVert *verts;
} MultiresLevel;
typedef struct Multires {
 ListBase levels;
 MVert *verts;
 unsigned char level_count, current, newlvl, edgelvl, pinlvl, renderlvl;
 unsigned char use_col, flag;
 CustomData vdata;
 CustomData fdata;
 short *edge_flags;
 char *edge_creases;
} Multires;
typedef struct MRecast {
 int i;
} MRecast;
struct Object;
struct AnimData;
struct Ipo;
struct BoundBox;
struct Path;
struct Material;
struct bConstraintChannel;
struct PartDeflect;
struct SoftBody;
struct FluidsimSettings;
struct ParticleSystem;
struct DerivedMesh;
struct SculptSession;
struct bGPdata;
typedef struct bDeformGroup {
 struct bDeformGroup *next, *prev;
 char name[64];
 char flag, pad[7];
} bDeformGroup;
typedef struct BoundBox {
 float vec[8][3];
 int flag, pad;
} BoundBox;
typedef struct Object {
 ID id;
 struct AnimData *adt;
 struct SculptSession *sculpt;
 short type, partype;
 int par1, par2, par3;
 char parsubstr[64];
 struct Object *parent, *track;
 struct Object *proxy, *proxy_group, *proxy_from;
 struct Ipo *ipo __attribute__ ((deprecated));
 struct BoundBox *bb;
 struct bAction *action __attribute__ ((deprecated));
 struct bAction *poselib;
 struct bPose *pose;
 void *data;
 struct bGPdata *gpd;
 bAnimVizSettings avs;
 bMotionPath *mpath;
 ListBase constraintChannels __attribute__ ((deprecated));
 ListBase effect __attribute__ ((deprecated));
 ListBase disp;
 ListBase defbase;
 ListBase modifiers;
 int mode;
 int restore_mode;
 struct Material **mat;
 char *matbits;
 int totcol;
 int actcol;
 float loc[3], dloc[3], orig[3];
 float size[3];
 float dsize[3] __attribute__ ((deprecated)) ;
 float dscale[3];
 float rot[3], drot[3];
 float quat[4], dquat[4];
 float rotAxis[3], drotAxis[3];
 float rotAngle, drotAngle;
 float obmat[4][4];
 float parentinv[4][4];
 float constinv[4][4];
 float imat[4][4];
 float imat_ren[4][4];
 unsigned int lay;
 int pad6;
 short flag;
 short colbits __attribute__ ((deprecated));
 short transflag, protectflag;
 short trackflag, upflag;
 short nlaflag;
 short ipoflag;
 short scaflag;
 char scavisflag;
 char pad5;
 int dupon, dupoff, dupsta, dupend;
 float sf, ctime;
 float mass, damping, inertia;
 float formfactor;
 float rdamping, sizefac;
 float margin;
 float max_vel;
 float min_vel;
 float m_contactProcessingThreshold;
 float obstacleRad;
 short rotmode;
 char boundtype;
 char collision_boundtype;
 char restrictflag;
 char dt;
 char dtx;
 char empty_drawtype;
 float empty_drawsize;
 float dupfacesca;
 ListBase prop;
 ListBase sensors;
 ListBase controllers;
 ListBase actuators;
 float bbsize[3] __attribute__ ((deprecated));
 short index;
 unsigned short actdef;
 float col[4];
 int gameflag;
 int gameflag2;
 struct BulletSoftBody *bsoft;
 short softflag;
 short recalc;
 float anisotropicFriction[3];
 ListBase constraints;
 ListBase nlastrips __attribute__ ((deprecated));
 ListBase hooks __attribute__ ((deprecated));
 ListBase particlesystem;
 struct PartDeflect *pd;
 struct SoftBody *soft;
 struct Group *dup_group;
 char body_type;
 char shapeflag;
 short shapenr;
 float smoothresh;
 struct FluidsimSettings *fluidsimSettings;
 struct DerivedMesh *derivedDeform, *derivedFinal;
 uint64_t lastDataMask;
 uint64_t customdata_mask;
 unsigned int state;
 unsigned int init_state;
 ListBase gpulamp;
 ListBase pc_ids;
 ListBase *duplilist;
 float ima_ofs[2];
} Object;
typedef struct ObHook {
 struct ObHook *next, *prev;
 struct Object *parent;
 float parentinv[4][4];
 float mat[4][4];
 float cent[3];
 float falloff;
 char name[64];
 int *indexar;
 int totindex, curindex;
 short type, active;
 float force;
} ObHook;
typedef struct DupliObject {
 struct DupliObject *next, *prev;
 struct Object *ob;
 unsigned int origlay;
 int index;
 float mat[4][4], omat[4][4];
 float orco[3], uv[2];
 short type;
 char no_draw, animated;
} DupliObject;
typedef enum ObjectMode {
 OB_MODE_OBJECT = 0,
 OB_MODE_EDIT = 1,
 OB_MODE_SCULPT = 2,
 OB_MODE_VERTEX_PAINT = 4,
 OB_MODE_WEIGHT_PAINT = 8,
 OB_MODE_TEXTURE_PAINT = 16,
 OB_MODE_PARTICLE_EDIT = 32,
 OB_MODE_POSE = 64
} ObjectMode;
struct Object;
struct Brush;
struct World;
struct Scene;
struct Image;
struct Group;
struct Text;
struct bNodeTree;
struct AnimData;
struct Editing;
struct SceneStats;
struct bGPdata;
struct MovieClip;
typedef struct Base {
 struct Base *next, *prev;
 unsigned int lay, selcol;
 int flag;
 short sx, sy;
 struct Object *object;
} Base;
typedef struct AviCodecData {
 void *lpFormat;
 void *lpParms;
 unsigned int cbFormat;
 unsigned int cbParms;
 unsigned int fccType;
 unsigned int fccHandler;
 unsigned int dwKeyFrameEvery;
 unsigned int dwQuality;
 unsigned int dwBytesPerSecond;
 unsigned int dwFlags;
 unsigned int dwInterleaveEvery;
 unsigned int pad;
 char avicodecname[128];
} AviCodecData;
typedef struct QuicktimeCodecData {
 void *cdParms;
 void *pad;
 unsigned int cdSize;
 unsigned int pad2;
 char qtcodecname[128];
} QuicktimeCodecData;
typedef struct QuicktimeCodecSettings {
 int codecType;
 int codecSpatialQuality;
 int codec;
 int codecFlags;
 int colorDepth;
 int codecTemporalQuality;
 int minSpatialQuality;
 int minTemporalQuality;
 int keyFrameRate;
 int bitRate;
 int audiocodecType;
 int audioSampleRate;
 short audioBitDepth;
 short audioChannels;
 int audioCodecFlags;
 int audioBitRate;
 int pad1;
} QuicktimeCodecSettings;
typedef struct FFMpegCodecData {
 int type;
 int codec;
 int audio_codec;
 int video_bitrate;
 int audio_bitrate;
 int audio_mixrate;
 int audio_channels;
 int audio_pad;
 float audio_volume;
 int gop_size;
 int flags;
 int rc_min_rate;
 int rc_max_rate;
 int rc_buffer_size;
 int mux_packet_size;
 int mux_rate;
 IDProperty *properties;
} FFMpegCodecData;
typedef struct AudioData {
 int mixrate;
 float main;
 float speed_of_sound;
 float doppler_factor;
 int distance_model;
 short flag;
 short pad;
 float volume;
 float pad2;
} AudioData;
typedef struct SceneRenderLayer {
 struct SceneRenderLayer *next, *prev;
 char name[64];
 struct Material *mat_override;
 struct Group *light_override;
 unsigned int lay;
 unsigned int lay_zmask;
 int layflag;
 int pad;
 int passflag;
 int pass_xor;
} SceneRenderLayer;
typedef struct ImageFormatData {
 char imtype;
 char depth;
 char planes ;
 char flag;
 char quality;
 char compress;
 char exr_codec;
 char cineon_flag;
 short cineon_white, cineon_black;
 float cineon_gamma;
 char jp2_flag;
 char pad[7];
} ImageFormatData;
typedef struct RenderData {
 struct ImageFormatData im_format;
 struct AviCodecData *avicodecdata;
 struct QuicktimeCodecData *qtcodecdata;
 struct QuicktimeCodecSettings qtcodecsettings;
 struct FFMpegCodecData ffcodecdata;
 int cfra, sfra, efra;
 float subframe;
 int psfra, pefra;
 int images, framapto;
 short flag, threads;
 float framelen, blurfac;
 float edgeR, edgeG, edgeB;
 short fullscreen __attribute__ ((deprecated)), xplay __attribute__ ((deprecated)), yplay __attribute__ ((deprecated));
 short freqplay __attribute__ ((deprecated));
 short depth __attribute__ ((deprecated)), attrib __attribute__ ((deprecated));
 int frame_step;
 short stereomode __attribute__ ((deprecated));
 short dimensionspreset;
 short filtertype;
 short size, maximsize;
 short xsch;
 short ysch;
 short xparts;
 short yparts;
 short planes __attribute__ ((deprecated)), imtype __attribute__ ((deprecated)), subimtype __attribute__ ((deprecated)), quality __attribute__ ((deprecated));
 short displaymode;
 int scemode;
 int mode;
 int raytrace_options;
 short raytrace_structure;
 short pad1;
 short ocres;
 short pad4;
 short alphamode;
 short osa;
 short frs_sec, edgeint;
 rctf safety, border;
 rcti disprect;
 ListBase layers;
 short actlay;
 short mblur_samples;
 float xasp, yasp;
 float frs_sec_base;
 float gauss;
 int color_mgt_flag;
 float postgamma, posthue, postsat;
 float dither_intensity;
 short bake_osa, bake_filter, bake_mode, bake_flag;
 short bake_normal_space, bake_quad_split;
 float bake_maxdist, bake_biasdist, bake_pad;
 char pic[1024];
 int stamp;
 short stamp_font_id, pad3;
 char stamp_udata[768];
 float fg_stamp[4];
 float bg_stamp[4];
 char seq_prev_type;
 char seq_rend_type;
 char seq_flag;
 char pad5[5];
 int simplify_flag;
 short simplify_subsurf;
 short simplify_shadowsamples;
 float simplify_particles;
 float simplify_aosss;
 short cineonwhite __attribute__ ((deprecated)), cineonblack __attribute__ ((deprecated));
 float cineongamma __attribute__ ((deprecated));
 short jp2_preset __attribute__ ((deprecated)), jp2_depth __attribute__ ((deprecated));
 int rpad3;
 short domeres __attribute__ ((deprecated)), domemode __attribute__ ((deprecated));
 short domeangle __attribute__ ((deprecated)), dometilt __attribute__ ((deprecated));
 float domeresbuf __attribute__ ((deprecated));
 float pad2;
 struct Text *dometext __attribute__ ((deprecated));
 char engine[32];
} RenderData;
typedef struct RenderProfile {
 struct RenderProfile *next, *prev;
 char name[32];
 short particle_perc;
 short subsurf_max;
 short shadbufsample_max;
 short pad1;
 float ao_error, pad2;
} RenderProfile;
typedef struct GameDome {
 short res, mode;
 short angle, tilt;
 float resbuf, pad2;
 struct Text *warptext;
} GameDome;
typedef struct GameFraming {
 float col[3];
 char type, pad1, pad2, pad3;
} GameFraming;
typedef struct RecastData {
 float cellsize;
 float cellheight;
 float agentmaxslope;
 float agentmaxclimb;
 float agentheight;
 float agentradius;
 float edgemaxlen;
 float edgemaxerror;
 float regionminsize;
 float regionmergesize;
 int vertsperpoly;
 float detailsampledist;
 float detailsamplemaxerror;
 short pad1, pad2;
} RecastData;
typedef struct GameData {
 struct GameFraming framing;
 short playerflag, xplay, yplay, freqplay;
 short depth, attrib, rt1, rt2;
 short aasamples, pad4[3];
 struct GameDome dome;
 short stereoflag, stereomode;
 float eyeseparation;
 RecastData recastData;
 float gravity;
 float activityBoxRadius;
 int flag;
 short mode, matmode;
 short occlusionRes;
 short physicsEngine;
 short exitkey, pad;
 short ticrate, maxlogicstep, physubstep, maxphystep;
 short obstacleSimulation, pad1;
 float levelHeight;
} GameData;
typedef struct TimeMarker {
 struct TimeMarker *next, *prev;
 int frame;
 char name[64];
 unsigned int flag;
 struct Object *camera;
} TimeMarker;
typedef struct Paint {
 struct Brush *brush;
 void *paint_cursor;
 unsigned char paint_cursor_col[4];
 int flags;
} Paint;
typedef struct ImagePaintSettings {
 Paint paint;
 short flag, pad;
 short seam_bleed, normal_angle;
 short screen_grab_size[2];
 int pad1;
 void *paintcursor;
} ImagePaintSettings;
typedef struct ParticleBrushData {
 short size;
 short step, invert, count;
 int flag;
 float strength;
} ParticleBrushData;
typedef struct ParticleEditSettings {
 short flag;
 short totrekey;
 short totaddkey;
 short brushtype;
 ParticleBrushData brush[7];
 void *paintcursor;
 float emitterdist, rt;
 int selectmode;
 int edittype;
 int draw_step, fade_frames;
 struct Scene *scene;
 struct Object *object;
} ParticleEditSettings;
typedef struct Sculpt {
 Paint paint;
 int flags;
 int radial_symm[3];
 float last_x, last_y;
 float last_angle;
 int draw_anchored;
 int anchored_size;
 float anchored_location[3];
 float anchored_initial_mouse[2];
 int draw_pressure;
 float pressure_value;
 float special_rotation;
 int pad;
} Sculpt;
typedef struct UvSculpt {
 Paint paint;
} UvSculpt;
typedef struct VPaint {
 Paint paint;
 short flag, pad;
 int tot;
 unsigned int *vpaint_prev;
 struct MDeformVert *wpaint_prev;
 void *paintcursor;
} VPaint;
typedef struct TransformOrientation {
 struct TransformOrientation *next, *prev;
 char name[64];
 float mat[3][3];
 int pad;
} TransformOrientation;
typedef struct UnifiedPaintSettings {
 int size;
 float unprojected_radius;
 float alpha;
 int flag;
} UnifiedPaintSettings;
typedef enum {
 UNIFIED_PAINT_SIZE = (1<<0),
 UNIFIED_PAINT_ALPHA = (1<<1),
 UNIFIED_PAINT_BRUSH_LOCK_SIZE = (1<<2),
 UNIFIED_PAINT_BRUSH_SIZE_PRESSURE = (1<<3),
 UNIFIED_PAINT_BRUSH_ALPHA_PRESSURE = (1<<4)
} UnifiedPaintSettingsFlags;
typedef struct ToolSettings {
 VPaint *vpaint;
 VPaint *wpaint;
 Sculpt *sculpt;
 UvSculpt *uvsculpt;
 float vgroup_weight;
 short cornertype;
 short editbutflag;
 float jointrilimit;
 float degr;
 short step;
 short turn;
 float extr_offs;
 float doublimit;
 float normalsize;
 short automerge;
 short selectmode;
 short segments;
 short rings;
 short vertices;
 short unwrapper;
 float uvcalc_radius;
 float uvcalc_cubesize;
 float uvcalc_margin;
 short uvcalc_mapdir;
 short uvcalc_mapalign;
 short uvcalc_flag;
 short uv_flag, uv_selectmode;
 short uv_subsurf_level;
 short gpencil_flags;
 short autoik_chainlen;
 struct ImagePaintSettings imapaint;
 struct ParticleEditSettings particle;
 float proportional_size;
 float select_thresh;
 float clean_thresh;
 short autokey_mode, autokey_flag;
 char multires_subdiv_type;
 char pad2[5];
 short skgen_resolution;
 float skgen_threshold_internal;
 float skgen_threshold_external;
 float skgen_length_ratio;
 float skgen_length_limit;
 float skgen_angle_limit;
 float skgen_correlation_limit;
 float skgen_symmetry_limit;
 float skgen_retarget_angle_weight;
 float skgen_retarget_length_weight;
 float skgen_retarget_distance_weight;
 short skgen_options;
 char skgen_postpro;
 char skgen_postpro_passes;
 char skgen_subdivisions[3];
 char skgen_multi_level;
 struct Object *skgen_template;
 char bone_sketching;
 char bone_sketching_convert;
 char skgen_subdivision_number;
 char skgen_retarget_options;
 char skgen_retarget_roll;
 char skgen_side_string[8];
 char skgen_num_string[8];
 char edge_mode;
 char edge_mode_live_unwrap;
 char snap_mode;
 short snap_flag, snap_target;
 short proportional, prop_mode;
 char proportional_objects;
 char pad[5];
 char auto_normalize;
 char multipaint;
 int use_uv_sculpt;
 int uv_sculpt_settings;
 int uv_sculpt_tool;
 int uv_relax_method;
 short sculpt_paint_settings __attribute__ ((deprecated)); short pad1;
 int sculpt_paint_unified_size __attribute__ ((deprecated));
 float sculpt_paint_unified_unprojected_radius __attribute__ ((deprecated));
 float sculpt_paint_unified_alpha __attribute__ ((deprecated));
 struct UnifiedPaintSettings unified_paint_settings;
} ToolSettings;
typedef struct bStats {
 int totobj, totlamp, totobjsel, totcurve, totmesh, totarmature;
 int totvert, totface;
} bStats;
typedef struct UnitSettings {
 float scale_length;
 char system;
 char system_rotation;
 short flag;
} UnitSettings;
typedef struct PhysicsSettings {
 float gravity[3];
 int flag, quick_cache_step, rt;
} PhysicsSettings;
typedef struct Scene {
 ID id;
 struct AnimData *adt;
 struct Object *camera;
 struct World *world;
 struct Scene *set;
 ListBase base;
 struct Base *basact;
 struct Object *obedit;
 float cursor[3];
 float twcent[3];
 float twmin[3], twmax[3];
 unsigned int lay;
 int layact;
 unsigned int lay_updated;
 short flag;
 short use_nodes;
 struct bNodeTree *nodetree;
 struct Editing *ed;
 struct ToolSettings *toolsettings;
 struct SceneStats *stats;
 struct RenderData r;
 struct AudioData audio;
 ListBase markers;
 ListBase transform_spaces;
 void *sound_scene;
 void *sound_scene_handle;
 void *sound_scrub_handle;
 void *speaker_handles;
 void *fps_info;
 struct DagForest *theDag;
 short dagisvalid, dagflags;
 short recalc;
 short pad6;
 int pad5;
 int active_keyingset;
 ListBase keyingsets;
 struct GameFraming framing __attribute__ ((deprecated));
 struct GameData gm;
 struct UnitSettings unit;
 struct bGPdata *gpd;
 struct PhysicsSettings physics_settings;
 struct MovieClip *clip;
 uint64_t customdata_mask;
 uint64_t customdata_mask_modal;
} Scene;
typedef enum {
 PAINT_SHOW_BRUSH = (1<<0),
 PAINT_FAST_NAVIGATE = (1<<1),
 PAINT_SHOW_BRUSH_ON_SURFACE = (1<<2),
} PaintFlags;
typedef enum SculptFlags {
 SCULPT_SYMM_X = (1<<0),
 SCULPT_SYMM_Y = (1<<1),
 SCULPT_SYMM_Z = (1<<2),
 SCULPT_LOCK_X = (1<<3),
 SCULPT_LOCK_Y = (1<<4),
 SCULPT_LOCK_Z = (1<<5),
 SCULPT_SYMMETRY_FEATHER = (1<<6),
 SCULPT_USE_OPENMP = (1<<7),
 SCULPT_ONLY_DEFORM = (1<<8),
} SculptFlags;
struct ID;
struct ListBase;
struct Main;
struct AnimData;
struct KeyingSet;
struct KS_Path;
struct PointerRNA;
struct ReportList;
struct bAction;
struct bActionGroup;
struct AnimMapper;
short id_type_can_have_animdata(struct ID *id);
struct AnimData *BKE_animdata_from_id(struct ID *id);
struct AnimData *BKE_id_add_animdata(struct ID *id);
short BKE_animdata_set_action(struct ReportList *reports, struct ID *id, struct bAction *act);
void BKE_free_animdata(struct ID *id);
struct AnimData *BKE_copy_animdata(struct AnimData *adt, const short do_action);
int BKE_copy_animdata_id(struct ID *id_to, struct ID *id_from, const short do_action);
void BKE_copy_animdata_id_action(struct ID *id);
void BKE_animdata_make_local(struct AnimData *adt);
void BKE_relink_animdata(struct AnimData *adt);
struct KeyingSet *BKE_keyingset_add(struct ListBase *list, const char name[], short flag, short keyingflag);
struct KS_Path *BKE_keyingset_add_path(struct KeyingSet *ks, struct ID *id, const char group_name[], const char rna_path[], int array_index, short flag, short groupmode);
struct KS_Path *BKE_keyingset_find_path(struct KeyingSet *ks, struct ID *id, const char group_name[], const char rna_path[], int array_index, int group_mode);
void BKE_keyingsets_copy(struct ListBase *newlist, struct ListBase *list);
void BKE_keyingset_free_path(struct KeyingSet *ks, struct KS_Path *ksp);
void BKE_keyingset_free(struct KeyingSet *ks);
void BKE_keyingsets_free(struct ListBase *list);
void BKE_animdata_fix_paths_rename(struct ID *owner_id, struct AnimData *adt, const char *prefix, const char *oldName, const char *newName, int oldSubscript, int newSubscript, int verify_paths);
void BKE_all_animdata_fix_paths_rename(const char *prefix, const char *oldName, const char *newName);
void BKE_animdata_separate_by_basepath(struct ID *srcID, struct ID *dstID, struct ListBase *basepaths);
void action_move_fcurves_by_basepath(struct bAction *srcAct, struct bAction *dstAct, const char basepath[]);
typedef void (*ID_AnimData_Edit_Callback)(struct ID *id, struct AnimData *adt, void *user_data);
void BKE_animdata_main_cb(struct Main *main, ID_AnimData_Edit_Callback func, void *user_data);
void BKE_animsys_evaluate_animdata(struct Scene *scene, struct ID *id, struct AnimData *adt, float ctime, short recalc);
void BKE_animsys_evaluate_all_animation(struct Main *main, struct Scene *scene, float ctime);
void animsys_evaluate_action(struct PointerRNA *ptr, struct bAction *act, struct AnimMapper *remap, float ctime);
void animsys_evaluate_action_group(struct PointerRNA *ptr, struct bAction *act, struct bActionGroup *agrp, struct AnimMapper *remap, float ctime);
struct BevList;
struct BezTriple;
struct Curve;
struct EditNurb;
struct ListBase;
struct ListBase;
struct Nurb;
struct Object;
struct Scene;
void unlink_curve( struct Curve *cu);
void free_curve_editNurb_keyIndex(struct EditNurb *editnurb);
void free_curve_editNurb(struct Curve *cu);
void free_curve( struct Curve *cu);
void BKE_free_editfont(struct Curve *cu);
struct Curve *add_curve(const char *name, int type);
struct Curve *copy_curve( struct Curve *cu);
void make_local_curve( struct Curve *cu);
struct ListBase *curve_editnurbs(struct Curve *cu);
short curve_type( struct Curve *cu);
void test_curve_type( struct Object *ob);
void update_curve_dimension( struct Curve *cu );
void tex_space_curve( struct Curve *cu);
int count_curveverts( struct ListBase *nurb);
int count_curveverts_without_handles( struct ListBase *nurb);
void freeNurb( struct Nurb *nu);
void freeNurblist( struct ListBase *lb);
struct Nurb *duplicateNurb( struct Nurb *nu);
void duplicateNurblist( struct ListBase *lb1, struct ListBase *lb2);
void test2DNurb( struct Nurb *nu);
void minmaxNurb( struct Nurb *nu, float *min, float *max);
void nurbs_knot_calc_u(struct Nurb *nu);
void nurbs_knot_calc_v(struct Nurb *nu);
void makeNurbfaces(struct Nurb *nu, float *coord_array, int rowstride, int resolu, int resolv);
void makeNurbcurve(struct Nurb *nu, float *coord_array, float *tilt_array, float *radius_array, float *weight_array, int resolu, int stride);
void forward_diff_bezier(float q0, float q1, float q2, float q3, float *p, int it, int stride);
float *make_orco_curve(struct Scene *scene, struct Object *ob);
float *make_orco_surf( struct Object *ob);
void makebevelcurve(struct Scene *scene, struct Object *ob, struct ListBase *disp, int forRender);
void makeBevelList( struct Object *ob);
void calchandleNurb( struct BezTriple *bezt, struct BezTriple *prev, struct BezTriple *next, int mode);
void calchandlesNurb( struct Nurb *nu);
void testhandlesNurb( struct Nurb *nu);
void autocalchandlesNurb( struct Nurb *nu, int flag);
void autocalchandlesNurb_all(ListBase *editnurb, int flag);
void sethandlesNurb(ListBase *editnurb, short code);
void switchdirectionNurb( struct Nurb *nu);
void addNurbPoints(struct Nurb *nu, int number);
void addNurbPointsBezier(struct Nurb *nu, int number);
float (*curve_getVertexCos(struct Curve *cu, struct ListBase *lb, int *numVerts_r))[3];
void curve_applyVertexCos(struct Curve *cu, struct ListBase *lb, float (*vertexCos)[3]);
float (*curve_getKeyVertexCos(struct Curve *cu, struct ListBase *lb, float *key))[3];
void curve_applyKeyVertexTilts(struct Curve *cu, struct ListBase *lb, float *key);
int check_valid_nurb_u( struct Nurb *nu);
int check_valid_nurb_v( struct Nurb *nu);
int clamp_nurb_order_u( struct Nurb *nu);
int clamp_nurb_order_v( struct Nurb *nu);
ListBase *BKE_curve_nurbs(struct Curve *cu);
int minmax_curve(struct Curve *cu, float min[3], float max[3]);
int curve_center_median(struct Curve *cu, float cent[3]);
int curve_center_bounds(struct Curve *cu, float cent[3]);
void curve_translate(struct Curve *cu, float offset[3], int do_keys);
void curve_delete_material_index(struct Curve *cu, int index);
struct ID;
struct CustomData;
struct CustomDataLayer;
typedef uint64_t CustomDataMask;
extern const CustomDataMask CD_MASK_BAREMESH;
extern const CustomDataMask CD_MASK_MESH;
extern const CustomDataMask CD_MASK_EDITMESH;
extern const CustomDataMask CD_MASK_DERIVEDMESH;
extern const CustomDataMask CD_MASK_BMESH;
extern const CustomDataMask CD_MASK_FACECORNERS;
void CustomData_copy(const struct CustomData *source, struct CustomData *dest,
      CustomDataMask mask, int alloctype, int totelem);
void CustomData_update_typemap(struct CustomData *data);
void CustomData_merge(const struct CustomData *source, struct CustomData *dest,
       CustomDataMask mask, int alloctype, int totelem);
void CustomData_free(struct CustomData *data, int totelem);
void CustomData_free_temporary(struct CustomData *data, int totelem);
void *CustomData_add_layer(struct CustomData *data, int type, int alloctype,
         void *layer, int totelem);
void *CustomData_add_layer_named(struct CustomData *data, int type, int alloctype,
         void *layer, int totelem, const char *name);
int CustomData_free_layer(struct CustomData *data, int type, int totelem, int index);
int CustomData_free_layer_active(struct CustomData *data, int type, int totelem);
void CustomData_free_layers(struct CustomData *data, int type, int totelem);
int CustomData_has_layer(const struct CustomData *data, int type);
int CustomData_number_of_layers(const struct CustomData *data, int type);
void *CustomData_duplicate_referenced_layer(struct CustomData *data, const int type, const int totelem);
void *CustomData_duplicate_referenced_layer_named(struct CustomData *data,
              const int type, const char *name, const int totelem);
int CustomData_is_referenced_layer(struct CustomData *data, int type);
void CustomData_set_only_copy(const struct CustomData *data,
         CustomDataMask mask);
void CustomData_copy_data(const struct CustomData *source,
        struct CustomData *dest, int source_index,
        int dest_index, int count);
void CustomData_copy_elements(int type, void *source, void *dest, int count);
void CustomData_em_copy_data(const struct CustomData *source,
       struct CustomData *dest, void *src_block,
       void **dest_block);
void CustomData_bmesh_copy_data(const struct CustomData *source,
                                struct CustomData *dest, void *src_block,
                                void **dest_block);
void CustomData_em_validate_data(struct CustomData *data, void *block, int sub_elements);
void CustomData_free_elem(struct CustomData *data, int index, int count);
void CustomData_interp(const struct CustomData *source, struct CustomData *dest,
        int *src_indices, float *weights, float *sub_weights,
        int count, int dest_index);
void CustomData_em_interp(struct CustomData *data, void **src_blocks,
        float *weights, float *sub_weights, int count,
        void *dest_block);
void CustomData_bmesh_interp(struct CustomData *data, void **src_blocks,
        float *weights, float *sub_weights, int count,
        void *dest_block);
void CustomData_swap(struct CustomData *data, int index, const int *corner_indices);
void *CustomData_get(const struct CustomData *data, int index, int type);
void *CustomData_get_n(const struct CustomData *data, int type, int index, int n);
void *CustomData_em_get(const struct CustomData *data, void *block, int type);
void *CustomData_em_get_n(const struct CustomData *data, void *block, int type, int n);
void *CustomData_bmesh_get(const struct CustomData *data, void *block, int type);
void *CustomData_bmesh_get_n(const struct CustomData *data, void *block, int type, int n);
void *CustomData_bmesh_get_layer_n(const struct CustomData *data, void *block, int n);
int CustomData_set_layer_name(const struct CustomData *data, int type, int n, const char *name);
void *CustomData_get_layer(const struct CustomData *data, int type);
void *CustomData_get_layer_n(const struct CustomData *data, int type, int n);
void *CustomData_get_layer_named(const struct CustomData *data, int type,
         const char *name);
int CustomData_get_layer_index(const struct CustomData *data, int type);
int CustomData_get_layer_index_n(const struct CustomData *data, int type, int n);
int CustomData_get_named_layer_index(const struct CustomData *data, int type, const char *name);
int CustomData_get_active_layer_index(const struct CustomData *data, int type);
int CustomData_get_render_layer_index(const struct CustomData *data, int type);
int CustomData_get_clone_layer_index(const struct CustomData *data, int type);
int CustomData_get_stencil_layer_index(const struct CustomData *data, int type);
int CustomData_get_active_layer(const struct CustomData *data, int type);
int CustomData_get_render_layer(const struct CustomData *data, int type);
int CustomData_get_clone_layer(const struct CustomData *data, int type);
int CustomData_get_stencil_layer(const struct CustomData *data, int type);
void CustomData_set(const struct CustomData *data, int index, int type,
     void *source);
void CustomData_em_set(struct CustomData *data, void *block, int type,
        void *source);
void CustomData_em_set_n(struct CustomData *data, void *block, int type, int n,
       void *source);
void CustomData_bmesh_set(const struct CustomData *data, void *block, int type,
        void *source);
void CustomData_bmesh_set_n(struct CustomData *data, void *block, int type, int n,
       void *source);
void CustomData_bmesh_set_layer_n(struct CustomData *data, void *block, int n,
       void *source);
void *CustomData_set_layer(const struct CustomData *data, int type, void *ptr);
void *CustomData_set_layer_n(const struct CustomData *data, int type, int n, void *ptr);
void CustomData_set_layer_active(struct CustomData *data, int type, int n);
void CustomData_set_layer_render(struct CustomData *data, int type, int n);
void CustomData_set_layer_clone(struct CustomData *data, int type, int n);
void CustomData_set_layer_stencil(struct CustomData *data, int type, int n);
void CustomData_set_layer_active_index(struct CustomData *data, int type, int n);
void CustomData_set_layer_render_index(struct CustomData *data, int type, int n);
void CustomData_set_layer_clone_index(struct CustomData *data, int type, int n);
void CustomData_set_layer_stencil_index(struct CustomData *data, int type, int n);
void CustomData_set_layer_flag(struct CustomData *data, int type, int flag);
void CustomData_em_set_default(struct CustomData *data, void **block);
void CustomData_em_free_block(struct CustomData *data, void **block);
void CustomData_bmesh_set_default(struct CustomData *data, void **block);
void CustomData_bmesh_free_block(struct CustomData *data, void **block);
void CustomData_to_em_block(const struct CustomData *source,
       struct CustomData *dest, int index, void **block);
void CustomData_from_em_block(const struct CustomData *source,
         struct CustomData *dest, void *block, int index);
void CustomData_to_bmesh_block(const struct CustomData *source,
       struct CustomData *dest, int src_index, void **dest_block);
void CustomData_from_bmesh_block(const struct CustomData *source,
       struct CustomData *dest, void *src_block, int dest_index);
void CustomData_file_write_info(int type, const char **structname, int *structnum);
int CustomData_sizeof(int type);
const char *CustomData_layertype_name(int type);
void CustomData_set_layer_unique_name(struct CustomData *data, int index);
void CustomData_validate_layer_name(const struct CustomData *data, int type, char *name, char *outname);
int CustomData_verify_versions(struct CustomData *data, int index);
void CustomData_to_bmeshpoly(struct CustomData *fdata, struct CustomData *pdata,
                             struct CustomData *ldata);
void CustomData_from_bmeshpoly(struct CustomData *fdata, struct CustomData *pdata, struct CustomData *ldata, int total);
void CustomData_bmesh_init_pool(struct CustomData *data, int allocsize);
void CustomData_external_add(struct CustomData *data,
 struct ID *id, int type, int totelem, const char *filename);
void CustomData_external_remove(struct CustomData *data,
 struct ID *id, int type, int totelem);
int CustomData_external_test(struct CustomData *data, int type);
void CustomData_external_write(struct CustomData *data,
 struct ID *id, CustomDataMask mask, int totelem, int free);
void CustomData_external_read(struct CustomData *data,
 struct ID *id, CustomDataMask mask, int totelem);
void CustomData_external_reload(struct CustomData *data,
 struct ID *id, CustomDataMask mask, int totelem);
struct Object;
struct ListBase;
struct bDeformGroup;
struct MDeformVert;
void defgroup_copy_list(struct ListBase *lb1, struct ListBase *lb2);
struct bDeformGroup *defgroup_duplicate(struct bDeformGroup *ingroup);
struct bDeformGroup *defgroup_find_name(struct Object *ob, const char *name);
int *defgroup_flip_map(struct Object *ob, int *flip_map_len, int use_default);
int *defgroup_flip_map_single(struct Object *ob, int *flip_map_len, int use_default, int defgroup);
int defgroup_flip_index(struct Object *ob, int index, int use_default);
int defgroup_name_index(struct Object *ob, const char *name);
void defgroup_unique_name(struct bDeformGroup *dg, struct Object *ob);
struct MDeformWeight *defvert_find_index(const struct MDeformVert *dv, const int defgroup);
struct MDeformWeight *defvert_verify_index(struct MDeformVert *dv, const int defgroup);
void defvert_add_index_notest(struct MDeformVert *dv, int defgroup, const float weight);
void defvert_remove_group(struct MDeformVert *dvert, struct MDeformWeight *dw);
float defvert_find_weight(const struct MDeformVert *dvert, const int defgroup);
float defvert_array_find_weight_safe(const struct MDeformVert *dvert, const int index, const int defgroup);
void defvert_copy(struct MDeformVert *dvert_dst, const struct MDeformVert *dvert_src);
void defvert_copy_index(struct MDeformVert *dvert_dst, const struct MDeformVert *dvert_src, const int defgroup);
void defvert_sync(struct MDeformVert *dvert_dst, const struct MDeformVert *dvert_src, int use_verify);
void defvert_sync_mapped(struct MDeformVert *dvert_dst, const struct MDeformVert *dvert_src,
                         const int *flip_map, const int flip_map_len, const int use_verify);
void defvert_remap (struct MDeformVert *dvert, int *map, const int map_len);
void defvert_flip(struct MDeformVert *dvert, const int *flip_map, const int flip_map_len);
void defvert_normalize(struct MDeformVert *dvert);
void defvert_normalize_lock(struct MDeformVert *dvert, const int def_nr_lock);
void flip_side_name(char name[64], const char from_name[64], int strip_number);
struct Main;
struct Object;
struct BME_Glob;
typedef struct Global {
 struct Main *main;
 char ima[1024], lib[1024];
 int relbase_valid;
 struct ListBase recent_files;
 short afbreek, moving, file_loaded;
 char background;
 char factory_startup;
 short winpos, displaymode;
 short rendering;
 short rt;
 int f;
 struct BME_Glob *editBMesh;
 int save_over;
 int have_quicktime;
 int ui_international;
 int charstart;
 int charmin;
 int charmax;
 struct VFont *selfont;
 struct ListBase ttfdata;
 int fileflags;
 int windowstate;
} Global;
extern Global G;
struct Key;
struct KeyBlock;
struct ID;
struct ListBase;
struct Curve;
struct Object;
struct Scene;
struct Lattice;
struct Mesh;
void free_key(struct Key *sc);
struct Key *add_key(struct ID *id);
struct Key *copy_key(struct Key *key);
void make_local_key(struct Key *key);
void sort_keys(struct Key *key);
void key_curve_position_weights(float t, float *data, int type);
void key_curve_tangent_weights(float t, float *data, int type);
void key_curve_normal_weights(float t, float *data, int type);
float *do_ob_key(struct Scene *scene, struct Object *ob);
struct Key *ob_get_key(struct Object *ob);
struct KeyBlock *add_keyblock(struct Key *key, const char *name);
struct KeyBlock *ob_get_keyblock(struct Object *ob);
struct KeyBlock *ob_get_reference_keyblock(struct Object *ob);
struct KeyBlock *key_get_keyblock(struct Key *key, int index);
struct KeyBlock *key_get_named_keyblock(struct Key *key, const char name[]);
char *key_get_curValue_rnaPath(struct Key *key, struct KeyBlock *kb);
void do_rel_key(const int start, int end, const int tot, char *basispoin, struct Key *key, struct KeyBlock *actkb, const int mode);
void key_to_mesh(struct KeyBlock *kb, struct Mesh *me);
void mesh_to_key(struct Mesh *me, struct KeyBlock *kb);
void key_to_latt(struct KeyBlock *kb, struct Lattice *lt);
void latt_to_key(struct Lattice *lt, struct KeyBlock *kb);
void key_to_curve(struct KeyBlock *kb, struct Curve *cu, struct ListBase *nurb);
void curve_to_key(struct Curve *cu, struct KeyBlock *kb, struct ListBase *nurb);
float (*key_to_vertcos(struct Object *ob, struct KeyBlock *kb))[3];
void vertcos_to_key(struct Object *ob, struct KeyBlock *kb, float (*vertCos)[3]);
void offset_to_key(struct Object *ob, struct KeyBlock *kb, float (*ofs)[3]);
extern int slurph_opt;
struct Lattice;
struct Object;
struct Scene;
struct DerivedMesh;
struct BPoint;
struct MDeformVert;
void resizelattice(struct Lattice *lt, int u, int v, int w, struct Object *ltOb);
struct Lattice *add_lattice(const char *name);
struct Lattice *copy_lattice(struct Lattice *lt);
void free_lattice(struct Lattice *lt);
void make_local_lattice(struct Lattice *lt);
void calc_lat_fudu(int flag, int res, float *fu, float *du);
void init_latt_deform(struct Object *oblatt, struct Object *ob);
void calc_latt_deform(struct Object *, float *co, float weight);
void end_latt_deform(struct Object *);
int object_deform_mball(struct Object *ob, struct ListBase *dispbase);
void outside_lattice(struct Lattice *lt);
void curve_deform_verts(struct Scene *scene, struct Object *cuOb, struct Object *target,
                        struct DerivedMesh *dm, float (*vertexCos)[3],
                        int numVerts, const char *vgroup, short defaxis);
void curve_deform_vector(struct Scene *scene, struct Object *cuOb, struct Object *target,
                         float orco[3], float vec[3], float mat[][3], int no_rot_axis);
void lattice_deform_verts(struct Object *laOb, struct Object *target,
                          struct DerivedMesh *dm, float (*vertexCos)[3],
                          int numVerts, const char *vgroup);
void armature_deform_verts(struct Object *armOb, struct Object *target,
                           struct DerivedMesh *dm, float (*vertexCos)[3],
                           float (*defMats)[3][3], int numVerts, int deformflag,
                           float (*prevCos)[3], const char *defgrp_name);
float (*lattice_getVertexCos(struct Object *ob, int *numVerts_r))[3];
void lattice_applyVertexCos(struct Object *ob, float (*vertexCos)[3]);
void lattice_calc_modifiers(struct Scene *scene, struct Object *ob);
struct MDeformVert* lattice_get_deform_verts(struct Object *lattice);
struct ListBase;
struct ID;
struct Main;
struct Library;
struct wmWindowManager;
struct bContext;
struct PointerRNA;
struct PropertyRNA;
void *alloc_libblock(struct ListBase *lb, short type, const char *name);
void *copy_libblock(struct ID *id);
void copy_libblock_data(struct ID *id, const struct ID *id_from, const short do_action);
void BKE_id_lib_local_paths(struct Main *bmain, struct Library *lib, struct ID *id);
void id_lib_extern(struct ID *id);
void BKE_library_filepath_set(struct Library *lib, const char *filepath);
void id_us_plus(struct ID *id);
void id_us_min(struct ID *id);
int id_make_local(struct ID *id, int test);
int id_single_user(struct bContext *C, struct ID *id, struct PointerRNA *ptr, struct PropertyRNA *prop);
int id_copy(struct ID *id, struct ID **newid, int test);
int id_unlink(struct ID *id, int test);
int new_id(struct ListBase *lb, struct ID *id, const char *name);
void id_clear_lib_data(struct Main *bmain, struct ID *id);
struct ListBase *which_libbase(struct Main *mainlib, short type);
int set_listbasepointers(struct Main *main, struct ListBase **lb);
void free_libblock(struct ListBase *lb, void *idv);
void free_libblock_us(struct ListBase *lb, void *idv);
void free_main(struct Main *mainvar);
void tag_main_idcode(struct Main *mainvar, const short type, const short tag);
void tag_main_lb(struct ListBase *lb, const short tag);
void tag_main(struct Main *mainvar, const short tag);
void rename_id(struct ID *id, const char *name);
void name_uiprefix_id(char *name, struct ID *id);
void test_idbutton(char *name);
void text_idbutton(struct ID *id, char *text);
void BKE_library_make_local(struct Main *bmain, struct Library *lib, int untagged_only);
struct ID *find_id(const char *type, const char *name);
void clear_id_newpoins(void);
void IDnames_to_pupstring(const char **str, const char *title, const char *extraops, struct ListBase *lb,struct ID* link, short *nr);
void IMAnames_to_pupstring(const char **str, const char *title, const char *extraops, struct ListBase *lb, struct ID *link, short *nr);
void IPOnames_to_pupstring(const char **str, const char *title, const char *extraops, struct ListBase *lb, struct ID* link, short *nr, int blocktype);
void flag_listbase_ids(ListBase *lb, short flag, short value);
void flag_all_listbases_ids(short flag, short value);
void recalc_all_library_objects(struct Main *main);
void set_free_windowmanager_cb(void (*func)(struct bContext *, struct wmWindowManager *) );
struct Library;
typedef struct Main {
 struct Main *next, *prev;
 char name[1024];
 short versionfile, subversionfile;
 short minversionfile, minsubversionfile;
 int revision;
 struct Library *curlib;
 ListBase scene;
 ListBase library;
 ListBase object;
 ListBase mesh;
 ListBase curve;
 ListBase mball;
 ListBase mat;
 ListBase tex;
 ListBase image;
 ListBase latt;
 ListBase lamp;
 ListBase camera;
 ListBase ipo;
 ListBase key;
 ListBase world;
 ListBase screen;
 ListBase script;
 ListBase vfont;
 ListBase text;
 ListBase speaker;
 ListBase sound;
 ListBase group;
 ListBase armature;
 ListBase action;
 ListBase nodetree;
 ListBase brush;
 ListBase particle;
 ListBase wm;
 ListBase gpencil;
 ListBase movieclip;
 char id_tag_update[256];
} Main;
struct Base;
struct Scene;
struct Object;
struct Camera;
struct BoundBox;
struct View3D;
struct SoftBody;
struct BulletSoftBody;
struct Group;
struct bAction;
struct RenderData;
struct rctf;
struct MovieClip;
void clear_workob(struct Object *workob);
void what_does_parent(struct Scene *scene, struct Object *ob, struct Object *workob);
void copy_baseflags(struct Scene *scene);
void copy_objectflags(struct Scene *scene);
struct SoftBody *copy_softbody(struct SoftBody *sb);
struct BulletSoftBody *copy_bulletsoftbody(struct BulletSoftBody *sb);
void copy_object_particlesystems(struct Object *obn, struct Object *ob);
void copy_object_softbody(struct Object *obn, struct Object *ob);
void object_free_particlesystems(struct Object *ob);
void object_free_softbody(struct Object *ob);
void object_free_bulletsoftbody(struct Object *ob);
void update_base_layer(struct Scene *scene, struct Object *ob);
void free_object(struct Object *ob);
void object_free_display(struct Object *ob);
void object_link_modifiers(struct Object *ob, struct Object *from);
void object_free_modifiers(struct Object *ob);
void object_make_proxy(struct Object *ob, struct Object *target, struct Object *gob);
void object_copy_proxy_drivers(struct Object *ob, struct Object *target);
void unlink_object(struct Object *ob);
int exist_object(struct Object *obtest);
struct Object *add_only_object(int type, const char *name);
struct Object *add_object(struct Scene *scene, int type);
struct Object *copy_object(struct Object *ob);
void make_local_object(struct Object *ob);
int object_is_libdata(struct Object *ob);
int object_data_is_libdata(struct Object *ob);
void set_mblur_offs(float blur);
void set_field_offs(float field);
void disable_speed_curve(int val);
void object_scale_to_mat3(struct Object *ob, float mat[][3]);
void object_rot_to_mat3(struct Object *ob, float mat[][3]);
void object_mat3_to_rot(struct Object *ob, float mat[][3], short use_compat);
void object_to_mat3(struct Object *ob, float mat[][3]);
void object_to_mat4(struct Object *ob, float mat[][4]);
void object_apply_mat4(struct Object *ob, float mat[][4], const short use_compat, const short use_parent);
struct Object *object_pose_armature_get(struct Object *ob);
void where_is_object_time(struct Scene *scene, struct Object *ob, float ctime);
void where_is_object(struct Scene *scene, struct Object *ob);
void where_is_object_simul(struct Scene *scene, struct Object *ob);
void where_is_object_mat(struct Scene *scene, struct Object *ob, float obmat[4][4]);
struct BoundBox *unit_boundbox(void);
void boundbox_set_from_min_max(struct BoundBox *bb, float min[3], float max[3]);
struct BoundBox *object_get_boundbox(struct Object *ob);
void object_get_dimensions(struct Object *ob, float *value);
void object_set_dimensions(struct Object *ob, const float *value);
void object_boundbox_flag(struct Object *ob, int flag, int set);
void minmax_object(struct Object *ob, float min[3], float max[3]);
int minmax_object_duplis(struct Scene *scene, struct Object *ob, float *min, float *max);
void BKE_object_foreach_display_point(
        struct Object *ob, float obmat[4][4],
        void (*func_cb)(const float[3], void *), void *user_data);
void BKE_scene_foreach_display_point(
        struct Scene *scene,
        struct View3D *v3d,
        const short flag,
        void (*func_cb)(const float[3], void *), void *user_data);
int BKE_object_parent_loop_check(const struct Object *parent, const struct Object *ob);
void solve_tracking (struct Object *ob, float targetmat[][4]);
int ray_hit_boundbox(struct BoundBox *bb, float ray_start[3], float ray_normal[3]);
void *object_tfm_backup(struct Object *ob);
void object_tfm_restore(struct Object *ob, void *obtfm_pt);
typedef struct ObjectTfmProtectedChannels {
 float loc[3], dloc[3];
 float size[3], dscale[3];
 float rot[3], drot[3];
 float quat[4], dquat[4];
 float rotAxis[3], drotAxis[3];
 float rotAngle, drotAngle;
} ObjectTfmProtectedChannels;
void object_tfm_protected_backup(const struct Object *ob,
                                 ObjectTfmProtectedChannels *obtfm);
void object_tfm_protected_restore(struct Object *ob,
                                  const ObjectTfmProtectedChannels *obtfm,
                                  const short protectflag);
void object_handle_update(struct Scene *scene, struct Object *ob);
void object_sculpt_modifiers_changed(struct Object *ob);
int give_obdata_texspace(struct Object *ob, short **texflag, float **loc, float **size, float **rot);
int object_insert_ptcache(struct Object *ob);
struct KeyBlock *object_insert_shape_key(struct Scene *scene, struct Object *ob, const char *name, int from_mix);
int object_is_modified(struct Scene *scene, struct Object *ob);
void object_relink(struct Object *ob);
struct MovieClip *object_get_movieclip(struct Scene *scene, struct Object *ob, int use_default);
struct AviCodecData;
struct Base;
struct bglMats;
struct Main;
struct Object;
struct QuicktimeCodecData;
struct RenderData;
struct SceneRenderLayer;
struct Scene;
struct Text;
struct Text;
struct Base *_setlooper_base_step(struct Scene **sce_iter, struct Base *base);
void free_avicodecdata(struct AviCodecData *acd);
void free_qtcodecdata(struct QuicktimeCodecData *acd);
void free_scene(struct Scene *sce);
struct Scene *add_scene(const char *name);
struct Base *object_in_scene(struct Object *ob, struct Scene *sce);
void set_scene_bg(struct Main *bmain, struct Scene *sce);
struct Scene *set_scene_name(struct Main *bmain, const char *name);
struct Scene *copy_scene(struct Scene *sce, int type);
void unlink_scene(struct Main *bmain, struct Scene *sce, struct Scene *newsce);
int next_object(struct Scene **scene, int val, struct Base **base, struct Object **ob);
struct Object *scene_find_camera(struct Scene *sc);
struct Object *scene_camera_switch_find(struct Scene *scene);
int scene_camera_switch_update(struct Scene *scene);
char *scene_find_marker_name(struct Scene *scene, int frame);
char *scene_find_last_marker_name(struct Scene *scene, int frame);
struct Base *scene_add_base(struct Scene *sce, struct Object *ob);
void scene_deselect_all(struct Scene *sce);
void scene_select_base(struct Scene *sce, struct Base *selbase);
int scene_check_setscene(struct Main *bmain, struct Scene *sce);
float BKE_curframe(struct Scene *scene);
float BKE_frame_to_ctime(struct Scene *scene, const float frame);
void scene_update_tagged(struct Main *bmain, struct Scene *sce);
void scene_update_for_newframe(struct Main *bmain, struct Scene *sce, unsigned int lay);
struct SceneRenderLayer *scene_add_render_layer(struct Scene *sce, const char *name);
int scene_remove_render_layer(struct Main *main, struct Scene *scene, struct SceneRenderLayer *srl);
int get_render_subsurf_level(struct RenderData *r, int level);
int get_render_child_particle_number(struct RenderData *r, int num);
int get_render_shadow_samples(struct RenderData *r, int samples);
float get_render_aosss_error(struct RenderData *r, float error);
int scene_use_new_shading_nodes(struct Scene *scene);
struct ParameterList;
struct FunctionRNA;
struct PropertyRNA;
struct EnumPropertyRNA;
struct StructRNA;
struct BlenderRNA;
struct IDProperty;
struct bContext;
struct Main;
struct ReportList;
typedef struct PointerRNA {
 struct {
  void *data;
 } id;
 struct StructRNA *type;
 void *data;
} PointerRNA;
typedef struct PropertyPointerRNA {
 PointerRNA ptr;
 struct PropertyRNA *prop;
} PropertyPointerRNA;
typedef enum PropertyType {
 PROP_BOOLEAN = 0,
 PROP_INT = 1,
 PROP_FLOAT = 2,
 PROP_STRING = 3,
 PROP_ENUM = 4,
 PROP_POINTER = 5,
 PROP_COLLECTION = 6
} PropertyType;
typedef enum PropertyUnit {
 PROP_UNIT_NONE = (0<<16),
 PROP_UNIT_LENGTH = (1<<16),
 PROP_UNIT_AREA = (2<<16),
 PROP_UNIT_VOLUME = (3<<16),
 PROP_UNIT_MASS = (4<<16),
 PROP_UNIT_ROTATION = (5<<16),
 PROP_UNIT_TIME = (6<<16),
 PROP_UNIT_VELOCITY = (7<<16),
 PROP_UNIT_ACCELERATION = (8<<16)
} PropertyUnit;
typedef enum PropertySubType {
 PROP_NONE = 0,
 PROP_FILEPATH = 1,
 PROP_DIRPATH = 2,
 PROP_FILENAME = 3,
 PROP_BYTESTRING = 4,
 PROP_TRANSLATE = 5,
 PROP_UNSIGNED = 13,
 PROP_PERCENTAGE = 14,
 PROP_FACTOR = 15,
 PROP_ANGLE = 16|PROP_UNIT_ROTATION,
 PROP_TIME = 17|PROP_UNIT_TIME,
 PROP_DISTANCE = 18|PROP_UNIT_LENGTH,
 PROP_COLOR = 20,
 PROP_TRANSLATION = 21|PROP_UNIT_LENGTH,
 PROP_DIRECTION = 22,
 PROP_VELOCITY = 23|PROP_UNIT_VELOCITY,
 PROP_ACCELERATION = 24|PROP_UNIT_ACCELERATION,
 PROP_MATRIX = 25,
 PROP_EULER = 26|PROP_UNIT_ROTATION,
 PROP_QUATERNION = 27,
 PROP_AXISANGLE = 28,
 PROP_XYZ = 29,
 PROP_XYZ_LENGTH = 29|PROP_UNIT_LENGTH,
 PROP_COLOR_GAMMA = 30,
 PROP_COORDS = 31,
 PROP_LAYER = 40,
 PROP_LAYER_MEMBER = 41
} PropertySubType;
typedef enum PropertyFlag {
 PROP_EDITABLE = 1<<0,
 PROP_LIB_EXCEPTION = 1<<16,
 PROP_ANIMATABLE = 1<<1,
 PROP_ICONS_CONSECUTIVE = 1<<12,
 PROP_HIDDEN = 1<<19,
 PROP_SKIP_SAVE = 1<<28,
 PROP_REQUIRED = 1<<2,
 PROP_OUTPUT = 1<<3,
 PROP_RNAPTR = 1<<11,
 PROP_REGISTER = 1<<4,
 PROP_REGISTER_OPTIONAL = (1<<4)|(1<<5),
 PROP_ID_REFCOUNT = 1<<6,
 PROP_ID_SELF_CHECK = 1<<20,
 PROP_NEVER_NULL = 1<<18,
 PROP_NEVER_UNLINK = 1<<25,
 PROP_ENUM_FLAG = 1<<21,
 PROP_CONTEXT_UPDATE = 1<<22,
 PROP_CONTEXT_PROPERTY_UPDATE = (1<<22)|(1<<27),
 PROP_THICK_WRAP = 1<<23,
 PROP_NEVER_CLAMP = 1<<26,
 PROP_BUILTIN = 1<<7,
 PROP_EXPORT = 1<<8,
 PROP_RUNTIME = 1<<9,
 PROP_IDPROPERTY = 1<<10,
 PROP_RAW_ACCESS = 1<<13,
 PROP_RAW_ARRAY = 1<<14,
 PROP_FREE_POINTERS = 1<<15,
 PROP_DYNAMIC = 1<<17,
 PROP_ENUM_NO_CONTEXT = 1<<24
} PropertyFlag;
typedef struct CollectionPropertyIterator {
 PointerRNA parent;
 PointerRNA builtin_parent;
 struct PropertyRNA *prop;
 void *internal;
 int idprop;
 int level;
 PointerRNA ptr;
 int valid;
} CollectionPropertyIterator;
typedef struct CollectionPointerLink {
 struct CollectionPointerLink *next, *prev;
 PointerRNA ptr;
} CollectionPointerLink;
typedef enum RawPropertyType {
 PROP_RAW_UNSET=-1,
 PROP_RAW_INT,
 PROP_RAW_SHORT,
 PROP_RAW_CHAR,
 PROP_RAW_DOUBLE,
 PROP_RAW_FLOAT
} RawPropertyType;
typedef struct RawArray {
 void *array;
 RawPropertyType type;
 int len;
 int stride;
} RawArray;
typedef struct EnumPropertyItem {
 int value;
 const char *identifier;
 int icon;
 const char *name;
 const char *description;
} EnumPropertyItem;
typedef EnumPropertyItem *(*EnumPropertyItemFunc)(struct bContext *C, PointerRNA *ptr, struct PropertyRNA *prop, int *free);
typedef struct PropertyRNA PropertyRNA;
typedef struct ParameterList {
 void *data;
 struct FunctionRNA *func;
 int alloc_size;
 int arg_count, ret_count;
} ParameterList;
typedef struct ParameterIterator {
 struct ParameterList *parms;
 void *data;
 int size, offset;
 PropertyRNA *parm;
 int valid;
} ParameterIterator;
typedef struct ParameterDynAlloc {
 intptr_t array_tot;
 void *array;
} ParameterDynAlloc;
typedef enum FunctionFlag {
 FUNC_NO_SELF = 1,
 FUNC_USE_MAIN = 2,
 FUNC_USE_CONTEXT = 4,
 FUNC_USE_REPORTS = 8,
 FUNC_USE_SELF_ID = 2048,
 FUNC_REGISTER = 16,
 FUNC_REGISTER_OPTIONAL = 16|32,
 FUNC_BUILTIN = 128,
 FUNC_EXPORT = 256,
 FUNC_RUNTIME = 512,
 FUNC_FREE_POINTERS = 1024
} FunctionFlag;
typedef void (*CallFunc)(struct bContext *C, struct ReportList *reports, PointerRNA *ptr, ParameterList *parms);
typedef struct FunctionRNA FunctionRNA;
typedef enum StructFlag {
 STRUCT_ID = 1,
 STRUCT_ID_REFCOUNT = 2,
 STRUCT_UNDO = 4,
 STRUCT_RUNTIME = 8,
 STRUCT_GENERATED = 16,
 STRUCT_FREE_POINTERS = 32,
 STRUCT_NO_IDPROPERTIES = 64
} StructFlag;
typedef int (*StructValidateFunc)(struct PointerRNA *ptr, void *data, int *have_function);
typedef int (*StructCallbackFunc)(struct bContext *C, struct PointerRNA *ptr, struct FunctionRNA *func, ParameterList *list);
typedef void (*StructFreeFunc)(void *data);
typedef struct StructRNA *(*StructRegisterFunc)(struct Main *bmain, struct ReportList *reports, void *data,
 const char *identifier, StructValidateFunc validate, StructCallbackFunc call, StructFreeFunc free);
typedef void (*StructUnregisterFunc)(struct Main *bmain, struct StructRNA *type);
typedef void **(*StructInstanceFunc)(PointerRNA *ptr);
typedef struct StructRNA StructRNA;
typedef struct BlenderRNA BlenderRNA;
typedef struct ExtensionRNA {
 void *data;
 StructRNA *srna;
 StructCallbackFunc call;
 StructFreeFunc free;
} ExtensionRNA;
struct bContext;
struct ID;
struct ListBase;
struct Main;
struct ReportList;
struct Scene;
extern BlenderRNA BLENDER_RNA;
extern StructRNA RNA_Action;
extern StructRNA RNA_ActionConstraint;
extern StructRNA RNA_ActionFCurves;
extern StructRNA RNA_ActionGroup;
extern StructRNA RNA_ActionGroups;
extern StructRNA RNA_ActionPoseMarkers;
extern StructRNA RNA_Actuator;
extern StructRNA RNA_ActuatorSensor;
extern StructRNA RNA_Addon;
extern StructRNA RNA_AdjustmentSequence;
extern StructRNA RNA_AlwaysSensor;
extern StructRNA RNA_AndController;
extern StructRNA RNA_AnimData;
extern StructRNA RNA_AnimViz;
extern StructRNA RNA_AnimVizMotionPaths;
extern StructRNA RNA_AnimVizOnionSkinning;
extern StructRNA RNA_AnyType;
extern StructRNA RNA_Area;
extern StructRNA RNA_AreaLamp;
extern StructRNA RNA_Armature;
extern StructRNA RNA_ArmatureModifier;
extern StructRNA RNA_ArmatureSensor;
extern StructRNA RNA_ArrayModifier;
extern StructRNA RNA_BackgroundImage;
extern StructRNA RNA_BevelModifier;
extern StructRNA RNA_SplinePoint;
extern StructRNA RNA_BezierSplinePoint;
extern StructRNA RNA_BlendData;
extern StructRNA RNA_BlendTexture;
extern StructRNA RNA_BlenderRNA;
extern StructRNA RNA_BoidRule;
extern StructRNA RNA_BoidRuleAverageSpeed;
extern StructRNA RNA_BoidRuleAvoid;
extern StructRNA RNA_BoidRuleAvoidCollision;
extern StructRNA RNA_BoidRuleFight;
extern StructRNA RNA_BoidRuleFollowLeader;
extern StructRNA RNA_BoidRuleGoal;
extern StructRNA RNA_BoidSettings;
extern StructRNA RNA_BoidState;
extern StructRNA RNA_Bone;
extern StructRNA RNA_BoneGroup;
extern StructRNA RNA_BooleanModifier;
extern StructRNA RNA_BoolProperty;
extern StructRNA RNA_Brush;
extern StructRNA RNA_BrushTextureSlot;
extern StructRNA RNA_BuildModifier;
extern StructRNA RNA_Camera;
extern StructRNA RNA_CastModifier;
extern StructRNA RNA_ChildOfConstraint;
extern StructRNA RNA_ChildParticle;
extern StructRNA RNA_ClampToConstraint;
extern StructRNA RNA_ClothCollisionSettings;
extern StructRNA RNA_ClothModifier;
extern StructRNA RNA_ClothSettings;
extern StructRNA RNA_CloudsTexture;
extern StructRNA RNA_CollectionProperty;
extern StructRNA RNA_CollisionModifier;
extern StructRNA RNA_CollisionSensor;
extern StructRNA RNA_CollisionSettings;
extern StructRNA RNA_ColorRamp;
extern StructRNA RNA_ColorRampElement;
extern StructRNA RNA_ColorSequence;
extern StructRNA RNA_CompositorNode;
extern StructRNA RNA_CompositorNodeAlphaOver;
extern StructRNA RNA_CompositorNodeBilateralblur;
extern StructRNA RNA_CompositorNodeBlur;
extern StructRNA RNA_CompositorNodeBrightContrast;
extern StructRNA RNA_CompositorNodeChannelMatte;
extern StructRNA RNA_CompositorNodeChromaMatte;
extern StructRNA RNA_CompositorNodeColorMatte;
extern StructRNA RNA_CompositorNodeColorSpill;
extern StructRNA RNA_CompositorNodeCombHSVA;
extern StructRNA RNA_CompositorNodeCombRGBA;
extern StructRNA RNA_CompositorNodeCombYCCA;
extern StructRNA RNA_CompositorNodeCombYUVA;
extern StructRNA RNA_CompositorNodeComposite;
extern StructRNA RNA_CompositorNodeCrop;
extern StructRNA RNA_CompositorNodeCurveRGB;
extern StructRNA RNA_CompositorNodeCurveVec;
extern StructRNA RNA_CompositorNodeDBlur;
extern StructRNA RNA_CompositorNodeDefocus;
extern StructRNA RNA_CompositorNodeDiffMatte;
extern StructRNA RNA_CompositorNodeDilateErode;
extern StructRNA RNA_CompositorNodeDisplace;
extern StructRNA RNA_CompositorNodeDistanceMatte;
extern StructRNA RNA_CompositorNodeFilter;
extern StructRNA RNA_CompositorNodeFlip;
extern StructRNA RNA_CompositorNodeGamma;
extern StructRNA RNA_CompositorNodeGlare;
extern StructRNA RNA_CompositorNodeHueSat;
extern StructRNA RNA_CompositorNodeIDMask;
extern StructRNA RNA_CompositorNodeDoubleEdgeMask;
extern StructRNA RNA_CompositorNodeImage;
extern StructRNA RNA_CompositorNodeInvert;
extern StructRNA RNA_CompositorNodeLensdist;
extern StructRNA RNA_CompositorNodeLevels;
extern StructRNA RNA_CompositorNodeLumaMatte;
extern StructRNA RNA_CompositorNodeMapUV;
extern StructRNA RNA_CompositorNodeMapValue;
extern StructRNA RNA_CompositorNodeMath;
extern StructRNA RNA_CompositorNodeMixRGB;
extern StructRNA RNA_CompositorNodeNormal;
extern StructRNA RNA_CompositorNodeNormalize;
extern StructRNA RNA_CompositorNodeOutputFile;
extern StructRNA RNA_CompositorNodePremulKey;
extern StructRNA RNA_CompositorNodeRGB;
extern StructRNA RNA_CompositorNodeRGBToBW;
extern StructRNA RNA_CompositorNodeRLayers;
extern StructRNA RNA_CompositorNodeRotate;
extern StructRNA RNA_CompositorNodeScale;
extern StructRNA RNA_CompositorNodeSepHSVA;
extern StructRNA RNA_CompositorNodeSepRGBA;
extern StructRNA RNA_CompositorNodeSepYCCA;
extern StructRNA RNA_CompositorNodeSepYUVA;
extern StructRNA RNA_CompositorNodeSetAlpha;
extern StructRNA RNA_CompositorNodeSplitViewer;
extern StructRNA RNA_CompositorNodeTexture;
extern StructRNA RNA_CompositorNodeTime;
extern StructRNA RNA_CompositorNodeTonemap;
extern StructRNA RNA_CompositorNodeTranslate;
extern StructRNA RNA_CompositorNodeTree;
extern StructRNA RNA_CompositorNodeValToRGB;
extern StructRNA RNA_CompositorNodeValue;
extern StructRNA RNA_CompositorNodeVecBlur;
extern StructRNA RNA_CompositorNodeViewer;
extern StructRNA RNA_CompositorNodeZcombine;
extern StructRNA RNA_ConsoleLine;
extern StructRNA RNA_Constraint;
extern StructRNA RNA_ConstraintTarget;
extern StructRNA RNA_Context;
extern StructRNA RNA_ControlFluidSettings;
extern StructRNA RNA_Controller;
extern StructRNA RNA_CopyLocationConstraint;
extern StructRNA RNA_CopyRotationConstraint;
extern StructRNA RNA_CopyScaleConstraint;
extern StructRNA RNA_CopyTransformsConstraint;
extern StructRNA RNA_Curve;
extern StructRNA RNA_CurveMap;
extern StructRNA RNA_CurveMapPoint;
extern StructRNA RNA_CurveMapping;
extern StructRNA RNA_CurveModifier;
extern StructRNA RNA_CurvePoint;
extern StructRNA RNA_DampedTrackConstraint;
extern StructRNA RNA_DecimateModifier;
extern StructRNA RNA_DelaySensor;
extern StructRNA RNA_DisplaceModifier;
extern StructRNA RNA_DistortedNoiseTexture;
extern StructRNA RNA_DomainFluidSettings;
extern StructRNA RNA_DopeSheet;
extern StructRNA RNA_Driver;
extern StructRNA RNA_DriverTarget;
extern StructRNA RNA_DriverVariable;
extern StructRNA RNA_DupliObject;
extern StructRNA RNA_DynamicPaintBrushSettings;
extern StructRNA RNA_DynamicPaintCanvasSettings;
extern StructRNA RNA_DynamicPaintModifier;
extern StructRNA RNA_DynamicPaintSurface;
extern StructRNA RNA_EdgeSplitModifier;
extern StructRNA RNA_EditBone;
extern StructRNA RNA_EffectSequence;
extern StructRNA RNA_EffectorWeights;
extern StructRNA RNA_EnumProperty;
extern StructRNA RNA_EnumPropertyItem;
extern StructRNA RNA_EnvironmentMap;
extern StructRNA RNA_EnvironmentMapTexture;
extern StructRNA RNA_Event;
extern StructRNA RNA_ExplodeModifier;
extern StructRNA RNA_ExpressionController;
extern StructRNA RNA_FCurve;
extern StructRNA RNA_FCurveSample;
extern StructRNA RNA_FFmpegSettings;
extern StructRNA RNA_FModifier;
extern StructRNA RNA_FModifierCycles;
extern StructRNA RNA_FModifierEnvelope;
extern StructRNA RNA_FModifierEnvelopeControlPoint;
extern StructRNA RNA_FModifierFunctionGenerator;
extern StructRNA RNA_FModifierGenerator;
extern StructRNA RNA_FModifierLimits;
extern StructRNA RNA_FModifierNoise;
extern StructRNA RNA_FModifierPython;
extern StructRNA RNA_FModifierStepped;
extern StructRNA RNA_FieldSettings;
extern StructRNA RNA_FileSelectParams;
extern StructRNA RNA_FloatProperty;
extern StructRNA RNA_FloorConstraint;
extern StructRNA RNA_FluidFluidSettings;
extern StructRNA RNA_FluidSettings;
extern StructRNA RNA_FluidSimulationModifier;
extern StructRNA RNA_FollowPathConstraint;
extern StructRNA RNA_Function;
extern StructRNA RNA_GPencilFrame;
extern StructRNA RNA_GPencilLayer;
extern StructRNA RNA_GPencilStroke;
extern StructRNA RNA_GPencilStrokePoint;
extern StructRNA RNA_GameBooleanProperty;
extern StructRNA RNA_GameFloatProperty;
extern StructRNA RNA_GameIntProperty;
extern StructRNA RNA_GameObjectSettings;
extern StructRNA RNA_GameProperty;
extern StructRNA RNA_GameSoftBodySettings;
extern StructRNA RNA_GameStringProperty;
extern StructRNA RNA_GameTimerProperty;
extern StructRNA RNA_GlowSequence;
extern StructRNA RNA_GreasePencil;
extern StructRNA RNA_Group;
extern StructRNA RNA_Header;
extern StructRNA RNA_HemiLamp;
extern StructRNA RNA_Histogram;
extern StructRNA RNA_HookModifier;
extern StructRNA RNA_ID;
extern StructRNA RNA_IKParam;
extern StructRNA RNA_Image;
extern StructRNA RNA_ImageFormatSettings;
extern StructRNA RNA_ImagePaint;
extern StructRNA RNA_ImageSequence;
extern StructRNA RNA_ImageTexture;
extern StructRNA RNA_ImageUser;
extern StructRNA RNA_InflowFluidSettings;
extern StructRNA RNA_IntProperty;
extern StructRNA RNA_Itasc;
extern StructRNA RNA_JoystickSensor;
extern StructRNA RNA_Key;
extern StructRNA RNA_KeyConfig;
extern StructRNA RNA_KeyMap;
extern StructRNA RNA_KeyMapItem;
extern StructRNA RNA_KeyMapItems;
extern StructRNA RNA_KeyboardSensor;
extern StructRNA RNA_Keyframe;
extern StructRNA RNA_KeyingSet;
extern StructRNA RNA_KeyingSetInfo;
extern StructRNA RNA_KeyingSetPath;
extern StructRNA RNA_KeyingSetsAll;
extern StructRNA RNA_KinematicConstraint;
extern StructRNA RNA_Lamp;
extern StructRNA RNA_LampSkySettings;
extern StructRNA RNA_LampTextureSlot;
extern StructRNA RNA_Lattice;
extern StructRNA RNA_LatticeModifier;
extern StructRNA RNA_LatticePoint;
extern StructRNA RNA_Library;
extern StructRNA RNA_LimitDistanceConstraint;
extern StructRNA RNA_LimitLocationConstraint;
extern StructRNA RNA_LimitRotationConstraint;
extern StructRNA RNA_LimitScaleConstraint;
extern StructRNA RNA_LockedTrackConstraint;
extern StructRNA RNA_Macro;
extern StructRNA RNA_MagicTexture;
extern StructRNA RNA_MarbleTexture;
extern StructRNA RNA_MaskModifier;
extern StructRNA RNA_Material;
extern StructRNA RNA_MaterialHalo;
extern StructRNA RNA_MaterialPhysics;
extern StructRNA RNA_MaterialRaytraceMirror;
extern StructRNA RNA_MaterialRaytraceTransparency;
extern StructRNA RNA_MaterialSlot;
extern StructRNA RNA_MaterialStrand;
extern StructRNA RNA_MaterialSubsurfaceScattering;
extern StructRNA RNA_MaterialTextureSlot;
extern StructRNA RNA_MaterialVolume;
extern StructRNA RNA_Menu;
extern StructRNA RNA_Mesh;
extern StructRNA RNA_MeshColor;
extern StructRNA RNA_MeshColorLayer;
extern StructRNA RNA_MeshDeformModifier;
extern StructRNA RNA_MeshEdge;
extern StructRNA RNA_MeshFace;
extern StructRNA RNA_MeshFloatProperty;
extern StructRNA RNA_MeshFloatPropertyLayer;
extern StructRNA RNA_MeshIntProperty;
extern StructRNA RNA_MeshIntPropertyLayer;
extern StructRNA RNA_MeshSticky;
extern StructRNA RNA_MeshStringProperty;
extern StructRNA RNA_MeshStringPropertyLayer;
extern StructRNA RNA_MeshTextureFace;
extern StructRNA RNA_MeshTextureFaceLayer;
extern StructRNA RNA_MeshVertex;
extern StructRNA RNA_MessageSensor;
extern StructRNA RNA_MetaBall;
extern StructRNA RNA_MetaElement;
extern StructRNA RNA_MetaSequence;
extern StructRNA RNA_MirrorModifier;
extern StructRNA RNA_Modifier;
extern StructRNA RNA_MotionPath;
extern StructRNA RNA_MotionPathVert;
extern StructRNA RNA_MouseSensor;
extern StructRNA RNA_MovieSequence;
extern StructRNA RNA_MovieTrackingObject;
extern StructRNA RNA_MulticamSequence;
extern StructRNA RNA_MultiresModifier;
extern StructRNA RNA_MusgraveTexture;
extern StructRNA RNA_NandController;
extern StructRNA RNA_NearSensor;
extern StructRNA RNA_NlaStrip;
extern StructRNA RNA_NlaTrack;
extern StructRNA RNA_Node;
extern StructRNA RNA_NodeForLoop;
extern StructRNA RNA_NodeGroup;
extern StructRNA RNA_NodeLink;
extern StructRNA RNA_NodeSocket;
extern StructRNA RNA_NodeSocketPanel;
extern StructRNA RNA_NodeTree;
extern StructRNA RNA_NodeWhileLoop;
extern StructRNA RNA_NoiseTexture;
extern StructRNA RNA_NorController;
extern StructRNA RNA_Object;
extern StructRNA RNA_ObjectBase;
extern StructRNA RNA_ObstacleFluidSettings;
extern StructRNA RNA_OceanModifier;
extern StructRNA RNA_OceanTexData;
extern StructRNA RNA_OceanTexture;
extern StructRNA RNA_Operator;
extern StructRNA RNA_OperatorFileListElement;
extern StructRNA RNA_OperatorMousePath;
extern StructRNA RNA_OperatorProperties;
extern StructRNA RNA_OperatorStrokeElement;
extern StructRNA RNA_OperatorMacro;
extern StructRNA RNA_OrController;
extern StructRNA RNA_OutflowFluidSettings;
extern StructRNA RNA_PackedFile;
extern StructRNA RNA_Paint;
extern StructRNA RNA_Panel;
extern StructRNA RNA_Particle;
extern StructRNA RNA_ParticleBrush;
extern StructRNA RNA_ParticleDupliWeight;
extern StructRNA RNA_ParticleEdit;
extern StructRNA RNA_ParticleFluidSettings;
extern StructRNA RNA_ParticleHairKey;
extern StructRNA RNA_ParticleInstanceModifier;
extern StructRNA RNA_ParticleKey;
extern StructRNA RNA_ParticleSettings;
extern StructRNA RNA_ParticleSettingsTextureSlot;
extern StructRNA RNA_ParticleSystem;
extern StructRNA RNA_ParticleSystemModifier;
extern StructRNA RNA_ParticleTarget;
extern StructRNA RNA_PivotConstraint;
extern StructRNA RNA_PluginSequence;
extern StructRNA RNA_PluginTexture;
extern StructRNA RNA_PointCache;
extern StructRNA RNA_PointDensity;
extern StructRNA RNA_PointDensityTexture;
extern StructRNA RNA_PointLamp;
extern StructRNA RNA_PointerProperty;
extern StructRNA RNA_Pose;
extern StructRNA RNA_PoseBone;
extern StructRNA RNA_Property;
extern StructRNA RNA_PropertyGroup;
extern StructRNA RNA_PropertyGroupItem;
extern StructRNA RNA_PropertySensor;
extern StructRNA RNA_PythonConstraint;
extern StructRNA RNA_PythonController;
extern StructRNA RNA_QuickTimeSettings;
extern StructRNA RNA_RadarSensor;
extern StructRNA RNA_RandomSensor;
extern StructRNA RNA_RaySensor;
extern StructRNA RNA_Region;
extern StructRNA RNA_RenderEngine;
extern StructRNA RNA_RenderLayer;
extern StructRNA RNA_RenderPass;
extern StructRNA RNA_RenderResult;
extern StructRNA RNA_RenderSettings;
extern StructRNA RNA_RigidBodyJointConstraint;
extern StructRNA RNA_SPHFluidSettings;
extern StructRNA RNA_Scene;
extern StructRNA RNA_SceneGameData;
extern StructRNA RNA_SceneRenderLayer;
extern StructRNA RNA_SceneSequence;
extern StructRNA RNA_SceneObjects;
extern StructRNA RNA_Scopes;
extern StructRNA RNA_Screen;
extern StructRNA RNA_ScrewModifier;
extern StructRNA RNA_Sculpt;
extern StructRNA RNA_SelectedUvElement;
extern StructRNA RNA_Sensor;
extern StructRNA RNA_Sequence;
extern StructRNA RNA_SequenceColorBalance;
extern StructRNA RNA_SequenceCrop;
extern StructRNA RNA_SequenceEditor;
extern StructRNA RNA_SequenceElement;
extern StructRNA RNA_SequenceProxy;
extern StructRNA RNA_SequenceTransform;
extern StructRNA RNA_ShaderNode;
extern StructRNA RNA_ShaderNodeCameraData;
extern StructRNA RNA_ShaderNodeCombineRGB;
extern StructRNA RNA_ShaderNodeExtendedMaterial;
extern StructRNA RNA_ShaderNodeGeometry;
extern StructRNA RNA_ShaderNodeHueSaturation;
extern StructRNA RNA_ShaderNodeInvert;
extern StructRNA RNA_ShaderNodeMapping;
extern StructRNA RNA_ShaderNodeMaterial;
extern StructRNA RNA_ShaderNodeMath;
extern StructRNA RNA_ShaderNodeMixRGB;
extern StructRNA RNA_ShaderNodeNormal;
extern StructRNA RNA_ShaderNodeOutput;
extern StructRNA RNA_ShaderNodeRGB;
extern StructRNA RNA_ShaderNodeRGBCurve;
extern StructRNA RNA_ShaderNodeRGBToBW;
extern StructRNA RNA_ShaderNodeSeparateRGB;
extern StructRNA RNA_ShaderNodeSqueeze;
extern StructRNA RNA_ShaderNodeTexture;
extern StructRNA RNA_ShaderNodeTree;
extern StructRNA RNA_ShaderNodeValToRGB;
extern StructRNA RNA_ShaderNodeValue;
extern StructRNA RNA_ShaderNodeVectorCurve;
extern StructRNA RNA_ShaderNodeVectorMath;
extern StructRNA RNA_ShapeKey;
extern StructRNA RNA_ShapeKeyBezierPoint;
extern StructRNA RNA_ShapeKeyCurvePoint;
extern StructRNA RNA_ShapeKeyPoint;
extern StructRNA RNA_ShrinkwrapConstraint;
extern StructRNA RNA_ShrinkwrapModifier;
extern StructRNA RNA_SimpleDeformModifier;
extern StructRNA RNA_SmokeCollSettings;
extern StructRNA RNA_SmokeDomainSettings;
extern StructRNA RNA_SmokeFlowSettings;
extern StructRNA RNA_SmokeModifier;
extern StructRNA RNA_SmoothModifier;
extern StructRNA RNA_SoftBodyModifier;
extern StructRNA RNA_SoftBodySettings;
extern StructRNA RNA_SolidifyModifier;
extern StructRNA RNA_Sound;
extern StructRNA RNA_SoundSequence;
extern StructRNA RNA_Space;
extern StructRNA RNA_SpaceConsole;
extern StructRNA RNA_SpaceDopeSheetEditor;
extern StructRNA RNA_SpaceFileBrowser;
extern StructRNA RNA_SpaceGraphEditor;
extern StructRNA RNA_SpaceImageEditor;
extern StructRNA RNA_SpaceInfo;
extern StructRNA RNA_SpaceLogicEditor;
extern StructRNA RNA_SpaceNLA;
extern StructRNA RNA_SpaceNodeEditor;
extern StructRNA RNA_SpaceOutliner;
extern StructRNA RNA_SpaceProperties;
extern StructRNA RNA_SpaceSequenceEditor;
extern StructRNA RNA_SpaceTextEditor;
extern StructRNA RNA_SpaceTimeline;
extern StructRNA RNA_SpaceUVEditor;
extern StructRNA RNA_SpaceUserPreferences;
extern StructRNA RNA_SpaceView3D;
extern StructRNA RNA_SpaceClipEditor;
extern StructRNA RNA_Speaker;
extern StructRNA RNA_SpeedControlSequence;
extern StructRNA RNA_Spline;
extern StructRNA RNA_SplineIKConstraint;
extern StructRNA RNA_SpotLamp;
extern StructRNA RNA_StretchToConstraint;
extern StructRNA RNA_StringProperty;
extern StructRNA RNA_Struct;
extern StructRNA RNA_StucciTexture;
extern StructRNA RNA_SubsurfModifier;
extern StructRNA RNA_SunLamp;
extern StructRNA RNA_SurfaceCurve;
extern StructRNA RNA_SurfaceModifier;
extern StructRNA RNA_TexMapping;
extern StructRNA RNA_Text;
extern StructRNA RNA_TextBox;
extern StructRNA RNA_TextCharacterFormat;
extern StructRNA RNA_TextCurve;
extern StructRNA RNA_TextLine;
extern StructRNA RNA_TextMarker;
extern StructRNA RNA_Texture;
extern StructRNA RNA_TextureNode;
extern StructRNA RNA_TextureNodeBricks;
extern StructRNA RNA_TextureNodeChecker;
extern StructRNA RNA_TextureNodeCompose;
extern StructRNA RNA_TextureNodeCoordinates;
extern StructRNA RNA_TextureNodeCurveRGB;
extern StructRNA RNA_TextureNodeCurveTime;
extern StructRNA RNA_TextureNodeDecompose;
extern StructRNA RNA_TextureNodeDistance;
extern StructRNA RNA_TextureNodeHueSaturation;
extern StructRNA RNA_TextureNodeImage;
extern StructRNA RNA_TextureNodeInvert;
extern StructRNA RNA_TextureNodeMath;
extern StructRNA RNA_TextureNodeMixRGB;
extern StructRNA RNA_TextureNodeOutput;
extern StructRNA RNA_TextureNodeRGBToBW;
extern StructRNA RNA_TextureNodeRotate;
extern StructRNA RNA_TextureNodeScale;
extern StructRNA RNA_TextureNodeTexture;
extern StructRNA RNA_TextureNodeTranslate;
extern StructRNA RNA_TextureNodeTree;
extern StructRNA RNA_TextureNodeValToNor;
extern StructRNA RNA_TextureNodeValToRGB;
extern StructRNA RNA_TextureNodeViewer;
extern StructRNA RNA_TextureSlot;
extern StructRNA RNA_Theme;
extern StructRNA RNA_ThemeAudioWindow;
extern StructRNA RNA_ThemeBoneColorSet;
extern StructRNA RNA_ThemeConsole;
extern StructRNA RNA_ThemeDopeSheet;
extern StructRNA RNA_ThemeFileBrowser;
extern StructRNA RNA_ThemeFontStyle;
extern StructRNA RNA_ThemeGraphEditor;
extern StructRNA RNA_ThemeImageEditor;
extern StructRNA RNA_ThemeInfo;
extern StructRNA RNA_ThemeLogicEditor;
extern StructRNA RNA_ThemeNLAEditor;
extern StructRNA RNA_ThemeNodeEditor;
extern StructRNA RNA_ThemeOutliner;
extern StructRNA RNA_ThemeProperties;
extern StructRNA RNA_ThemeSequenceEditor;
extern StructRNA RNA_ThemeSpaceGeneric;
extern StructRNA RNA_ThemeSpaceListGeneric;
extern StructRNA RNA_ThemeStyle;
extern StructRNA RNA_ThemeTextEditor;
extern StructRNA RNA_ThemeTimeline;
extern StructRNA RNA_ThemeUserInterface;
extern StructRNA RNA_ThemeUserPreferences;
extern StructRNA RNA_ThemeView3D;
extern StructRNA RNA_ThemeWidgetColors;
extern StructRNA RNA_ThemeWidgetStateColors;
extern StructRNA RNA_TimelineMarker;
extern StructRNA RNA_Timer;
extern StructRNA RNA_ToolSettings;
extern StructRNA RNA_TouchSensor;
extern StructRNA RNA_TrackToConstraint;
extern StructRNA RNA_TransformConstraint;
extern StructRNA RNA_TransformSequence;
extern StructRNA RNA_UILayout;
extern StructRNA RNA_UIListItem;
extern StructRNA RNA_UVProjectModifier;
extern StructRNA RNA_UVProjector;
extern StructRNA RNA_UnitSettings;
extern StructRNA RNA_UnknownType;
extern StructRNA RNA_UserPreferences;
extern StructRNA RNA_UserPreferencesEdit;
extern StructRNA RNA_UserPreferencesFilePaths;
extern StructRNA RNA_UserPreferencesInput;
extern StructRNA RNA_UserPreferencesSystem;
extern StructRNA RNA_UserPreferencesView;
extern StructRNA RNA_UserSolidLight;
extern StructRNA RNA_VectorFont;
extern StructRNA RNA_VertexGroup;
extern StructRNA RNA_VertexGroupElement;
extern StructRNA RNA_VertexPaint;
extern StructRNA RNA_VoronoiTexture;
extern StructRNA RNA_VoxelData;
extern StructRNA RNA_VoxelDataTexture;
extern StructRNA RNA_WarpModifier;
extern StructRNA RNA_WaveModifier;
extern StructRNA RNA_VertexWeightEditModifier;
extern StructRNA RNA_VertexWeightMixModifier;
extern StructRNA RNA_VertexWeightProximityModifier;
extern StructRNA RNA_Window;
extern StructRNA RNA_WindowManager;
extern StructRNA RNA_WipeSequence;
extern StructRNA RNA_WoodTexture;
extern StructRNA RNA_World;
extern StructRNA RNA_WorldAmbientOcclusion;
extern StructRNA RNA_WorldLighting;
extern StructRNA RNA_WorldMistSettings;
extern StructRNA RNA_WorldStarsSettings;
extern StructRNA RNA_WorldTextureSlot;
extern StructRNA RNA_XnorController;
extern StructRNA RNA_XorController;
void RNA_main_pointer_create(struct Main *main, PointerRNA *r_ptr);
void RNA_id_pointer_create(struct ID *id, PointerRNA *r_ptr);
void RNA_pointer_create(struct ID *id, StructRNA *type, void *data, PointerRNA *r_ptr);
void RNA_blender_rna_pointer_create(PointerRNA *r_ptr);
void RNA_pointer_recast(PointerRNA *ptr, PointerRNA *r_ptr);
extern const PointerRNA PointerRNA_NULL;
StructRNA *RNA_struct_find(const char *identifier);
const char *RNA_struct_identifier(StructRNA *type);
const char *RNA_struct_ui_name(StructRNA *type);
const char *RNA_struct_ui_description(StructRNA *type);
int RNA_struct_ui_icon(StructRNA *type);
PropertyRNA *RNA_struct_name_property(StructRNA *type);
PropertyRNA *RNA_struct_iterator_property(StructRNA *type);
StructRNA *RNA_struct_base(StructRNA *type);
int RNA_struct_is_ID(StructRNA *type);
int RNA_struct_is_a(StructRNA *type, StructRNA *srna);
int RNA_struct_undo_check(StructRNA *type);
StructRegisterFunc RNA_struct_register(StructRNA *type);
StructUnregisterFunc RNA_struct_unregister(StructRNA *type);
void **RNA_struct_instance(PointerRNA *ptr);
void *RNA_struct_py_type_get(StructRNA *srna);
void RNA_struct_py_type_set(StructRNA *srna, void *py_type);
void *RNA_struct_blender_type_get(StructRNA *srna);
void RNA_struct_blender_type_set(StructRNA *srna, void *blender_type);
struct IDProperty *RNA_struct_idprops(PointerRNA *ptr, int create);
int RNA_struct_idprops_check(StructRNA *srna);
int RNA_struct_idprops_register_check(StructRNA *type);
int RNA_struct_idprops_unset(PointerRNA *ptr, const char *identifier);
PropertyRNA *RNA_struct_find_property(PointerRNA *ptr, const char *identifier);
int RNA_struct_contains_property(PointerRNA *ptr, PropertyRNA *prop_test);
const struct ListBase *RNA_struct_type_properties(StructRNA *srna);
PropertyRNA *RNA_struct_type_find_property(StructRNA *srna, const char *identifier);
FunctionRNA *RNA_struct_find_function(PointerRNA *ptr, const char *identifier);
const struct ListBase *RNA_struct_type_functions(StructRNA *srna);
char *RNA_struct_name_get_alloc(PointerRNA *ptr, char *fixedbuf, int fixedlen, int *r_len);
const char *RNA_property_identifier(PropertyRNA *prop);
const char *RNA_property_description(PropertyRNA *prop);
PropertyType RNA_property_type(PropertyRNA *prop);
PropertySubType RNA_property_subtype(PropertyRNA *prop);
PropertyUnit RNA_property_unit(PropertyRNA *prop);
int RNA_property_flag(PropertyRNA *prop);
void *RNA_property_py_data_get(PropertyRNA *prop);
int RNA_property_array_length(PointerRNA *ptr, PropertyRNA *prop);
int RNA_property_array_check(PropertyRNA *prop);
int RNA_property_multi_array_length(PointerRNA *ptr, PropertyRNA *prop, int dimension);
int RNA_property_array_dimension(PointerRNA *ptr, PropertyRNA *prop, int length[]);
char RNA_property_array_item_char(PropertyRNA *prop, int index);
int RNA_property_array_item_index(PropertyRNA *prop, char name);
int RNA_property_string_maxlength(PropertyRNA *prop);
const char *RNA_property_ui_name(PropertyRNA *prop);
const char *RNA_property_ui_description(PropertyRNA *prop);
int RNA_property_ui_icon(PropertyRNA *prop);
void RNA_property_int_range(PointerRNA *ptr, PropertyRNA *prop, int *hardmin, int *hardmax);
void RNA_property_int_ui_range(PointerRNA *ptr, PropertyRNA *prop, int *softmin, int *softmax, int *step);
void RNA_property_float_range(PointerRNA *ptr, PropertyRNA *prop, float *hardmin, float *hardmax);
void RNA_property_float_ui_range(PointerRNA *ptr, PropertyRNA *prop, float *softmin, float *softmax, float *step, float *precision);
int RNA_property_float_clamp(PointerRNA *ptr, PropertyRNA *prop, float *value);
int RNA_property_int_clamp(PointerRNA *ptr, PropertyRNA *prop, int *value);
int RNA_enum_identifier(EnumPropertyItem *item, const int value, const char **identifier);
int RNA_enum_bitflag_identifiers(EnumPropertyItem *item, const int value, const char **identifier);
int RNA_enum_name(EnumPropertyItem *item, const int value, const char **name);
int RNA_enum_description(EnumPropertyItem *item, const int value, const char **description);
void RNA_property_enum_items(struct bContext *C, PointerRNA *ptr, PropertyRNA *prop, EnumPropertyItem **item, int *totitem, int *free);
void RNA_property_enum_items_gettexted(struct bContext *C, PointerRNA *ptr, PropertyRNA *prop, EnumPropertyItem **item, int *totitem, int *free);
int RNA_property_enum_value(struct bContext *C, PointerRNA *ptr, PropertyRNA *prop, const char *identifier, int *value);
int RNA_property_enum_identifier(struct bContext *C, PointerRNA *ptr, PropertyRNA *prop, const int value, const char **identifier);
int RNA_property_enum_name(struct bContext *C, PointerRNA *ptr, PropertyRNA *prop, const int value, const char **name);
int RNA_property_enum_bitflag_identifiers(struct bContext *C, PointerRNA *ptr, PropertyRNA *prop, const int value, const char **identifier);
StructRNA *RNA_property_pointer_type(PointerRNA *ptr, PropertyRNA *prop);
int RNA_property_pointer_poll(PointerRNA *ptr, PropertyRNA *prop, PointerRNA *value);
int RNA_property_editable(PointerRNA *ptr, PropertyRNA *prop);
int RNA_property_editable_index(PointerRNA *ptr, PropertyRNA *prop, int index);
int RNA_property_editable_flag(PointerRNA *ptr, PropertyRNA *prop);
int RNA_property_animateable(PointerRNA *ptr, PropertyRNA *prop);
int RNA_property_animated(PointerRNA *ptr, PropertyRNA *prop);
int RNA_property_path_from_ID_check(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_update(struct bContext *C, PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_update_main(struct Main *bmain, struct Scene *scene, PointerRNA *ptr, PropertyRNA *prop);
int RNA_property_update_check(struct PropertyRNA *prop);
void RNA_property_update_cache_add(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_update_cache_flush(struct Main *bmain, struct Scene *scene);
void RNA_property_update_cache_free(void);
int RNA_property_boolean_get(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_boolean_set(PointerRNA *ptr, PropertyRNA *prop, int value);
void RNA_property_boolean_get_array(PointerRNA *ptr, PropertyRNA *prop, int *values);
int RNA_property_boolean_get_index(PointerRNA *ptr, PropertyRNA *prop, int index);
void RNA_property_boolean_set_array(PointerRNA *ptr, PropertyRNA *prop, const int *values);
void RNA_property_boolean_set_index(PointerRNA *ptr, PropertyRNA *prop, int index, int value);
int RNA_property_boolean_get_default(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_boolean_get_default_array(PointerRNA *ptr, PropertyRNA *prop, int *values);
int RNA_property_boolean_get_default_index(PointerRNA *ptr, PropertyRNA *prop, int index);
int RNA_property_int_get(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_int_set(PointerRNA *ptr, PropertyRNA *prop, int value);
void RNA_property_int_get_array(PointerRNA *ptr, PropertyRNA *prop, int *values);
void RNA_property_int_get_array_range(PointerRNA *ptr, PropertyRNA *prop, int values[2]);
int RNA_property_int_get_index(PointerRNA *ptr, PropertyRNA *prop, int index);
void RNA_property_int_set_array(PointerRNA *ptr, PropertyRNA *prop, const int *values);
void RNA_property_int_set_index(PointerRNA *ptr, PropertyRNA *prop, int index, int value);
int RNA_property_int_get_default(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_int_get_default_array(PointerRNA *ptr, PropertyRNA *prop, int *values);
int RNA_property_int_get_default_index(PointerRNA *ptr, PropertyRNA *prop, int index);
float RNA_property_float_get(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_float_set(PointerRNA *ptr, PropertyRNA *prop, float value);
void RNA_property_float_get_array(PointerRNA *ptr, PropertyRNA *prop, float *values);
void RNA_property_float_get_array_range(PointerRNA *ptr, PropertyRNA *prop, float values[2]);
float RNA_property_float_get_index(PointerRNA *ptr, PropertyRNA *prop, int index);
void RNA_property_float_set_array(PointerRNA *ptr, PropertyRNA *prop, const float *values);
void RNA_property_float_set_index(PointerRNA *ptr, PropertyRNA *prop, int index, float value);
float RNA_property_float_get_default(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_float_get_default_array(PointerRNA *ptr, PropertyRNA *prop, float *values);
float RNA_property_float_get_default_index(PointerRNA *ptr, PropertyRNA *prop, int index);
void RNA_property_string_get(PointerRNA *ptr, PropertyRNA *prop, char *value);
char *RNA_property_string_get_alloc(PointerRNA *ptr, PropertyRNA *prop, char *fixedbuf, int fixedlen, int *r_len);
void RNA_property_string_set(PointerRNA *ptr, PropertyRNA *prop, const char *value);
int RNA_property_string_length(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_string_get_default(PointerRNA *ptr, PropertyRNA *prop, char *value);
char *RNA_property_string_get_default_alloc(PointerRNA *ptr, PropertyRNA *prop, char *fixedbuf, int fixedlen);
int RNA_property_string_default_length(PointerRNA *ptr, PropertyRNA *prop);
int RNA_property_enum_get(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_enum_set(PointerRNA *ptr, PropertyRNA *prop, int value);
int RNA_property_enum_get_default(PointerRNA *ptr, PropertyRNA *prop);
void *RNA_property_enum_py_data_get(PropertyRNA *prop);
PointerRNA RNA_property_pointer_get(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_pointer_set(PointerRNA *ptr, PropertyRNA *prop, PointerRNA ptr_value);
PointerRNA RNA_property_pointer_get_default(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_collection_begin(PointerRNA *ptr, PropertyRNA *prop, CollectionPropertyIterator *iter);
void RNA_property_collection_next(CollectionPropertyIterator *iter);
void RNA_property_collection_end(CollectionPropertyIterator *iter);
int RNA_property_collection_length(PointerRNA *ptr, PropertyRNA *prop);
int RNA_property_collection_lookup_index(PointerRNA *ptr, PropertyRNA *prop, PointerRNA *t_ptr);
int RNA_property_collection_lookup_int(PointerRNA *ptr, PropertyRNA *prop, int key, PointerRNA *r_ptr);
int RNA_property_collection_lookup_string(PointerRNA *ptr, PropertyRNA *prop, const char *key, PointerRNA *r_ptr);
int RNA_property_collection_assign_int(PointerRNA *ptr, PropertyRNA *prop, const int key, const PointerRNA *assign_ptr);
int RNA_property_collection_type_get(PointerRNA *ptr, PropertyRNA *prop, PointerRNA *r_ptr);
int RNA_property_collection_raw_array(PointerRNA *ptr, PropertyRNA *prop, PropertyRNA *itemprop, RawArray *array);
int RNA_property_collection_raw_get(struct ReportList *reports, PointerRNA *ptr, PropertyRNA *prop, const char *propname, void *array, RawPropertyType type, int len);
int RNA_property_collection_raw_set(struct ReportList *reports, PointerRNA *ptr, PropertyRNA *prop, const char *propname, void *array, RawPropertyType type, int len);
int RNA_raw_type_sizeof(RawPropertyType type);
RawPropertyType RNA_property_raw_type(PropertyRNA *prop);
void RNA_property_pointer_add(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_pointer_remove(PointerRNA *ptr, PropertyRNA *prop);
void RNA_property_collection_add(PointerRNA *ptr, PropertyRNA *prop, PointerRNA *r_ptr);
int RNA_property_collection_remove(PointerRNA *ptr, PropertyRNA *prop, int key);
void RNA_property_collection_clear(PointerRNA *ptr, PropertyRNA *prop);
int RNA_property_collection_move(PointerRNA *ptr, PropertyRNA *prop, int key, int pos);
int RNA_property_copy(PointerRNA *ptr, PointerRNA *fromptr, PropertyRNA *prop, int index);
int RNA_property_reset(PointerRNA *ptr, PropertyRNA *prop, int index);
char *RNA_path_append(const char *path, PointerRNA *ptr, PropertyRNA *prop,
 int intkey, const char *strkey);
char *RNA_path_back(const char *path);
int RNA_path_resolve(PointerRNA *ptr, const char *path,
  PointerRNA *r_ptr, PropertyRNA **r_prop);
int RNA_path_resolve_full(PointerRNA *ptr, const char *path,
  PointerRNA *r_ptr, PropertyRNA **r_prop, int *index);
char *RNA_path_from_ID_to_struct(PointerRNA *ptr);
char *RNA_path_from_ID_to_property(PointerRNA *ptr, PropertyRNA *prop);
int RNA_boolean_get(PointerRNA *ptr, const char *name);
void RNA_boolean_set(PointerRNA *ptr, const char *name, int value);
void RNA_boolean_get_array(PointerRNA *ptr, const char *name, int *values);
void RNA_boolean_set_array(PointerRNA *ptr, const char *name, const int *values);
int RNA_int_get(PointerRNA *ptr, const char *name);
void RNA_int_set(PointerRNA *ptr, const char *name, int value);
void RNA_int_get_array(PointerRNA *ptr, const char *name, int *values);
void RNA_int_set_array(PointerRNA *ptr, const char *name, const int *values);
float RNA_float_get(PointerRNA *ptr, const char *name);
void RNA_float_set(PointerRNA *ptr, const char *name, float value);
void RNA_float_get_array(PointerRNA *ptr, const char *name, float *values);
void RNA_float_set_array(PointerRNA *ptr, const char *name, const float *values);
int RNA_enum_get(PointerRNA *ptr, const char *name);
void RNA_enum_set(PointerRNA *ptr, const char *name, int value);
void RNA_enum_set_identifier(PointerRNA *ptr, const char *name, const char *id);
int RNA_enum_is_equal(struct bContext *C, PointerRNA *ptr, const char *name, const char *enumname);
int RNA_enum_value_from_id(EnumPropertyItem *item, const char *identifier, int *value);
int RNA_enum_id_from_value(EnumPropertyItem *item, int value, const char **identifier);
int RNA_enum_icon_from_value(EnumPropertyItem *item, int value, int *icon);
void RNA_string_get(PointerRNA *ptr, const char *name, char *value);
char *RNA_string_get_alloc(PointerRNA *ptr, const char *name, char *fixedbuf, int fixedlen);
int RNA_string_length(PointerRNA *ptr, const char *name);
void RNA_string_set(PointerRNA *ptr, const char *name, const char *value);
PointerRNA RNA_pointer_get(PointerRNA *ptr, const char *name);
void RNA_pointer_set(PointerRNA *ptr, const char *name, PointerRNA ptr_value);
void RNA_pointer_add(PointerRNA *ptr, const char *name);
void RNA_collection_begin(PointerRNA *ptr, const char *name, CollectionPropertyIterator *iter);
int RNA_collection_length(PointerRNA *ptr, const char *name);
void RNA_collection_add(PointerRNA *ptr, const char *name, PointerRNA *r_value);
void RNA_collection_clear(PointerRNA *ptr, const char *name);
int RNA_property_is_set(PointerRNA *ptr, PropertyRNA *prop);
int RNA_struct_property_is_set(PointerRNA *ptr, const char *identifier);
int RNA_property_is_idprop(PropertyRNA *prop);
char *RNA_property_as_string(struct bContext *C, PointerRNA *ptr, PropertyRNA *prop);
char *RNA_pointer_as_string(struct bContext *C, PointerRNA *ptr);
const char *RNA_function_identifier(FunctionRNA *func);
const char *RNA_function_ui_description(FunctionRNA *func);
int RNA_function_flag(FunctionRNA *func);
int RNA_function_defined(FunctionRNA *func);
PropertyRNA *RNA_function_get_parameter(PointerRNA *ptr, FunctionRNA *func, int index);
PropertyRNA *RNA_function_find_parameter(PointerRNA *ptr, FunctionRNA *func, const char *identifier);
const struct ListBase *RNA_function_defined_parameters(FunctionRNA *func);
ParameterList *RNA_parameter_list_create(ParameterList *parms, PointerRNA *ptr, FunctionRNA *func);
void RNA_parameter_list_free(ParameterList *parms);
int RNA_parameter_list_size(ParameterList *parms);
int RNA_parameter_list_arg_count(ParameterList *parms);
int RNA_parameter_list_ret_count(ParameterList *parms);
void RNA_parameter_list_begin(ParameterList *parms, ParameterIterator *iter);
void RNA_parameter_list_next(ParameterIterator *iter);
void RNA_parameter_list_end(ParameterIterator *iter);
void RNA_parameter_get(ParameterList *parms, PropertyRNA *parm, void **value);
void RNA_parameter_get_lookup(ParameterList *parms, const char *identifier, void **value);
void RNA_parameter_set(ParameterList *parms, PropertyRNA *parm, const void *value);
void RNA_parameter_set_lookup(ParameterList *parms, const char *identifier, const void *value);
int RNA_parameter_length_get(ParameterList *parms, PropertyRNA *parm);
int RNA_parameter_length_get_data(ParameterList *parms, PropertyRNA *parm, void *data);
void RNA_parameter_length_set(ParameterList *parms, PropertyRNA *parm, int length);
void RNA_parameter_length_set_data(ParameterList *parms, PropertyRNA *parm, void *data, int length);
int RNA_function_call(struct bContext *C, struct ReportList *reports, PointerRNA *ptr, FunctionRNA *func, ParameterList *parms);
int RNA_function_call_lookup(struct bContext *C, struct ReportList *reports, PointerRNA *ptr, const char *identifier, ParameterList *parms);
int RNA_function_call_direct(struct bContext *C, struct ReportList *reports, PointerRNA *ptr, FunctionRNA *func, const char *format, ...)
__attribute__ ((format (printf, 5, 6)))
;
int RNA_function_call_direct_lookup(struct bContext *C, struct ReportList *reports, PointerRNA *ptr, const char *identifier, const char *format, ...)
__attribute__ ((format (printf, 5, 6)))
;
int RNA_function_call_direct_va(struct bContext *C, struct ReportList *reports, PointerRNA *ptr, FunctionRNA *func, const char *format, va_list args);
int RNA_function_call_direct_va_lookup(struct bContext *C, struct ReportList *reports, PointerRNA *ptr, const char *identifier, const char *format, va_list args);
short RNA_type_to_ID_code(StructRNA *type);
StructRNA *ID_code_to_RNA_type(short idcode);
void _RNA_warning(const char *format, ...)
__attribute__ ((format (printf, 1, 2)))
;
int slurph_opt= 1;
void free_key(Key *key)
{
 KeyBlock *kb;
 BKE_free_animdata((ID *)key);
 while( (kb= key->block.first) ) {
  if(kb->data) MEM_freeN(kb->data);
  BLI_remlink(&key->block, kb);
  MEM_freeN(kb);
 }
}
Key *add_key(ID *id)
{
 Key *key;
 char *el;
 key= alloc_libblock(&G.main->key, ( ('E')<<8 | ('K') ), "Key");
 key->type= 0;
 key->from= id;
 if( (*((short *)(id->name)))==( ('E')<<8 | ('M') )) {
  el= key->elemstr;
  el[0]= 3;
  el[1]= 4;
  el[2]= 0;
  key->elemsize= 12;
 }
 else if( (*((short *)(id->name)))==( ('T')<<8 | ('L') )) {
  el= key->elemstr;
  el[0]= 3;
  el[1]= 4;
  el[2]= 0;
  key->elemsize= 12;
 }
 else if( (*((short *)(id->name)))==( ('U')<<8 | ('C') )) {
  el= key->elemstr;
  el[0]= 4;
  el[1]= 101;
  el[2]= 0;
  key->elemsize= 16;
 }
 return key;
}
Key *copy_key(Key *key)
{
 Key *keyn;
 KeyBlock *kbn, *kb;
 if(key==((void *)0)) return ((void *)0);
 keyn= copy_libblock(&key->id);
 BLI_duplicatelist(&keyn->block, &key->block);
 kb= key->block.first;
 kbn= keyn->block.first;
 while(kbn) {
  if(kbn->data) kbn->data= MEM_dupallocN(kbn->data);
  if(kb==key->refkey) keyn->refkey= kbn;
  kbn= kbn->next;
  kb= kb->next;
 }
 return keyn;
}
void make_local_key(Key *key)
{
 if(key==((void *)0)) return;
 key->id.lib= ((void *)0);
 new_id(((void *)0), &key->id, ((void *)0));
}
void sort_keys(Key *key)
{
 KeyBlock *kb;
 KeyBlock *kb2;
 for (kb= key->block.first; kb; kb= kb->next)
  if ((kb->next) && (kb->pos > kb->next->pos))
   break;
 if (kb) {
  kb = kb->next;
  BLI_remlink(&key->block, kb);
  for (kb2=key->block.first; kb2; kb2= kb2->next) {
   if (kb2->pos > kb->pos) {
    BLI_insertlink(&key->block, kb2->prev, kb);
    break;
   }
  }
 }
 key->refkey= key->block.first;
}
void key_curve_position_weights(float t, float *data, int type)
{
 float t2, t3, fc;
 if(type==0) {
  data[0]= 0.0f;
  data[1]= -t + 1.0f;
  data[2]= t;
  data[3]= 0.0f;
 }
 else if(type==1) {
  t2= t*t;
  t3= t2*t;
  fc= 0.71f;
  data[0]= -fc*t3 + 2.0f*fc*t2 - fc*t;
  data[1]= (2.0f-fc)*t3 + (fc-3.0f)*t2 + 1.0f;
  data[2]= (fc-2.0f)*t3 + (3.0f-2.0f*fc)*t2 + fc*t;
  data[3]= fc*t3 - fc*t2;
 }
 else if(type==2) {
  t2= t*t;
  t3= t2*t;
  data[0]= -0.16666666f*t3 + 0.5f*t2 - 0.5f*t + 0.16666666f;
  data[1]= 0.5f*t3 - t2 + 0.6666666f;
  data[2]= -0.5f*t3 + 0.5f*t2 + 0.5f*t + 0.16666666f;
  data[3]= 0.16666666f*t3;
 }
}
void key_curve_tangent_weights(float t, float *data, int type)
{
 float t2, fc;
 if(type==0) {
  data[0]= 0.0f;
  data[1]= -1.0f;
  data[2]= 1.0f;
  data[3]= 0.0f;
 }
 else if(type==1) {
  t2= t*t;
  fc= 0.71f;
  data[0]= -3.0f*fc*t2 +4.0f*fc*t - fc;
  data[1]= 3.0f*(2.0f-fc)*t2 +2.0f*(fc-3.0f)*t;
  data[2]= 3.0f*(fc-2.0f)*t2 +2.0f*(3.0f-2.0f*fc)*t + fc;
  data[3]= 3.0f*fc*t2 -2.0f*fc*t;
 }
 else if(type==2) {
  t2= t*t;
  data[0]= -0.5f*t2 + t - 0.5f;
  data[1]= 1.5f*t2 - 2.0f*t;
  data[2]= -1.5f*t2 + t + 0.5f;
  data[3]= 0.5f*t2;
 }
}
void key_curve_normal_weights(float t, float *data, int type)
{
 float fc;
 if(type==0) {
  data[0]= 0.0f;
  data[1]= 0.0f;
  data[2]= 0.0f;
  data[3]= 0.0f;
 }
 else if(type==1) {
  fc= 0.71f;
  data[0]= -6.0f*fc*t + 4.0f*fc;
  data[1]= 6.0f*(2.0f-fc)*t + 2.0f*(fc-3.0f);
  data[2]= 6.0f*(fc-2.0f)*t + 2.0f*(3.0f-2.0f*fc);
  data[3]= 6.0f*fc*t - 2.0f*fc;
 }
 else if(type==2) {
  data[0]= -1.0f*t + 1.0f;
  data[1]= 3.0f*t - 2.0f;
  data[2]= -3.0f*t + 1.0f;
  data[3]= 1.0f*t;
 }
}
static int setkeys(float fac, ListBase *lb, KeyBlock *k[], float *t, int cycl)
{
 KeyBlock *k1, *firstkey;
 float d, dpos, ofs=0, lastpos, temp, fval[4];
 short bsplinetype;
 firstkey= lb->first;
 k1= lb->last;
 lastpos= k1->pos;
 dpos= lastpos - firstkey->pos;
 if(fac < firstkey->pos) fac= firstkey->pos;
 else if(fac > k1->pos) fac= k1->pos;
 k1=k[0]=k[1]=k[2]=k[3]= firstkey;
 t[0]=t[1]=t[2]=t[3]= k1->pos;
 if(k1->next==((void *)0)) return 1;
 if(cycl) {
  k[2]= k1->next;
  k[3]= k[2]->next;
  if(k[3]==((void *)0)) k[3]=k1;
  while(k1) {
   if(k1->next==((void *)0)) k[0]=k1;
   k1=k1->next;
  }
  t[0]= k[0]->pos;
  t[1]+= dpos;
  t[2]= k[2]->pos + dpos;
  t[3]= k[3]->pos + dpos;
  fac+= dpos;
  ofs= dpos;
  if(k[3]==k[1]) {
   t[3]+= dpos;
   ofs= 2.0f*dpos;
  }
  if(fac<t[1]) fac+= dpos;
  k1= k[3];
 }
 else {
  k[2]= k1->next;
  t[2]= k[2]->pos;
  k[3]= k[2]->next;
  if(k[3]==((void *)0)) k[3]= k[2];
  t[3]= k[3]->pos;
  k1= k[3];
 }
 while( t[2]<fac ) {
  if(k1->next==((void *)0)) {
   if(cycl) {
    k1= firstkey;
    ofs+= dpos;
   }
   else if(t[2]==t[3]) break;
  }
  else k1= k1->next;
  t[0]= t[1];
  k[0]= k[1];
  t[1]= t[2];
  k[1]= k[2];
  t[2]= t[3];
  k[2]= k[3];
  t[3]= k1->pos+ofs;
  k[3]= k1;
  if(ofs > 2.1f + lastpos) break;
 }
 bsplinetype= 0;
 if(k[1]->type==2 || k[2]->type==2) bsplinetype= 1;
 if(cycl==0) {
  if(bsplinetype==0) {
   if(fac<=t[1]) {
    t[2]= t[1];
    k[2]= k[1];
    return 1;
   }
   if(fac>=t[2] ) {
    return 1;
   }
  }
  else if(fac>t[2]) {
   fac= t[2];
   k[3]= k[2];
   t[3]= t[2];
  }
 }
 d= t[2]-t[1];
 if(d == 0.0f) {
  if(bsplinetype==0) {
   return 1;
  }
 }
 else d= (fac-t[1])/d;
 key_curve_position_weights(d, t, k[1]->type);
 if(k[1]->type != k[2]->type) {
  key_curve_position_weights(d, fval, k[2]->type);
  temp= 1.0f-d;
  t[0]= temp*t[0]+ d*fval[0];
  t[1]= temp*t[1]+ d*fval[1];
  t[2]= temp*t[2]+ d*fval[2];
  t[3]= temp*t[3]+ d*fval[3];
 }
 return 0;
}
static void flerp(int tot, float *in, float *f0, float *f1, float *f2, float *f3, float *t)
{
 int a;
 for(a=0; a<tot; a++) {
  in[a]= t[0]*f0[a]+t[1]*f1[a]+t[2]*f2[a]+t[3]*f3[a];
 }
}
static void rel_flerp(int tot, float *in, float *ref, float *out, float fac)
{
 int a;
 for(a=0; a<tot; a++) {
  in[a]-= fac*(ref[a]-out[a]);
 }
}
static char *key_block_get_data(Key *key, KeyBlock *actkb, KeyBlock *kb, char **freedata)
{
 if(kb == actkb) {
  if((*((short *)(key->from->name))) == ( ('E')<<8 | ('M') )) {
   Mesh *me;
   EditVert *eve;
   float (*co)[3];
   int a;
   me= (Mesh*)key->from;
   if(me->edit_mesh && me->edit_mesh->totvert == kb->totelem) {
    a= 0;
    co= MEM_callocN(sizeof(float)*3*me->edit_mesh->totvert, "key_block_get_data");
    for(eve=me->edit_mesh->verts.first; eve; eve=eve->next, a++)
     copy_v3_v3(co[a], eve->co);
    *freedata= (char*)co;
    return (char*)co;
   }
  }
 }
 *freedata= ((void *)0);
 return kb->data;
}
static short key_pointer_size(const Key *key, const int mode, int *poinsize, int *ofs)
{
 if(key->from==((void *)0)) {
  return 0;
 }
 switch((*((short *)(key->from->name)))) {
 case ( ('E')<<8 | ('M') ):
  *ofs= sizeof(float)*3;
  *poinsize= *ofs;
  break;
 case ( ('T')<<8 | ('L') ):
  *ofs= sizeof(float)*3;
  *poinsize= *ofs;
  break;
 case ( ('U')<<8 | ('C') ):
  if(mode == 1) {
   *ofs= sizeof(float)*4;
   *poinsize= *ofs;
  } else {
   ofs[0]= sizeof(float)*12;
   *poinsize= (*ofs) / 3;
  }
  break;
 default:
  (void)0;
  return 0;
 }
 return 1;
}
static void cp_key(const int start, int end, const int tot, char *poin, Key *key, KeyBlock *actkb, KeyBlock *kb, float *weights, const int mode)
{
 float ktot = 0.0, kd = 0.0;
 int elemsize, poinsize = 0, a, *ofsp, ofs[32], flagflo=0;
 char *k1, *kref, *freek1, *freekref;
 char *cp, elemstr[8];
 ofs[1]= 0;
 if(!key_pointer_size(key, mode, &poinsize, &ofs[0]))
  return;
 if(end>tot) end= tot;
 if(tot != kb->totelem) {
  ktot= 0.0;
  flagflo= 1;
  if(kb->totelem) {
   kd= kb->totelem/(float)tot;
  }
  else return;
 }
 k1= key_block_get_data(key, actkb, kb, &freek1);
 kref= key_block_get_data(key, actkb, key->refkey, &freekref);
 if(start!=0) {
  poin+= poinsize*start;
  if(flagflo) {
   ktot+= start*kd;
   a= (int)floor(ktot);
   if(a) {
    ktot-= a;
    k1+= a*key->elemsize;
   }
  }
  else k1+= start*key->elemsize;
 }
 if(mode == 2) {
  elemstr[0]= 1;
  elemstr[1]= 100;
  elemstr[2]= 0;
 }
 elemsize= key->elemsize;
 if(mode == 2) elemsize*= 3;
 for(a=start; a<end; a++) {
  cp= key->elemstr;
  if(mode == 2) cp= elemstr;
  ofsp= ofs;
  while( cp[0] ) {
   switch(cp[1]) {
   case 4:
    if(weights) {
     memcpy(poin, kref, sizeof(float)*3);
     if(*weights!=0.0f)
      rel_flerp(cp[0], (float *)poin, (float *)kref, (float *)k1, *weights);
     weights++;
    }
    else
     memcpy(poin, k1, sizeof(float)*3);
    break;
   case 101:
    memcpy(poin, k1, sizeof(float)*4);
    break;
   case 100:
    memcpy(poin, k1, sizeof(float)*12);
    break;
   default:
    if(freek1) MEM_freeN(freek1);
    if(freekref) MEM_freeN(freekref);
    (void)0;
    return;
   }
   poin+= ofsp[0];
   cp+= 2; ofsp++;
  }
  if(flagflo) {
   ktot+= kd;
   while(ktot >= 1.0f) {
    ktot -= 1.0f;
    k1+= elemsize;
    kref+= elemsize;
   }
  }
  else {
   k1+= elemsize;
   kref+= elemsize;
  }
  if(mode == 2) a+=2;
 }
 if(freek1) MEM_freeN(freek1);
 if(freekref) MEM_freeN(freekref);
}
static void cp_cu_key(Curve *cu, Key *key, KeyBlock *actkb, KeyBlock *kb, const int start, int end, char *out, const int tot)
{
 Nurb *nu;
 int a, step, a1, a2;
 for(a=0, nu=cu->nurb.first; nu; nu=nu->next, a+=step) {
  if(nu->bp) {
   step= nu->pntsu*nu->pntsv;
   a1= ( (a)>(start) ? (a) : (start) );
   a2= ( (a+step)<(end) ? (a+step) : (end) );
   if(a1<a2) cp_key(a1, a2, tot, out, key, actkb, kb, ((void *)0), 1);
  }
  else if(nu->bezt) {
   step= 3*nu->pntsu;
   a1= ( (a)>(start) ? (a) : (start) );
   a2= ( (a+step)<(end) ? (a+step) : (end) );
   if(a1<a2) cp_key(a1, a2, tot, out, key, actkb, kb, ((void *)0), 2);
  }
  else
   step= 0;
 }
}
void do_rel_key(const int start, int end, const int tot, char *basispoin, Key *key, KeyBlock *actkb, const int mode)
{
 KeyBlock *kb;
 int *ofsp, ofs[3], elemsize, b;
 char *cp, *poin, *reffrom, *from, elemstr[8];
 char *freefrom, *freereffrom;
 int poinsize;
 ofs[1]= 0;
 if(!key_pointer_size(key, mode, &poinsize, &ofs[0]))
  return;
 if(end>tot) end= tot;
 elemstr[0]= 1;
 elemstr[1]= 100;
 elemstr[2]= 0;
 elemsize= key->elemsize;
 if(mode == 2) elemsize*= 3;
 cp_key(start, end, tot, basispoin, key, actkb, key->refkey, ((void *)0), mode);
 for(kb=key->block.first; kb; kb=kb->next) {
  if(kb!=key->refkey) {
   float icuval= kb->curval;
   if(!(kb->flag & (1<<0)) && icuval!=0.0f && kb->totelem==tot) {
    KeyBlock *refb;
    float weight, *weights= kb->weights;
    refb= BLI_findlink(&key->block, kb->relative);
    if(refb==((void *)0)) continue;
    poin= basispoin;
    from= key_block_get_data(key, actkb, kb, &freefrom);
    reffrom= key_block_get_data(key, actkb, refb, &freereffrom);
    poin+= start*poinsize;
    reffrom+= key->elemsize*start;
    from+= key->elemsize*start;
    for(b=start; b<end; b++) {
     if(weights)
      weight= *weights * icuval;
     else
      weight= icuval;
     cp= key->elemstr;
     if(mode == 2) cp= elemstr;
     ofsp= ofs;
     while( cp[0] ) {
      switch(cp[1]) {
      case 4:
       rel_flerp(3, (float *)poin, (float *)reffrom, (float *)from, weight);
       break;
      case 101:
       rel_flerp(4, (float *)poin, (float *)reffrom, (float *)from, weight);
       break;
      case 100:
       rel_flerp(12, (float *)poin, (float *)reffrom, (float *)from, weight);
       break;
      default:
       if(freefrom) MEM_freeN(freefrom);
       if(freereffrom) MEM_freeN(freereffrom);
       (void)0;
       return;
      }
      poin+= ofsp[0];
      cp+= 2;
      ofsp++;
     }
     reffrom+= elemsize;
     from+= elemsize;
     if(mode == 2) b+= 2;
     if(weights) weights++;
    }
    if(freefrom) MEM_freeN(freefrom);
    if(freereffrom) MEM_freeN(freereffrom);
   }
  }
 }
}
static void do_key(const int start, int end, const int tot, char *poin, Key *key, KeyBlock *actkb, KeyBlock **k, float *t, const int mode)
{
 float k1tot = 0.0, k2tot = 0.0, k3tot = 0.0, k4tot = 0.0;
 float k1d = 0.0, k2d = 0.0, k3d = 0.0, k4d = 0.0;
 int a, ofs[32], *ofsp;
 int flagdo= 15, flagflo=0, elemsize, poinsize=0;
 char *k1, *k2, *k3, *k4, *freek1, *freek2, *freek3, *freek4;
 char *cp, elemstr[8];
 ofs[1]= 0;
 if(!key_pointer_size(key, mode, &poinsize, &ofs[0]))
  return;
 if(end>tot) end= tot;
 k1= key_block_get_data(key, actkb, k[0], &freek1);
 k2= key_block_get_data(key, actkb, k[1], &freek2);
 k3= key_block_get_data(key, actkb, k[2], &freek3);
 k4= key_block_get_data(key, actkb, k[3], &freek4);
 if(tot != k[0]->totelem) {
  k1tot= 0.0;
  flagflo |= 1;
  if(k[0]->totelem) {
   k1d= k[0]->totelem/(float)tot;
  }
  else flagdo -= 1;
 }
 if(tot != k[1]->totelem) {
  k2tot= 0.0;
  flagflo |= 2;
  if(k[0]->totelem) {
   k2d= k[1]->totelem/(float)tot;
  }
  else flagdo -= 2;
 }
 if(tot != k[2]->totelem) {
  k3tot= 0.0;
  flagflo |= 4;
  if(k[0]->totelem) {
   k3d= k[2]->totelem/(float)tot;
  }
  else flagdo -= 4;
 }
 if(tot != k[3]->totelem) {
  k4tot= 0.0;
  flagflo |= 8;
  if(k[0]->totelem) {
   k4d= k[3]->totelem/(float)tot;
  }
  else flagdo -= 8;
 }
 if(start!=0) {
  poin+= poinsize*start;
  if(flagdo & 1) {
   if(flagflo & 1) {
    k1tot+= start*k1d;
    a= (int)floor(k1tot);
    if(a) {
     k1tot-= a;
     k1+= a*key->elemsize;
    }
   }
   else k1+= start*key->elemsize;
  }
  if(flagdo & 2) {
   if(flagflo & 2) {
    k2tot+= start*k2d;
    a= (int)floor(k2tot);
    if(a) {
     k2tot-= a;
     k2+= a*key->elemsize;
    }
   }
   else k2+= start*key->elemsize;
  }
  if(flagdo & 4) {
   if(flagflo & 4) {
    k3tot+= start*k3d;
    a= (int)floor(k3tot);
    if(a) {
     k3tot-= a;
     k3+= a*key->elemsize;
    }
   }
   else k3+= start*key->elemsize;
  }
  if(flagdo & 8) {
   if(flagflo & 8) {
    k4tot+= start*k4d;
    a= (int)floor(k4tot);
    if(a) {
     k4tot-= a;
     k4+= a*key->elemsize;
    }
   }
   else k4+= start*key->elemsize;
  }
 }
 elemstr[0]= 1;
 elemstr[1]= 100;
 elemstr[2]= 0;
 elemsize= key->elemsize;
 if(mode == 2) elemsize*= 3;
 for(a=start; a<end; a++) {
  cp= key->elemstr;
  if(mode == 2) cp= elemstr;
  ofsp= ofs;
  while( cp[0] ) {
   switch(cp[1]) {
   case 4:
    flerp(3, (float *)poin, (float *)k1, (float *)k2, (float *)k3, (float *)k4, t);
    break;
   case 101:
    flerp(4, (float *)poin, (float *)k1, (float *)k2, (float *)k3, (float *)k4, t);
    break;
   case 100:
    flerp(12, (void *)poin, (void *)k1, (void *)k2, (void *)k3, (void *)k4, t);
    break;
   default:
    if(freek1) MEM_freeN(freek1);
    if(freek2) MEM_freeN(freek2);
    if(freek3) MEM_freeN(freek3);
    if(freek4) MEM_freeN(freek4);
    (void)0;
    return;
   }
   poin+= ofsp[0];
   cp+= 2;
   ofsp++;
  }
  if(flagdo & 1) {
   if(flagflo & 1) {
    k1tot+= k1d;
    while(k1tot >= 1.0f) {
     k1tot -= 1.0f;
     k1+= elemsize;
    }
   }
   else k1+= elemsize;
  }
  if(flagdo & 2) {
   if(flagflo & 2) {
    k2tot+= k2d;
    while(k2tot >= 1.0f) {
     k2tot -= 1.0f;
     k2+= elemsize;
    }
   }
   else k2+= elemsize;
  }
  if(flagdo & 4) {
   if(flagflo & 4) {
    k3tot+= k3d;
    while(k3tot >= 1.0f) {
     k3tot -= 1.0f;
     k3+= elemsize;
    }
   }
   else k3+= elemsize;
  }
  if(flagdo & 8) {
   if(flagflo & 8) {
    k4tot+= k4d;
    while(k4tot >= 1.0f) {
     k4tot -= 1.0f;
     k4+= elemsize;
    }
   }
   else k4+= elemsize;
  }
  if(mode == 2) a+= 2;
 }
 if(freek1) MEM_freeN(freek1);
 if(freek2) MEM_freeN(freek2);
 if(freek3) MEM_freeN(freek3);
 if(freek4) MEM_freeN(freek4);
}
static float *get_weights_array(Object *ob, char *vgroup)
{
 MDeformVert *dvert= ((void *)0);
 EditMesh *em= ((void *)0);
 EditVert *eve;
 int totvert= 0, defgrp_index= 0;
 if(vgroup[0]==0) return ((void *)0);
 if(ob->type==1) {
  Mesh *me= ob->data;
  dvert= me->dvert;
  totvert= me->totvert;
  if(me->edit_mesh && me->edit_mesh->totvert == totvert)
   em= me->edit_mesh;
 }
 else if(ob->type==22) {
  Lattice *lt= ob->data;
  dvert= lt->dvert;
  totvert= lt->pntsu*lt->pntsv*lt->pntsw;
 }
 if(dvert==((void *)0)) return ((void *)0);
 defgrp_index= defgroup_name_index(ob, vgroup);
 if(defgrp_index >= 0) {
  float *weights;
  int i;
  weights= MEM_callocN(totvert*sizeof(float), "weights");
  if(em) {
   for(i=0, eve=em->verts.first; eve; eve=eve->next, i++) {
    dvert= CustomData_em_get(&em->vdata, eve->data, 2);
    if(dvert) {
     weights[i]= defvert_find_weight(dvert, defgrp_index);
    }
   }
  }
  else {
   for(i=0; i < totvert; i++, dvert++) {
    weights[i]= defvert_find_weight(dvert, defgrp_index);
   }
  }
  return weights;
 }
 return ((void *)0);
}
static void do_mesh_key(Scene *scene, Object *ob, Key *key, char *out, const int tot)
{
 KeyBlock *k[4], *actkb= ob_get_keyblock(ob);
 float cfra, ctime, t[4], delta;
 int a, flag = 0, step;
 if(key->slurph && key->type!=1 ) {
  delta= key->slurph;
  delta/= tot;
  step= 1;
  if(tot>100 && slurph_opt) {
   step= tot/50;
   delta*= step;
  }
  cfra= (float)scene->r.cfra;
  for(a=0; a<tot; a+=step, cfra+= delta) {
   ctime= BKE_curframe(scene);
   ctime /= 100.0f;
   if((ctime)<(0.0f)) (ctime)=(0.0f); else if((ctime)>(1.0f)) (ctime)=(1.0f);
   flag= setkeys(ctime, &key->block, k, t, 0);
   if(flag==0)
    do_key(a, a+step, tot, (char *)out, key, actkb, k, t, 0);
   else
    cp_key(a, a+step, tot, (char *)out, key, actkb, k[2], ((void *)0), 0);
  }
 }
 else {
  if(key->type==1) {
   KeyBlock *kb;
   for(kb= key->block.first; kb; kb= kb->next)
    kb->weights= get_weights_array(ob, kb->vgroup);
   do_rel_key(0, tot, tot, (char *)out, key, actkb, 0);
   for(kb= key->block.first; kb; kb= kb->next) {
    if(kb->weights) MEM_freeN(kb->weights);
    kb->weights= ((void *)0);
   }
  }
  else {
   ctime= BKE_curframe(scene);
   ctime /= 100.0f;
   if((ctime)<(0.0f)) (ctime)=(0.0f); else if((ctime)>(1.0f)) (ctime)=(1.0f);
   flag= setkeys(ctime, &key->block, k, t, 0);
   if(flag==0)
    do_key(0, tot, tot, (char *)out, key, actkb, k, t, 0);
   else
    cp_key(0, tot, tot, (char *)out, key, actkb, k[2], ((void *)0), 0);
  }
 }
}
static void do_cu_key(Curve *cu, Key *key, KeyBlock *actkb, KeyBlock **k, float *t, char *out, const int tot)
{
 Nurb *nu;
 int a, step;
 for(a=0, nu=cu->nurb.first; nu; nu=nu->next, a+=step) {
  if(nu->bp) {
   step= nu->pntsu*nu->pntsv;
   do_key(a, a+step, tot, out, key, actkb, k, t, 1);
  }
  else if(nu->bezt) {
   step= 3*nu->pntsu;
   do_key(a, a+step, tot, out, key, actkb, k, t, 2);
  }
  else
   step= 0;
 }
}
static void do_rel_cu_key(Curve *cu, Key *key, KeyBlock *actkb, float UNUSED_ctime __attribute__((__unused__)), char *out, const int tot)
{
 Nurb *nu;
 int a, step;
 for(a=0, nu=cu->nurb.first; nu; nu=nu->next, a+=step) {
  if(nu->bp) {
   step= nu->pntsu*nu->pntsv;
   do_rel_key(a, a+step, tot, out, key, actkb, 1);
  }
  else if(nu->bezt) {
   step= 3*nu->pntsu;
   do_rel_key(a, a+step, tot, out, key, actkb, 2);
  }
  else
   step= 0;
 }
}
static void do_curve_key(Scene *scene, Object *ob, Key *key, char *out, const int tot)
{
 Curve *cu= ob->data;
 KeyBlock *k[4], *actkb= ob_get_keyblock(ob);
 float cfra, ctime, t[4], delta;
 int a, flag = 0, step = 0;
 if(key->slurph && key->type!=1) {
  Nurb *nu;
  int mode=0, i= 0, remain= 0, estep=0, count=0;
  delta= (float)key->slurph / tot;
  step= 1;
  if(tot>100 && slurph_opt) {
   step= tot/50;
   delta*= step;
  }
  cfra= (float)scene->r.cfra;
  for(nu=cu->nurb.first; nu; nu=nu->next) {
   if(nu->bp) {
    mode= 1;
    estep= nu->pntsu*nu->pntsv;
   }
   else if(nu->bezt) {
    mode= 2;
    estep= 3*nu->pntsu;
   }
   else
    step= 0;
   a= 0;
   while (a < estep) {
    if (remain <= 0) {
     cfra+= delta;
     ctime= BKE_curframe(scene);
     ctime /= 100.0f;
     if((ctime)<(0.0f)) (ctime)=(0.0f); else if((ctime)>(1.0f)) (ctime)=(1.0f);
     flag= setkeys(ctime, &key->block, k, t, 0);
     remain= step;
    }
    count= ( (remain)<(estep) ? (remain) : (estep) );
    if (mode == 2) {
     count += 3 - count % 3;
    }
    if(flag==0)
     do_key(i, i+count, tot, (char *)out, key, actkb, k, t, mode);
    else
     cp_key(i, i+count, tot, (char *)out, key, actkb, k[2], ((void *)0), mode);
    a += count;
    i += count;
    remain -= count;
   }
  }
 }
 else {
  ctime= BKE_curframe(scene);
  if(key->type==1) {
   do_rel_cu_key(cu, cu->key, actkb, ctime, out, tot);
  }
  else {
   flag= setkeys(ctime, &key->block, k, t, 0);
   if(flag==0) do_cu_key(cu, key, actkb, k, t, out, tot);
   else cp_cu_key(cu, key, actkb, k[2], 0, tot, out, tot);
  }
 }
}
static void do_latt_key(Scene *scene, Object *ob, Key *key, char *out, const int tot)
{
 Lattice *lt= ob->data;
 KeyBlock *k[4], *actkb= ob_get_keyblock(ob);
 float delta, cfra, ctime, t[4];
 int a, flag;
 if(key->slurph) {
  delta= key->slurph;
  delta/= (float)tot;
  cfra= (float)scene->r.cfra;
  for(a=0; a<tot; a++, cfra+= delta) {
   ctime= BKE_curframe(scene);
   flag= setkeys(ctime, &key->block, k, t, 0);
   if(flag==0)
    do_key(a, a+1, tot, out, key, actkb, k, t, 0);
   else
    cp_key(a, a+1, tot, out, key, actkb, k[2], ((void *)0), 0);
  }
 }
 else {
  if(key->type==1) {
   KeyBlock *kb;
   for(kb= key->block.first; kb; kb= kb->next)
    kb->weights= get_weights_array(ob, kb->vgroup);
   do_rel_key(0, tot, tot, out, key, actkb, 0);
   for(kb= key->block.first; kb; kb= kb->next) {
    if(kb->weights) MEM_freeN(kb->weights);
    kb->weights= ((void *)0);
   }
  }
  else {
   ctime= BKE_curframe(scene);
   flag= setkeys(ctime, &key->block, k, t, 0);
   if(flag==0)
    do_key(0, tot, tot, (char *)out, key, actkb, k, t, 0);
   else
    cp_key(0, tot, tot, (char *)out, key, actkb, k[2], ((void *)0), 0);
  }
 }
 if(lt->flag & 2) outside_lattice(lt);
}
float *do_ob_key(Scene *scene, Object *ob)
{
 Key *key= ob_get_key(ob);
 KeyBlock *actkb= ob_get_keyblock(ob);
 char *out;
 int tot= 0, size= 0;
 if(key==((void *)0) || key->block.first==((void *)0))
  return ((void *)0);
 if(ob->type == 1) {
  Mesh *me= ob->data;
  tot= me->totvert;
  size= tot*3*sizeof(float);
 }
 else if(ob->type == 22) {
  Lattice *lt= ob->data;
  tot= lt->pntsu*lt->pntsv*lt->pntsw;
  size= tot*3*sizeof(float);
 }
 else if(( (ob->type)==(2) || (ob->type)==(3) )) {
  Curve *cu= ob->data;
  Nurb *nu;
  for(nu=cu->nurb.first; nu; nu=nu->next) {
   if(nu->bezt) {
    tot += 3*nu->pntsu;
    size += nu->pntsu*12*sizeof(float);
   }
   else if(nu->bp) {
    tot += nu->pntsu*nu->pntsv;
    size += nu->pntsu*nu->pntsv*12*sizeof(float);
   }
  }
 }
 if(tot == 0 || size == 0)
  return ((void *)0);
 out= MEM_callocN(size, "do_ob_key out");
 key->from= (ID *)ob->data;
 if(ob->shapeflag & 1) {
  KeyBlock *kb= BLI_findlink(&key->block, ob->shapenr-1);
  if(kb && (kb->flag & (1<<0)))
   kb= key->refkey;
  if(kb==((void *)0)) {
   kb= key->block.first;
   ob->shapenr= 1;
  }
  if ((( (ob->type)==(1) || (ob->type)==(22) ))) {
   float *weights= get_weights_array(ob, kb->vgroup);
   cp_key(0, tot, tot, out, key, actkb, kb, weights, 0);
   if(weights) MEM_freeN(weights);
  }
  else if(( (ob->type)==(2) || (ob->type)==(3) ))
   cp_cu_key(ob->data, key, actkb, kb, 0, tot, out, tot);
 }
 else {
  float ctime= (float)scene->r.cfra;
  BKE_animsys_evaluate_animdata(scene, &key->id, key->adt, ctime, ADT_RECALC_DRIVERS);
  if(ob->type==1) do_mesh_key(scene, ob, key, out, tot);
  else if(ob->type==22) do_latt_key(scene, ob, key, out, tot);
  else if(ob->type==2) do_curve_key(scene, ob, key, out, tot);
  else if(ob->type==3) do_curve_key(scene, ob, key, out, tot);
 }
 return (float*)out;
}
Key *ob_get_key(Object *ob)
{
 if(ob==((void *)0)) return ((void *)0);
 if(ob->type==1) {
  Mesh *me= ob->data;
  return me->key;
 }
 else if ( (ob->type)==(2) || (ob->type)==(3) ) {
  Curve *cu= ob->data;
  return cu->key;
 }
 else if(ob->type==22) {
  Lattice *lt= ob->data;
  return lt->key;
 }
 return ((void *)0);
}
KeyBlock *add_keyblock(Key *key, const char *name)
{
 KeyBlock *kb;
 float curpos= -0.1;
 int tot;
 kb= key->block.last;
 if(kb) curpos= kb->pos;
 kb= MEM_callocN(sizeof(KeyBlock), "Keyblock");
 BLI_addtail(&key->block, kb);
 kb->type= 1;
 tot= BLI_countlist(&key->block);
 if(name) {
  BLI_strncpy(kb->name, name, sizeof(kb->name));
 } else {
  if(tot==1) BLI_strncpy(kb->name, "Basis", sizeof(kb->name));
  else BLI_snprintf(kb->name, sizeof(kb->name), "Key %d", tot-1);
 }
 BLI_uniquename(&key->block, kb, "Key", '.', __builtin_offsetof (KeyBlock, name), sizeof(kb->name));
 kb->adrcode= tot-1;
 key->totkey++;
 if(key->totkey==1) key->refkey= kb;
 kb->slidermin= 0.0f;
 kb->slidermax= 1.0f;
 if(key->type == 1)
  kb->pos= curpos + 0.1f;
 else {
 }
 return kb;
}
KeyBlock *ob_get_keyblock(Object *ob)
{
 Key *key= ob_get_key(ob);
 if (key) {
  KeyBlock *kb= BLI_findlink(&key->block, ob->shapenr-1);
  return kb;
 }
 return ((void *)0);
}
KeyBlock *ob_get_reference_keyblock(Object *ob)
{
 Key *key= ob_get_key(ob);
 if (key)
  return key->refkey;
 return ((void *)0);
}
KeyBlock *key_get_keyblock(Key *key, int index)
{
 KeyBlock *kb;
 int i;
 if (key) {
  kb= key->block.first;
  for (i= 1; i < key->totkey; i++) {
   kb= kb->next;
   if (index==i)
    return kb;
  }
 }
 return ((void *)0);
}
KeyBlock *key_get_named_keyblock(Key *key, const char name[])
{
 if (key && name)
  return BLI_findstring(&key->block, name, __builtin_offsetof (KeyBlock, name));
 return ((void *)0);
}
char *key_get_curValue_rnaPath(Key *key, KeyBlock *kb)
{
 PointerRNA ptr;
 PropertyRNA *prop;
 if ( (((void *)0))==(key) || (((void *)0))==(kb) )
  return ((void *)0);
 RNA_pointer_create(&key->id, &RNA_ShapeKey, kb, &ptr);
 prop= RNA_struct_find_property(&ptr, "value");
 return RNA_path_from_ID_to_property(&ptr, prop);
}
void latt_to_key(Lattice *lt, KeyBlock *kb)
{
 BPoint *bp;
 float *fp;
 int a, tot;
 tot= lt->pntsu*lt->pntsv*lt->pntsw;
 if(tot==0) return;
 if(kb->data) MEM_freeN(kb->data);
 kb->data= MEM_callocN(lt->key->elemsize*tot, "kb->data");
 kb->totelem= tot;
 bp= lt->def;
 fp= kb->data;
 for(a=0; a<kb->totelem; a++, fp+=3, bp++) {
  copy_v3_v3(fp, bp->vec);
 }
}
void key_to_latt(KeyBlock *kb, Lattice *lt)
{
 BPoint *bp;
 float *fp;
 int a, tot;
 bp= lt->def;
 fp= kb->data;
 tot= lt->pntsu*lt->pntsv*lt->pntsw;
 tot= ( (kb->totelem)<(tot) ? (kb->totelem) : (tot) );
 for(a=0; a<tot; a++, fp+=3, bp++) {
  copy_v3_v3(bp->vec, fp);
 }
}
void curve_to_key(Curve *cu, KeyBlock *kb, ListBase *nurb)
{
 Nurb *nu;
 BezTriple *bezt;
 BPoint *bp;
 float *fp;
 int a, tot;
 tot= count_curveverts(nurb);
 if(tot==0) return;
 if(kb->data) MEM_freeN(kb->data);
 kb->data= MEM_callocN(cu->key->elemsize*tot, "kb->data");
 kb->totelem= tot;
 nu= nurb->first;
 fp= kb->data;
 while(nu) {
  if(nu->bezt) {
   bezt= nu->bezt;
   a= nu->pntsu;
   while(a--) {
    copy_v3_v3(fp, bezt->vec[0]);
    fp+= 3;
    copy_v3_v3(fp, bezt->vec[1]);
    fp+= 3;
    copy_v3_v3(fp, bezt->vec[2]);
    fp+= 3;
    fp[0]= bezt->alfa;
    fp+= 3;
    bezt++;
   }
  }
  else {
   bp= nu->bp;
   a= nu->pntsu*nu->pntsv;
   while(a--) {
    copy_v3_v3(fp, bp->vec);
    fp[3]= bp->alfa;
    fp+= 4;
    bp++;
   }
  }
  nu= nu->next;
 }
}
void key_to_curve(KeyBlock *kb, Curve *UNUSED_cu __attribute__((__unused__)), ListBase *nurb)
{
 Nurb *nu;
 BezTriple *bezt;
 BPoint *bp;
 float *fp;
 int a, tot;
 nu= nurb->first;
 fp= kb->data;
 tot= count_curveverts(nurb);
 tot= ( (kb->totelem)<(tot) ? (kb->totelem) : (tot) );
 while(nu && tot>0) {
  if(nu->bezt) {
   bezt= nu->bezt;
   a= nu->pntsu;
   while(a-- && tot>0) {
    copy_v3_v3(bezt->vec[0], fp);
    fp+= 3;
    copy_v3_v3(bezt->vec[1], fp);
    fp+= 3;
    copy_v3_v3(bezt->vec[2], fp);
    fp+= 3;
    bezt->alfa= fp[0];
    fp+= 3;
    tot-= 3;
    bezt++;
   }
  }
  else {
   bp= nu->bp;
   a= nu->pntsu*nu->pntsv;
   while(a-- && tot>0) {
    copy_v3_v3(bp->vec, fp);
    bp->alfa= fp[3];
    fp+= 4;
    tot--;
    bp++;
   }
  }
  nu= nu->next;
 }
}
void mesh_to_key(Mesh *me, KeyBlock *kb)
{
 MVert *mvert;
 float *fp;
 int a;
 if(me->totvert==0) return;
 if(kb->data) MEM_freeN(kb->data);
 kb->data= MEM_callocN(me->key->elemsize*me->totvert, "kb->data");
 kb->totelem= me->totvert;
 mvert= me->mvert;
 fp= kb->data;
 for(a=0; a<kb->totelem; a++, fp+=3, mvert++) {
  copy_v3_v3(fp, mvert->co);
 }
}
void key_to_mesh(KeyBlock *kb, Mesh *me)
{
 MVert *mvert;
 float *fp;
 int a, tot;
 mvert= me->mvert;
 fp= kb->data;
 tot= ( (kb->totelem)<(me->totvert) ? (kb->totelem) : (me->totvert) );
 for(a=0; a<tot; a++, fp+=3, mvert++) {
  copy_v3_v3(mvert->co, fp);
 }
}
float (*key_to_vertcos(Object *ob, KeyBlock *kb))[3]
{
 float (*vertCos)[3], *co;
 float *fp= kb->data;
 int tot= 0, a;
 if(ob->type == 1) {
  Mesh *me= (Mesh*)ob->data;
  tot= me->totvert;
 } else if(ob->type == 22) {
  Lattice *lt= (Lattice*)ob->data;
  tot= lt->pntsu*lt->pntsv*lt->pntsw;
 } else if(( (ob->type)==(2) || (ob->type)==(3) )) {
  Curve *cu= (Curve*)ob->data;
  tot= count_curveverts(&cu->nurb);
 }
 if (tot == 0) return ((void *)0);
 vertCos= MEM_callocN(tot*sizeof(*vertCos), "key_to_vertcos vertCos");
 co= (float*)vertCos;
 if(( (ob->type)==(1) || (ob->type)==(22) )) {
  for (a= 0; a<tot; a++, fp+=3, co+=3) {
   copy_v3_v3(co, fp);
  }
 } else if(( (ob->type)==(2) || (ob->type)==(3) )) {
  Curve *cu= (Curve*)ob->data;
  Nurb *nu= cu->nurb.first;
  BezTriple *bezt;
  BPoint *bp;
  while (nu) {
   if(nu->bezt) {
    int i;
    bezt= nu->bezt;
    a= nu->pntsu;
    while (a--) {
     for (i= 0; i<3; i++) {
      copy_v3_v3(co, fp);
      fp+= 3; co+= 3;
     }
     fp+= 3;
     bezt++;
    }
   }
   else {
    bp= nu->bp;
    a= nu->pntsu*nu->pntsv;
    while (a--) {
     copy_v3_v3(co, fp);
     fp+= 4;
     co+= 3;
     bp++;
    }
   }
   nu= nu->next;
  }
 }
 return vertCos;
}
void vertcos_to_key(Object *ob, KeyBlock *kb, float (*vertCos)[3])
{
 float *co= (float*)vertCos, *fp;
 int tot= 0, a, elemsize;
 if (kb->data) MEM_freeN(kb->data);
 if(ob->type == 1) {
  Mesh *me= (Mesh*)ob->data;
  tot= me->totvert;
  elemsize= me->key->elemsize;
 } else if(ob->type == 22) {
  Lattice *lt= (Lattice*)ob->data;
  tot= lt->pntsu*lt->pntsv*lt->pntsw;
  elemsize= lt->key->elemsize;
 } else if(( (ob->type)==(2) || (ob->type)==(3) )) {
  Curve *cu= (Curve*)ob->data;
  elemsize= cu->key->elemsize;
  tot= count_curveverts(&cu->nurb);
 }
 if (tot == 0) {
  kb->data= ((void *)0);
  return;
 }
 fp= kb->data= MEM_callocN(tot*elemsize, "key_to_vertcos vertCos");
 if(( (ob->type)==(1) || (ob->type)==(22) )) {
  for (a= 0; a<tot; a++, fp+=3, co+=3) {
   copy_v3_v3(fp, co);
  }
 } else if(( (ob->type)==(2) || (ob->type)==(3) )) {
  Curve *cu= (Curve*)ob->data;
  Nurb *nu= cu->nurb.first;
  BezTriple *bezt;
  BPoint *bp;
  while (nu) {
   if(nu->bezt) {
    int i;
    bezt= nu->bezt;
    a= nu->pntsu;
    while (a--) {
     for (i= 0; i<3; i++) {
      copy_v3_v3(fp, co);
      fp+= 3; co+= 3;
     }
     fp+= 3;
     bezt++;
    }
   }
   else {
    bp= nu->bp;
    a= nu->pntsu*nu->pntsv;
    while (a--) {
     copy_v3_v3(fp, co);
     fp+= 4;
     co+= 3;
     bp++;
    }
   }
   nu= nu->next;
  }
 }
}
void offset_to_key(Object *ob, KeyBlock *kb, float (*ofs)[3])
{
 int a;
 float *co= (float*)ofs, *fp= kb->data;
 if(( (ob->type)==(1) || (ob->type)==(22) )) {
  for (a= 0; a<kb->totelem; a++, fp+=3, co+=3) {
   add_v3_v3(fp, co);
  }
 } else if(( (ob->type)==(2) || (ob->type)==(3) )) {
  Curve *cu= (Curve*)ob->data;
  Nurb *nu= cu->nurb.first;
  BezTriple *bezt;
  BPoint *bp;
  while (nu) {
   if(nu->bezt) {
    int i;
    bezt= nu->bezt;
    a= nu->pntsu;
    while (a--) {
     for (i= 0; i<3; i++) {
      add_v3_v3(fp, co);
      fp+= 3; co+= 3;
     }
     fp+= 3;
     bezt++;
    }
   }
   else {
    bp= nu->bp;
    a= nu->pntsu*nu->pntsv;
    while (a--) {
     add_v3_v3(fp, co);
     fp+= 4;
     co+= 3;
     bp++;
    }
   }
   nu= nu->next;
  }
 }
}
