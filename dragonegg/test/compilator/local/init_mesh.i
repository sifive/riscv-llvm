 typedef float float_t;
 typedef double double_t;
extern unsigned int __math_errhandling ( void );
extern int __fpclassifyf(float );
extern int __fpclassifyd(double );
extern int __fpclassify (long double);
 static __inline__ int __inline_isfinitef (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isfinited (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isfinite (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_isinff (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isinfd (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isinf (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_isnanf (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnand (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnan (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_isnormalf (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnormald (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnormal (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_signbitf (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_signbitd (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_signbit (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_isinff( float __x ) { return __builtin_fabsf(__x) == __builtin_inff(); }
 static __inline__ int __inline_isinfd( double __x ) { return __builtin_fabs(__x) == __builtin_inf(); }
 static __inline__ int __inline_isinf( long double __x ) { return __builtin_fabsl(__x) == __builtin_infl(); }
 static __inline__ int __inline_isfinitef( float __x ) { return __x == __x && __builtin_fabsf(__x) != __builtin_inff(); }
 static __inline__ int __inline_isfinited( double __x ) { return __x == __x && __builtin_fabs(__x) != __builtin_inf(); }
 static __inline__ int __inline_isfinite( long double __x ) { return __x == __x && __builtin_fabsl(__x) != __builtin_infl(); }
 static __inline__ int __inline_isnanf( float __x ) { return __x != __x; }
 static __inline__ int __inline_isnand( double __x ) { return __x != __x; }
 static __inline__ int __inline_isnan( long double __x ) { return __x != __x; }
 static __inline__ int __inline_signbitf( float __x ) { union{ float __f; unsigned int __u; }__u; __u.__f = __x; return (int)(__u.__u >> 31); }
 static __inline__ int __inline_signbitd( double __x ) { union{ double __f; unsigned int __u[2]; }__u; __u.__f = __x; return (int)(__u.__u[1] >> 31); }
 static __inline__ int __inline_signbit( long double __x ){ union{ long double __ld; struct{ unsigned int __m[2]; short __sexp; }__p; }__u; __u.__ld = __x; return (int) (((unsigned short) __u.__p.__sexp) >> 15); }
 static __inline__ int __inline_isnormalf( float __x ) { float fabsf = __builtin_fabsf(__x); if( __x != __x ) return 0; return fabsf < __builtin_inff() && fabsf >= 1.17549435082228750797e-38F; }
 static __inline__ int __inline_isnormald( double __x ) { double fabsf = __builtin_fabs(__x); if( __x != __x ) return 0; return fabsf < __builtin_inf() && fabsf >= ((double)2.22507385850720138309e-308L); }
 static __inline__ int __inline_isnormal( long double __x ) { long double fabsf = __builtin_fabsl(__x); if( __x != __x ) return 0; return fabsf < __builtin_infl() && fabsf >= 3.36210314311209350626e-4932L; }
extern double acos( double );
extern float acosf( float );
extern double asin( double );
extern float asinf( float );
extern double atan( double );
extern float atanf( float );
extern double atan2( double, double );
extern float atan2f( float, float );
extern double cos( double );
extern float cosf( float );
extern double sin( double );
extern float sinf( float );
extern double tan( double );
extern float tanf( float );
extern double acosh( double );
extern float acoshf( float );
extern double asinh( double );
extern float asinhf( float );
extern double atanh( double );
extern float atanhf( float );
extern double cosh( double );
extern float coshf( float );
extern double sinh( double );
extern float sinhf( float );
extern double tanh( double );
extern float tanhf( float );
extern double exp ( double );
extern float expf ( float );
extern double exp2 ( double );
extern float exp2f ( float );
extern double expm1 ( double );
extern float expm1f ( float );
extern double log ( double );
extern float logf ( float );
extern double log10 ( double );
extern float log10f ( float );
extern double log2 ( double );
extern float log2f ( float );
extern double log1p ( double );
extern float log1pf ( float );
extern double logb ( double );
extern float logbf ( float );
extern double modf ( double, double * );
extern float modff ( float, float * );
extern double ldexp ( double, int );
extern float ldexpf ( float, int );
extern double frexp ( double, int * );
extern float frexpf ( float, int * );
extern int ilogb ( double );
extern int ilogbf ( float );
extern double scalbn ( double, int );
extern float scalbnf ( float, int );
extern double scalbln ( double, long int );
extern float scalblnf ( float, long int );
extern double fabs( double );
extern float fabsf( float );
extern double cbrt( double );
extern float cbrtf( float );
extern double hypot ( double, double );
extern float hypotf ( float, float );
extern double pow ( double, double );
extern float powf ( float, float );
extern double sqrt( double );
extern float sqrtf( float );
extern double erf( double );
extern float erff( float );
extern double erfc( double );
extern float erfcf( float );
extern double lgamma( double );
extern float lgammaf( float );
extern double tgamma( double );
extern float tgammaf( float );
extern double ceil ( double );
extern float ceilf ( float );
extern double floor ( double );
extern float floorf ( float );
extern double nearbyint ( double );
extern float nearbyintf ( float );
extern double rint ( double );
extern float rintf ( float );
extern long int lrint ( double );
extern long int lrintf ( float );
extern double round ( double );
extern float roundf ( float );
extern long int lround ( double );
extern long int lroundf ( float );
    extern long long int llrint ( double );
    extern long long int llrintf ( float );
    extern long long int llround ( double );
    extern long long int llroundf ( float );
extern double trunc ( double );
extern float truncf ( float );
extern double fmod ( double, double );
extern float fmodf ( float, float );
extern double remainder ( double, double );
extern float remainderf ( float, float );
extern double remquo ( double, double, int * );
extern float remquof ( float, float, int * );
extern double copysign ( double, double );
extern float copysignf ( float, float );
extern double nan( const char * );
extern float nanf( const char * );
extern double nextafter ( double, double );
extern float nextafterf ( float, float );
extern double fdim ( double, double );
extern float fdimf ( float, float );
extern double fmax ( double, double );
extern float fmaxf ( float, float );
extern double fmin ( double, double );
extern float fminf ( float, float );
extern double fma ( double, double, double );
extern float fmaf ( float, float, float );
extern long double acosl(long double);
extern long double asinl(long double);
extern long double atanl(long double);
extern long double atan2l(long double, long double);
extern long double cosl(long double);
extern long double sinl(long double);
extern long double tanl(long double);
extern long double acoshl(long double);
extern long double asinhl(long double);
extern long double atanhl(long double);
extern long double coshl(long double);
extern long double sinhl(long double);
extern long double tanhl(long double);
extern long double expl(long double);
extern long double exp2l(long double);
extern long double expm1l(long double);
extern long double logl(long double);
extern long double log10l(long double);
extern long double log2l(long double);
extern long double log1pl(long double);
extern long double logbl(long double);
extern long double modfl(long double, long double *);
extern long double ldexpl(long double, int);
extern long double frexpl(long double, int *);
extern int ilogbl(long double);
extern long double scalbnl(long double, int);
extern long double scalblnl(long double, long int);
extern long double fabsl(long double);
extern long double cbrtl(long double);
extern long double hypotl(long double, long double);
extern long double powl(long double, long double);
extern long double sqrtl(long double);
extern long double erfl(long double);
extern long double erfcl(long double);
extern long double lgammal(long double);
extern long double tgammal(long double);
extern long double ceill(long double);
extern long double floorl(long double);
extern long double nearbyintl(long double);
extern long double rintl(long double);
extern long int lrintl(long double);
extern long double roundl(long double);
extern long int lroundl(long double);
    extern long long int llrintl(long double);
    extern long long int llroundl(long double);
extern long double truncl(long double);
extern long double fmodl(long double, long double);
extern long double remainderl(long double, long double);
extern long double remquol(long double, long double, int *);
extern long double copysignl(long double, long double);
extern long double nanl(const char *);
extern long double nextafterl(long double, long double);
extern double nexttoward(double, long double);
extern float nexttowardf(float, long double);
extern long double nexttowardl(long double, long double);
extern long double fdiml(long double, long double);
extern long double fmaxl(long double, long double);
extern long double fminl(long double, long double);
extern long double fmal(long double, long double, long double);
extern double __inf( void );
extern float __inff( void );
extern long double __infl( void );
extern float __nan( void );
extern double j0 ( double );
extern double j1 ( double );
extern double jn ( int, double );
extern double y0 ( double );
extern double y1 ( double );
extern double yn ( int, double );
extern double scalb ( double, double );
extern int signgam;
extern long int rinttol ( double );
extern long int roundtol ( double );
struct exception {
 int type;
 char *name;
 double arg1;
 double arg2;
 double retval;
};
extern int finite ( double );
extern double gamma ( double );
extern int matherr ( struct exception * );
extern double significand ( double );
extern double drem ( double, double );
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef short __int16_t;
typedef unsigned short __uint16_t;
typedef int __int32_t;
typedef unsigned int __uint32_t;
typedef long long __int64_t;
typedef unsigned long long __uint64_t;
typedef long __darwin_intptr_t;
typedef unsigned int __darwin_natural_t;
typedef int __darwin_ct_rune_t;
typedef union {
 char __mbstate8[128];
 long long _mbstateL;
} __mbstate_t;
typedef __mbstate_t __darwin_mbstate_t;
typedef long int __darwin_ptrdiff_t;
typedef long unsigned int __darwin_size_t;
typedef __builtin_va_list __darwin_va_list;
typedef int __darwin_wchar_t;
typedef __darwin_wchar_t __darwin_rune_t;
typedef int __darwin_wint_t;
typedef unsigned long __darwin_clock_t;
typedef __uint32_t __darwin_socklen_t;
typedef long __darwin_ssize_t;
typedef long __darwin_time_t;
struct __darwin_pthread_handler_rec
{
 void (*__routine)(void *);
 void *__arg;
 struct __darwin_pthread_handler_rec *__next;
};
struct _opaque_pthread_attr_t { long __sig; char __opaque[56]; };
struct _opaque_pthread_cond_t { long __sig; char __opaque[40]; };
struct _opaque_pthread_condattr_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_mutex_t { long __sig; char __opaque[56]; };
struct _opaque_pthread_mutexattr_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_once_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_rwlock_t { long __sig; char __opaque[192]; };
struct _opaque_pthread_rwlockattr_t { long __sig; char __opaque[16]; };
struct _opaque_pthread_t { long __sig; struct __darwin_pthread_handler_rec *__cleanup_stack; char __opaque[1168]; };
typedef __int64_t __darwin_blkcnt_t;
typedef __int32_t __darwin_blksize_t;
typedef __int32_t __darwin_dev_t;
typedef unsigned int __darwin_fsblkcnt_t;
typedef unsigned int __darwin_fsfilcnt_t;
typedef __uint32_t __darwin_gid_t;
typedef __uint32_t __darwin_id_t;
typedef __uint64_t __darwin_ino64_t;
typedef __darwin_ino64_t __darwin_ino_t;
typedef __darwin_natural_t __darwin_mach_port_name_t;
typedef __darwin_mach_port_name_t __darwin_mach_port_t;
typedef __uint16_t __darwin_mode_t;
typedef __int64_t __darwin_off_t;
typedef __int32_t __darwin_pid_t;
typedef struct _opaque_pthread_attr_t
   __darwin_pthread_attr_t;
typedef struct _opaque_pthread_cond_t
   __darwin_pthread_cond_t;
typedef struct _opaque_pthread_condattr_t
   __darwin_pthread_condattr_t;
typedef unsigned long __darwin_pthread_key_t;
typedef struct _opaque_pthread_mutex_t
   __darwin_pthread_mutex_t;
typedef struct _opaque_pthread_mutexattr_t
   __darwin_pthread_mutexattr_t;
typedef struct _opaque_pthread_once_t
   __darwin_pthread_once_t;
typedef struct _opaque_pthread_rwlock_t
   __darwin_pthread_rwlock_t;
typedef struct _opaque_pthread_rwlockattr_t
   __darwin_pthread_rwlockattr_t;
typedef struct _opaque_pthread_t
   *__darwin_pthread_t;
typedef __uint32_t __darwin_sigset_t;
typedef __int32_t __darwin_suseconds_t;
typedef __uint32_t __darwin_uid_t;
typedef __uint32_t __darwin_useconds_t;
typedef unsigned char __darwin_uuid_t[16];
typedef char __darwin_uuid_string_t[37];
typedef int __darwin_nl_item;
typedef int __darwin_wctrans_t;
typedef __uint32_t __darwin_wctype_t;
typedef enum {
 P_ALL,
 P_PID,
 P_PGID
} idtype_t;
typedef __darwin_pid_t pid_t;
typedef __darwin_id_t id_t;
typedef int sig_atomic_t;
struct __darwin_i386_thread_state
{
    unsigned int __eax;
    unsigned int __ebx;
    unsigned int __ecx;
    unsigned int __edx;
    unsigned int __edi;
    unsigned int __esi;
    unsigned int __ebp;
    unsigned int __esp;
    unsigned int __ss;
    unsigned int __eflags;
    unsigned int __eip;
    unsigned int __cs;
    unsigned int __ds;
    unsigned int __es;
    unsigned int __fs;
    unsigned int __gs;
};
struct __darwin_fp_control
{
    unsigned short __invalid :1,
        __denorm :1,
    __zdiv :1,
    __ovrfl :1,
    __undfl :1,
    __precis :1,
      :2,
    __pc :2,
    __rc :2,
             :1,
      :3;
};
typedef struct __darwin_fp_control __darwin_fp_control_t;
struct __darwin_fp_status
{
    unsigned short __invalid :1,
        __denorm :1,
    __zdiv :1,
    __ovrfl :1,
    __undfl :1,
    __precis :1,
    __stkflt :1,
    __errsumm :1,
    __c0 :1,
    __c1 :1,
    __c2 :1,
    __tos :3,
    __c3 :1,
    __busy :1;
};
typedef struct __darwin_fp_status __darwin_fp_status_t;
struct __darwin_mmst_reg
{
 char __mmst_reg[10];
 char __mmst_rsrv[6];
};
struct __darwin_xmm_reg
{
 char __xmm_reg[16];
};
struct __darwin_i386_float_state
{
 int __fpu_reserved[2];
 struct __darwin_fp_control __fpu_fcw;
 struct __darwin_fp_status __fpu_fsw;
 __uint8_t __fpu_ftw;
 __uint8_t __fpu_rsrv1;
 __uint16_t __fpu_fop;
 __uint32_t __fpu_ip;
 __uint16_t __fpu_cs;
 __uint16_t __fpu_rsrv2;
 __uint32_t __fpu_dp;
 __uint16_t __fpu_ds;
 __uint16_t __fpu_rsrv3;
 __uint32_t __fpu_mxcsr;
 __uint32_t __fpu_mxcsrmask;
 struct __darwin_mmst_reg __fpu_stmm0;
 struct __darwin_mmst_reg __fpu_stmm1;
 struct __darwin_mmst_reg __fpu_stmm2;
 struct __darwin_mmst_reg __fpu_stmm3;
 struct __darwin_mmst_reg __fpu_stmm4;
 struct __darwin_mmst_reg __fpu_stmm5;
 struct __darwin_mmst_reg __fpu_stmm6;
 struct __darwin_mmst_reg __fpu_stmm7;
 struct __darwin_xmm_reg __fpu_xmm0;
 struct __darwin_xmm_reg __fpu_xmm1;
 struct __darwin_xmm_reg __fpu_xmm2;
 struct __darwin_xmm_reg __fpu_xmm3;
 struct __darwin_xmm_reg __fpu_xmm4;
 struct __darwin_xmm_reg __fpu_xmm5;
 struct __darwin_xmm_reg __fpu_xmm6;
 struct __darwin_xmm_reg __fpu_xmm7;
 char __fpu_rsrv4[14*16];
 int __fpu_reserved1;
};
struct __darwin_i386_exception_state
{
    unsigned int __trapno;
    unsigned int __err;
    unsigned int __faultvaddr;
};
struct __darwin_x86_debug_state32
{
 unsigned int __dr0;
 unsigned int __dr1;
 unsigned int __dr2;
 unsigned int __dr3;
 unsigned int __dr4;
 unsigned int __dr5;
 unsigned int __dr6;
 unsigned int __dr7;
};
struct __darwin_x86_thread_state64
{
 __uint64_t __rax;
 __uint64_t __rbx;
 __uint64_t __rcx;
 __uint64_t __rdx;
 __uint64_t __rdi;
 __uint64_t __rsi;
 __uint64_t __rbp;
 __uint64_t __rsp;
 __uint64_t __r8;
 __uint64_t __r9;
 __uint64_t __r10;
 __uint64_t __r11;
 __uint64_t __r12;
 __uint64_t __r13;
 __uint64_t __r14;
 __uint64_t __r15;
 __uint64_t __rip;
 __uint64_t __rflags;
 __uint64_t __cs;
 __uint64_t __fs;
 __uint64_t __gs;
};
struct __darwin_x86_float_state64
{
 int __fpu_reserved[2];
 struct __darwin_fp_control __fpu_fcw;
 struct __darwin_fp_status __fpu_fsw;
 __uint8_t __fpu_ftw;
 __uint8_t __fpu_rsrv1;
 __uint16_t __fpu_fop;
 __uint32_t __fpu_ip;
 __uint16_t __fpu_cs;
 __uint16_t __fpu_rsrv2;
 __uint32_t __fpu_dp;
 __uint16_t __fpu_ds;
 __uint16_t __fpu_rsrv3;
 __uint32_t __fpu_mxcsr;
 __uint32_t __fpu_mxcsrmask;
 struct __darwin_mmst_reg __fpu_stmm0;
 struct __darwin_mmst_reg __fpu_stmm1;
 struct __darwin_mmst_reg __fpu_stmm2;
 struct __darwin_mmst_reg __fpu_stmm3;
 struct __darwin_mmst_reg __fpu_stmm4;
 struct __darwin_mmst_reg __fpu_stmm5;
 struct __darwin_mmst_reg __fpu_stmm6;
 struct __darwin_mmst_reg __fpu_stmm7;
 struct __darwin_xmm_reg __fpu_xmm0;
 struct __darwin_xmm_reg __fpu_xmm1;
 struct __darwin_xmm_reg __fpu_xmm2;
 struct __darwin_xmm_reg __fpu_xmm3;
 struct __darwin_xmm_reg __fpu_xmm4;
 struct __darwin_xmm_reg __fpu_xmm5;
 struct __darwin_xmm_reg __fpu_xmm6;
 struct __darwin_xmm_reg __fpu_xmm7;
 struct __darwin_xmm_reg __fpu_xmm8;
 struct __darwin_xmm_reg __fpu_xmm9;
 struct __darwin_xmm_reg __fpu_xmm10;
 struct __darwin_xmm_reg __fpu_xmm11;
 struct __darwin_xmm_reg __fpu_xmm12;
 struct __darwin_xmm_reg __fpu_xmm13;
 struct __darwin_xmm_reg __fpu_xmm14;
 struct __darwin_xmm_reg __fpu_xmm15;
 char __fpu_rsrv4[6*16];
 int __fpu_reserved1;
};
struct __darwin_x86_exception_state64
{
    unsigned int __trapno;
    unsigned int __err;
    __uint64_t __faultvaddr;
};
struct __darwin_x86_debug_state64
{
 __uint64_t __dr0;
 __uint64_t __dr1;
 __uint64_t __dr2;
 __uint64_t __dr3;
 __uint64_t __dr4;
 __uint64_t __dr5;
 __uint64_t __dr6;
 __uint64_t __dr7;
};
struct __darwin_mcontext32
{
 struct __darwin_i386_exception_state __es;
 struct __darwin_i386_thread_state __ss;
 struct __darwin_i386_float_state __fs;
};
struct __darwin_mcontext64
{
 struct __darwin_x86_exception_state64 __es;
 struct __darwin_x86_thread_state64 __ss;
 struct __darwin_x86_float_state64 __fs;
};
typedef struct __darwin_mcontext64 *mcontext_t;
struct __darwin_sigaltstack
{
 void *ss_sp;
 __darwin_size_t ss_size;
 int ss_flags;
};
struct __darwin_ucontext
{
 int uc_onstack;
 __darwin_sigset_t uc_sigmask;
 struct __darwin_sigaltstack uc_stack;
 struct __darwin_ucontext *uc_link;
 __darwin_size_t uc_mcsize;
 struct __darwin_mcontext64 *uc_mcontext;
};
typedef struct __darwin_sigaltstack stack_t;
typedef struct __darwin_ucontext ucontext_t;
typedef __darwin_pthread_attr_t pthread_attr_t;
typedef __darwin_sigset_t sigset_t;
typedef __darwin_size_t size_t;
typedef __darwin_uid_t uid_t;
union sigval {
 int sival_int;
 void *sival_ptr;
};
struct sigevent {
 int sigev_notify;
 int sigev_signo;
 union sigval sigev_value;
 void (*sigev_notify_function)(union sigval);
 pthread_attr_t *sigev_notify_attributes;
};
typedef struct __siginfo {
 int si_signo;
 int si_errno;
 int si_code;
 pid_t si_pid;
 uid_t si_uid;
 int si_status;
 void *si_addr;
 union sigval si_value;
 long si_band;
 unsigned long __pad[7];
} siginfo_t;
union __sigaction_u {
 void (*__sa_handler)(int);
 void (*__sa_sigaction)(int, struct __siginfo *,
         void *);
};
struct __sigaction {
 union __sigaction_u __sigaction_u;
 void (*sa_tramp)(void *, int, int, siginfo_t *, void *);
 sigset_t sa_mask;
 int sa_flags;
};
struct sigaction {
 union __sigaction_u __sigaction_u;
 sigset_t sa_mask;
 int sa_flags;
};
typedef void (*sig_t)(int);
struct sigvec {
 void (*sv_handler)(int);
 int sv_mask;
 int sv_flags;
};
struct sigstack {
 char *ss_sp;
 int ss_onstack;
};
void (*signal(int, void (*)(int)))(int);
struct timeval
{
 __darwin_time_t tv_sec;
 __darwin_suseconds_t tv_usec;
};
typedef __uint64_t rlim_t;
struct rusage {
 struct timeval ru_utime;
 struct timeval ru_stime;
 long ru_maxrss;
 long ru_ixrss;
 long ru_idrss;
 long ru_isrss;
 long ru_minflt;
 long ru_majflt;
 long ru_nswap;
 long ru_inblock;
 long ru_oublock;
 long ru_msgsnd;
 long ru_msgrcv;
 long ru_nsignals;
 long ru_nvcsw;
 long ru_nivcsw;
};
struct rlimit {
 rlim_t rlim_cur;
 rlim_t rlim_max;
};
int getpriority(int, id_t);
int getiopolicy_np(int, int);
int getrlimit(int, struct rlimit *) __asm("_" "getrlimit" );
int getrusage(int, struct rusage *);
int setpriority(int, id_t, int);
int setiopolicy_np(int, int, int);
int setrlimit(int, const struct rlimit *) __asm("_" "setrlimit" );
static __inline__
__uint16_t
_OSSwapInt16(
    __uint16_t _data
)
{
    return ((_data << 8) | (_data >> 8));
}
static __inline__
__uint32_t
_OSSwapInt32(
    __uint32_t _data
)
{
    __asm__ ("bswap   %0" : "+r" (_data));
    return _data;
}
static __inline__
__uint64_t
_OSSwapInt64(
    __uint64_t _data
)
{
    __asm__ ("bswap   %0" : "+r" (_data));
    return _data;
}
union wait {
 int w_status;
 struct {
  unsigned int w_Termsig:7,
    w_Coredump:1,
    w_Retcode:8,
    w_Filler:16;
 } w_T;
 struct {
  unsigned int w_Stopval:8,
    w_Stopsig:8,
    w_Filler:16;
 } w_S;
};
pid_t wait(int *) __asm("_" "wait" );
pid_t waitpid(pid_t, int *, int) __asm("_" "waitpid" );
int waitid(idtype_t, id_t, siginfo_t *, int) __asm("_" "waitid" );
pid_t wait3(int *, int, struct rusage *);
pid_t wait4(pid_t, int *, int, struct rusage *);
void *alloca(size_t);
typedef __darwin_ct_rune_t ct_rune_t;
typedef __darwin_rune_t rune_t;
typedef __darwin_wchar_t wchar_t;
typedef struct {
 int quot;
 int rem;
} div_t;
typedef struct {
 long quot;
 long rem;
} ldiv_t;
typedef struct {
 long long quot;
 long long rem;
} lldiv_t;
extern int __mb_cur_max;
void abort(void) __attribute__((__noreturn__));
int abs(int) __attribute__((__const__));
int atexit(void (*)(void));
double atof(const char *);
int atoi(const char *);
long atol(const char *);
long long
  atoll(const char *);
void *bsearch(const void *, const void *, size_t,
     size_t, int (*)(const void *, const void *));
void *calloc(size_t, size_t);
div_t div(int, int) __attribute__((__const__));
void exit(int) __attribute__((__noreturn__));
void free(void *);
char *getenv(const char *);
long labs(long) __attribute__((__const__));
ldiv_t ldiv(long, long) __attribute__((__const__));
long long
  llabs(long long);
lldiv_t lldiv(long long, long long);
void *malloc(size_t);
int mblen(const char *, size_t);
size_t mbstowcs(wchar_t * , const char * , size_t);
int mbtowc(wchar_t * , const char * , size_t);
int posix_memalign(void **, size_t, size_t);
void qsort(void *, size_t, size_t,
     int (*)(const void *, const void *));
int rand(void);
void *realloc(void *, size_t);
void srand(unsigned);
double strtod(const char *, char **) __asm("_" "strtod" );
float strtof(const char *, char **) __asm("_" "strtof" );
long strtol(const char *, char **, int);
long double
  strtold(const char *, char **) ;
long long
  strtoll(const char *, char **, int);
unsigned long
  strtoul(const char *, char **, int);
unsigned long long
  strtoull(const char *, char **, int);
int system(const char *) __asm("_" "system" );
size_t wcstombs(char * , const wchar_t * , size_t);
int wctomb(char *, wchar_t);
void _Exit(int) __attribute__((__noreturn__));
long a64l(const char *);
double drand48(void);
char *ecvt(double, int, int *, int *);
double erand48(unsigned short[3]);
char *fcvt(double, int, int *, int *);
char *gcvt(double, int, char *);
int getsubopt(char **, char * const *, char **);
int grantpt(int);
char *initstate(unsigned, char *, size_t);
long jrand48(unsigned short[3]);
char *l64a(long);
void lcong48(unsigned short[7]);
long lrand48(void);
char *mktemp(char *);
int mkstemp(char *);
long mrand48(void);
long nrand48(unsigned short[3]);
int posix_openpt(int);
char *ptsname(int);
int putenv(char *) __asm("_" "putenv" );
long random(void);
int rand_r(unsigned *);
char *realpath(const char * , char * ) __asm("_" "realpath" "$DARWIN_EXTSN");
unsigned short
 *seed48(unsigned short[3]);
int setenv(const char *, const char *, int) __asm("_" "setenv" );
void setkey(const char *) __asm("_" "setkey" );
char *setstate(const char *);
void srand48(long);
void srandom(unsigned);
int unlockpt(int);
int unsetenv(const char *) __asm("_" "unsetenv" );
typedef signed char int8_t;
typedef unsigned char u_int8_t;
typedef short int16_t;
typedef unsigned short u_int16_t;
typedef int int32_t;
typedef unsigned int u_int32_t;
typedef long long int64_t;
typedef unsigned long long u_int64_t;
typedef int64_t register_t;
typedef __darwin_intptr_t intptr_t;
typedef unsigned long uintptr_t;
typedef u_int64_t user_addr_t;
typedef u_int64_t user_size_t;
typedef int64_t user_ssize_t;
typedef int64_t user_long_t;
typedef u_int64_t user_ulong_t;
typedef int64_t user_time_t;
typedef int64_t user_off_t;
typedef u_int64_t syscall_arg_t;
typedef __darwin_dev_t dev_t;
typedef __darwin_mode_t mode_t;
u_int32_t
  arc4random(void);
void arc4random_addrandom(unsigned char *dat, int datlen);
void arc4random_stir(void);
char *cgetcap(char *, const char *, int);
int cgetclose(void);
int cgetent(char **, char **, const char *);
int cgetfirst(char **, char **);
int cgetmatch(const char *, const char *);
int cgetnext(char **, char **);
int cgetnum(char *, const char *, long *);
int cgetset(const char *);
int cgetstr(char *, const char *, char **);
int cgetustr(char *, const char *, char **);
int daemon(int, int) __asm("_" "daemon" "$1050") __attribute__((deprecated,visibility("default")));
char *devname(dev_t, mode_t);
char *devname_r(dev_t, mode_t, char *buf, int len);
char *getbsize(int *, long *);
int getloadavg(double [], int);
const char
 *getprogname(void);
int heapsort(void *, size_t, size_t,
     int (*)(const void *, const void *));
int mergesort(void *, size_t, size_t,
     int (*)(const void *, const void *));
void psort(void *, size_t, size_t,
     int (*)(const void *, const void *));
void psort_r(void *, size_t, size_t, void *,
     int (*)(void *, const void *, const void *));
void qsort_r(void *, size_t, size_t, void *,
     int (*)(void *, const void *, const void *));
int radixsort(const unsigned char **, int, const unsigned char *,
     unsigned);
void setprogname(const char *);
int sradixsort(const unsigned char **, int, const unsigned char *,
     unsigned);
void sranddev(void);
void srandomdev(void);
void *reallocf(void *, size_t);
long long
  strtoq(const char *, char **, int);
unsigned long long
  strtouq(const char *, char **, int);
extern char *suboptarg;
void *valloc(size_t);
enum {
  nghost = 4,
  num_digit = 4
};
 enum {NWAVE = 7, NVAR = 8 + 0};
typedef double Real;
typedef struct Real3Vect_s{
  Real x, y, z;
}Real3Vect;
typedef struct Int3Vect_s{
  int i, j, k;
}Int3Vect;
typedef struct Side_s{
  int ijkl[3];
  int ijkr[3];
}SideS;
typedef struct GridsData_s{
  int Nx[3];
  int Disp[3];
  int ID_Comm_world;
  int ID_Comm_Domain;
}GridsDataS;
typedef struct Cons_s{
  Real d;
  Real M1;
  Real M2;
  Real M3;
  Real E;
  Real B1c;
  Real B2c;
  Real B3c;
}ConsS;
typedef struct Prim_s{
  Real d;
  Real V1;
  Real V2;
  Real V3;
  Real P;
  Real B1c;
  Real B2c;
  Real B3c;
}PrimS;
typedef struct Cons1D_s{
  Real d;
  Real Mx;
  Real My;
  Real Mz;
  Real E;
  Real By;
  Real Bz;
}Cons1DS;
typedef struct Prim1D_s{
  Real d;
  Real Vx;
  Real Vy;
  Real Vz;
  Real P;
  Real By;
  Real Bz;
}Prim1DS;
typedef struct Grid_s{
  ConsS ***U;
  Real ***B1i,***B2i,***B3i;
  Real MinX[3];
  Real MaxX[3];
  Real dx1,dx2,dx3;
  Real time, dt;
  int is,ie;
  int js,je;
  int ks,ke;
  int Nx[3];
  int Disp[3];
  int rx1_id, lx1_id;
  int rx2_id, lx2_id;
  int rx3_id, lx3_id;
}GridS;
typedef void (*VGFun_t)(GridS *pG);
typedef struct Domain_s{
  Real RootMinX[3];
  Real RootMaxX[3];
  Real MinX[3];
  Real MaxX[3];
  Real dx[3];
  int Nx[3];
  int NGrid[3];
  int Disp[3];
  int Level,DomNumber;
  int InputBlock;
  GridS *Grid;
  GridsDataS ***GData;
  VGFun_t ix1_BCFun, ox1_BCFun;
  VGFun_t ix2_BCFun, ox2_BCFun;
  VGFun_t ix3_BCFun, ox3_BCFun;
}DomainS;
typedef void (*VDFun_t)(DomainS *pD);
typedef struct Mesh_s{
  Real RootMinX[3];
  Real RootMaxX[3];
  Real dx[3];
  Real time, dt;
  int Nx[3];
  int nstep;
  int BCFlag_ix1, BCFlag_ox1;
  int BCFlag_ix2, BCFlag_ox2;
  int BCFlag_ix3, BCFlag_ox3;
  int NLevels;
  int *DomainsPerLevel;
  DomainS **Domain;
  char *outfilename;
}MeshS;
struct Output_s;
typedef void (*VOutFun_t)(MeshS *pM, struct Output_s *pout);
typedef void (*VResFun_t)(MeshS *pM, struct Output_s *pout);
typedef Real (*ConsFun_t)(const GridS *pG, const int i,const int j,const int k);
typedef struct Output_s{
  int n;
  Real dt;
  Real t;
  int num;
  char *out;
  char *id;
  int nlevel, ndomain;
  Real dmin,dmax;
  Real gmin,gmax;
  int sdmin,sdmax;
  int ndim;
  int reduce_x1;
  int reduce_x2;
  int reduce_x3;
  Real x1l, x1u;
  Real x2l, x2u;
  Real x3l, x3u;
  char *out_fmt;
  char *dat_fmt;
  char *palette;
  float *rgb;
  float *der;
  VOutFun_t out_fun;
  VResFun_t res_fun;
  ConsFun_t expr;
}OutputS;
typedef Real (*GravPotFun_t)(const Real x1, const Real x2, const Real x3);
typedef Real (*CoolingFun_t)(const Real d, const Real p, const Real dt);
enum BCDirection {left_x1, right_x1, left_x2, right_x2, left_x3, right_x3};
extern Real CourNo;
extern Real Gamma, Gamma_1, Gamma_2;
extern int myID_Comm_world;
extern GravPotFun_t StaticGravPot;
extern CoolingFun_t CoolingFunc;
typedef __darwin_va_list va_list;
typedef __darwin_off_t off_t;
typedef __darwin_off_t fpos_t;
struct __sbuf {
 unsigned char *_base;
 int _size;
};
struct __sFILEX;
typedef struct __sFILE {
 unsigned char *_p;
 int _r;
 int _w;
 short _flags;
 short _file;
 struct __sbuf _bf;
 int _lbfsize;
 void *_cookie;
 int (*_close)(void *);
 int (*_read) (void *, char *, int);
 fpos_t (*_seek) (void *, fpos_t, int);
 int (*_write)(void *, const char *, int);
 struct __sbuf _ub;
 struct __sFILEX *_extra;
 int _ur;
 unsigned char _ubuf[3];
 unsigned char _nbuf[1];
 struct __sbuf _lb;
 int _blksize;
 fpos_t _offset;
} FILE;
extern FILE *__stdinp;
extern FILE *__stdoutp;
extern FILE *__stderrp;
void clearerr(FILE *);
int fclose(FILE *);
int feof(FILE *);
int ferror(FILE *);
int fflush(FILE *);
int fgetc(FILE *);
int fgetpos(FILE * , fpos_t *);
char *fgets(char * , int, FILE *);
FILE *fopen(const char * , const char * ) __asm("_" "fopen" );
int fprintf(FILE * , const char * , ...) ;
int fputc(int, FILE *);
int fputs(const char * , FILE * ) __asm("_" "fputs" );
size_t fread(void * , size_t, size_t, FILE * );
FILE *freopen(const char * , const char * ,
     FILE * ) __asm("_" "freopen" );
int fscanf(FILE * , const char * , ...) ;
int fseek(FILE *, long, int);
int fsetpos(FILE *, const fpos_t *);
long ftell(FILE *);
size_t fwrite(const void * , size_t, size_t, FILE * ) __asm("_" "fwrite" );
int getc(FILE *);
int getchar(void);
char *gets(char *);
extern const int sys_nerr;
extern const char *const sys_errlist[];
void perror(const char *);
int printf(const char * , ...) ;
int putc(int, FILE *);
int putchar(int);
int puts(const char *);
int remove(const char *);
int rename (const char *, const char *);
void rewind(FILE *);
int scanf(const char * , ...) ;
void setbuf(FILE * , char * );
int setvbuf(FILE * , char * , int, size_t);
int sprintf(char * , const char * , ...) ;
int sscanf(const char * , const char * , ...) ;
FILE *tmpfile(void);
char *tmpnam(char *);
int ungetc(int, FILE *);
int vfprintf(FILE * , const char * , va_list) ;
int vprintf(const char * , va_list) ;
int vsprintf(char * , const char * , va_list) ;
int asprintf(char **, const char *, ...) ;
int vasprintf(char **, const char *, va_list) ;
char *ctermid(char *);
char *ctermid_r(char *);
FILE *fdopen(int, const char *) __asm("_" "fdopen" );
char *fgetln(FILE *, size_t *);
int fileno(FILE *);
void flockfile(FILE *);
const char
 *fmtcheck(const char *, const char *);
int fpurge(FILE *);
int fseeko(FILE *, off_t, int);
off_t ftello(FILE *);
int ftrylockfile(FILE *);
void funlockfile(FILE *);
int getc_unlocked(FILE *);
int getchar_unlocked(void);
int getw(FILE *);
int pclose(FILE *);
FILE *popen(const char *, const char *) __asm("_" "popen" );
int putc_unlocked(int, FILE *);
int putchar_unlocked(int);
int putw(int, FILE *);
void setbuffer(FILE *, char *, int);
int setlinebuf(FILE *);
int snprintf(char * , size_t, const char * , ...) ;
char *tempnam(const char *, const char *) __asm("_" "tempnam" );
int vfscanf(FILE * , const char * , va_list) ;
int vscanf(const char * , va_list) ;
int vsnprintf(char * , size_t, const char * , va_list) ;
int vsscanf(const char * , const char * , va_list) ;
FILE *zopen(const char *, const char *, int);
FILE *funopen(const void *,
  int (*)(void *, char *, int),
  int (*)(void *, const char *, int),
  fpos_t (*)(void *, fpos_t, int),
  int (*)(void *));
int __srget(FILE *);
int __svfscanf(FILE *, const char *, va_list) ;
int __swbuf(int, FILE *);
static __inline int __sputc(int _c, FILE *_p) {
 if (--_p->_w >= 0 || (_p->_w >= _p->_lbfsize && (char)_c != '\n'))
  return (*_p->_p++ = _c);
 else
  return (__swbuf(_c, _p));
}
extern int __sprintf_chk (char * , int, size_t,
     const char * , ...)
  ;
extern int __snprintf_chk (char * , size_t, int, size_t,
      const char * , ...)
  ;
extern int __vsprintf_chk (char * , int, size_t,
      const char * , va_list)
  ;
extern int __vsnprintf_chk (char * , size_t, int, size_t,
       const char * , va_list)
  ;
typedef __builtin_va_list __gnuc_va_list;
VDFun_t integrate_init(MeshS *pM);
void integrate_destruct(void);
void integrate_destruct_1d(void);
void integrate_init_1d(MeshS *pM);
void integrate_1d_ctu(DomainS *pD);
void integrate_1d_vl(DomainS *pD);
void integrate_destruct_2d(void);
void integrate_init_2d(MeshS *pM);
void integrate_2d_ctu(DomainS *pD);
void integrate_2d_vl(DomainS *pD);
void integrate_destruct_3d(void);
void integrate_init_3d(MeshS *pM);
void integrate_3d_ctu(DomainS *pD);
void integrate_3d_vl(DomainS *pD);
Real KoyInut(const Real dens, const Real Press, const Real dt);
Real diff_dt(MeshS *pM);
void integrate_diff(MeshS *pM);
void integrate_diff_init(MeshS *pM);
void integrate_diff_destruct(void);
void esys_prim_adb_mhd(const Real d, const Real v1, const Real rho_a2,
  const Real b1, const Real b2, const Real b3, Real eigenvalues[],
  Real right_eigenmatrix[][7], Real left_eigenmatrix[][7]);
void lr_states_destruct(void);
void lr_states_init(MeshS *pM);
void lr_states(const GridS* pG, const Prim1DS W[], const Real Bxc[],
               const Real dt, const Real dx, const int is, const int ie,
               Prim1DS Wl[], Prim1DS Wr[], const int dir);
void esys_roe_iso_hyd(const Real v1, const Real v2, const Real v3,
  Real eigenvalues[],
  Real right_eigenmatrix[][4], Real left_eigenmatrix[][4]);
void esys_roe_adb_hyd(const Real v1, const Real v2, const Real v3,
  const Real h, Real eigenvalues[],
  Real right_eigenmatrix[][5], Real left_eigenmatrix[][5]);
void esys_roe_iso_mhd(const Real d, const Real v1, const Real v2,
  const Real v3, const Real b1, const Real b2, const Real b3,
  const Real x, const Real y, Real eigenvalues[],
  Real right_eigenmatrix[][6], Real left_eigenmatrix[][6]);
void esys_roe_adb_mhd(const Real d, const Real v1, const Real v2,
  const Real v3, const Real h, const Real b1, const Real b2, const Real b3,
  const Real x, const Real y, Real eigenvalues[],
  Real right_eigenmatrix[][7], Real left_eigenmatrix[][7]);
void fluxes(const Cons1DS Ul, const Cons1DS Ur,
            const Prim1DS Wl, const Prim1DS Wr,
            const Real Bxi, Cons1DS *pF);
int athena_main(int argc, char *argv[]);
void* calloc_1d_array( size_t nc, size_t size);
void** calloc_2d_array( size_t nr, size_t nc, size_t size);
void*** calloc_3d_array(size_t nt, size_t nr, size_t nc, size_t size);
void free_1d_array(void *array);
void free_2d_array(void *array);
void free_3d_array(void *array);
void ath_log_set_level(const int out, const int err);
void ath_log_open(const char *basename, const int lazy, const char *mode);
void ath_log_close(void);
FILE *athout_fp(void);
FILE *atherr_fp(void);
void ath_flush_out(void);
void ath_flush_err(void);
int ath_perr(const int level, const char *fmt, ...);
int ath_pout(const int level, const char *fmt, ...);
char *ath_fname(const char *path, const char *basename,
                const char *levstr, const char *domstr,
                const int dlen, const int idump,
                const char *id, const char *ext);
void ath_sig_init(void);
int ath_sig_act(int *piquit);
void baton_start(const int Nb, const int tag);
void baton_stop(const int Nb, const int tag);
void bvals_mhd_init(MeshS *pM);
void bvals_mhd_fun(DomainS *pD, enum BCDirection dir, VGFun_t prob_bc);
void bvals_mhd(DomainS *pDomain);
void cc_pos(const GridS *pG, const int i, const int j,const int k,
            Real *px1, Real *px2, Real *px3);
void fc_pos(const GridS *pG, const int i, const int j,const int k,
            Real *px1, Real *px2, Real *px3);
PrimS Cons_to_Prim(const ConsS *pU);
ConsS Prim_to_Cons(const PrimS *pW);
Prim1DS Cons1D_to_Prim1D(const Cons1DS *pU, const Real *pBx);
Cons1DS Prim1D_to_Cons1D(const Prim1DS *pW, const Real *pBx);
Real cfast(const Cons1DS *U, const Real *Bx);
void init_grid(MeshS *pM);
void init_mesh(MeshS *pM);
void get_myGridIndex(DomainS *pD, const int my_id, int *pi, int *pj, int *pk);
void new_dt(MeshS *pM);
void init_output(MeshS *pM);
void data_output(MeshS *pM, const int flag);
int add_output(OutputS *new_out);
void add_rst_out(OutputS *new_out);
void data_output_destruct(void);
void dump_history_enroll(const ConsFun_t pfun, const char *label);
void data_output_enroll(Real time, Real dt, int num, const VOutFun_t fun,
   const char *fmt, const ConsFun_t expr, int n,
   const Real dmin, const Real dmax, int sdmin, int sdmax
);
Real ***OutData3(GridS *pGrid, OutputS *pOut, int *Nx1, int *Nx2, int *Nx3);
Real **OutData2(GridS *pGrid, OutputS *pOut, int *Nx1, int *Nx2);
Real *OutData1(GridS *pGrid, OutputS *pOut, int *Nx1);
void output_pdf (MeshS *pM, OutputS *pOut);
void output_pgm (MeshS *pM, OutputS *pOut);
void output_ppm (MeshS *pM, OutputS *pOut);
void output_vtk (MeshS *pM, OutputS *pOut);
void output_tab (MeshS *pM, OutputS *pOut);
void dump_binary (MeshS *pM, OutputS *pOut);
void dump_history (MeshS *pM, OutputS *pOut);
void dump_tab_cons(MeshS *pM, OutputS *pOut);
void dump_tab_prim(MeshS *pM, OutputS *pOut);
void dump_vtk (MeshS *pM, OutputS *pOut);
void par_open(char *filename);
void par_cmdline(int argc, char *argv[]);
int par_exist(char *block, char *name);
char *par_gets(char *block, char *name);
int par_geti(char *block, char *name);
double par_getd(char *block, char *name);
char *par_gets_def(char *block, char *name, char *def);
int par_geti_def(char *block, char *name, int def);
double par_getd_def(char *block, char *name, double def);
void par_sets(char *block, char *name, char *sval, char *comment);
void par_seti(char *block, char *name, char *fmt, int ival, char *comment);
void par_setd(char *block, char *name, char *fmt, double dval, char *comment);
void par_dump(int mode, FILE *fp);
void par_close(void);
void problem(DomainS *pD);
void Userwork_in_loop(MeshS *pM);
void Userwork_after_loop(MeshS *pM);
void problem_read_restart(MeshS *pM, FILE *fp);
void problem_write_restart(MeshS *pM, FILE *fp);
ConsFun_t get_usr_expr(const char *expr);
VOutFun_t get_usr_out_fun(const char *name);
void dump_restart(MeshS *pM, OutputS *pout);
void restart_grids(char *res_file, MeshS *pM);
void show_config(void);
void show_config_par(void);
void RestrictCorrect(MeshS *pM);
void Prolongate(MeshS *pM);
void SMR_init(MeshS *pM);
char *ath_strdup(const char *in);
int ath_gcd(int a, int b);
int ath_big_endian(void);
void ath_bswap(void *vdat, int sizeof_len, int cnt);
void ath_error(char *fmt, ...);
void minmax1(Real *data, int nx1, Real *dmin, Real *dmax);
void minmax2(Real **data, int nx2, int nx1, Real *dmin, Real *dmax);
void minmax3(Real ***data, int nx3, int nx2, int nx1, Real *dmin, Real *dmax);
void do_nothing_bc(GridS *pG);
Real compute_div_b(GridS *pG);
int sign_change(Real (*func)(const Real,const Real), const Real a0, const Real b0, const Real x, Real *a, Real *b);
int bisection(Real (*func)(const Real,const Real), const Real a0, const Real b0, const Real x, Real *root);
Real trapzd(Real (*func)(Real), const Real a, const Real b, const int n, const Real s);
Real qsimp(Real (*func)(Real), const Real a, const Real b);
Real avg1d(Real (*func)(Real, Real, Real), const GridS *pG, const int i, const int j, const int k);
Real avg2d(Real (*func)(Real, Real, Real), const GridS *pG, const int i, const int j, const int k);
Real avg3d(Real (*func)(Real, Real, Real), const GridS *pG, const int i, const int j, const int k);
Real avgXZ(Real (*func)(Real, Real, Real), const GridS *pG, const int i, const int j, const int k);
Real vecpot2b1i(Real (*A2)(Real,Real,Real), Real (*A3)(Real,Real,Real),
                const GridS *pG, const int i, const int j, const int k);
Real vecpot2b2i(Real (*A1)(Real,Real,Real), Real (*A3)(Real,Real,Real),
                const GridS *pG, const int i, const int j, const int k);
Real vecpot2b3i(Real (*A1)(Real,Real,Real), Real (*A2)(Real,Real,Real),
                const GridS *pG, const int i, const int j, const int k);
void init_mesh(MeshS *pM)
{
  int nblock,num_domains,nd,nl,level,maxlevel=0,nd_this_level;
  int nDim,nDim_test,dim;
  int *next_domainid;
  char block[80];
  int ncd,ir,irefine,l,m,n,roffset;
  int i,Nx[3],izones;
  div_t xdiv[3];
  Real root_xmin[3], root_xmax[3];
  int Nproc_Comm_world=1,nproc=0,next_procID;
  SideS D1,D2;
  DomainS *pD, *pCD;
  pM->time = 0.0;
  pM->nstep = 0;
  pM->outfilename = par_gets("job","problem_id");
  num_domains = par_geti("job","num_domains");
  if (num_domains > 1)
    ath_error("[init_mesh]: num_domains=%d; for num_domains > 1 configure with --enable-smr\n",num_domains);
  for (nblock=1; nblock<=num_domains; nblock++){
    __builtin___sprintf_chk (block, 0, __builtin_object_size (block, 2 > 1), "domain%d",nblock);
    if (par_exist(block,"level") == 0)
      ath_error("[init_mesh]: level does not exist in block %s\n",block);
    level = par_geti(block,"level");
    maxlevel = ( ((maxlevel) > (level)) ? (maxlevel) : (level) );
  }
  pM->NLevels = maxlevel + 1;
  pM->DomainsPerLevel = (int*)calloc_1d_array(pM->NLevels,sizeof(int));
  if (pM->DomainsPerLevel == ((void *)0))
    ath_error("[init_mesh]: malloc returned a NULL pointer\n");
  for (nl=0; nl<=maxlevel; nl++){
    nd_this_level=0;
    for (nblock=1; nblock<=num_domains; nblock++){
      __builtin___sprintf_chk (block, 0, __builtin_object_size (block, 2 > 1), "domain%d",nblock);
      if (par_geti(block,"level") == nl) nd_this_level++;
    }
    if (nd_this_level == 0) {
      ath_error("[init_mesh]: Level %d has zero domains\n",nl);
    } else {
      pM->DomainsPerLevel[nl] = nd_this_level;
    }
if (myID_Comm_world==0){
printf("level=%d, domains=%d\n",nl,pM->DomainsPerLevel[nl]);
}
  }
  if (pM->DomainsPerLevel[0] != 1)
    ath_error("[init_mesh]: Level 0 has %d domains\n",pM->DomainsPerLevel[0]);
  for (nblock=1; nblock<=num_domains; nblock++){
    __builtin___sprintf_chk (block, 0, __builtin_object_size (block, 2 > 1), "domain%d",nblock);
    level = par_geti(block,"level");
    if (level == 0){
      root_xmin[0] = par_getd(block,"x1min");
      root_xmax[0] = par_getd(block,"x1max");
      root_xmin[1] = par_getd(block,"x2min");
      root_xmax[1] = par_getd(block,"x2max");
      root_xmin[2] = par_getd(block,"x3min");
      root_xmax[2] = par_getd(block,"x3max");
      Nx[0] = par_geti(block,"Nx1");
      Nx[1] = par_geti(block,"Nx2");
      Nx[2] = par_geti(block,"Nx3");
      nDim=0;
      for (i=0; i<3; i++) if (Nx[i]>1) nDim++;
      if (nDim==0) ath_error("[init_mesh] None of Nx1,Nx2,Nx3 > 1\n");
      for (i=0; i<3; i++) {
        if (Nx[i] < 1) {
          ath_error("[init_mesh]: Nx%d in %s must be >= 1\n",(i+1),block);
        }
        if(root_xmax[i] < root_xmin[i]) {
          ath_error("[init_mesh]: x%dmax < x%dmin in %s\n",(i+1),block);
        }
      }
      if (nDim==1 && Nx[0]==1) {
        ath_error("[init_mesh]:1D requires Nx1>1: in %s Nx1=1,Nx2=%d,Nx3=%d\n",
        block,Nx[1],Nx[2]);
      }
      if (nDim==2 && Nx[2]>1) {ath_error(
        "[init_mesh]:2D requires Nx1,Nx2>1: in %s Nx1=%d,Nx2=%d,Nx3=%d\n",
        block,Nx[0],Nx[1],Nx[2]);
      }
      for (i=0; i<3; i++) {
        pM->Nx[i] = Nx[i];
        pM->RootMinX[i] = root_xmin[i];
        pM->RootMaxX[i] = root_xmax[i];
        pM->dx[i] = (root_xmax[i] - root_xmin[i])/(Real)(Nx[i]);
      }
      pM->BCFlag_ix1 = par_geti_def(block,"bc_ix1",0);
      pM->BCFlag_ix2 = par_geti_def(block,"bc_ix2",0);
      pM->BCFlag_ix3 = par_geti_def(block,"bc_ix3",0);
      pM->BCFlag_ox1 = par_geti_def(block,"bc_ox1",0);
      pM->BCFlag_ox2 = par_geti_def(block,"bc_ox2",0);
      pM->BCFlag_ox3 = par_geti_def(block,"bc_ox3",0);
    }
  }
  if((pM->Domain = (DomainS**)calloc((maxlevel+1),sizeof(DomainS*))) == ((void *)0)){
    ath_error("[init_mesh] failed to allocate memory for %d Domain pointers\n",
     (maxlevel+1));
  }
  if((pM->Domain[0]=(DomainS*)calloc(num_domains,sizeof(DomainS))) == ((void *)0)){
    ath_error("[init_mesh] failed to allocate memory for Domains\n");
  }
  for(nl=1; nl<=maxlevel; nl++)
    pM->Domain[nl] = (DomainS*)((unsigned char *)pM->Domain[nl-1] +
      pM->DomainsPerLevel[nl-1]*sizeof(DomainS));
  next_domainid = (int*)calloc_1d_array(pM->NLevels,sizeof(int));
  for(nl=0; nl<=maxlevel; nl++) next_domainid[nl] = 0;
  for (nblock=1; nblock<=num_domains; nblock++){
    __builtin___sprintf_chk (block, 0, __builtin_object_size (block, 2 > 1), "domain%d",nblock);
    nl = par_geti(block,"level");
if(myID_Comm_world==0){
printf("level=%d next_domainid=%d pM->DomainsPerLevel=%d\n",
nl,next_domainid[nl],pM->DomainsPerLevel[nl]);
}
    if (next_domainid[nl] > (pM->DomainsPerLevel[nl])-1)
      ath_error("[init_mesh]: Exceeded available domain ids on level %d\n",nl);
    nd = next_domainid[nl];
    next_domainid[nl]++;
    irefine = 1;
    for (ir=1;ir<=nl;ir++) irefine *= 2;
    pM->Domain[nl][nd].Level = nl;
    pM->Domain[nl][nd].DomNumber = nd;
    pM->Domain[nl][nd].InputBlock = nblock;
    pM->Domain[nl][nd].Nx[0] = par_geti(block,"Nx1");
    pM->Domain[nl][nd].Nx[1] = par_geti(block,"Nx2");
    pM->Domain[nl][nd].Nx[2] = par_geti(block,"Nx3");
    nDim_test=0;
    for (i=0; i<3; i++) if (pM->Domain[nl][nd].Nx[i]>1) nDim_test++;
    if (nDim_test != nDim) {
      ath_error("[init_mesh]: in %s grid is %dD, but in root level it is %dD\n",
      block,nDim_test,nDim);
    }
    for (i=0; i<3; i++) {
      if (pM->Domain[nl][nd].Nx[i] < 1) {
        ath_error("[init_mesh]: %s/Nx%d = %d must be >= 1\n",
          block,(i+1),pM->Domain[nl][nd].Nx[i]);
      }
    }
    if (nDim==1 && pM->Domain[nl][nd].Nx[0]==1) {ath_error(
      "[init_mesh]: 1D requires Nx1>1 but in %s Nx1=1,Nx2=%d,Nx3=%d\n",
      block,pM->Domain[nl][nd].Nx[1],pM->Domain[nl][nd].Nx[2]);
    }
    if (nDim==2 && pM->Domain[nl][nd].Nx[2]>1) {ath_error(
      "[init_mesh]:2D requires Nx1,Nx2 > 1 but in %s Nx1=%d,Nx2=%d,Nx3=%d\n",
      block,pM->Domain[nl][nd].Nx[0],pM->Domain[nl][nd].Nx[1],
      pM->Domain[nl][nd].Nx[2]);
    }
    for (i=0; i<nDim; i++) {
      xdiv[i] = div(pM->Domain[nl][nd].Nx[i], irefine);
      if (xdiv[i].rem != 0){
        ath_error("[init_mesh]: %s/Nx%d = %d must be divisible by %d\n",
          block,(i+1),pM->Domain[nl][nd].Nx[i],irefine);
      }
    }
    for (i=0; i<3; i++) {
      if (pM->Domain[nl][nd].Nx[i] > 1) {
        pM->Domain[nl][nd].dx[i] = pM->dx[i]/(Real)(irefine);
      } else {
        pM->Domain[nl][nd].dx[i] = pM->dx[i];
      }
    }
    for (i=0; i<3; i++) pM->Domain[nl][nd].Disp[i] = 0;
    if (nl != 0) {
      if (par_exist(block,"iDisp") == 0)
        ath_error("[init_mesh]: iDisp does not exist in block %s\n",block);
      pM->Domain[nl][nd].Disp[0] = par_geti(block,"iDisp");
      if (pM->Nx[1] > 1) {
        if (par_exist(block,"jDisp") == 0)
          ath_error("[init_mesh]: jDisp does not exist in block %s\n",block);
        pM->Domain[nl][nd].Disp[1] = par_geti(block,"jDisp");
      }
      if (pM->Nx[2] > 1) {
        if (par_exist(block,"kDisp") == 0)
          ath_error("[init_mesh]: kDisp does not exist in block %s\n",block);
        pM->Domain[nl][nd].Disp[2] = par_geti(block,"kDisp");
      }
    }
    for (i=0; i<nDim; i++) {
      xdiv[i] = div(pM->Domain[nl][nd].Disp[i], irefine);
      if (xdiv[i].rem != 0){
        ath_error("[init_mesh]: %s/Disp%d = %d must be divisible by %d\n",
          block,(i+1),pM->Domain[nl][nd].Disp[i],irefine);
      }
    }
    for (i=0; i<3; i++){
      if (pM->Domain[nl][nd].Disp[i] == 0) {
        pM->Domain[nl][nd].MinX[i] = root_xmin[i];
      } else {
        pM->Domain[nl][nd].MinX[i] = root_xmin[i]
          + ((Real)(pM->Domain[nl][nd].Disp[i]))*pM->Domain[nl][nd].dx[i];
      }
      izones= (pM->Domain[nl][nd].Disp[i] + pM->Domain[nl][nd].Nx[i])/irefine;
      if(izones == pM->Nx[i]){
        pM->Domain[nl][nd].MaxX[i] = root_xmax[i];
      } else {
        pM->Domain[nl][nd].MaxX[i] = pM->Domain[nl][nd].MinX[i]
          + ((Real)(pM->Domain[nl][nd].Nx[i]))*pM->Domain[nl][nd].dx[i];
      }
      pM->Domain[nl][nd].RootMinX[i] = root_xmin[i];
      pM->Domain[nl][nd].RootMaxX[i] = root_xmax[i];
    }
  }
  for (nl=maxlevel; nl>0; nl--){
  for (nd=0; nd<(pM->DomainsPerLevel[nl])-1; nd++){
    for (i=0; i<3; i++) {
      D1.ijkl[i] = pM->Domain[nl][nd].Disp[i];
      D1.ijkr[i] = pM->Domain[nl][nd].Disp[i] + pM->Domain[nl][nd].Nx[i];
    }
    for (ncd=nd+1; ncd<(pM->DomainsPerLevel[nl]); ncd++) {
      for (i=0; i<3; i++) {
        D2.ijkl[i] = pM->Domain[nl][ncd].Disp[i];
        D2.ijkr[i] = pM->Domain[nl][ncd].Disp[i] + pM->Domain[nl][ncd].Nx[i];
      }
      if (D1.ijkl[0] <= D2.ijkr[0] && D1.ijkr[0] >= D2.ijkl[0] &&
          D1.ijkl[1] <= D2.ijkr[1] && D1.ijkr[1] >= D2.ijkl[1] &&
          D1.ijkl[2] <= D2.ijkr[2] && D1.ijkr[2] >= D2.ijkl[2]){
          ath_error("Domains %d and %d at same level overlap or touch\n",
          pM->Domain[nl][nd].InputBlock,pM->Domain[nl][ncd].InputBlock);
      }
    }
  }}
  for (nl=0; nl<maxlevel; nl++){
  for (nd=0; nd<pM->DomainsPerLevel[nl]; nd++){
    pD = (DomainS*)&(pM->Domain[nl][nd]);
    for (i=0; i<3; i++) {
      D1.ijkl[i] = pD->Disp[i];
      D1.ijkr[i] = pD->Disp[i] + pD->Nx[i];
    }
    for (ncd=0; ncd<pM->DomainsPerLevel[nl+1]; ncd++){
      pCD = (DomainS*)&(pM->Domain[nl+1][ncd]);
      for (i=0; i<3; i++) {
        D2.ijkl[i] = pCD->Disp[i]/2;
        D2.ijkr[i] = 1;
        if (pCD->Nx[i] > 1) D2.ijkr[i] = (pCD->Disp[i] + pCD->Nx[i])/2;
      }
      if (D1.ijkl[0] <= D2.ijkr[0] && D1.ijkr[0] >= D2.ijkl[0] &&
          D1.ijkl[1] <= D2.ijkr[1] && D1.ijkr[1] >= D2.ijkl[1] &&
          D1.ijkl[2] <= D2.ijkr[2] && D1.ijkr[2] >= D2.ijkl[2]){
        for (dim=0; dim<nDim; dim++){
          irefine = 1;
          for (i=1;i<=nl;i++) irefine *= 2;
          roffset = (pCD->Disp[dim] + pCD->Nx[dim])/(2*irefine) - pM->Nx[dim];
          if (((D2.ijkl[dim] == D1.ijkl[dim]) && (pD->Disp[dim] != 0)) ||
              ((D2.ijkr[dim] == D1.ijkr[dim]) && (roffset != 0))) {
            for (i=0; i<nDim; i++) {
              D1.ijkl[i] /= irefine;
              D1.ijkr[i] /= irefine;
              D2.ijkl[i] /= irefine;
              D2.ijkr[i] /= irefine;
            }
            ath_error("[init_mesh] child Domain D%d[is,ie,js,je,ks,ke]=[%d %d %d %d %d %d] touches parent D%d[is,ie,js,je,ks,ke]=[%d %d %d %d %d %d]\n",
              pCD->InputBlock,D2.ijkl[0],D2.ijkr[0],D2.ijkl[1],D2.ijkr[1],
              D2.ijkl[2],D2.ijkr[2],pD->InputBlock,D1.ijkl[0],D1.ijkr[0],
              D1.ijkl[1],D1.ijkr[1],D1.ijkl[2],D1.ijkr[2]);
          }
          if ((D2.ijkl[dim] < D1.ijkl[dim]) ||
              (D2.ijkr[dim] > D1.ijkr[dim])) {
            for (i=0; i<nDim; i++) {
              D1.ijkl[i] /= irefine;
              D1.ijkr[i] /= irefine;
              D2.ijkl[i] /= irefine;
              D2.ijkr[i] /= irefine;
            }
            ath_error("[init_mesh] child Domain D%d[is,ie,js,je,ks,ke]=[%d %d %d %d %d %d] extends past parent D%d[is,ie,js,je,ks,ke]=[%d %d %d %d %d %d]\n",
              pCD->InputBlock,D2.ijkl[0],D2.ijkr[0],D2.ijkl[1],D2.ijkr[1],
              D2.ijkl[2],D2.ijkr[2],pD->InputBlock,D1.ijkl[0],D1.ijkr[0],
              D1.ijkl[1],D1.ijkr[1],D1.ijkl[2],D1.ijkr[2]);
          }
          if (((2*(D2.ijkl[dim]-D1.ijkl[dim]) < nghost) &&
               (2*(D2.ijkl[dim]-D1.ijkl[dim]) > 0 )) ||
              ((2*(D1.ijkr[dim]-D2.ijkr[dim]) < nghost) &&
               (2*(D1.ijkr[dim]-D2.ijkr[dim]) > 0 ))) {
            for (i=0; i<nDim; i++) {
              D1.ijkl[i] /= irefine;
              D1.ijkr[i] /= irefine;
              D2.ijkl[i] /= irefine;
              D2.ijkr[i] /= irefine;
            }
            ath_error("[init_mesh] child Domain D%d[is,ie,js,je,ks,ke]=[%d %d %d %d %d %d] closer than nghost/2 to parent D%d[is,ie,js,je,ks,ke]=[%d %d %d %d %d %d]\n",
              pCD->InputBlock,D2.ijkl[0],D2.ijkr[0],D2.ijkl[1],D2.ijkr[1],
              D2.ijkl[2],D2.ijkr[2],pD->InputBlock,D1.ijkl[0],D1.ijkr[0],
              D1.ijkl[1],D1.ijkr[1],D1.ijkl[2],D1.ijkr[2]);
          }
        }
      }
    }
  }}
  next_procID = 0;
  for (nl=0; nl<=maxlevel; nl++){
    for (nd=0; nd<(pM->DomainsPerLevel[nl]); nd++){
      pD = (DomainS*)&(pM->Domain[nl][nd]);
      __builtin___sprintf_chk (block, 0, __builtin_object_size (block, 2 > 1), "domain%d",pD->InputBlock);
      for (i=0; i<3; i++) pD->NGrid[i] = 1;
      for (i=0; i<3; i++){
        if(pD->NGrid[i] > 1 && pD->Nx[i] <= 1)
          ath_error("[init_mesh]: %s/NGrid_x%d = %d and Nx%d = %d\n",block,
          (i+1),pD->NGrid[i],(i+1),pD->Nx[i]);
      }
      nproc = (pD->NGrid[0])*(pD->NGrid[1])*(pD->NGrid[2]);
      if(nproc > Nproc_Comm_world) ath_error(
        "[init_mesh]: %d Grids requested by block %s and only %d procs\n"
        ,nproc,block,Nproc_Comm_world);
      if ((pD->GData = (GridsDataS***)calloc_3d_array(pD->NGrid[2],pD->NGrid[1],
        pD->NGrid[0],sizeof(GridsDataS))) == ((void *)0)) ath_error(
        "[init_mesh]: GData calloc returned a NULL pointer\n");
      for (i=0; i<3; i++) {
        xdiv[i] = div(pD->Nx[i], pD->NGrid[i]);
      }
      for(n=0; n<(pD->NGrid[2]); n++){
      for(m=0; m<(pD->NGrid[1]); m++){
      for(l=0; l<(pD->NGrid[0]); l++){
        for (i=0; i<3; i++) pD->GData[n][m][l].Nx[i] = xdiv[i].quot;
        pD->GData[n][m][l].ID_Comm_world = next_procID++;
        if (next_procID > ((Nproc_Comm_world)-1)) next_procID=0;
      }}}
      while (xdiv[0].rem > 0){
        for(n=0; n<(pD->NGrid[2]); n++){
          for(m=0; m<(pD->NGrid[1]); m++){
            pD->GData[n][m][0].Nx[0]++;
          }
        }
        xdiv[0].rem--;
      }
      while (xdiv[1].rem > 0){
        for(n=0; n<(pD->NGrid[2]); n++){
          for(l=0; l<(pD->NGrid[0]); l++){
            pD->GData[n][0][l].Nx[1]++;
          }
        }
        xdiv[1].rem--;
      }
      while (xdiv[2].rem > 0){
        for(m=0; m<(pD->NGrid[1]); m++){
          for(l=0; l<(pD->NGrid[0]); l++){
            pD->GData[0][m][l].Nx[2]++;
          }
        }
        xdiv[2].rem--;
      }
      for(n=0; n<(pD->NGrid[2]); n++){
        for(m=0; m<(pD->NGrid[1]); m++){
          pD->GData[n][m][0].Disp[0] = pD->Disp[0];
          for(l=1; l<(pD->NGrid[0]); l++){
            pD->GData[n][m][l].Disp[0] = pD->GData[n][m][l-1].Disp[0] +
                                         pD->GData[n][m][l-1].Nx[0];
          }
        }
      }
      for(n=0; n<(pD->NGrid[2]); n++){
        for(l=0; l<(pD->NGrid[0]); l++){
          pD->GData[n][0][l].Disp[1] = pD->Disp[1];
          for(m=1; m<(pD->NGrid[1]); m++){
            pD->GData[n][m][l].Disp[1] = pD->GData[n][m-1][l].Disp[1] +
                                         pD->GData[n][m-1][l].Nx[1];
          }
        }
      }
      for(m=0; m<(pD->NGrid[1]); m++){
        for(l=0; l<(pD->NGrid[0]); l++){
          pD->GData[0][m][l].Disp[2] = pD->Disp[2];
          for(n=1; n<(pD->NGrid[2]); n++){
            pD->GData[n][m][l].Disp[2] = pD->GData[n-1][m][l].Disp[2] +
                                         pD->GData[n-1][m][l].Nx[2];
          }
        }
      }
    }
  }
  if (next_procID != 0)
    ath_error("[init_mesh]:total # of Grids != total # of MPI procs\n");
  for (nl=0; nl<=maxlevel; nl++){
    for (nd=0; nd<(pM->DomainsPerLevel[nl]); nd++){
      pD = (DomainS*)&(pM->Domain[nl][nd]);
      __builtin___sprintf_chk (block, 0, __builtin_object_size (block, 2 > 1), "domain%d",pD->InputBlock);
      pD->Grid = ((void *)0);
      for(n=0; n<(pD->NGrid[2]); n++){
      for(m=0; m<(pD->NGrid[1]); m++){
      for(l=0; l<(pD->NGrid[0]); l++){
        if (pD->GData[n][m][l].ID_Comm_world == myID_Comm_world) {
          if ((pD->Grid = (GridS*)malloc(sizeof(GridS))) == ((void *)0))
            ath_error("[init_mesh]: Failed to malloc a Grid for %s\n",block);
        }
      }}}
    }
  }
  free(next_domainid);
  return;
}
void get_myGridIndex(DomainS *pD, const int myID,
                     int *pi, int *pj, int *pk)
{
  int i, j, k;
  for (k=0; k<(pD->NGrid[2]); k++){
    for (j=0; j<(pD->NGrid[1]); j++){
      for (i=0; i<(pD->NGrid[0]); i++){
        if (pD->GData[k][j][i].ID_Comm_world == myID) {
          *pi = i; *pj = j; *pk = k;
          return;
        }
      }
    }
  }
  ath_error("[get_myGridIndex]: Can't find ID=%i in GData\n", myID);
}
