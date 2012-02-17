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
typedef __darwin_va_list va_list;
typedef __darwin_off_t off_t;
typedef __darwin_size_t size_t;
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
typedef __darwin_ssize_t ssize_t;
void *memchr(const void *, int, size_t);
int memcmp(const void *, const void *, size_t);
void *memcpy(void *, const void *, size_t);
void *memmove(void *, const void *, size_t);
void *memset(void *, int, size_t);
char *stpcpy(char *, const char *);
char *strcasestr(const char *, const char *);
char *strcat(char *, const char *);
char *strchr(const char *, int);
int strcmp(const char *, const char *);
int strcoll(const char *, const char *);
char *strcpy(char *, const char *);
size_t strcspn(const char *, const char *);
char *strerror(int) __asm("_" "strerror" );
int strerror_r(int, char *, size_t);
size_t strlen(const char *);
char *strncat(char *, const char *, size_t);
int strncmp(const char *, const char *, size_t);
char *strncpy(char *, const char *, size_t);
char *strnstr(const char *, const char *, size_t);
char *strpbrk(const char *, const char *);
char *strrchr(const char *, int);
size_t strspn(const char *, const char *);
char *strstr(const char *, const char *);
char *strtok(char *, const char *);
size_t strxfrm(char *, const char *, size_t);
void *memccpy(void *, const void *, int, size_t);
char *strtok_r(char *, const char *, char **);
char *strdup(const char *);
int bcmp(const void *, const void *, size_t);
void bcopy(const void *, void *, size_t);
void bzero(void *, size_t);
int ffs(int);
int ffsl(long);
int fls(int);
int flsl(long);
char *index(const char *, int);
void memset_pattern4(void *, const void *, size_t);
void memset_pattern8(void *, const void *, size_t);
void memset_pattern16(void *, const void *, size_t);
char *rindex(const char *, int);
int strcasecmp(const char *, const char *);
size_t strlcat(char *, const char *, size_t);
size_t strlcpy(char *, const char *, size_t);
void strmode(int, char *);
int strncasecmp(const char *, const char *, size_t);
char *strsep(char **, const char *);
char *strsignal(int sig);
void swab(const void * , void * , ssize_t);
static __inline void *
__inline_memcpy_chk (void *__dest, const void *__src, size_t __len)
{
  return __builtin___memcpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}
static __inline void *
__inline_memmove_chk (void *__dest, const void *__src, size_t __len)
{
  return __builtin___memmove_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}
static __inline void *
__inline_memset_chk (void *__dest, int __val, size_t __len)
{
  return __builtin___memset_chk (__dest, __val, __len, __builtin_object_size (__dest, 0));
}
static __inline char *
__inline_strcpy_chk (char * __dest, const char * __src)
{
  return __builtin___strcpy_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}
static __inline char *
__inline_stpcpy_chk (char *__dest, const char *__src)
{
  return __builtin___stpcpy_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}
static __inline char *
__inline_strncpy_chk (char * __dest, const char * __src,
        size_t __len)
{
  return __builtin___strncpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 2 > 1));
}
static __inline char *
__inline_strcat_chk (char * __dest, const char * __src)
{
  return __builtin___strcat_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}
static __inline char *
__inline_strncat_chk (char * __dest, const char * __src,
        size_t __len)
{
  return __builtin___strncat_chk (__dest, __src, __len, __builtin_object_size (__dest, 2 > 1));
}
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
static void output_vtk_2d(MeshS *pM, OutputS *pOut, int nl, int nd);
static void output_vtk_3d(MeshS *pM, OutputS *pOut, int nl, int nd);
void output_vtk(MeshS *pM, OutputS *pOut)
{
  int nl,nd;
  for (nl=0; nl<(pM->NLevels); nl++){
    for (nd=0; nd<(pM->DomainsPerLevel[nl]); nd++){
      if (pM->Domain[nl][nd].Grid != ((void *)0)){
      if ((pOut->nlevel == -1 || pOut->nlevel == nl) &&
          (pOut->ndomain == -1 || pOut->ndomain == nd)){
        if (pOut->ndim == 3) {
          output_vtk_3d(pM, pOut, nl, nd);
        } else if (pOut->ndim == 2) {
          output_vtk_2d(pM, pOut, nl, nd);
        } else {
          ath_error("[output_vtk]: Only able to output 2D or 3D");
        }
      }}
    }
  }
  return;
}
static void output_vtk_2d(MeshS *pM, OutputS *pOut, int nl, int nd)
{
  GridS *pGrid=pM->Domain[nl][nd].Grid;
  FILE *pfile;
  char *fname,*plev=((void *)0),*pdom=((void *)0);
  char levstr[8],domstr[8];
  int big_end = ath_big_endian();
  int i,j,nx1,nx2;
  Real dmin, dmax;
  Real **data2d=((void *)0);
  float *data;
  double x1, x2, x3, dx1, dx2, dx3;
  data2d = OutData2(pGrid,pOut,&nx1,&nx2);
  if (data2d == ((void *)0)) return;
  if (nl>0) {
    plev = &levstr[0];
    __builtin___sprintf_chk (plev, 0, __builtin_object_size (plev, 2 > 1), "lev%d",nl);
  }
  if (nd>0) {
    pdom = &domstr[0];
    __builtin___sprintf_chk (pdom, 0, __builtin_object_size (pdom, 2 > 1), "dom%d",nd);
  }
  if((fname = ath_fname(plev,pM->outfilename,plev,pdom,num_digit,
      pOut->num,pOut->id,"vtk")) == ((void *)0)){
    ath_error("[output_vtk]: Error constructing filename\n");
  }
  if((pfile = fopen(fname,"w")) == ((void *)0)){
    ath_error("[output_vtk]: Unable to open vtk file %s\n",fname);
  }
  minmax2(data2d,nx2,nx1,&dmin,&dmax);
  pOut->gmin = ( ((dmin) < (pOut->gmin)) ? (dmin) : (pOut->gmin) );
  pOut->gmax = ( ((dmax) > (pOut->gmax)) ? (dmax) : (pOut->gmax) );
  if((data = (float *)malloc(nx1*sizeof(float))) == ((void *)0)){
     ath_error("[output_vtk]: malloc failed for temporary array\n");
     return;
  }
  fprintf(pfile,"# vtk DataFile Version 2.0\n");
  fprintf(pfile,"Really cool Athena data at time= %e, level= %i, domain= %i\n",
    pGrid->time,nl,nd);
  fprintf(pfile,"BINARY\n");
  x1 = pGrid->MinX[0];
  x2 = pGrid->MinX[1];
  x3 = pGrid->MinX[2];
  dx1 = (pOut->reduce_x1 == 1 ? pGrid->dx1 * pGrid->Nx[0] : pGrid->dx1);
  dx2 = (pOut->reduce_x2 == 1 ? pGrid->dx2 * pGrid->Nx[1] : pGrid->dx2);
  dx3 = (pOut->reduce_x3 == 1 ? pGrid->dx3 * pGrid->Nx[2] : pGrid->dx3);
  fprintf(pfile,"DATASET STRUCTURED_POINTS\n");
  fprintf(pfile,"DIMENSIONS %d %d %d\n",nx1+1,nx2+1,1);
  fprintf(pfile,"ORIGIN %e %e %e \n",x1,x2,x3);
  fprintf(pfile,"SPACING %e %e %e \n",dx1,dx2,dx3);
  fprintf(pfile,"CELL_DATA %d \n", nx1*nx2);
  fprintf(pfile,"SCALARS %s float\n", pOut->id);
  fprintf(pfile,"LOOKUP_TABLE default\n");
  for (j=0; j<nx2; j++){
    for (i=0; i<nx1; i++){
      data[i] = (float)data2d[j][i];
    }
    if(!big_end) ath_bswap(data,sizeof(float),nx1);
    fwrite(data,sizeof(float),(size_t)nx1,pfile);
  }
  fclose(pfile);
  free(data);
  free_2d_array(data2d);
  return;
}
static void output_vtk_3d(MeshS *pM, OutputS *pOut, int nl, int nd)
{
  GridS *pGrid=pM->Domain[nl][nd].Grid;
  FILE *pfile;
  char *fname,*plev=((void *)0),*pdom=((void *)0);
  char levstr[8],domstr[8];
  int big_end = ath_big_endian();
  int nx1,nx2,nx3,i,j,k;
  Real dmin, dmax;
  Real ***data3d=((void *)0);
  float *data;
  double x1, x2, x3;
  data3d = OutData3(pGrid,pOut,&nx1,&nx2,&nx3);
  if (nl>0) {
    plev = &levstr[0];
    __builtin___sprintf_chk (plev, 0, __builtin_object_size (plev, 2 > 1), "lev%d",nl);
  }
  if (nd>0) {
    pdom = &domstr[0];
    __builtin___sprintf_chk (pdom, 0, __builtin_object_size (pdom, 2 > 1), "dom%d",nd);
  }
  if((fname = ath_fname(plev,pM->outfilename,plev,pdom,num_digit,
      pOut->num,pOut->id,"vtk")) == ((void *)0)){
    ath_error("[output_vtk]: Error constructing filename\n");
  }
  if((pfile = fopen(fname,"w")) == ((void *)0)){
    ath_error("[output_vtk]: Unable to open vtk file %s\n",fname);
  }
  minmax3(data3d,nx3,nx2,nx1,&dmin,&dmax);
  pOut->gmin = ( ((dmin) < (pOut->gmin)) ? (dmin) : (pOut->gmin) );
  pOut->gmax = ( ((dmax) > (pOut->gmax)) ? (dmax) : (pOut->gmax) );
  x1 = pGrid->MinX[0];
  x2 = pGrid->MinX[1];
  x3 = pGrid->MinX[2];
  if((data = (float *)malloc(nx1*sizeof(float))) == ((void *)0)){
     ath_error("[output_vtk]: malloc failed for temporary array\n");
     return;
  }
  fprintf(pfile,"# vtk DataFile Version 2.0\n");
  fprintf(pfile,"Really cool Athena data at time= %e, level= %i, domain= %i\n",
    pGrid->time,nl,nd);
  fprintf(pfile,"BINARY\n");
  fprintf(pfile,"DATASET STRUCTURED_POINTS\n");
  fprintf(pfile,"DIMENSIONS %d %d %d\n",nx1+1,nx2+1,nx3+1);
  fprintf(pfile,"ORIGIN %e %e %e \n",x1,x2,x3);
  fprintf(pfile,"SPACING %e %e %e \n",pGrid->dx1,pGrid->dx2,pGrid->dx3);
  fprintf(pfile,"CELL_DATA %d \n", nx1*nx2*nx3);
  fprintf(pfile,"SCALARS %s float\n", pOut->id);
  fprintf(pfile,"LOOKUP_TABLE default\n");
  for (k=0; k<nx3; k++) {
    for (j=0; j<nx2; j++) {
      for (i=0; i<nx1; i++) {
        data[i] = (float)data3d[k][j][i];
      }
      if(!big_end) ath_bswap(data,sizeof(float),nx1);
      fwrite(data,sizeof(float),(size_t)nx1,pfile);
    }
  }
  fclose(pfile);
  free(data);
  free_3d_array(data3d);
  return;
}
