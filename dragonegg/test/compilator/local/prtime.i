typedef long int ptrdiff_t;
typedef long unsigned int size_t;
typedef int wchar_t;
typedef unsigned char PRUint8;
typedef signed char PRInt8;
typedef unsigned short PRUint16;
typedef short PRInt16;
typedef unsigned int PRUint32;
typedef int PRInt32;
typedef long PRInt64;
typedef unsigned long PRUint64;
typedef int PRIntn;
typedef unsigned int PRUintn;
typedef double PRFloat64;
typedef size_t PRSize;
typedef PRInt32 PROffset32;
typedef PRInt64 PROffset64;
typedef ptrdiff_t PRPtrdiff;
typedef unsigned long PRUptrdiff;
typedef PRIntn PRBool;
typedef PRUint8 PRPackedBool;
typedef enum { PR_FAILURE = -1, PR_SUCCESS = 0 } PRStatus;
typedef PRUint16 PRUnichar;
typedef long PRWord;
typedef unsigned long PRUword;
typedef PRUintn uintn;
typedef PRIntn intn;
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
typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;
typedef long int __quad_t;
typedef unsigned long int __u_quad_t;
typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;
typedef int __daddr_t;
typedef long int __swblk_t;
typedef int __key_t;
typedef int __clockid_t;
typedef void * __timer_t;
typedef long int __blksize_t;
typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;
typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;
typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;
typedef long int __ssize_t;
typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;
typedef long int __intptr_t;
typedef unsigned int __socklen_t;
typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;
typedef __loff_t loff_t;
typedef __ino_t ino_t;
typedef __ino64_t ino64_t;
typedef __dev_t dev_t;
typedef __gid_t gid_t;
typedef __mode_t mode_t;
typedef __nlink_t nlink_t;
typedef __uid_t uid_t;
typedef __off_t off_t;
typedef __off64_t off64_t;
typedef __pid_t pid_t;
typedef __id_t id_t;
typedef __ssize_t ssize_t;
typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;
typedef __key_t key_t;
typedef __clock_t clock_t;
typedef __time_t time_t;
typedef __clockid_t clockid_t;
typedef __timer_t timer_t;
typedef __useconds_t useconds_t;
typedef __suseconds_t suseconds_t;
typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;
typedef int int8_t __attribute__ ((__mode__ (__QI__)));
typedef int int16_t __attribute__ ((__mode__ (__HI__)));
typedef int int32_t __attribute__ ((__mode__ (__SI__)));
typedef int int64_t __attribute__ ((__mode__ (__DI__)));
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
typedef long int __fd_mask;
typedef struct
  {
    __fd_mask fds_bits[1024 / (8 * sizeof (__fd_mask))];
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
     __attribute__ ((__nothrow__));
__extension__
extern unsigned int gnu_dev_minor (unsigned long long int __dev)
     __attribute__ ((__nothrow__));
__extension__
extern unsigned long long int gnu_dev_makedev (unsigned int __major,
            unsigned int __minor)
     __attribute__ ((__nothrow__));
__extension__ extern __inline unsigned int
__attribute__ ((__nothrow__)) gnu_dev_major (unsigned long long int __dev)
{
  return ((__dev >> 8) & 0xfff) | ((unsigned int) (__dev >> 32) & ~0xfff);
}
__extension__ extern __inline unsigned int
__attribute__ ((__nothrow__)) gnu_dev_minor (unsigned long long int __dev)
{
  return (__dev & 0xff) | ((unsigned int) (__dev >> 12) & ~0xff);
}
__extension__ extern __inline unsigned long long int
__attribute__ ((__nothrow__)) gnu_dev_makedev (unsigned int __major, unsigned int __minor)
{
  return ((__minor & 0xff) | ((__major & 0xfff) << 8)
   | (((unsigned long long int) (__minor & ~0xff)) << 12)
   | (((unsigned long long int) (__major & ~0xfff)) << 32));
}
typedef __blksize_t blksize_t;
typedef __blkcnt_t blkcnt_t;
typedef __fsblkcnt_t fsblkcnt_t;
typedef __fsfilcnt_t fsfilcnt_t;
typedef __blkcnt64_t blkcnt64_t;
typedef __fsblkcnt64_t fsblkcnt64_t;
typedef __fsfilcnt64_t fsfilcnt64_t;
typedef unsigned long int pthread_t;
typedef union
{
  char __size[56];
  long int __align;
} pthread_attr_t;
typedef struct __pthread_internal_list
{
  struct __pthread_internal_list *__prev;
  struct __pthread_internal_list *__next;
} __pthread_list_t;
typedef union
{
  struct __pthread_mutex_s
  {
    int __lock;
    unsigned int __count;
    int __owner;
    unsigned int __nusers;
    int __kind;
    int __spins;
    __pthread_list_t __list;
  } __data;
  char __size[40];
  long int __align;
} pthread_mutex_t;
typedef union
{
  char __size[4];
  int __align;
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
  int __align;
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
    int __writer;
    int __shared;
    unsigned long int __pad1;
    unsigned long int __pad2;
    unsigned int __flags;
  } __data;
  char __size[56];
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
  char __size[32];
  long int __align;
} pthread_barrier_t;
typedef union
{
  char __size[4];
  int __align;
} pthread_barrierattr_t;
typedef PRUint64 uint64;
typedef PRUint32 uint32;
typedef PRUint16 uint16;
typedef PRUint8 uint8;
typedef PRInt64 int64;
typedef PRInt32 int32;
typedef PRInt16 int16;
typedef PRInt8 int8;
typedef PRFloat64 float64;
typedef PRUptrdiff uptrdiff_t;
typedef PRUword uprword_t;
typedef PRWord prword_t;
typedef PRUint32 PRIntervalTime;
extern __attribute__((visibility("default"))) PRIntervalTime PR_IntervalNow(void);
extern __attribute__((visibility("default"))) PRUint32 PR_TicksPerSecond(void);
extern __attribute__((visibility("default"))) PRIntervalTime PR_SecondsToInterval(PRUint32 seconds);
extern __attribute__((visibility("default"))) PRIntervalTime PR_MillisecondsToInterval(PRUint32 milli);
extern __attribute__((visibility("default"))) PRIntervalTime PR_MicrosecondsToInterval(PRUint32 micro);
extern __attribute__((visibility("default"))) PRUint32 PR_IntervalToSeconds(PRIntervalTime ticks);
extern __attribute__((visibility("default"))) PRUint32 PR_IntervalToMilliseconds(PRIntervalTime ticks);
extern __attribute__((visibility("default"))) PRUint32 PR_IntervalToMicroseconds(PRIntervalTime ticks);
typedef struct PRThread PRThread;
typedef struct PRThreadStack PRThreadStack;
typedef enum PRThreadType {
    PR_USER_THREAD,
    PR_SYSTEM_THREAD
} PRThreadType;
typedef enum PRThreadScope {
    PR_LOCAL_THREAD,
    PR_GLOBAL_THREAD,
    PR_GLOBAL_BOUND_THREAD
} PRThreadScope;
typedef enum PRThreadState {
    PR_JOINABLE_THREAD,
    PR_UNJOINABLE_THREAD
} PRThreadState;
typedef enum PRThreadPriority
{
    PR_PRIORITY_FIRST = 0,
    PR_PRIORITY_LOW = 0,
    PR_PRIORITY_NORMAL = 1,
    PR_PRIORITY_HIGH = 2,
    PR_PRIORITY_URGENT = 3,
    PR_PRIORITY_LAST = 3
} PRThreadPriority;
extern __attribute__((visibility("default"))) PRThread* PR_CreateThread(PRThreadType type,
                     void ( *start)(void *arg),
                     void *arg,
                     PRThreadPriority priority,
                     PRThreadScope scope,
                     PRThreadState state,
                     PRUint32 stackSize);
extern __attribute__((visibility("default"))) PRStatus PR_JoinThread(PRThread *thread);
extern __attribute__((visibility("default"))) PRThread* PR_GetCurrentThread(void);
extern __attribute__((visibility("default"))) PRThreadPriority PR_GetThreadPriority(const PRThread *thread);
extern __attribute__((visibility("default"))) void PR_SetThreadPriority(PRThread *thread, PRThreadPriority priority);
typedef void ( *PRThreadPrivateDTOR)(void *priv);
extern __attribute__((visibility("default"))) PRStatus PR_NewThreadPrivateIndex(
    PRUintn *newIndex, PRThreadPrivateDTOR destructor);
extern __attribute__((visibility("default"))) PRStatus PR_SetThreadPrivate(PRUintn tpdIndex, void *priv);
extern __attribute__((visibility("default"))) void* PR_GetThreadPrivate(PRUintn tpdIndex);
extern __attribute__((visibility("default"))) PRStatus PR_Interrupt(PRThread *thread);
extern __attribute__((visibility("default"))) void PR_ClearInterrupt(void);
extern __attribute__((visibility("default"))) void PR_BlockInterrupt(void);
extern __attribute__((visibility("default"))) void PR_UnblockInterrupt(void);
extern __attribute__((visibility("default"))) PRStatus PR_Sleep(PRIntervalTime ticks);
extern __attribute__((visibility("default"))) PRThreadScope PR_GetThreadScope(const PRThread *thread);
extern __attribute__((visibility("default"))) PRThreadType PR_GetThreadType(const PRThread *thread);
extern __attribute__((visibility("default"))) PRThreadState PR_GetThreadState(const PRThread *thread);
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
typedef __io_read_fn cookie_read_function_t;
typedef __io_write_fn cookie_write_function_t;
typedef __io_seek_fn cookie_seek_function_t;
typedef __io_close_fn cookie_close_function_t;
typedef struct
{
  __io_read_fn *read;
  __io_write_fn *write;
  __io_seek_fn *seek;
  __io_close_fn *close;
} _IO_cookie_io_functions_t;
typedef _IO_cookie_io_functions_t cookie_io_functions_t;
struct _IO_cookie_file;
extern void _IO_cookie_init (struct _IO_cookie_file *__cfile, int __read_write,
        void *__cookie, _IO_cookie_io_functions_t __fns);
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
typedef _G_fpos_t fpos_t;
typedef _G_fpos64_t fpos64_t;
extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;
extern int remove (__const char *__filename) __attribute__ ((__nothrow__));
extern int rename (__const char *__old, __const char *__new) __attribute__ ((__nothrow__));
extern int renameat (int __oldfd, __const char *__old, int __newfd,
       __const char *__new) __attribute__ ((__nothrow__));
extern FILE *tmpfile (void) ;
extern FILE *tmpfile64 (void) ;
extern char *tmpnam (char *__s) __attribute__ ((__nothrow__)) ;
extern char *tmpnam_r (char *__s) __attribute__ ((__nothrow__)) ;
extern char *tempnam (__const char *__dir, __const char *__pfx)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) ;
extern int fclose (FILE *__stream);
extern int fflush (FILE *__stream);
extern int fflush_unlocked (FILE *__stream);
extern int fcloseall (void);
extern FILE *fopen (__const char *__restrict __filename,
      __const char *__restrict __modes) ;
extern FILE *freopen (__const char *__restrict __filename,
        __const char *__restrict __modes,
        FILE *__restrict __stream) ;
extern FILE *fopen64 (__const char *__restrict __filename,
        __const char *__restrict __modes) ;
extern FILE *freopen64 (__const char *__restrict __filename,
   __const char *__restrict __modes,
   FILE *__restrict __stream) ;
extern FILE *fdopen (int __fd, __const char *__modes) __attribute__ ((__nothrow__)) ;
extern FILE *fopencookie (void *__restrict __magic_cookie,
     __const char *__restrict __modes,
     _IO_cookie_io_functions_t __io_funcs) __attribute__ ((__nothrow__)) ;
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
extern int vasprintf (char **__restrict __ptr, __const char *__restrict __f,
        __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 0))) ;
extern int __asprintf (char **__restrict __ptr,
         __const char *__restrict __fmt, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 3))) ;
extern int asprintf (char **__restrict __ptr,
       __const char *__restrict __fmt, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 3))) ;
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
extern int vfscanf (FILE *__restrict __s, __const char *__restrict __format,
      __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 2, 0))) ;
extern int vscanf (__const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 1, 0))) ;
extern int vsscanf (__const char *__restrict __s,
      __const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__scanf__, 2, 0)));
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
extern char *fgets_unlocked (char *__restrict __s, int __n,
        FILE *__restrict __stream) ;
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
extern int fputs_unlocked (__const char *__restrict __s,
      FILE *__restrict __stream);
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite_unlocked (__const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream) ;
extern int fseek (FILE *__stream, long int __off, int __whence);
extern long int ftell (FILE *__stream) ;
extern void rewind (FILE *__stream);
extern int fseeko (FILE *__stream, __off_t __off, int __whence);
extern __off_t ftello (FILE *__stream) ;
extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);
extern int fsetpos (FILE *__stream, __const fpos_t *__pos);
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
extern int _sys_nerr;
extern __const char *__const _sys_errlist[];
extern int fileno (FILE *__stream) __attribute__ ((__nothrow__)) ;
extern int fileno_unlocked (FILE *__stream) __attribute__ ((__nothrow__)) ;
extern FILE *popen (__const char *__command, __const char *__modes) ;
extern int pclose (FILE *__stream);
extern char *ctermid (char *__s) __attribute__ ((__nothrow__));
extern char *cuserid (char *__s);
struct obstack;
extern int obstack_printf (struct obstack *__restrict __obstack,
      __const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 3)));
extern int obstack_vprintf (struct obstack *__restrict __obstack,
       __const char *__restrict __format,
       __gnuc_va_list __args)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 0)));
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
extern __inline __ssize_t
getline (char **__lineptr, size_t *__n, FILE *__stream)
{
  return __getdelim (__lineptr, __n, '\n', __stream);
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
typedef PRBool (*PRVersionCheck)(const char*);
extern __attribute__((visibility("default"))) PRBool PR_VersionCheck(const char *importedVersion);
extern __attribute__((visibility("default"))) void PR_Init(
    PRThreadType type, PRThreadPriority priority, PRUintn maxPTDs);
typedef PRIntn ( *PRPrimordialFn)(PRIntn argc, char **argv);
extern __attribute__((visibility("default"))) PRIntn PR_Initialize(
    PRPrimordialFn prmain, PRIntn argc, char **argv, PRUintn maxPTDs);
extern __attribute__((visibility("default"))) PRBool PR_Initialized(void);
extern __attribute__((visibility("default"))) PRStatus PR_Cleanup(void);
extern __attribute__((visibility("default"))) void PR_DisableClockInterrupts(void);
extern __attribute__((visibility("default"))) void PR_EnableClockInterrupts(void);
extern __attribute__((visibility("default"))) void PR_BlockClockInterrupts(void);
extern __attribute__((visibility("default"))) void PR_UnblockClockInterrupts(void);
extern __attribute__((visibility("default"))) void PR_SetConcurrency(PRUintn numCPUs);
extern __attribute__((visibility("default"))) PRStatus PR_SetFDCacheSize(PRIntn low, PRIntn high);
extern __attribute__((visibility("default"))) void PR_ProcessExit(PRIntn status);
extern __attribute__((visibility("default"))) void PR_Abort(void);
typedef struct PRCallOnceType {
    PRIntn initialized;
    PRInt32 inProgress;
    PRStatus status;
} PRCallOnceType;
typedef PRStatus ( *PRCallOnceFN)(void);
typedef PRStatus ( *PRCallOnceWithArgFN)(void *arg);
extern __attribute__((visibility("default"))) PRStatus PR_CallOnce(
    PRCallOnceType *once,
    PRCallOnceFN func
);
extern __attribute__((visibility("default"))) PRStatus PR_CallOnceWithArg(
    PRCallOnceType *once,
    PRCallOnceWithArgFN func,
    void *arg
);
extern __attribute__((visibility("default"))) PRInt64 LL_MaxInt(void);
extern __attribute__((visibility("default"))) PRInt64 LL_MinInt(void);
extern __attribute__((visibility("default"))) PRInt64 LL_Zero(void);
extern __attribute__((visibility("default"))) PRUint64 LL_MaxUint(void);
typedef PRInt64 PRTime;
typedef struct PRTimeParameters {
    PRInt32 tp_gmt_offset;
    PRInt32 tp_dst_offset;
} PRTimeParameters;
typedef struct PRExplodedTime {
    PRInt32 tm_usec;
    PRInt32 tm_sec;
    PRInt32 tm_min;
    PRInt32 tm_hour;
    PRInt32 tm_mday;
    PRInt32 tm_month;
    PRInt16 tm_year;
    PRInt8 tm_wday;
    PRInt16 tm_yday;
    PRTimeParameters tm_params;
} PRExplodedTime;
typedef PRTimeParameters ( *PRTimeParamFn)(const PRExplodedTime *gmt);
extern __attribute__((visibility("default"))) PRTime
PR_Now(void);
extern __attribute__((visibility("default"))) void PR_ExplodeTime(
    PRTime usecs, PRTimeParamFn params, PRExplodedTime *exploded);
extern __attribute__((visibility("default"))) PRTime
PR_ImplodeTime(const PRExplodedTime *exploded);
extern __attribute__((visibility("default"))) void PR_NormalizeTime(
    PRExplodedTime *exploded, PRTimeParamFn params);
extern __attribute__((visibility("default"))) PRTimeParameters PR_LocalTimeParameters(const PRExplodedTime *gmt);
extern __attribute__((visibility("default"))) PRTimeParameters PR_GMTParameters(const PRExplodedTime *gmt);
extern __attribute__((visibility("default"))) PRTimeParameters PR_USPacificTimeParameters(const PRExplodedTime *gmt);
extern __attribute__((visibility("default"))) PRStatus PR_ParseTimeStringToExplodedTime (
        const char *string,
        PRBool default_to_gmt,
        PRExplodedTime *result);
extern __attribute__((visibility("default"))) PRStatus PR_ParseTimeString (
 const char *string,
 PRBool default_to_gmt,
 PRTime *result);
extern __attribute__((visibility("default"))) PRUint32 PR_FormatTime(char *buf, int buflen, const char *fmt,
                                           const PRExplodedTime *tm);
extern __attribute__((visibility("default"))) PRUint32
PR_FormatTimeUSEnglish( char* buf, PRUint32 bufSize,
                        const char* format, const PRExplodedTime* tm );
typedef struct PRLock PRLock;
extern __attribute__((visibility("default"))) PRLock* PR_NewLock(void);
extern __attribute__((visibility("default"))) void PR_DestroyLock(PRLock *lock);
extern __attribute__((visibility("default"))) void PR_Lock(PRLock *lock);
extern __attribute__((visibility("default"))) PRStatus PR_Unlock(PRLock *lock);
struct iovec
  {
    void *iov_base;
    size_t iov_len;
  };
extern ssize_t readv (int __fd, __const struct iovec *__iovec, int __count);
extern ssize_t writev (int __fd, __const struct iovec *__iovec, int __count);
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
    MSG_TRYHARD = MSG_DONTROUTE,
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
    return 0;
  __cmsg = (struct cmsghdr *) ((unsigned char *) __cmsg
          + (((__cmsg->cmsg_len) + sizeof (size_t) - 1) & (size_t) ~(sizeof (size_t) - 1)));
  if ((unsigned char *) (__cmsg + 1) > ((unsigned char *) __mhdr->msg_control
     + __mhdr->msg_controllen)
      || ((unsigned char *) __cmsg + (((__cmsg->cmsg_len) + sizeof (size_t) - 1) & (size_t) ~(sizeof (size_t) - 1))
   > ((unsigned char *) __mhdr->msg_control + __mhdr->msg_controllen)))
    return 0;
  return __cmsg;
}
enum
  {
    SCM_RIGHTS = 0x01
    , SCM_CREDENTIALS = 0x02
  };
struct ucred
{
  pid_t pid;
  uid_t uid;
  gid_t gid;
};
struct linger
  {
    int l_onoff;
    int l_linger;
  };
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
typedef union { struct sockaddr *__restrict __sockaddr__; struct sockaddr_at *__restrict __sockaddr_at__; struct sockaddr_ax25 *__restrict __sockaddr_ax25__; struct sockaddr_dl *__restrict __sockaddr_dl__; struct sockaddr_eon *__restrict __sockaddr_eon__; struct sockaddr_in *__restrict __sockaddr_in__; struct sockaddr_in6 *__restrict __sockaddr_in6__; struct sockaddr_inarp *__restrict __sockaddr_inarp__; struct sockaddr_ipx *__restrict __sockaddr_ipx__; struct sockaddr_iso *__restrict __sockaddr_iso__; struct sockaddr_ns *__restrict __sockaddr_ns__; struct sockaddr_un *__restrict __sockaddr_un__; struct sockaddr_x25 *__restrict __sockaddr_x25__;
       } __SOCKADDR_ARG __attribute__ ((__transparent_union__));
typedef union { __const struct sockaddr *__restrict __sockaddr__; __const struct sockaddr_at *__restrict __sockaddr_at__; __const struct sockaddr_ax25 *__restrict __sockaddr_ax25__; __const struct sockaddr_dl *__restrict __sockaddr_dl__; __const struct sockaddr_eon *__restrict __sockaddr_eon__; __const struct sockaddr_in *__restrict __sockaddr_in__; __const struct sockaddr_in6 *__restrict __sockaddr_in6__; __const struct sockaddr_inarp *__restrict __sockaddr_inarp__; __const struct sockaddr_ipx *__restrict __sockaddr_ipx__; __const struct sockaddr_iso *__restrict __sockaddr_iso__; __const struct sockaddr_ns *__restrict __sockaddr_ns__; __const struct sockaddr_un *__restrict __sockaddr_un__; __const struct sockaddr_x25 *__restrict __sockaddr_x25__;
       } __CONST_SOCKADDR_ARG __attribute__ ((__transparent_union__));
extern int socket (int __domain, int __type, int __protocol) __attribute__ ((__nothrow__));
extern int socketpair (int __domain, int __type, int __protocol,
         int __fds[2]) __attribute__ ((__nothrow__));
extern int bind (int __fd, __CONST_SOCKADDR_ARG __addr, socklen_t __len)
     __attribute__ ((__nothrow__));
extern int getsockname (int __fd, __SOCKADDR_ARG __addr,
   socklen_t *__restrict __len) __attribute__ ((__nothrow__));
extern int connect (int __fd, __CONST_SOCKADDR_ARG __addr, socklen_t __len);
extern int getpeername (int __fd, __SOCKADDR_ARG __addr,
   socklen_t *__restrict __len) __attribute__ ((__nothrow__));
extern ssize_t send (int __fd, __const void *__buf, size_t __n, int __flags);
extern ssize_t recv (int __fd, void *__buf, size_t __n, int __flags);
extern ssize_t sendto (int __fd, __const void *__buf, size_t __n,
         int __flags, __CONST_SOCKADDR_ARG __addr,
         socklen_t __addr_len);
extern ssize_t recvfrom (int __fd, void *__restrict __buf, size_t __n,
    int __flags, __SOCKADDR_ARG __addr,
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
extern int accept (int __fd, __SOCKADDR_ARG __addr,
     socklen_t *__restrict __addr_len);
extern int shutdown (int __fd, int __how) __attribute__ ((__nothrow__));
extern int sockatmark (int __fd) __attribute__ ((__nothrow__));
extern int isfdtype (int __fd, int __fdtype) __attribute__ ((__nothrow__));
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long int uint64_t;
typedef signed char int_least8_t;
typedef short int int_least16_t;
typedef int int_least32_t;
typedef long int int_least64_t;
typedef unsigned char uint_least8_t;
typedef unsigned short int uint_least16_t;
typedef unsigned int uint_least32_t;
typedef unsigned long int uint_least64_t;
typedef signed char int_fast8_t;
typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;
typedef unsigned char uint_fast8_t;
typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;
typedef long int intptr_t;
typedef unsigned long int uintptr_t;
typedef long int intmax_t;
typedef unsigned long int uintmax_t;
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
struct in6_pktinfo
  {
    struct in6_addr ipi6_addr;
    unsigned int ipi6_ifindex;
  };
struct ip6_mtuinfo
  {
    struct sockaddr_in6 ip6m_addr;
    uint32_t ip6m_mtu;
  };
extern int inet6_option_space (int __nbytes)
     __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern int inet6_option_init (void *__bp, struct cmsghdr **__cmsgp,
         int __type) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern int inet6_option_append (struct cmsghdr *__cmsg,
    __const uint8_t *__typep, int __multx,
    int __plusy) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern uint8_t *inet6_option_alloc (struct cmsghdr *__cmsg, int __datalen,
        int __multx, int __plusy)
     __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern int inet6_option_next (__const struct cmsghdr *__cmsg,
         uint8_t **__tptrp)
     __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern int inet6_option_find (__const struct cmsghdr *__cmsg,
         uint8_t **__tptrp, int __type)
     __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern int inet6_opt_init (void *__extbuf, socklen_t __extlen) __attribute__ ((__nothrow__));
extern int inet6_opt_append (void *__extbuf, socklen_t __extlen, int __offset,
        uint8_t __type, socklen_t __len, uint8_t __align,
        void **__databufp) __attribute__ ((__nothrow__));
extern int inet6_opt_finish (void *__extbuf, socklen_t __extlen, int __offset)
     __attribute__ ((__nothrow__));
extern int inet6_opt_set_val (void *__databuf, int __offset, void *__val,
         socklen_t __vallen) __attribute__ ((__nothrow__));
extern int inet6_opt_next (void *__extbuf, socklen_t __extlen, int __offset,
      uint8_t *__typep, socklen_t *__lenp,
      void **__databufp) __attribute__ ((__nothrow__));
extern int inet6_opt_find (void *__extbuf, socklen_t __extlen, int __offset,
      uint8_t __type, socklen_t *__lenp,
      void **__databufp) __attribute__ ((__nothrow__));
extern int inet6_opt_get_val (void *__databuf, int __offset, void *__val,
         socklen_t __vallen) __attribute__ ((__nothrow__));
extern socklen_t inet6_rth_space (int __type, int __segments) __attribute__ ((__nothrow__));
extern void *inet6_rth_init (void *__bp, socklen_t __bp_len, int __type,
        int __segments) __attribute__ ((__nothrow__));
extern int inet6_rth_add (void *__bp, __const struct in6_addr *__addr) __attribute__ ((__nothrow__));
extern int inet6_rth_reverse (__const void *__in, void *__out) __attribute__ ((__nothrow__));
extern int inet6_rth_segments (__const void *__bp) __attribute__ ((__nothrow__));
extern struct in6_addr *inet6_rth_getaddr (__const void *__bp, int __index)
     __attribute__ ((__nothrow__));
extern int getipv4sourcefilter (int __s, struct in_addr __interface_addr,
    struct in_addr __group, uint32_t *__fmode,
    uint32_t *__numsrc, struct in_addr *__slist)
     __attribute__ ((__nothrow__));
extern int setipv4sourcefilter (int __s, struct in_addr __interface_addr,
    struct in_addr __group, uint32_t __fmode,
    uint32_t __numsrc,
    __const struct in_addr *__slist)
     __attribute__ ((__nothrow__));
extern int getsourcefilter (int __s, uint32_t __interface_addr,
       __const struct sockaddr *__group,
       socklen_t __grouplen, uint32_t *__fmode,
       uint32_t *__numsrc,
       struct sockaddr_storage *__slist) __attribute__ ((__nothrow__));
extern int setsourcefilter (int __s, uint32_t __interface_addr,
       __const struct sockaddr *__group,
       socklen_t __grouplen, uint32_t __fmode,
       uint32_t __numsrc,
       __const struct sockaddr_storage *__slist) __attribute__ ((__nothrow__));
extern in_addr_t inet_addr (__const char *__cp) __attribute__ ((__nothrow__));
extern in_addr_t inet_lnaof (struct in_addr __in) __attribute__ ((__nothrow__));
extern struct in_addr inet_makeaddr (in_addr_t __net, in_addr_t __host)
     __attribute__ ((__nothrow__));
extern in_addr_t inet_netof (struct in_addr __in) __attribute__ ((__nothrow__));
extern in_addr_t inet_network (__const char *__cp) __attribute__ ((__nothrow__));
extern char *inet_ntoa (struct in_addr __in) __attribute__ ((__nothrow__));
extern int inet_pton (int __af, __const char *__restrict __cp,
        void *__restrict __buf) __attribute__ ((__nothrow__));
extern __const char *inet_ntop (int __af, __const void *__restrict __cp,
    char *__restrict __buf, socklen_t __len)
     __attribute__ ((__nothrow__));
extern int inet_aton (__const char *__cp, struct in_addr *__inp) __attribute__ ((__nothrow__));
extern char *inet_neta (in_addr_t __net, char *__buf, size_t __len) __attribute__ ((__nothrow__));
extern char *inet_net_ntop (int __af, __const void *__cp, int __bits,
       char *__buf, size_t __len) __attribute__ ((__nothrow__));
extern int inet_net_pton (int __af, __const char *__cp,
     void *__buf, size_t __len) __attribute__ ((__nothrow__));
extern unsigned int inet_nsap_addr (__const char *__cp,
        unsigned char *__buf, int __len) __attribute__ ((__nothrow__));
extern char *inet_nsap_ntoa (int __len, __const unsigned char *__cp,
        char *__buf) __attribute__ ((__nothrow__));
struct rpcent
{
  char *r_name;
  char **r_aliases;
  int r_number;
};
extern void setrpcent (int __stayopen) __attribute__ ((__nothrow__));
extern void endrpcent (void) __attribute__ ((__nothrow__));
extern struct rpcent *getrpcbyname (__const char *__name) __attribute__ ((__nothrow__));
extern struct rpcent *getrpcbynumber (int __number) __attribute__ ((__nothrow__));
extern struct rpcent *getrpcent (void) __attribute__ ((__nothrow__));
extern int getrpcbyname_r (__const char *__name, struct rpcent *__result_buf,
      char *__buffer, size_t __buflen,
      struct rpcent **__result) __attribute__ ((__nothrow__));
extern int getrpcbynumber_r (int __number, struct rpcent *__result_buf,
        char *__buffer, size_t __buflen,
        struct rpcent **__result) __attribute__ ((__nothrow__));
extern int getrpcent_r (struct rpcent *__result_buf, char *__buffer,
   size_t __buflen, struct rpcent **__result) __attribute__ ((__nothrow__));
typedef union sigval
  {
    int sival_int;
    void *sival_ptr;
  } sigval_t;
typedef struct sigevent
  {
    sigval_t sigev_value;
    int sigev_signo;
    int sigev_notify;
    union
      {
 int _pad[((64 / sizeof (int)) - 4)];
 __pid_t _tid;
 struct
   {
     void (*_function) (sigval_t);
     void *_attribute;
   } _sigev_thread;
      } _sigev_un;
  } sigevent_t;
enum
{
  SIGEV_SIGNAL = 0,
  SIGEV_NONE,
  SIGEV_THREAD,
  SIGEV_THREAD_ID = 4
};
struct netent
{
  char *n_name;
  char **n_aliases;
  int n_addrtype;
  uint32_t n_net;
};
extern int *__h_errno_location (void) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern void herror (__const char *__str) __attribute__ ((__nothrow__));
extern __const char *hstrerror (int __err_num) __attribute__ ((__nothrow__));
struct hostent
{
  char *h_name;
  char **h_aliases;
  int h_addrtype;
  int h_length;
  char **h_addr_list;
};
extern void sethostent (int __stay_open);
extern void endhostent (void);
extern struct hostent *gethostent (void);
extern struct hostent *gethostbyaddr (__const void *__addr, __socklen_t __len,
          int __type);
extern struct hostent *gethostbyname (__const char *__name);
extern struct hostent *gethostbyname2 (__const char *__name, int __af);
extern int gethostent_r (struct hostent *__restrict __result_buf,
    char *__restrict __buf, size_t __buflen,
    struct hostent **__restrict __result,
    int *__restrict __h_errnop);
extern int gethostbyaddr_r (__const void *__restrict __addr, __socklen_t __len,
       int __type,
       struct hostent *__restrict __result_buf,
       char *__restrict __buf, size_t __buflen,
       struct hostent **__restrict __result,
       int *__restrict __h_errnop);
extern int gethostbyname_r (__const char *__restrict __name,
       struct hostent *__restrict __result_buf,
       char *__restrict __buf, size_t __buflen,
       struct hostent **__restrict __result,
       int *__restrict __h_errnop);
extern int gethostbyname2_r (__const char *__restrict __name, int __af,
        struct hostent *__restrict __result_buf,
        char *__restrict __buf, size_t __buflen,
        struct hostent **__restrict __result,
        int *__restrict __h_errnop);
extern void setnetent (int __stay_open);
extern void endnetent (void);
extern struct netent *getnetent (void);
extern struct netent *getnetbyaddr (uint32_t __net, int __type);
extern struct netent *getnetbyname (__const char *__name);
extern int getnetent_r (struct netent *__restrict __result_buf,
   char *__restrict __buf, size_t __buflen,
   struct netent **__restrict __result,
   int *__restrict __h_errnop);
extern int getnetbyaddr_r (uint32_t __net, int __type,
      struct netent *__restrict __result_buf,
      char *__restrict __buf, size_t __buflen,
      struct netent **__restrict __result,
      int *__restrict __h_errnop);
extern int getnetbyname_r (__const char *__restrict __name,
      struct netent *__restrict __result_buf,
      char *__restrict __buf, size_t __buflen,
      struct netent **__restrict __result,
      int *__restrict __h_errnop);
struct servent
{
  char *s_name;
  char **s_aliases;
  int s_port;
  char *s_proto;
};
extern void setservent (int __stay_open);
extern void endservent (void);
extern struct servent *getservent (void);
extern struct servent *getservbyname (__const char *__name,
          __const char *__proto);
extern struct servent *getservbyport (int __port, __const char *__proto);
extern int getservent_r (struct servent *__restrict __result_buf,
    char *__restrict __buf, size_t __buflen,
    struct servent **__restrict __result);
extern int getservbyname_r (__const char *__restrict __name,
       __const char *__restrict __proto,
       struct servent *__restrict __result_buf,
       char *__restrict __buf, size_t __buflen,
       struct servent **__restrict __result);
extern int getservbyport_r (int __port, __const char *__restrict __proto,
       struct servent *__restrict __result_buf,
       char *__restrict __buf, size_t __buflen,
       struct servent **__restrict __result);
struct protoent
{
  char *p_name;
  char **p_aliases;
  int p_proto;
};
extern void setprotoent (int __stay_open);
extern void endprotoent (void);
extern struct protoent *getprotoent (void);
extern struct protoent *getprotobyname (__const char *__name);
extern struct protoent *getprotobynumber (int __proto);
extern int getprotoent_r (struct protoent *__restrict __result_buf,
     char *__restrict __buf, size_t __buflen,
     struct protoent **__restrict __result);
extern int getprotobyname_r (__const char *__restrict __name,
        struct protoent *__restrict __result_buf,
        char *__restrict __buf, size_t __buflen,
        struct protoent **__restrict __result);
extern int getprotobynumber_r (int __proto,
          struct protoent *__restrict __result_buf,
          char *__restrict __buf, size_t __buflen,
          struct protoent **__restrict __result);
extern int setnetgrent (__const char *__netgroup);
extern void endnetgrent (void);
extern int getnetgrent (char **__restrict __hostp,
   char **__restrict __userp,
   char **__restrict __domainp);
extern int innetgr (__const char *__netgroup, __const char *__host,
      __const char *__user, __const char *domain);
extern int getnetgrent_r (char **__restrict __hostp,
     char **__restrict __userp,
     char **__restrict __domainp,
     char *__restrict __buffer, size_t __buflen);
extern int rcmd (char **__restrict __ahost, unsigned short int __rport,
   __const char *__restrict __locuser,
   __const char *__restrict __remuser,
   __const char *__restrict __cmd, int *__restrict __fd2p);
extern int rcmd_af (char **__restrict __ahost, unsigned short int __rport,
      __const char *__restrict __locuser,
      __const char *__restrict __remuser,
      __const char *__restrict __cmd, int *__restrict __fd2p,
      sa_family_t __af);
extern int rexec (char **__restrict __ahost, int __rport,
    __const char *__restrict __name,
    __const char *__restrict __pass,
    __const char *__restrict __cmd, int *__restrict __fd2p);
extern int rexec_af (char **__restrict __ahost, int __rport,
       __const char *__restrict __name,
       __const char *__restrict __pass,
       __const char *__restrict __cmd, int *__restrict __fd2p,
       sa_family_t __af);
extern int ruserok (__const char *__rhost, int __suser,
      __const char *__remuser, __const char *__locuser);
extern int ruserok_af (__const char *__rhost, int __suser,
         __const char *__remuser, __const char *__locuser,
         sa_family_t __af);
extern int rresvport (int *__alport);
extern int rresvport_af (int *__alport, sa_family_t __af);
struct addrinfo
{
  int ai_flags;
  int ai_family;
  int ai_socktype;
  int ai_protocol;
  socklen_t ai_addrlen;
  struct sockaddr *ai_addr;
  char *ai_canonname;
  struct addrinfo *ai_next;
};
struct gaicb
{
  const char *ar_name;
  const char *ar_service;
  const struct addrinfo *ar_request;
  struct addrinfo *ar_result;
  int __return;
  int __unused[5];
};
extern int getaddrinfo (__const char *__restrict __name,
   __const char *__restrict __service,
   __const struct addrinfo *__restrict __req,
   struct addrinfo **__restrict __pai);
extern void freeaddrinfo (struct addrinfo *__ai) __attribute__ ((__nothrow__));
extern __const char *gai_strerror (int __ecode) __attribute__ ((__nothrow__));
extern int getnameinfo (__const struct sockaddr *__restrict __sa,
   socklen_t __salen, char *__restrict __host,
   socklen_t __hostlen, char *__restrict __serv,
   socklen_t __servlen, unsigned int __flags);
extern int getaddrinfo_a (int __mode, struct gaicb *__list[__restrict],
     int __ent, struct sigevent *__restrict __sig);
extern int gai_suspend (__const struct gaicb *__const __list[], int __ent,
   __const struct timespec *__timeout);
extern int gai_error (struct gaicb *__req) __attribute__ ((__nothrow__));
extern int gai_cancel (struct gaicb *__gaicbp) __attribute__ ((__nothrow__));
typedef struct PRDir PRDir;
typedef struct PRDirEntry PRDirEntry;
typedef struct PRFileDesc PRFileDesc;
typedef struct PRFileInfo PRFileInfo;
typedef struct PRFileInfo64 PRFileInfo64;
typedef union PRNetAddr PRNetAddr;
typedef struct PRIOMethods PRIOMethods;
typedef struct PRPollDesc PRPollDesc;
typedef struct PRFilePrivate PRFilePrivate;
typedef struct PRSendFileData PRSendFileData;
typedef PRIntn PRDescIdentity;
struct PRFileDesc {
    const PRIOMethods *methods;
    PRFilePrivate *secret;
    PRFileDesc *lower, *higher;
    void ( *dtor)(PRFileDesc *fd);
    PRDescIdentity identity;
};
typedef enum PRTransmitFileFlags {
    PR_TRANSMITFILE_KEEP_OPEN = 0,
    PR_TRANSMITFILE_CLOSE_SOCKET = 1
} PRTransmitFileFlags;
struct PRIPv6Addr {
 union {
  PRUint8 _S6_u8[16];
  PRUint16 _S6_u16[8];
  PRUint32 _S6_u32[4];
  PRUint64 _S6_u64[2];
 } _S6_un;
};
typedef struct PRIPv6Addr PRIPv6Addr;
union PRNetAddr {
    struct {
        PRUint16 family;
        char data[14];
    } raw;
    struct {
        PRUint16 family;
        PRUint16 port;
        PRUint32 ip;
        char pad[8];
    } inet;
    struct {
        PRUint16 family;
        PRUint16 port;
        PRUint32 flowinfo;
        PRIPv6Addr ip;
        PRUint32 scope_id;
    } ipv6;
    struct {
        PRUint16 family;
        char path[104];
    } local;
};
typedef enum PRSockOption
{
    PR_SockOpt_Nonblocking,
    PR_SockOpt_Linger,
    PR_SockOpt_Reuseaddr,
    PR_SockOpt_Keepalive,
    PR_SockOpt_RecvBufferSize,
    PR_SockOpt_SendBufferSize,
    PR_SockOpt_IpTimeToLive,
    PR_SockOpt_IpTypeOfService,
    PR_SockOpt_AddMember,
    PR_SockOpt_DropMember,
    PR_SockOpt_McastInterface,
    PR_SockOpt_McastTimeToLive,
    PR_SockOpt_McastLoopback,
    PR_SockOpt_NoDelay,
    PR_SockOpt_MaxSegment,
    PR_SockOpt_Broadcast,
    PR_SockOpt_Last
} PRSockOption;
typedef struct PRLinger {
 PRBool polarity;
 PRIntervalTime linger;
} PRLinger;
typedef struct PRMcastRequest {
 PRNetAddr mcaddr;
 PRNetAddr ifaddr;
} PRMcastRequest;
typedef struct PRSocketOptionData
{
    PRSockOption option;
    union
    {
        PRUintn ip_ttl;
        PRUintn mcast_ttl;
        PRUintn tos;
        PRBool non_blocking;
        PRBool reuse_addr;
        PRBool keep_alive;
        PRBool mcast_loopback;
        PRBool no_delay;
        PRBool broadcast;
        PRSize max_segment;
        PRSize recv_buffer_size;
        PRSize send_buffer_size;
        PRLinger linger;
        PRMcastRequest add_member;
        PRMcastRequest drop_member;
        PRNetAddr mcast_if;
    } value;
} PRSocketOptionData;
typedef struct PRIOVec {
    char *iov_base;
    int iov_len;
} PRIOVec;
typedef enum PRDescType
{
    PR_DESC_FILE = 1,
    PR_DESC_SOCKET_TCP = 2,
    PR_DESC_SOCKET_UDP = 3,
    PR_DESC_LAYERED = 4,
    PR_DESC_PIPE = 5
} PRDescType;
typedef enum PRSeekWhence {
    PR_SEEK_SET = 0,
    PR_SEEK_CUR = 1,
    PR_SEEK_END = 2
} PRSeekWhence;
extern __attribute__((visibility("default"))) PRDescType PR_GetDescType(PRFileDesc *file);
typedef PRStatus ( *PRCloseFN)(PRFileDesc *fd);
typedef PRInt32 ( *PRReadFN)(PRFileDesc *fd, void *buf, PRInt32 amount);
typedef PRInt32 ( *PRWriteFN)(PRFileDesc *fd, const void *buf, PRInt32 amount);
typedef PRInt32 ( *PRAvailableFN)(PRFileDesc *fd);
typedef PRInt64 ( *PRAvailable64FN)(PRFileDesc *fd);
typedef PRStatus ( *PRFsyncFN)(PRFileDesc *fd);
typedef PROffset32 ( *PRSeekFN)(PRFileDesc *fd, PROffset32 offset, PRSeekWhence how);
typedef PROffset64 ( *PRSeek64FN)(PRFileDesc *fd, PROffset64 offset, PRSeekWhence how);
typedef PRStatus ( *PRFileInfoFN)(PRFileDesc *fd, PRFileInfo *info);
typedef PRStatus ( *PRFileInfo64FN)(PRFileDesc *fd, PRFileInfo64 *info);
typedef PRInt32 ( *PRWritevFN)(
    PRFileDesc *fd, const PRIOVec *iov, PRInt32 iov_size,
    PRIntervalTime timeout);
typedef PRStatus ( *PRConnectFN)(
    PRFileDesc *fd, const PRNetAddr *addr, PRIntervalTime timeout);
typedef PRFileDesc* ( *PRAcceptFN) (
    PRFileDesc *fd, PRNetAddr *addr, PRIntervalTime timeout);
typedef PRStatus ( *PRBindFN)(PRFileDesc *fd, const PRNetAddr *addr);
typedef PRStatus ( *PRListenFN)(PRFileDesc *fd, PRIntn backlog);
typedef PRStatus ( *PRShutdownFN)(PRFileDesc *fd, PRIntn how);
typedef PRInt32 ( *PRRecvFN)(
    PRFileDesc *fd, void *buf, PRInt32 amount,
    PRIntn flags, PRIntervalTime timeout);
typedef PRInt32 ( *PRSendFN) (
    PRFileDesc *fd, const void *buf, PRInt32 amount,
    PRIntn flags, PRIntervalTime timeout);
typedef PRInt32 ( *PRRecvfromFN)(
    PRFileDesc *fd, void *buf, PRInt32 amount,
    PRIntn flags, PRNetAddr *addr, PRIntervalTime timeout);
typedef PRInt32 ( *PRSendtoFN)(
    PRFileDesc *fd, const void *buf, PRInt32 amount,
    PRIntn flags, const PRNetAddr *addr, PRIntervalTime timeout);
typedef PRInt16 ( *PRPollFN)(
    PRFileDesc *fd, PRInt16 in_flags, PRInt16 *out_flags);
typedef PRInt32 ( *PRAcceptreadFN)(
    PRFileDesc *sd, PRFileDesc **nd, PRNetAddr **raddr,
    void *buf, PRInt32 amount, PRIntervalTime t);
typedef PRInt32 ( *PRTransmitfileFN)(
     PRFileDesc *sd, PRFileDesc *fd, const void *headers,
     PRInt32 hlen, PRTransmitFileFlags flags, PRIntervalTime t);
typedef PRStatus ( *PRGetsocknameFN)(PRFileDesc *fd, PRNetAddr *addr);
typedef PRStatus ( *PRGetpeernameFN)(PRFileDesc *fd, PRNetAddr *addr);
typedef PRStatus ( *PRGetsocketoptionFN)(
    PRFileDesc *fd, PRSocketOptionData *data);
typedef PRStatus ( *PRSetsocketoptionFN)(
    PRFileDesc *fd, const PRSocketOptionData *data);
typedef PRInt32 ( *PRSendfileFN)(
 PRFileDesc *networkSocket, PRSendFileData *sendData,
 PRTransmitFileFlags flags, PRIntervalTime timeout);
typedef PRStatus ( *PRConnectcontinueFN)(
    PRFileDesc *fd, PRInt16 out_flags);
typedef PRIntn ( *PRReservedFN)(PRFileDesc *fd);
struct PRIOMethods {
    PRDescType file_type;
    PRCloseFN close;
    PRReadFN read;
    PRWriteFN write;
    PRAvailableFN available;
    PRAvailable64FN available64;
    PRFsyncFN fsync;
    PRSeekFN seek;
    PRSeek64FN seek64;
    PRFileInfoFN fileInfo;
    PRFileInfo64FN fileInfo64;
    PRWritevFN writev;
    PRConnectFN connect;
    PRAcceptFN accept;
    PRBindFN bind;
    PRListenFN listen;
    PRShutdownFN shutdown;
    PRRecvFN recv;
    PRSendFN send;
    PRRecvfromFN recvfrom;
    PRSendtoFN sendto;
    PRPollFN poll;
    PRAcceptreadFN acceptread;
    PRTransmitfileFN transmitfile;
    PRGetsocknameFN getsockname;
    PRGetpeernameFN getpeername;
    PRReservedFN reserved_fn_6;
    PRReservedFN reserved_fn_5;
    PRGetsocketoptionFN getsocketoption;
    PRSetsocketoptionFN setsocketoption;
    PRSendfileFN sendfile;
    PRConnectcontinueFN connectcontinue;
    PRReservedFN reserved_fn_3;
    PRReservedFN reserved_fn_2;
    PRReservedFN reserved_fn_1;
    PRReservedFN reserved_fn_0;
};
typedef enum PRSpecialFD
{
    PR_StandardInput,
    PR_StandardOutput,
    PR_StandardError
} PRSpecialFD;
extern __attribute__((visibility("default"))) PRFileDesc* PR_GetSpecialFD(PRSpecialFD id);
extern __attribute__((visibility("default"))) PRDescIdentity PR_GetUniqueIdentity(const char *layer_name);
extern __attribute__((visibility("default"))) const char* PR_GetNameForIdentity(PRDescIdentity ident);
extern __attribute__((visibility("default"))) PRDescIdentity PR_GetLayersIdentity(PRFileDesc* fd);
extern __attribute__((visibility("default"))) PRFileDesc* PR_GetIdentitiesLayer(PRFileDesc* fd_stack, PRDescIdentity id);
extern __attribute__((visibility("default"))) const PRIOMethods * PR_GetDefaultIOMethods(void);
extern __attribute__((visibility("default"))) PRFileDesc* PR_CreateIOLayerStub(
    PRDescIdentity ident, const PRIOMethods *methods);
extern __attribute__((visibility("default"))) PRFileDesc* PR_CreateIOLayer(PRFileDesc* fd);
extern __attribute__((visibility("default"))) PRStatus PR_PushIOLayer(
    PRFileDesc *fd_stack, PRDescIdentity id, PRFileDesc *layer);
extern __attribute__((visibility("default"))) PRFileDesc* PR_PopIOLayer(PRFileDesc *fd_stack, PRDescIdentity id);
extern __attribute__((visibility("default"))) PRFileDesc* PR_Open(const char *name, PRIntn flags, PRIntn mode);
extern __attribute__((visibility("default"))) PRFileDesc* PR_OpenFile(
    const char *name, PRIntn flags, PRIntn mode);
extern __attribute__((visibility("default"))) PRStatus PR_Close(PRFileDesc *fd);
extern __attribute__((visibility("default"))) PRInt32 PR_Read(PRFileDesc *fd, void *buf, PRInt32 amount);
extern __attribute__((visibility("default"))) PRInt32 PR_Write(PRFileDesc *fd,const void *buf,PRInt32 amount);
extern __attribute__((visibility("default"))) PRInt32 PR_Writev(
    PRFileDesc *fd, const PRIOVec *iov, PRInt32 iov_size,
    PRIntervalTime timeout);
extern __attribute__((visibility("default"))) PRStatus PR_Delete(const char *name);
typedef enum PRFileType
{
    PR_FILE_FILE = 1,
    PR_FILE_DIRECTORY = 2,
    PR_FILE_OTHER = 3
} PRFileType;
struct PRFileInfo {
    PRFileType type;
    PROffset32 size;
    PRTime creationTime;
    PRTime modifyTime;
};
struct PRFileInfo64 {
    PRFileType type;
    PROffset64 size;
    PRTime creationTime;
    PRTime modifyTime;
};
extern __attribute__((visibility("default"))) PRStatus PR_GetFileInfo(const char *fn, PRFileInfo *info);
extern __attribute__((visibility("default"))) PRStatus PR_GetFileInfo64(const char *fn, PRFileInfo64 *info);
extern __attribute__((visibility("default"))) PRStatus PR_GetOpenFileInfo(PRFileDesc *fd, PRFileInfo *info);
extern __attribute__((visibility("default"))) PRStatus PR_GetOpenFileInfo64(PRFileDesc *fd, PRFileInfo64 *info);
extern __attribute__((visibility("default"))) PRStatus PR_Rename(const char *from, const char *to);
typedef enum PRAccessHow {
    PR_ACCESS_EXISTS = 1,
    PR_ACCESS_WRITE_OK = 2,
    PR_ACCESS_READ_OK = 3
} PRAccessHow;
extern __attribute__((visibility("default"))) PRStatus PR_Access(const char *name, PRAccessHow how);
extern __attribute__((visibility("default"))) PROffset32 PR_Seek(PRFileDesc *fd, PROffset32 offset, PRSeekWhence whence);
extern __attribute__((visibility("default"))) PROffset64 PR_Seek64(PRFileDesc *fd, PROffset64 offset, PRSeekWhence whence);
extern __attribute__((visibility("default"))) PRInt32 PR_Available(PRFileDesc *fd);
extern __attribute__((visibility("default"))) PRInt64 PR_Available64(PRFileDesc *fd);
extern __attribute__((visibility("default"))) PRStatus PR_Sync(PRFileDesc *fd);
struct PRDirEntry {
    const char *name;
};
extern __attribute__((visibility("default"))) PRDir* PR_OpenDir(const char *name);
typedef enum PRDirFlags {
    PR_SKIP_NONE = 0x0,
    PR_SKIP_DOT = 0x1,
    PR_SKIP_DOT_DOT = 0x2,
    PR_SKIP_BOTH = 0x3,
    PR_SKIP_HIDDEN = 0x4
} PRDirFlags;
extern __attribute__((visibility("default"))) PRDirEntry* PR_ReadDir(PRDir *dir, PRDirFlags flags);
extern __attribute__((visibility("default"))) PRStatus PR_CloseDir(PRDir *dir);
extern __attribute__((visibility("default"))) PRStatus PR_MkDir(const char *name, PRIntn mode);
extern __attribute__((visibility("default"))) PRStatus PR_MakeDir(const char *name, PRIntn mode);
extern __attribute__((visibility("default"))) PRStatus PR_RmDir(const char *name);
extern __attribute__((visibility("default"))) PRFileDesc* PR_NewUDPSocket(void);
extern __attribute__((visibility("default"))) PRFileDesc* PR_NewTCPSocket(void);
extern __attribute__((visibility("default"))) PRFileDesc* PR_OpenUDPSocket(PRIntn af);
extern __attribute__((visibility("default"))) PRFileDesc* PR_OpenTCPSocket(PRIntn af);
extern __attribute__((visibility("default"))) PRStatus PR_Connect(
    PRFileDesc *fd, const PRNetAddr *addr, PRIntervalTime timeout);
extern __attribute__((visibility("default"))) PRStatus PR_ConnectContinue(PRFileDesc *fd, PRInt16 out_flags);
extern __attribute__((visibility("default"))) PRStatus PR_GetConnectStatus(const PRPollDesc *pd);
extern __attribute__((visibility("default"))) PRFileDesc* PR_Accept(
    PRFileDesc *fd, PRNetAddr *addr, PRIntervalTime timeout);
extern __attribute__((visibility("default"))) PRStatus PR_Bind(PRFileDesc *fd, const PRNetAddr *addr);
extern __attribute__((visibility("default"))) PRStatus PR_Listen(PRFileDesc *fd, PRIntn backlog);
typedef enum PRShutdownHow
{
    PR_SHUTDOWN_RCV = 0,
    PR_SHUTDOWN_SEND = 1,
    PR_SHUTDOWN_BOTH = 2
} PRShutdownHow;
extern __attribute__((visibility("default"))) PRStatus PR_Shutdown(PRFileDesc *fd, PRShutdownHow how);
extern __attribute__((visibility("default"))) PRInt32 PR_Recv(PRFileDesc *fd, void *buf, PRInt32 amount,
                PRIntn flags, PRIntervalTime timeout);
extern __attribute__((visibility("default"))) PRInt32 PR_Send(PRFileDesc *fd, const void *buf, PRInt32 amount,
                                PRIntn flags, PRIntervalTime timeout);
extern __attribute__((visibility("default"))) PRInt32 PR_RecvFrom(
    PRFileDesc *fd, void *buf, PRInt32 amount, PRIntn flags,
    PRNetAddr *addr, PRIntervalTime timeout);
extern __attribute__((visibility("default"))) PRInt32 PR_SendTo(
    PRFileDesc *fd, const void *buf, PRInt32 amount, PRIntn flags,
    const PRNetAddr *addr, PRIntervalTime timeout);
extern __attribute__((visibility("default"))) PRInt32 PR_TransmitFile(
    PRFileDesc *networkSocket, PRFileDesc *sourceFile,
    const void *headers, PRInt32 hlen, PRTransmitFileFlags flags,
    PRIntervalTime timeout);
struct PRSendFileData {
 PRFileDesc *fd;
 PRUint32 file_offset;
 PRSize file_nbytes;
 const void *header;
 PRInt32 hlen;
 const void *trailer;
 PRInt32 tlen;
};
extern __attribute__((visibility("default"))) PRInt32 PR_SendFile(
    PRFileDesc *networkSocket, PRSendFileData *sendData,
 PRTransmitFileFlags flags, PRIntervalTime timeout);
extern __attribute__((visibility("default"))) PRInt32 PR_AcceptRead(
    PRFileDesc *listenSock, PRFileDesc **acceptedSock,
    PRNetAddr **peerAddr, void *buf, PRInt32 amount, PRIntervalTime timeout);
extern __attribute__((visibility("default"))) PRStatus PR_NewTCPSocketPair(PRFileDesc *fds[2]);
extern __attribute__((visibility("default"))) PRStatus PR_GetSockName(PRFileDesc *fd, PRNetAddr *addr);
extern __attribute__((visibility("default"))) PRStatus PR_GetPeerName(PRFileDesc *fd, PRNetAddr *addr);
extern __attribute__((visibility("default"))) PRStatus PR_GetSocketOption(
    PRFileDesc *fd, PRSocketOptionData *data);
extern __attribute__((visibility("default"))) PRStatus PR_SetSocketOption(
    PRFileDesc *fd, const PRSocketOptionData *data);
extern __attribute__((visibility("default"))) PRStatus PR_SetFDInheritable(
    PRFileDesc *fd,
    PRBool inheritable);
extern __attribute__((visibility("default"))) PRFileDesc * PR_GetInheritedFD(const char *name);
typedef struct PRFileMap PRFileMap;
typedef enum PRFileMapProtect {
    PR_PROT_READONLY,
    PR_PROT_READWRITE,
    PR_PROT_WRITECOPY
} PRFileMapProtect;
extern __attribute__((visibility("default"))) PRFileMap * PR_CreateFileMap(
    PRFileDesc *fd,
    PRInt64 size,
    PRFileMapProtect prot);
extern __attribute__((visibility("default"))) PRInt32 PR_GetMemMapAlignment(void);
extern __attribute__((visibility("default"))) void * PR_MemMap(
    PRFileMap *fmap,
    PROffset64 offset,
    PRUint32 len);
extern __attribute__((visibility("default"))) PRStatus PR_MemUnmap(void *addr, PRUint32 len);
extern __attribute__((visibility("default"))) PRStatus PR_CloseFileMap(PRFileMap *fmap);
extern __attribute__((visibility("default"))) PRStatus PR_CreatePipe(
    PRFileDesc **readPipe,
    PRFileDesc **writePipe
);
struct PRPollDesc {
    PRFileDesc* fd;
    PRInt16 in_flags;
    PRInt16 out_flags;
};
extern __attribute__((visibility("default"))) PRInt32 PR_Poll(
    PRPollDesc *pds, PRIntn npds, PRIntervalTime timeout);
extern __attribute__((visibility("default"))) PRFileDesc * PR_NewPollableEvent(void);
extern __attribute__((visibility("default"))) PRStatus PR_DestroyPollableEvent(PRFileDesc *event);
extern __attribute__((visibility("default"))) PRStatus PR_SetPollableEvent(PRFileDesc *event);
extern __attribute__((visibility("default"))) PRStatus PR_WaitForPollableEvent(PRFileDesc *event);
extern __attribute__((visibility("default"))) PRUint32 PR_snprintf(char *out, PRUint32 outlen, const char *fmt, ...);
extern __attribute__((visibility("default"))) char* PR_smprintf(const char *fmt, ...);
extern __attribute__((visibility("default"))) void PR_smprintf_free(char *mem);
extern __attribute__((visibility("default"))) char* PR_sprintf_append(char *last, const char *fmt, ...);
typedef PRIntn (*PRStuffFunc)(void *arg, const char *s, PRUint32 slen);
extern __attribute__((visibility("default"))) PRUint32 PR_sxprintf(PRStuffFunc f, void *arg, const char *fmt, ...);
extern __attribute__((visibility("default"))) PRUint32 PR_fprintf(struct PRFileDesc* fd, const char *fmt, ...);
extern __attribute__((visibility("default"))) PRUint32 PR_vsnprintf(char *out, PRUint32 outlen, const char *fmt, va_list ap);
extern __attribute__((visibility("default"))) char* PR_vsmprintf(const char *fmt, va_list ap);
extern __attribute__((visibility("default"))) char* PR_vsprintf_append(char *last, const char *fmt, va_list ap);
extern __attribute__((visibility("default"))) PRUint32 PR_vsxprintf(PRStuffFunc f, void *arg, const char *fmt, va_list ap);
extern __attribute__((visibility("default"))) PRUint32 PR_vfprintf(struct PRFileDesc* fd, const char *fmt, va_list ap);
extern __attribute__((visibility("default"))) PRInt32 PR_sscanf(const char *buf, const char *fmt, ...);
typedef enum PRLogModuleLevel {
    PR_LOG_NONE = 0,
    PR_LOG_ALWAYS = 1,
    PR_LOG_ERROR = 2,
    PR_LOG_WARNING = 3,
    PR_LOG_DEBUG = 4,
    PR_LOG_NOTICE = PR_LOG_DEBUG,
    PR_LOG_WARN = PR_LOG_WARNING,
    PR_LOG_MIN = PR_LOG_DEBUG,
    PR_LOG_MAX = PR_LOG_DEBUG
} PRLogModuleLevel;
typedef struct PRLogModuleInfo {
    const char *name;
    PRLogModuleLevel level;
    struct PRLogModuleInfo *next;
} PRLogModuleInfo;
extern __attribute__((visibility("default"))) PRLogModuleInfo* PR_NewLogModule(const char *name);
extern __attribute__((visibility("default"))) PRBool PR_SetLogFile(const char *name);
extern __attribute__((visibility("default"))) void PR_SetLogBuffering(PRIntn buffer_size);
extern __attribute__((visibility("default"))) void PR_LogPrint(const char *fmt, ...);
extern __attribute__((visibility("default"))) void PR_LogFlush(void);
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
extern void *rawmemchr (__const void *__s, int __c)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern void *memrchr (__const void *__s, int __c, size_t __n)
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
  struct locale_data *__locales[13];
  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;
  const char *__names[13];
} *__locale_t;
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
extern char *strchrnul (__const char *__s, int __c)
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
extern char *strcasestr (__const char *__haystack, __const char *__needle)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern void *memmem (__const void *__haystack, size_t __haystacklen,
       __const void *__needle, size_t __needlelen)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 3)));
extern void *__mempcpy (void *__restrict __dest,
   __const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern void *mempcpy (void *__restrict __dest,
        __const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern size_t strlen (__const char *__s)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern size_t strnlen (__const char *__string, size_t __maxlen)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern char *strerror (int __errnum) __attribute__ ((__nothrow__));
extern char *strerror_r (int __errnum, char *__buf, size_t __buflen)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
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
extern int ffsl (long int __l) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
__extension__ extern int ffsll (long long int __ll)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int strcasecmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern int strncasecmp (__const char *__s1, __const char *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
extern int strcasecmp_l (__const char *__s1, __const char *__s2,
    __locale_t __loc)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 3)));
extern int strncasecmp_l (__const char *__s1, __const char *__s2,
     size_t __n, __locale_t __loc)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 4)));
extern char *strsep (char **__restrict __stringp,
       __const char *__restrict __delim)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int strverscmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
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
extern char *strfry (char *__string) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern void *memfrob (void *__s, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern char *basename (__const char *__filename) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
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
enum
{
  _ISupper = ((0) < 8 ? ((1 << (0)) << 8) : ((1 << (0)) >> 8)),
  _ISlower = ((1) < 8 ? ((1 << (1)) << 8) : ((1 << (1)) >> 8)),
  _ISalpha = ((2) < 8 ? ((1 << (2)) << 8) : ((1 << (2)) >> 8)),
  _ISdigit = ((3) < 8 ? ((1 << (3)) << 8) : ((1 << (3)) >> 8)),
  _ISxdigit = ((4) < 8 ? ((1 << (4)) << 8) : ((1 << (4)) >> 8)),
  _ISspace = ((5) < 8 ? ((1 << (5)) << 8) : ((1 << (5)) >> 8)),
  _ISprint = ((6) < 8 ? ((1 << (6)) << 8) : ((1 << (6)) >> 8)),
  _ISgraph = ((7) < 8 ? ((1 << (7)) << 8) : ((1 << (7)) >> 8)),
  _ISblank = ((8) < 8 ? ((1 << (8)) << 8) : ((1 << (8)) >> 8)),
  _IScntrl = ((9) < 8 ? ((1 << (9)) << 8) : ((1 << (9)) >> 8)),
  _ISpunct = ((10) < 8 ? ((1 << (10)) << 8) : ((1 << (10)) >> 8)),
  _ISalnum = ((11) < 8 ? ((1 << (11)) << 8) : ((1 << (11)) >> 8))
};
extern __const unsigned short int **__ctype_b_loc (void)
     __attribute__ ((__nothrow__)) __attribute__ ((__const));
extern __const __int32_t **__ctype_tolower_loc (void)
     __attribute__ ((__nothrow__)) __attribute__ ((__const));
extern __const __int32_t **__ctype_toupper_loc (void)
     __attribute__ ((__nothrow__)) __attribute__ ((__const));
extern int isalnum (int) __attribute__ ((__nothrow__));
extern int isalpha (int) __attribute__ ((__nothrow__));
extern int iscntrl (int) __attribute__ ((__nothrow__));
extern int isdigit (int) __attribute__ ((__nothrow__));
extern int islower (int) __attribute__ ((__nothrow__));
extern int isgraph (int) __attribute__ ((__nothrow__));
extern int isprint (int) __attribute__ ((__nothrow__));
extern int ispunct (int) __attribute__ ((__nothrow__));
extern int isspace (int) __attribute__ ((__nothrow__));
extern int isupper (int) __attribute__ ((__nothrow__));
extern int isxdigit (int) __attribute__ ((__nothrow__));
extern int tolower (int __c) __attribute__ ((__nothrow__));
extern int toupper (int __c) __attribute__ ((__nothrow__));
extern int isblank (int) __attribute__ ((__nothrow__));
extern int isctype (int __c, int __mask) __attribute__ ((__nothrow__));
extern int isascii (int __c) __attribute__ ((__nothrow__));
extern int toascii (int __c) __attribute__ ((__nothrow__));
extern int _toupper (int) __attribute__ ((__nothrow__));
extern int _tolower (int) __attribute__ ((__nothrow__));
extern __inline int
__attribute__ ((__nothrow__)) tolower (int __c)
{
  return __c >= -128 && __c < 256 ? (*__ctype_tolower_loc ())[__c] : __c;
}
extern __inline int
__attribute__ ((__nothrow__)) toupper (int __c)
{
  return __c >= -128 && __c < 256 ? (*__ctype_toupper_loc ())[__c] : __c;
}
extern int isalnum_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isalpha_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int iscntrl_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isdigit_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int islower_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isgraph_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isprint_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int ispunct_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isspace_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isupper_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isxdigit_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isblank_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int __tolower_l (int __c, __locale_t __l) __attribute__ ((__nothrow__));
extern int tolower_l (int __c, __locale_t __l) __attribute__ ((__nothrow__));
extern int __toupper_l (int __c, __locale_t __l) __attribute__ ((__nothrow__));
extern int toupper_l (int __c, __locale_t __l) __attribute__ ((__nothrow__));
extern int *__errno_location (void) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern char *program_invocation_name, *program_invocation_short_name;
typedef int error_t;
struct tm
{
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;
  long int tm_gmtoff;
  __const char *tm_zone;
};
struct itimerspec
  {
    struct timespec it_interval;
    struct timespec it_value;
  };
struct sigevent;
extern clock_t clock (void) __attribute__ ((__nothrow__));
extern time_t time (time_t *__timer) __attribute__ ((__nothrow__));
extern double difftime (time_t __time1, time_t __time0)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern time_t mktime (struct tm *__tp) __attribute__ ((__nothrow__));
extern size_t strftime (char *__restrict __s, size_t __maxsize,
   __const char *__restrict __format,
   __const struct tm *__restrict __tp) __attribute__ ((__nothrow__));
extern char *strptime (__const char *__restrict __s,
         __const char *__restrict __fmt, struct tm *__tp)
     __attribute__ ((__nothrow__));
extern size_t strftime_l (char *__restrict __s, size_t __maxsize,
     __const char *__restrict __format,
     __const struct tm *__restrict __tp,
     __locale_t __loc) __attribute__ ((__nothrow__));
extern char *strptime_l (__const char *__restrict __s,
    __const char *__restrict __fmt, struct tm *__tp,
    __locale_t __loc) __attribute__ ((__nothrow__));
extern struct tm *gmtime (__const time_t *__timer) __attribute__ ((__nothrow__));
extern struct tm *localtime (__const time_t *__timer) __attribute__ ((__nothrow__));
extern struct tm *gmtime_r (__const time_t *__restrict __timer,
       struct tm *__restrict __tp) __attribute__ ((__nothrow__));
extern struct tm *localtime_r (__const time_t *__restrict __timer,
          struct tm *__restrict __tp) __attribute__ ((__nothrow__));
extern char *asctime (__const struct tm *__tp) __attribute__ ((__nothrow__));
extern char *ctime (__const time_t *__timer) __attribute__ ((__nothrow__));
extern char *asctime_r (__const struct tm *__restrict __tp,
   char *__restrict __buf) __attribute__ ((__nothrow__));
extern char *ctime_r (__const time_t *__restrict __timer,
        char *__restrict __buf) __attribute__ ((__nothrow__));
extern char *__tzname[2];
extern int __daylight;
extern long int __timezone;
extern char *tzname[2];
extern void tzset (void) __attribute__ ((__nothrow__));
extern int daylight;
extern long int timezone;
extern int stime (__const time_t *__when) __attribute__ ((__nothrow__));
extern time_t timegm (struct tm *__tp) __attribute__ ((__nothrow__));
extern time_t timelocal (struct tm *__tp) __attribute__ ((__nothrow__));
extern int dysize (int __year) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
extern int nanosleep (__const struct timespec *__requested_time,
        struct timespec *__remaining);
extern int clock_getres (clockid_t __clock_id, struct timespec *__res) __attribute__ ((__nothrow__));
extern int clock_gettime (clockid_t __clock_id, struct timespec *__tp) __attribute__ ((__nothrow__));
extern int clock_settime (clockid_t __clock_id, __const struct timespec *__tp)
     __attribute__ ((__nothrow__));
extern int clock_nanosleep (clockid_t __clock_id, int __flags,
       __const struct timespec *__req,
       struct timespec *__rem);
extern int clock_getcpuclockid (pid_t __pid, clockid_t *__clock_id) __attribute__ ((__nothrow__));
extern int timer_create (clockid_t __clock_id,
    struct sigevent *__restrict __evp,
    timer_t *__restrict __timerid) __attribute__ ((__nothrow__));
extern int timer_delete (timer_t __timerid) __attribute__ ((__nothrow__));
extern int timer_settime (timer_t __timerid, int __flags,
     __const struct itimerspec *__restrict __value,
     struct itimerspec *__restrict __ovalue) __attribute__ ((__nothrow__));
extern int timer_gettime (timer_t __timerid, struct itimerspec *__value)
     __attribute__ ((__nothrow__));
extern int timer_getoverrun (timer_t __timerid) __attribute__ ((__nothrow__));
extern int getdate_err;
extern struct tm *getdate (__const char *__string);
extern int getdate_r (__const char *__restrict __string,
        struct tm *__restrict __resbufp);
static const int lastDayOfMonth[2][13] = {
    {-1, 30, 58, 89, 119, 150, 180, 211, 242, 272, 303, 333, 364},
    {-1, 30, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365}
};
static const PRInt8 nDays[2][12] = {
    {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
    {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
};
static void ComputeGMT(PRTime time, PRExplodedTime *gmt);
static int IsLeapYear(PRInt16 year);
static void ApplySecOffset(PRExplodedTime *time, PRInt32 secOffset);
static void
ComputeGMT(PRTime time, PRExplodedTime *gmt)
{
    PRInt32 tmp, rem;
    PRInt32 numDays;
    PRInt64 numDays64, rem64;
    int isLeap;
    PRInt64 sec;
    PRInt64 usec;
    PRInt64 usecPerSec;
    PRInt64 secPerDay;
    ((usecPerSec) = (PRInt64)(1000000L));
    ((sec) = (time) / (usecPerSec));
    ((usec) = (time) % (usecPerSec));
    ((gmt->tm_usec) = (PRInt32)(usec));
    if (gmt->tm_usec < 0) {
        PRInt64 one;
        ((one) = (PRInt64)(1L));
        ((sec) = (sec) - (one));
        gmt->tm_usec += 1000000L;
    }
    ((secPerDay) = (PRInt64)(86400L));
    ((numDays64) = (sec) / (secPerDay));
    ((rem64) = (sec) % (secPerDay));
    ((numDays) = (PRInt32)(numDays64));
    ((rem) = (PRInt32)(rem64));
    if (rem < 0) {
        numDays--;
        rem += 86400L;
    }
    gmt->tm_wday = (numDays + 4) % 7;
    if (gmt->tm_wday < 0) {
        gmt->tm_wday += 7;
    }
    gmt->tm_hour = rem / 3600;
    rem %= 3600;
    gmt->tm_min = rem / 60;
    gmt->tm_sec = rem % 60;
    tmp = numDays / (4 * 365 + 1);
    rem = numDays % (4 * 365 + 1);
    if (rem < 0) {
        tmp--;
        rem += (4 * 365 + 1);
    }
    tmp = (tmp * 4) + 1970;
    isLeap = 0;
    if (rem >= 365) {
        tmp++;
        rem -= 365;
        if (rem >= 365) {
            tmp++;
            rem -= 365;
            if (rem >= 366) {
                tmp++;
                rem -= 366;
            } else {
                isLeap = 1;
            }
        }
    }
    gmt->tm_year = tmp;
    gmt->tm_yday = rem;
    for (tmp = 1; lastDayOfMonth[isLeap][tmp] < gmt->tm_yday; tmp++) {
    }
    gmt->tm_month = --tmp;
    gmt->tm_mday = gmt->tm_yday - lastDayOfMonth[isLeap][tmp];
    gmt->tm_params.tp_gmt_offset = 0;
    gmt->tm_params.tp_dst_offset = 0;
}
__attribute__((visibility("default"))) void
PR_ExplodeTime(
        PRTime usecs,
        PRTimeParamFn params,
        PRExplodedTime *exploded)
{
    ComputeGMT(usecs, exploded);
    exploded->tm_params = params(exploded);
    ApplySecOffset(exploded, exploded->tm_params.tp_gmt_offset
            + exploded->tm_params.tp_dst_offset);
}
__attribute__((visibility("default"))) PRTime
PR_ImplodeTime(const PRExplodedTime *exploded)
{
    PRExplodedTime copy;
    PRTime retVal;
    PRInt64 secPerDay, usecPerSec;
    PRInt64 temp;
    PRInt64 numSecs64;
    PRInt32 numDays;
    PRInt32 numSecs;
    copy = *exploded;
    PR_NormalizeTime(&copy, PR_GMTParameters);
    numDays = (( ((copy.tm_year)-1)*365 + ( ((copy.tm_year)-1)/4 - ((copy.tm_year)-1)/100 + ((copy.tm_year)-1)/400 ) ) - ( ((1970)-1)*365 + ( ((1970)-1)/4 - ((1970)-1)/100 + ((1970)-1)/400 ) ));
    numSecs = copy.tm_yday * 86400 + copy.tm_hour * 3600
            + copy.tm_min * 60 + copy.tm_sec;
    ((temp) = (PRInt64)(numDays));
    ((secPerDay) = (PRInt64)(86400));
    ((temp) = (temp) * (secPerDay));
    ((numSecs64) = (PRInt64)(numSecs));
    ((numSecs64) = (numSecs64) + (temp));
    ((temp) = (PRInt64)(copy.tm_params.tp_gmt_offset));
    ((numSecs64) = (numSecs64) - (temp));
    ((temp) = (PRInt64)(copy.tm_params.tp_dst_offset));
    ((numSecs64) = (numSecs64) - (temp));
    ((usecPerSec) = (PRInt64)(1000000L));
    ((temp) = (numSecs64) * (usecPerSec));
    ((retVal) = (PRInt64)(copy.tm_usec));
    ((retVal) = (retVal) + (temp));
    return retVal;
}
static int IsLeapYear(PRInt16 year)
{
    if ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0)
        return 1;
    else
        return 0;
}
static void
ApplySecOffset(PRExplodedTime *time, PRInt32 secOffset)
{
    time->tm_sec += secOffset;
    if (time->tm_sec < 0 || time->tm_sec >= 60) {
        time->tm_min += time->tm_sec / 60;
        time->tm_sec %= 60;
        if (time->tm_sec < 0) {
            time->tm_sec += 60;
            time->tm_min--;
        }
    }
    if (time->tm_min < 0 || time->tm_min >= 60) {
        time->tm_hour += time->tm_min / 60;
        time->tm_min %= 60;
        if (time->tm_min < 0) {
            time->tm_min += 60;
            time->tm_hour--;
        }
    }
    if (time->tm_hour < 0) {
        time->tm_hour += 24;
        time->tm_mday--;
        time->tm_yday--;
        if (time->tm_mday < 1) {
            time->tm_month--;
            if (time->tm_month < 0) {
                time->tm_month = 11;
                time->tm_year--;
                if (IsLeapYear(time->tm_year))
                    time->tm_yday = 365;
                else
                    time->tm_yday = 364;
            }
            time->tm_mday = nDays[IsLeapYear(time->tm_year)][time->tm_month];
        }
        time->tm_wday--;
        if (time->tm_wday < 0)
            time->tm_wday = 6;
    } else if (time->tm_hour > 23) {
        time->tm_hour -= 24;
        time->tm_mday++;
        time->tm_yday++;
        if (time->tm_mday >
                nDays[IsLeapYear(time->tm_year)][time->tm_month]) {
            time->tm_mday = 1;
            time->tm_month++;
            if (time->tm_month > 11) {
                time->tm_month = 0;
                time->tm_year++;
                time->tm_yday = 0;
            }
        }
        time->tm_wday++;
        if (time->tm_wday > 6)
            time->tm_wday = 0;
    }
}
__attribute__((visibility("default"))) void
PR_NormalizeTime(PRExplodedTime *time, PRTimeParamFn params)
{
    int daysInMonth;
    PRInt32 numDays;
    time->tm_sec -= time->tm_params.tp_gmt_offset
            + time->tm_params.tp_dst_offset;
    time->tm_params.tp_gmt_offset = 0;
    time->tm_params.tp_dst_offset = 0;
    if (time->tm_usec < 0 || time->tm_usec >= 1000000) {
        time->tm_sec += time->tm_usec / 1000000;
        time->tm_usec %= 1000000;
        if (time->tm_usec < 0) {
            time->tm_usec += 1000000;
            time->tm_sec--;
        }
    }
    if (time->tm_sec < 0 || time->tm_sec >= 60) {
        time->tm_min += time->tm_sec / 60;
        time->tm_sec %= 60;
        if (time->tm_sec < 0) {
            time->tm_sec += 60;
            time->tm_min--;
        }
    }
    if (time->tm_min < 0 || time->tm_min >= 60) {
        time->tm_hour += time->tm_min / 60;
        time->tm_min %= 60;
        if (time->tm_min < 0) {
            time->tm_min += 60;
            time->tm_hour--;
        }
    }
    if (time->tm_hour < 0 || time->tm_hour >= 24) {
        time->tm_mday += time->tm_hour / 24;
        time->tm_hour %= 24;
        if (time->tm_hour < 0) {
            time->tm_hour += 24;
            time->tm_mday--;
        }
    }
    if (time->tm_month < 0 || time->tm_month >= 12) {
        time->tm_year += time->tm_month / 12;
        time->tm_month %= 12;
        if (time->tm_month < 0) {
            time->tm_month += 12;
            time->tm_year--;
        }
    }
    if (time->tm_mday < 1) {
        do {
            time->tm_month--;
            if (time->tm_month < 0) {
                time->tm_month = 11;
                time->tm_year--;
            }
            time->tm_mday += nDays[IsLeapYear(time->tm_year)][time->tm_month];
        } while (time->tm_mday < 1);
    } else {
        daysInMonth = nDays[IsLeapYear(time->tm_year)][time->tm_month];
        while (time->tm_mday > daysInMonth) {
            time->tm_mday -= daysInMonth;
            time->tm_month++;
            if (time->tm_month > 11) {
                time->tm_month = 0;
                time->tm_year++;
            }
            daysInMonth = nDays[IsLeapYear(time->tm_year)][time->tm_month];
        }
    }
    time->tm_yday = time->tm_mday +
            lastDayOfMonth[IsLeapYear(time->tm_year)][time->tm_month];
    numDays = (( ((time->tm_year)-1)*365 + ( ((time->tm_year)-1)/4 - ((time->tm_year)-1)/100 + ((time->tm_year)-1)/400 ) ) - ( ((1970)-1)*365 + ( ((1970)-1)/4 - ((1970)-1)/100 + ((1970)-1)/400 ) )) + time->tm_yday;
    time->tm_wday = (numDays + 4) % 7;
    if (time->tm_wday < 0) {
        time->tm_wday += 7;
    }
    time->tm_params = params(time);
    ApplySecOffset(time, time->tm_params.tp_gmt_offset
            + time->tm_params.tp_dst_offset);
}
static PRLock *monitor = ((void *)0);
static struct tm *MT_safe_localtime(const time_t *clock, struct tm *result)
{
    struct tm *tmPtr;
    int needLock = PR_Initialized();
    if (needLock) PR_Lock(monitor);
    tmPtr = localtime(clock);
    if (tmPtr) {
        *result = *tmPtr;
    } else {
        result = ((void *)0);
    }
    if (needLock) PR_Unlock(monitor);
    return result;
}
void _PR_InitTime(void)
{
    monitor = PR_NewLock();
}
void _PR_CleanupTime(void)
{
    if (monitor) {
        PR_DestroyLock(monitor);
        monitor = ((void *)0);
    }
}
__attribute__((visibility("default"))) PRTimeParameters
PR_LocalTimeParameters(const PRExplodedTime *gmt)
{
    PRTimeParameters retVal;
    struct tm localTime;
    time_t secs;
    PRTime secs64;
    PRInt64 usecPerSec;
    PRInt64 usecPerSec_1;
    PRInt64 maxInt32;
    PRInt64 minInt32;
    PRInt32 dayOffset;
    PRInt32 offset2Jan1970;
    PRInt32 offsetNew;
    int isdst2Jan1970;
    secs = 86400L;
    (void) MT_safe_localtime(&secs, &localTime);
    offset2Jan1970 = (PRInt32)localTime.tm_sec
            + 60L * (PRInt32)localTime.tm_min
            + 3600L * (PRInt32)localTime.tm_hour
            + 86400L * (PRInt32)((PRInt32)localTime.tm_mday - 2L);
    isdst2Jan1970 = localTime.tm_isdst;
    secs64 = PR_ImplodeTime(gmt);
    ((usecPerSec) = (PRInt64)(1000000UL));
    ((usecPerSec_1) = (PRInt64)(1000000UL - 1));
    if (((secs64) >= 0)) {
        ((secs64) = (secs64) / (usecPerSec));
    } else {
        ((secs64) = -(secs64));
        ((secs64) = (secs64) + (usecPerSec_1));
        ((secs64) = (secs64) / (usecPerSec));
        ((secs64) = -(secs64));
    }
    ((maxInt32) = (PRInt64)(2147483647));
    ((minInt32) = (PRInt64)((-2147483647 - 1)));
    if (((PRInt64)(secs64) > (PRInt64)(maxInt32)) || ((PRInt64)(secs64) < (PRInt64)(minInt32))) {
        retVal.tp_gmt_offset = offset2Jan1970;
        retVal.tp_dst_offset = 0;
        return retVal;
    }
    ((secs) = (PRInt32)(secs64));
    if (MT_safe_localtime(&secs, &localTime) == ((void *)0)) {
        retVal.tp_gmt_offset = offset2Jan1970;
        retVal.tp_dst_offset = 0;
        return retVal;
    }
    dayOffset = (PRInt32) localTime.tm_wday - gmt->tm_wday;
    if (dayOffset == -6) {
        dayOffset = 1;
    } else if (dayOffset == 6) {
        dayOffset = -1;
    }
    offsetNew = (PRInt32)localTime.tm_sec - gmt->tm_sec
            + 60L * ((PRInt32)localTime.tm_min - gmt->tm_min)
            + 3600L * ((PRInt32)localTime.tm_hour - gmt->tm_hour)
            + 86400L * (PRInt32)dayOffset;
    if (localTime.tm_isdst <= 0) {
        retVal.tp_gmt_offset = offsetNew;
        retVal.tp_dst_offset = 0;
    } else {
        if (isdst2Jan1970 <=0) {
            retVal.tp_gmt_offset = offset2Jan1970;
            retVal.tp_dst_offset = offsetNew - offset2Jan1970;
        } else {
            retVal.tp_gmt_offset = offsetNew - 3600;
            retVal.tp_dst_offset = 3600;
        }
    }
    return retVal;
}
static PRInt32
NthSunday(PRInt32 mday, PRInt32 wday, PRInt32 N, PRInt32 ndays)
{
    PRInt32 firstSun = (((mday - wday + 7 - 1) % 7) + 1);
    if (N < 0)
        N = (ndays - firstSun) / 7;
    return firstSun + (7 * N);
}
typedef struct DSTParams {
    PRInt8 dst_start_month;
    PRInt8 dst_start_Nth_Sunday;
    PRInt8 dst_start_month_ndays;
    PRInt8 dst_end_month;
    PRInt8 dst_end_Nth_Sunday;
    PRInt8 dst_end_month_ndays;
} DSTParams;
static const DSTParams dstParams[2] = {
    { 3, 0, 30, 9, -1, 31 },
    { 2, 1, 31, 10, 0, 30 }
};
__attribute__((visibility("default"))) PRTimeParameters
PR_USPacificTimeParameters(const PRExplodedTime *gmt)
{
    const DSTParams *dst;
    PRTimeParameters retVal;
    PRExplodedTime st;
    retVal.tp_gmt_offset = -8L * 3600L;
    st.tm_usec = gmt->tm_usec;
    st.tm_sec = gmt->tm_sec;
    st.tm_min = gmt->tm_min;
    st.tm_hour = gmt->tm_hour;
    st.tm_mday = gmt->tm_mday;
    st.tm_month = gmt->tm_month;
    st.tm_year = gmt->tm_year;
    st.tm_wday = gmt->tm_wday;
    st.tm_yday = gmt->tm_yday;
    ApplySecOffset(&st, retVal.tp_gmt_offset);
    if (st.tm_year < 2007) {
 dst = &dstParams[0];
    } else {
 dst = &dstParams[1];
    }
    if (st.tm_month < dst->dst_start_month) {
        retVal.tp_dst_offset = 0L;
    } else if (st.tm_month == dst->dst_start_month) {
 int NthSun = NthSunday(st.tm_mday, st.tm_wday,
          dst->dst_start_Nth_Sunday,
          dst->dst_start_month_ndays);
 if (st.tm_mday < NthSun) {
     retVal.tp_dst_offset = 0L;
        } else if (st.tm_mday == NthSun) {
     if (st.tm_hour < 2) {
  retVal.tp_dst_offset = 0L;
     } else {
  retVal.tp_dst_offset = 3600L;
     }
 } else {
     retVal.tp_dst_offset = 3600L;
        }
    } else if (st.tm_month < dst->dst_end_month) {
        retVal.tp_dst_offset = 3600L;
    } else if (st.tm_month == dst->dst_end_month) {
 int NthSun = NthSunday(st.tm_mday, st.tm_wday,
          dst->dst_end_Nth_Sunday,
          dst->dst_end_month_ndays);
 if (st.tm_mday < NthSun) {
     retVal.tp_dst_offset = 3600L;
        } else if (st.tm_mday == NthSun) {
     if (st.tm_hour < 1) {
  retVal.tp_dst_offset = 3600L;
     } else {
  retVal.tp_dst_offset = 0L;
     }
 } else {
     retVal.tp_dst_offset = 0L;
        }
    } else {
        retVal.tp_dst_offset = 0L;
    }
    return retVal;
}
__attribute__((visibility("default"))) PRTimeParameters
PR_GMTParameters(const PRExplodedTime *gmt)
{
    PRTimeParameters retVal = { 0, 0 };
    return retVal;
}
typedef enum
{
  TT_UNKNOWN,
  TT_SUN, TT_MON, TT_TUE, TT_WED, TT_THU, TT_FRI, TT_SAT,
  TT_JAN, TT_FEB, TT_MAR, TT_APR, TT_MAY, TT_JUN,
  TT_JUL, TT_AUG, TT_SEP, TT_OCT, TT_NOV, TT_DEC,
  TT_PST, TT_PDT, TT_MST, TT_MDT, TT_CST, TT_CDT, TT_EST, TT_EDT,
  TT_AST, TT_NST, TT_GMT, TT_BST, TT_MET, TT_EET, TT_JST
} TIME_TOKEN;
__attribute__((visibility("default"))) PRStatus
PR_ParseTimeStringToExplodedTime(
        const char *string,
        PRBool default_to_gmt,
        PRExplodedTime *result)
{
  TIME_TOKEN dotw = TT_UNKNOWN;
  TIME_TOKEN month = TT_UNKNOWN;
  TIME_TOKEN zone = TT_UNKNOWN;
  int zone_offset = -1;
  int dst_offset = 0;
  int date = -1;
  PRInt32 year = -1;
  int hour = -1;
  int min = -1;
  int sec = -1;
  const char *rest = string;
  int iterations = 0;
  ((void) 0);
  if (!string || !result) return PR_FAILURE;
  while (*rest)
        {
          if (iterations++ > 1000)
                {
                  return PR_FAILURE;
                }
          switch (*rest)
                {
                case 'a': case 'A':
                  if (month == TT_UNKNOWN &&
                          (rest[1] == 'p' || rest[1] == 'P') &&
                          (rest[2] == 'r' || rest[2] == 'R'))
                        month = TT_APR;
                  else if (zone == TT_UNKNOWN &&
                                   (rest[1] == 's' || rest[1] == 'S') &&
                                   (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_AST;
                  else if (month == TT_UNKNOWN &&
                                   (rest[1] == 'u' || rest[1] == 'U') &&
                                   (rest[2] == 'g' || rest[2] == 'G'))
                        month = TT_AUG;
                  break;
                case 'b': case 'B':
                  if (zone == TT_UNKNOWN &&
                          (rest[1] == 's' || rest[1] == 'S') &&
                          (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_BST;
                  break;
                case 'c': case 'C':
                  if (zone == TT_UNKNOWN &&
                          (rest[1] == 'd' || rest[1] == 'D') &&
                          (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_CDT;
                  else if (zone == TT_UNKNOWN &&
                                   (rest[1] == 's' || rest[1] == 'S') &&
                                   (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_CST;
                  break;
                case 'd': case 'D':
                  if (month == TT_UNKNOWN &&
                          (rest[1] == 'e' || rest[1] == 'E') &&
                          (rest[2] == 'c' || rest[2] == 'C'))
                        month = TT_DEC;
                  break;
                case 'e': case 'E':
                  if (zone == TT_UNKNOWN &&
                          (rest[1] == 'd' || rest[1] == 'D') &&
                          (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_EDT;
                  else if (zone == TT_UNKNOWN &&
                                   (rest[1] == 'e' || rest[1] == 'E') &&
                                   (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_EET;
                  else if (zone == TT_UNKNOWN &&
                                   (rest[1] == 's' || rest[1] == 'S') &&
                                   (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_EST;
                  break;
                case 'f': case 'F':
                  if (month == TT_UNKNOWN &&
                          (rest[1] == 'e' || rest[1] == 'E') &&
                          (rest[2] == 'b' || rest[2] == 'B'))
                        month = TT_FEB;
                  else if (dotw == TT_UNKNOWN &&
                                   (rest[1] == 'r' || rest[1] == 'R') &&
                                   (rest[2] == 'i' || rest[2] == 'I'))
                        dotw = TT_FRI;
                  break;
                case 'g': case 'G':
                  if (zone == TT_UNKNOWN &&
                          (rest[1] == 'm' || rest[1] == 'M') &&
                          (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_GMT;
                  break;
                case 'j': case 'J':
                  if (month == TT_UNKNOWN &&
                          (rest[1] == 'a' || rest[1] == 'A') &&
                          (rest[2] == 'n' || rest[2] == 'N'))
                        month = TT_JAN;
                  else if (zone == TT_UNKNOWN &&
                                   (rest[1] == 's' || rest[1] == 'S') &&
                                   (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_JST;
                  else if (month == TT_UNKNOWN &&
                                   (rest[1] == 'u' || rest[1] == 'U') &&
                                   (rest[2] == 'l' || rest[2] == 'L'))
                        month = TT_JUL;
                  else if (month == TT_UNKNOWN &&
                                   (rest[1] == 'u' || rest[1] == 'U') &&
                                   (rest[2] == 'n' || rest[2] == 'N'))
                        month = TT_JUN;
                  break;
                case 'm': case 'M':
                  if (month == TT_UNKNOWN &&
                          (rest[1] == 'a' || rest[1] == 'A') &&
                          (rest[2] == 'r' || rest[2] == 'R'))
                        month = TT_MAR;
                  else if (month == TT_UNKNOWN &&
                                   (rest[1] == 'a' || rest[1] == 'A') &&
                                   (rest[2] == 'y' || rest[2] == 'Y'))
                        month = TT_MAY;
                  else if (zone == TT_UNKNOWN &&
                                   (rest[1] == 'd' || rest[1] == 'D') &&
                                   (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_MDT;
                  else if (zone == TT_UNKNOWN &&
                                   (rest[1] == 'e' || rest[1] == 'E') &&
                                   (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_MET;
                  else if (dotw == TT_UNKNOWN &&
                                   (rest[1] == 'o' || rest[1] == 'O') &&
                                   (rest[2] == 'n' || rest[2] == 'N'))
                        dotw = TT_MON;
                  else if (zone == TT_UNKNOWN &&
                                   (rest[1] == 's' || rest[1] == 'S') &&
                                   (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_MST;
                  break;
                case 'n': case 'N':
                  if (month == TT_UNKNOWN &&
                          (rest[1] == 'o' || rest[1] == 'O') &&
                          (rest[2] == 'v' || rest[2] == 'V'))
                        month = TT_NOV;
                  else if (zone == TT_UNKNOWN &&
                                   (rest[1] == 's' || rest[1] == 'S') &&
                                   (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_NST;
                  break;
                case 'o': case 'O':
                  if (month == TT_UNKNOWN &&
                          (rest[1] == 'c' || rest[1] == 'C') &&
                          (rest[2] == 't' || rest[2] == 'T'))
                        month = TT_OCT;
                  break;
                case 'p': case 'P':
                  if (zone == TT_UNKNOWN &&
                          (rest[1] == 'd' || rest[1] == 'D') &&
                          (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_PDT;
                  else if (zone == TT_UNKNOWN &&
                                   (rest[1] == 's' || rest[1] == 'S') &&
                                   (rest[2] == 't' || rest[2] == 'T'))
                        zone = TT_PST;
                  break;
                case 's': case 'S':
                  if (dotw == TT_UNKNOWN &&
                          (rest[1] == 'a' || rest[1] == 'A') &&
                          (rest[2] == 't' || rest[2] == 'T'))
                        dotw = TT_SAT;
                  else if (month == TT_UNKNOWN &&
                                   (rest[1] == 'e' || rest[1] == 'E') &&
                                   (rest[2] == 'p' || rest[2] == 'P'))
                        month = TT_SEP;
                  else if (dotw == TT_UNKNOWN &&
                                   (rest[1] == 'u' || rest[1] == 'U') &&
                                   (rest[2] == 'n' || rest[2] == 'N'))
                        dotw = TT_SUN;
                  break;
                case 't': case 'T':
                  if (dotw == TT_UNKNOWN &&
                          (rest[1] == 'h' || rest[1] == 'H') &&
                          (rest[2] == 'u' || rest[2] == 'U'))
                        dotw = TT_THU;
                  else if (dotw == TT_UNKNOWN &&
                                   (rest[1] == 'u' || rest[1] == 'U') &&
                                   (rest[2] == 'e' || rest[2] == 'E'))
                        dotw = TT_TUE;
                  break;
                case 'u': case 'U':
                  if (zone == TT_UNKNOWN &&
                          (rest[1] == 't' || rest[1] == 'T') &&
                          !(rest[2] >= 'A' && rest[2] <= 'Z') &&
                          !(rest[2] >= 'a' && rest[2] <= 'z'))
                        zone = TT_GMT;
                  break;
                case 'w': case 'W':
                  if (dotw == TT_UNKNOWN &&
                          (rest[1] == 'e' || rest[1] == 'E') &&
                          (rest[2] == 'd' || rest[2] == 'D'))
                        dotw = TT_WED;
                  break;
                case '+': case '-':
                  {
                        const char *end;
                        int sign;
                        if (zone_offset != -1)
                          {
                                rest++;
                                break;
                          }
                        if (zone != TT_UNKNOWN && zone != TT_GMT)
                          {
                                rest++;
                                break;
                          }
                        sign = ((*rest == '+') ? 1 : -1);
                        rest++;
                        end = rest;
                        while (*end >= '0' && *end <= '9')
                          end++;
                        if (rest == end)
                          break;
                        if ((end - rest) == 4)
                          zone_offset = (((((rest[0]-'0')*10) + (rest[1]-'0')) * 60) +
                                                         (((rest[2]-'0')*10) + (rest[3]-'0')));
                        else if ((end - rest) == 2)
                          zone_offset = (((rest[0]-'0')*10) + (rest[1]-'0')) * 60;
                        else if ((end - rest) == 1)
                          zone_offset = (rest[0]-'0') * 60;
                        else
                          break;
                        zone_offset *= sign;
                        zone = TT_GMT;
                        break;
                  }
                case '0': case '1': case '2': case '3': case '4':
                case '5': case '6': case '7': case '8': case '9':
                  {
                        int tmp_hour = -1;
                        int tmp_min = -1;
                        int tmp_sec = -1;
                        const char *end = rest + 1;
                        while (*end >= '0' && *end <= '9')
                          end++;
                        if (*end == ':')
                          {
                                if (hour >= 0 && min >= 0)
                                  break;
                                if ((end - rest) > 2)
                                  break;
                                else if ((end - rest) == 2)
                                  tmp_hour = ((rest[0]-'0')*10 +
                                                          (rest[1]-'0'));
                                else
                                  tmp_hour = (rest[0]-'0');
                                rest = ++end;
                                while (*end >= '0' && *end <= '9')
                                  end++;
                                if (end == rest)
                                  break;
                                else if ((end - rest) > 2)
                                  break;
                                else if ((end - rest) == 2)
                                  tmp_min = ((rest[0]-'0')*10 +
                                                         (rest[1]-'0'));
                                else
                                  tmp_min = (rest[0]-'0');
                                rest = end;
                                if (*rest == ':')
                                  rest++;
                                end = rest;
                                while (*end >= '0' && *end <= '9')
                                  end++;
                                if (end == rest)
                                  ;
                                else if ((end - rest) > 2)
                                  break;
                                else if ((end - rest) == 2)
                                  tmp_sec = ((rest[0]-'0')*10 +
                                                         (rest[1]-'0'));
                                else
                                  tmp_sec = (rest[0]-'0');
                                if (tmp_hour <= 12)
                                  {
                                        const char *s = end;
                                        while (*s && (*s == ' ' || *s == '\t'))
                                          s++;
                                        if ((s[0] == 'p' || s[0] == 'P') &&
                                                (s[1] == 'm' || s[1] == 'M'))
                                          tmp_hour = (tmp_hour == 12 ? 12 : tmp_hour + 12);
                                        else if (tmp_hour == 12 &&
                                                         (s[0] == 'a' || s[0] == 'A') &&
                                                         (s[1] == 'm' || s[1] == 'M'))
                                          tmp_hour = 0;
                                  }
                                hour = tmp_hour;
                                min = tmp_min;
                                sec = tmp_sec;
                                rest = end;
                                break;
                          }
                        else if ((*end == '/' || *end == '-') &&
                                         end[1] >= '0' && end[1] <= '9')
                          {
                                int n1, n2, n3;
                                const char *s;
                                if (month != TT_UNKNOWN)
                                  break;
                                s = rest;
                                n1 = (*s++ - '0');
                                if (*s >= '0' && *s <= '9')
                                  n1 = n1*10 + (*s++ - '0');
                                if (*s != '/' && *s != '-')
                                  break;
                                s++;
                                if (*s < '0' || *s > '9')
                                  break;
                                n2 = (*s++ - '0');
                                if (*s >= '0' && *s <= '9')
                                  n2 = n2*10 + (*s++ - '0');
                                if (*s != '/' && *s != '-')
                                  break;
                                s++;
                                if (*s < '0' || *s > '9')
                                  break;
                                n3 = (*s++ - '0');
                                if (*s >= '0' && *s <= '9')
                                  n3 = n3*10 + (*s++ - '0');
                                if (*s >= '0' && *s <= '9')
                                  {
                                        n3 = n3*10 + (*s++ - '0');
                                        if (*s < '0' || *s > '9')
                                          break;
                                        n3 = n3*10 + (*s++ - '0');
                                        if (*s >= '0' && *s <= '9')
                                          n3 = n3*10 + (*s++ - '0');
                                  }
                                if ((*s >= '0' && *s <= '9') ||
                                        (*s >= 'A' && *s <= 'Z') ||
                                        (*s >= 'a' && *s <= 'z'))
                                  break;
                                if (n1 > 31 || n1 == 0)
                                  {
                                        if (n2 > 12) break;
                                        if (n3 > 31) break;
                                        year = n1;
                                        if (year < 70)
                                            year += 2000;
                                        else if (year < 100)
                                            year += 1900;
                                        month = (TIME_TOKEN)(n2 + ((int)TT_JAN) - 1);
                                        date = n3;
                                        rest = s;
                                        break;
                                  }
                                if (n1 > 12 && n2 > 12)
                                  {
                                        rest = s;
                                        break;
                                  }
                                if (n3 < 70)
                                    n3 += 2000;
                                else if (n3 < 100)
                                    n3 += 1900;
                                if (n1 > 12)
                                  {
                                        date = n1;
                                        month = (TIME_TOKEN)(n2 + ((int)TT_JAN) - 1);
                                        year = n3;
                                  }
                                else
                                  {
                                        month = (TIME_TOKEN)(n1 + ((int)TT_JAN) - 1);
                                        date = n2;
                                        year = n3;
                                  }
                                rest = s;
                          }
                        else if ((*end >= 'A' && *end <= 'Z') ||
                                         (*end >= 'a' && *end <= 'z'))
                          ;
                        else if ((end - rest) == 5)
                          year = (year < 0
                                          ? ((rest[0]-'0')*10000L +
                                                 (rest[1]-'0')*1000L +
                                                 (rest[2]-'0')*100L +
                                                 (rest[3]-'0')*10L +
                                                 (rest[4]-'0'))
                                          : year);
                        else if ((end - rest) == 4)
                          year = (year < 0
                                          ? ((rest[0]-'0')*1000L +
                                                 (rest[1]-'0')*100L +
                                                 (rest[2]-'0')*10L +
                                                 (rest[3]-'0'))
                                          : year);
                        else if ((end - rest) == 2)
                          {
                                int n = ((rest[0]-'0')*10 +
                                                 (rest[1]-'0'));
                                if (date < 0 && n < 32)
                                  date = n;
                                else if (year < 0)
                                  {
                                        if (n < 70)
                                          year = 2000 + n;
                                        else if (n < 100)
                                          year = 1900 + n;
                                        else
                                          year = n;
                                  }
                          }
                        else if ((end - rest) == 1)
                          date = (date < 0 ? (rest[0]-'0') : date);
                        break;
                  }
                }
          while (*rest &&
                         *rest != ' ' && *rest != '\t' &&
                         *rest != ',' && *rest != ';' &&
                         *rest != '-' && *rest != '+' &&
                         *rest != '/' &&
                         *rest != '(' && *rest != ')' && *rest != '[' && *rest != ']')
                rest++;
        SKIP_MORE:
          while (*rest &&
                         (*rest == ' ' || *rest == '\t' ||
                          *rest == ',' || *rest == ';' || *rest == '/' ||
                          *rest == '(' || *rest == ')' || *rest == '[' || *rest == ']'))
                rest++;
          if (*rest == '-' && ((rest > string && ((*__ctype_b_loc ())[(int) ((rest[-1]))] & (unsigned short int) _ISalpha) && year < 0)
              || rest[1] < '0' || rest[1] > '9'))
                {
                  rest++;
                  goto SKIP_MORE;
                }
        }
  if (zone != TT_UNKNOWN && zone_offset == -1)
        {
          switch (zone)
                {
                case TT_PST: zone_offset = -8 * 60; break;
                case TT_PDT: zone_offset = -8 * 60; dst_offset = 1 * 60; break;
                case TT_MST: zone_offset = -7 * 60; break;
                case TT_MDT: zone_offset = -7 * 60; dst_offset = 1 * 60; break;
                case TT_CST: zone_offset = -6 * 60; break;
                case TT_CDT: zone_offset = -6 * 60; dst_offset = 1 * 60; break;
                case TT_EST: zone_offset = -5 * 60; break;
                case TT_EDT: zone_offset = -5 * 60; dst_offset = 1 * 60; break;
                case TT_AST: zone_offset = -4 * 60; break;
                case TT_NST: zone_offset = -3 * 60 - 30; break;
                case TT_GMT: zone_offset = 0 * 60; break;
                case TT_BST: zone_offset = 0 * 60; dst_offset = 1 * 60; break;
                case TT_MET: zone_offset = 1 * 60; break;
                case TT_EET: zone_offset = 2 * 60; break;
                case TT_JST: zone_offset = 9 * 60; break;
                default:
                  ((void) 0);
                  break;
                }
        }
  if (month == TT_UNKNOWN || date == -1 || year == -1 || year > 32767)
      return PR_FAILURE;
  memset(result, 0, sizeof(*result));
  if (sec != -1)
        result->tm_sec = sec;
  if (min != -1)
        result->tm_min = min;
  if (hour != -1)
        result->tm_hour = hour;
  if (date != -1)
        result->tm_mday = date;
  if (month != TT_UNKNOWN)
        result->tm_month = (((int)month) - ((int)TT_JAN));
  if (year != -1)
        result->tm_year = year;
  if (dotw != TT_UNKNOWN)
        result->tm_wday = (((int)dotw) - ((int)TT_SUN));
  PR_NormalizeTime(result, PR_GMTParameters);
  if (zone == TT_UNKNOWN && default_to_gmt)
        {
          zone = TT_GMT;
          zone_offset = 0;
        }
  if (zone_offset == -1)
         {
          struct tm localTime;
          time_t secs;
          ((void) 0);
          if(result->tm_year >= 1970)
                {
                  PRInt64 usec_per_sec;
                  localTime.tm_sec = result->tm_sec;
                  localTime.tm_min = result->tm_min;
                  localTime.tm_hour = result->tm_hour;
                  localTime.tm_mday = result->tm_mday;
                  localTime.tm_mon = result->tm_month;
                  localTime.tm_year = result->tm_year - 1900;
                  localTime.tm_isdst = -1;
                  secs = mktime(&localTime);
                  if (secs != (time_t) -1)
                    {
                      PRTime usecs64;
                      ((usecs64) = (PRInt64)(secs));
                      ((usec_per_sec) = (PRInt64)(1000000UL));
                      ((usecs64) = (usecs64) * (usec_per_sec));
                      PR_ExplodeTime(usecs64, PR_LocalTimeParameters, result);
                      return PR_SUCCESS;
                    }
                }
                secs = 86400;
                (void) MT_safe_localtime(&secs, &localTime);
                zone_offset = localTime.tm_min
                              + 60 * localTime.tm_hour
                              + 1440 * (localTime.tm_mday - 2);
        }
  result->tm_params.tp_gmt_offset = zone_offset * 60;
  result->tm_params.tp_dst_offset = dst_offset * 60;
  return PR_SUCCESS;
}
__attribute__((visibility("default"))) PRStatus
PR_ParseTimeString(
        const char *string,
        PRBool default_to_gmt,
        PRTime *result)
{
  PRExplodedTime tm;
  PRStatus rv;
  rv = PR_ParseTimeStringToExplodedTime(string,
                                        default_to_gmt,
                                        &tm);
  if (rv != PR_SUCCESS)
        return rv;
  *result = PR_ImplodeTime(&tm);
  return PR_SUCCESS;
}
__attribute__((visibility("default"))) PRUint32
PR_FormatTime(char *buf, int buflen, const char *fmt, const PRExplodedTime *tm)
{
    size_t rv;
    struct tm a;
    struct tm *ap;
    if (tm) {
        ap = &a;
        a.tm_sec = tm->tm_sec;
        a.tm_min = tm->tm_min;
        a.tm_hour = tm->tm_hour;
        a.tm_mday = tm->tm_mday;
        a.tm_mon = tm->tm_month;
        a.tm_wday = tm->tm_wday;
        a.tm_year = tm->tm_year - 1900;
        a.tm_yday = tm->tm_yday;
        a.tm_isdst = tm->tm_params.tp_dst_offset ? 1 : 0;
        a.tm_zone = ((void *)0);
        a.tm_gmtoff = tm->tm_params.tp_gmt_offset +
                      tm->tm_params.tp_dst_offset;
    } else {
        ap = ((void *)0);
    }
    rv = strftime(buf, buflen, fmt, ap);
    if (!rv && buf && buflen > 0) {
        buf[0] = '\0';
    }
    return rv;
}
static const char* abbrevDays[] =
{
   "Sun","Mon","Tue","Wed","Thu","Fri","Sat"
};
static const char* days[] =
{
   "Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
};
static const char* abbrevMonths[] =
{
   "Jan", "Feb", "Mar", "Apr", "May", "Jun",
   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};
static const char* months[] =
{
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
};
static unsigned int pr_WeekOfYear(const PRExplodedTime* time,
        unsigned int firstDayOfWeek);
__attribute__((visibility("default"))) PRUint32
PR_FormatTimeUSEnglish( char* buf, PRUint32 bufSize,
                        const char* format, const PRExplodedTime* time )
{
   char* bufPtr = buf;
   const char* fmtPtr;
   char tmpBuf[ 40 ];
   const int tmpBufSize = sizeof( tmpBuf );
   for( fmtPtr=format; *fmtPtr != '\0'; fmtPtr++ )
   {
      if( *fmtPtr != '%' )
      {
         do { if( bufSize < 1 ) { *(--bufPtr) = '\0'; return 0; } *bufPtr++ = *fmtPtr; bufSize--; } while(0);
      }
      else
      {
         switch( *(++fmtPtr) )
         {
         case '%':
            do { if( bufSize < 1 ) { *(--bufPtr) = '\0'; return 0; } *bufPtr++ = '%'; bufSize--; } while(0);
            break;
         case 'a':
            do { PRUint32 strSize = strlen( abbrevDays[ time->tm_wday ] ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, abbrevDays[ time->tm_wday ], strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'A':
            do { PRUint32 strSize = strlen( days[ time->tm_wday ] ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, days[ time->tm_wday ], strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'b':
            do { PRUint32 strSize = strlen( abbrevMonths[ time->tm_month ] ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, abbrevMonths[ time->tm_month ], strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'B':
            do { PRUint32 strSize = strlen( months[ time->tm_month ] ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, months[ time->tm_month ], strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'c':
            PR_FormatTimeUSEnglish( tmpBuf, tmpBufSize, "%a %b %d %H:%M:%S %Y", time );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'd':
            PR_snprintf(tmpBuf,tmpBufSize,"%.2ld",time->tm_mday );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'H':
            PR_snprintf(tmpBuf,tmpBufSize,"%.2ld",time->tm_hour );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'I':
            PR_snprintf(tmpBuf,tmpBufSize,"%.2ld",
                        (time->tm_hour%12) ? time->tm_hour%12 : (PRInt32) 12 );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'j':
            PR_snprintf(tmpBuf,tmpBufSize,"%.3d",time->tm_yday + 1);
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'm':
            PR_snprintf(tmpBuf,tmpBufSize,"%.2ld",time->tm_month+1);
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'M':
            PR_snprintf(tmpBuf,tmpBufSize,"%.2ld",time->tm_min );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'p':
            do { PRUint32 strSize = strlen( (time->tm_hour<12)?"AM":"PM" ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, (time->tm_hour<12)?"AM":"PM", strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'S':
            PR_snprintf(tmpBuf,tmpBufSize,"%.2ld",time->tm_sec );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'U':
            PR_snprintf(tmpBuf,tmpBufSize,"%.2d", pr_WeekOfYear( time, 0 ) );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'w':
            PR_snprintf(tmpBuf,tmpBufSize,"%d",time->tm_wday );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'W':
            PR_snprintf(tmpBuf,tmpBufSize,"%.2d", pr_WeekOfYear( time, 1 ) );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'x':
            PR_FormatTimeUSEnglish( tmpBuf, tmpBufSize, "%m/%d/%y", time );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'X':
            PR_FormatTimeUSEnglish( tmpBuf, tmpBufSize, "%H:%M:%S", time );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'y':
            PR_snprintf(tmpBuf,tmpBufSize,"%.2d",time->tm_year % 100 );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'Y':
            PR_snprintf(tmpBuf,tmpBufSize,"%.4d",time->tm_year );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         case 'Z':
            PR_FormatTime( tmpBuf, tmpBufSize, "%Z", time );
            do { PRUint32 strSize = strlen( tmpBuf ); if( strSize > bufSize ) { if( bufSize==0 ) *(--bufPtr) = '\0'; else *bufPtr = '\0'; return 0; } memcpy(bufPtr, tmpBuf, strSize); bufPtr += strSize; bufSize -= strSize; } while(0);
            break;
         default:
            do { if( bufSize < 1 ) { *(--bufPtr) = '\0'; return 0; } *bufPtr++ = '%'; bufSize--; } while(0);
            do { if( bufSize < 1 ) { *(--bufPtr) = '\0'; return 0; } *bufPtr++ = *fmtPtr; bufSize--; } while(0);
            break;
         }
      }
   }
   do { if( bufSize < 1 ) { *(--bufPtr) = '\0'; return 0; } *bufPtr++ = '\0'; bufSize--; } while(0);
   return (PRUint32)(bufPtr - buf - 1);
}
static unsigned int
pr_WeekOfYear(const PRExplodedTime* time, unsigned int firstDayOfWeek)
{
   int dayOfWeek;
   int dayOfYear;
  dayOfWeek = time->tm_wday - firstDayOfWeek;
  if (dayOfWeek < 0)
    dayOfWeek += 7;
  dayOfYear = time->tm_yday - dayOfWeek;
  if( dayOfYear <= 0 )
  {
     return 0;
  }
  else
  {
     return (dayOfYear / 7) + ( (dayOfYear % 7) == 0 ? 0 : 1 );
  }
}
