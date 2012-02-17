typedef long unsigned int size_t;
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
typedef __off_t off_t;
typedef __ssize_t ssize_t;
typedef _G_fpos_t fpos_t;
extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;
extern int remove (__const char *__filename) __attribute__ ((__nothrow__));
extern int rename (__const char *__old, __const char *__new) __attribute__ ((__nothrow__));
extern int renameat (int __oldfd, __const char *__old, int __newfd,
       __const char *__new) __attribute__ ((__nothrow__));
extern FILE *tmpfile (void) ;
extern char *tmpnam (char *__s) __attribute__ ((__nothrow__)) ;
extern char *tmpnam_r (char *__s) __attribute__ ((__nothrow__)) ;
extern char *tempnam (__const char *__dir, __const char *__pfx)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) ;
extern int fclose (FILE *__stream);
extern int fflush (FILE *__stream);
extern int fflush_unlocked (FILE *__stream);
extern FILE *fopen (__const char *__restrict __filename,
      __const char *__restrict __modes) ;
extern FILE *freopen (__const char *__restrict __filename,
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
        size_t __n, FILE *__restrict __s);
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite_unlocked (__const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream);
extern int fseek (FILE *__stream, long int __off, int __whence);
extern long int ftell (FILE *__stream) ;
extern void rewind (FILE *__stream);
extern int fseeko (FILE *__stream, __off_t __off, int __whence);
extern __off_t ftello (FILE *__stream) ;
extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);
extern int fsetpos (FILE *__stream, __const fpos_t *__pos);
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
typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;
typedef __loff_t loff_t;
typedef __ino_t ino_t;
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
extern int __sigismember (__const __sigset_t *, int);
extern int __sigaddset (__sigset_t *, int);
extern int __sigdelset (__sigset_t *, int);
extern __inline int __sigismember (__const __sigset_t *__set, int __sig) { unsigned long int __mask = (((unsigned long int) 1) << (((__sig) - 1) % (8 * sizeof (unsigned long int)))); unsigned long int __word = (((__sig) - 1) / (8 * sizeof (unsigned long int))); return (__set->__val[__word] & __mask) ? 1 : 0; }
extern __inline int __sigaddset ( __sigset_t *__set, int __sig) { unsigned long int __mask = (((unsigned long int) 1) << (((__sig) - 1) % (8 * sizeof (unsigned long int)))); unsigned long int __word = (((__sig) - 1) / (8 * sizeof (unsigned long int))); return ((__set->__val[__word] |= __mask), 0); }
extern __inline int __sigdelset ( __sigset_t *__set, int __sig) { unsigned long int __mask = (((unsigned long int) 1) << (((__sig) - 1) % (8 * sizeof (unsigned long int)))); unsigned long int __word = (((__sig) - 1) / (8 * sizeof (unsigned long int))); return ((__set->__val[__word] &= ~__mask), 0); }
typedef __sig_atomic_t sig_atomic_t;
typedef union sigval
  {
    int sival_int;
    void *sival_ptr;
  } sigval_t;
typedef struct siginfo
  {
    int si_signo;
    int si_errno;
    int si_code;
    union
      {
 int _pad[((128 / sizeof (int)) - 4)];
 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
   } _kill;
 struct
   {
     int si_tid;
     int si_overrun;
     sigval_t si_sigval;
   } _timer;
 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
     sigval_t si_sigval;
   } _rt;
 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
     int si_status;
     __clock_t si_utime;
     __clock_t si_stime;
   } _sigchld;
 struct
   {
     void *si_addr;
   } _sigfault;
 struct
   {
     long int si_band;
     int si_fd;
   } _sigpoll;
      } _sifields;
  } siginfo_t;
enum
{
  SI_ASYNCNL = -60,
  SI_TKILL = -6,
  SI_SIGIO,
  SI_ASYNCIO,
  SI_MESGQ,
  SI_TIMER,
  SI_QUEUE,
  SI_USER,
  SI_KERNEL = 0x80
};
enum
{
  ILL_ILLOPC = 1,
  ILL_ILLOPN,
  ILL_ILLADR,
  ILL_ILLTRP,
  ILL_PRVOPC,
  ILL_PRVREG,
  ILL_COPROC,
  ILL_BADSTK
};
enum
{
  FPE_INTDIV = 1,
  FPE_INTOVF,
  FPE_FLTDIV,
  FPE_FLTOVF,
  FPE_FLTUND,
  FPE_FLTRES,
  FPE_FLTINV,
  FPE_FLTSUB
};
enum
{
  SEGV_MAPERR = 1,
  SEGV_ACCERR
};
enum
{
  BUS_ADRALN = 1,
  BUS_ADRERR,
  BUS_OBJERR
};
enum
{
  TRAP_BRKPT = 1,
  TRAP_TRACE
};
enum
{
  CLD_EXITED = 1,
  CLD_KILLED,
  CLD_DUMPED,
  CLD_TRAPPED,
  CLD_STOPPED,
  CLD_CONTINUED
};
enum
{
  POLL_IN = 1,
  POLL_OUT,
  POLL_MSG,
  POLL_ERR,
  POLL_PRI,
  POLL_HUP
};
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
typedef void (*__sighandler_t) (int);
extern __sighandler_t __sysv_signal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__));
extern __sighandler_t signal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__));
extern int kill (__pid_t __pid, int __sig) __attribute__ ((__nothrow__));
extern int killpg (__pid_t __pgrp, int __sig) __attribute__ ((__nothrow__));
extern int raise (int __sig) __attribute__ ((__nothrow__));
extern __sighandler_t ssignal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__));
extern int gsignal (int __sig) __attribute__ ((__nothrow__));
extern void psignal (int __sig, __const char *__s);
extern void psiginfo (__const siginfo_t *__pinfo, __const char *__s);
extern int __sigpause (int __sig_or_mask, int __is_sig);
extern int sigblock (int __mask) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern int sigsetmask (int __mask) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern int siggetmask (void) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
typedef __sighandler_t sig_t;
extern int sigemptyset (sigset_t *__set) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int sigfillset (sigset_t *__set) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int sigaddset (sigset_t *__set, int __signo) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int sigdelset (sigset_t *__set, int __signo) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int sigismember (__const sigset_t *__set, int __signo)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
struct sigaction
  {
    union
      {
 __sighandler_t sa_handler;
 void (*sa_sigaction) (int, siginfo_t *, void *);
      }
    __sigaction_handler;
    __sigset_t sa_mask;
    int sa_flags;
    void (*sa_restorer) (void);
  };
extern int sigprocmask (int __how, __const sigset_t *__restrict __set,
   sigset_t *__restrict __oset) __attribute__ ((__nothrow__));
extern int sigsuspend (__const sigset_t *__set) __attribute__ ((__nonnull__ (1)));
extern int sigaction (int __sig, __const struct sigaction *__restrict __act,
        struct sigaction *__restrict __oact) __attribute__ ((__nothrow__));
extern int sigpending (sigset_t *__set) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int sigwait (__const sigset_t *__restrict __set, int *__restrict __sig)
     __attribute__ ((__nonnull__ (1, 2)));
extern int sigwaitinfo (__const sigset_t *__restrict __set,
   siginfo_t *__restrict __info) __attribute__ ((__nonnull__ (1)));
extern int sigtimedwait (__const sigset_t *__restrict __set,
    siginfo_t *__restrict __info,
    __const struct timespec *__restrict __timeout)
     __attribute__ ((__nonnull__ (1)));
extern int sigqueue (__pid_t __pid, int __sig, __const union sigval __val)
     __attribute__ ((__nothrow__));
extern __const char *__const _sys_siglist[65];
extern __const char *__const sys_siglist[65];
struct sigvec
  {
    __sighandler_t sv_handler;
    int sv_mask;
    int sv_flags;
  };
extern int sigvec (int __sig, __const struct sigvec *__vec,
     struct sigvec *__ovec) __attribute__ ((__nothrow__));
struct _fpreg
{
  unsigned short significand[4];
  unsigned short exponent;
};
struct _fpxreg
{
  unsigned short significand[4];
  unsigned short exponent;
  unsigned short padding[3];
};
struct _xmmreg
{
  __uint32_t element[4];
};
struct _fpstate
{
  __uint16_t cwd;
  __uint16_t swd;
  __uint16_t ftw;
  __uint16_t fop;
  __uint64_t rip;
  __uint64_t rdp;
  __uint32_t mxcsr;
  __uint32_t mxcr_mask;
  struct _fpxreg _st[8];
  struct _xmmreg _xmm[16];
  __uint32_t padding[24];
};
struct sigcontext
{
  unsigned long r8;
  unsigned long r9;
  unsigned long r10;
  unsigned long r11;
  unsigned long r12;
  unsigned long r13;
  unsigned long r14;
  unsigned long r15;
  unsigned long rdi;
  unsigned long rsi;
  unsigned long rbp;
  unsigned long rbx;
  unsigned long rdx;
  unsigned long rax;
  unsigned long rcx;
  unsigned long rsp;
  unsigned long rip;
  unsigned long eflags;
  unsigned short cs;
  unsigned short gs;
  unsigned short fs;
  unsigned short __pad0;
  unsigned long err;
  unsigned long trapno;
  unsigned long oldmask;
  unsigned long cr2;
  struct _fpstate * fpstate;
  unsigned long __reserved1 [8];
};
extern int sigreturn (struct sigcontext *__scp) __attribute__ ((__nothrow__));
extern int siginterrupt (int __sig, int __interrupt) __attribute__ ((__nothrow__));
struct sigstack
  {
    void *ss_sp;
    int ss_onstack;
  };
enum
{
  SS_ONSTACK = 1,
  SS_DISABLE
};
typedef struct sigaltstack
  {
    void *ss_sp;
    int ss_flags;
    size_t ss_size;
  } stack_t;
typedef long int greg_t;
typedef greg_t gregset_t[23];
struct _libc_fpxreg
{
  unsigned short int significand[4];
  unsigned short int exponent;
  unsigned short int padding[3];
};
struct _libc_xmmreg
{
  __uint32_t element[4];
};
struct _libc_fpstate
{
  __uint16_t cwd;
  __uint16_t swd;
  __uint16_t ftw;
  __uint16_t fop;
  __uint64_t rip;
  __uint64_t rdp;
  __uint32_t mxcsr;
  __uint32_t mxcr_mask;
  struct _libc_fpxreg _st[8];
  struct _libc_xmmreg _xmm[16];
  __uint32_t padding[24];
};
typedef struct _libc_fpstate *fpregset_t;
typedef struct
  {
    gregset_t gregs;
    fpregset_t fpregs;
    unsigned long __reserved1 [8];
} mcontext_t;
typedef struct ucontext
  {
    unsigned long int uc_flags;
    struct ucontext *uc_link;
    stack_t uc_stack;
    mcontext_t uc_mcontext;
    __sigset_t uc_sigmask;
    struct _libc_fpstate __fpregs_mem;
  } ucontext_t;
extern int sigstack (struct sigstack *__ss, struct sigstack *__oss)
     __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
extern int sigaltstack (__const struct sigaltstack *__restrict __ss,
   struct sigaltstack *__restrict __oss) __attribute__ ((__nothrow__));
extern int pthread_sigmask (int __how,
       __const __sigset_t *__restrict __newmask,
       __sigset_t *__restrict __oldmask)__attribute__ ((__nothrow__));
extern int pthread_kill (pthread_t __threadid, int __signo) __attribute__ ((__nothrow__));
extern int __libc_current_sigrtmin (void) __attribute__ ((__nothrow__));
extern int __libc_current_sigrtmax (void) __attribute__ ((__nothrow__));
  enum rounding {
    DEC_ROUND_CEILING,
    DEC_ROUND_UP,
    DEC_ROUND_HALF_UP,
    DEC_ROUND_HALF_EVEN,
    DEC_ROUND_HALF_DOWN,
    DEC_ROUND_DOWN,
    DEC_ROUND_FLOOR,
    DEC_ROUND_05UP,
    DEC_ROUND_MAX
    };
  typedef struct {
    int32_t digits;
    int32_t emax;
    int32_t emin;
    enum rounding round;
    uint32_t traps;
    uint32_t status;
    uint8_t clamp;
    } decContext;
  enum decClass {
    DEC_CLASS_SNAN,
    DEC_CLASS_QNAN,
    DEC_CLASS_NEG_INF,
    DEC_CLASS_NEG_NORMAL,
    DEC_CLASS_NEG_SUBNORMAL,
    DEC_CLASS_NEG_ZERO,
    DEC_CLASS_POS_ZERO,
    DEC_CLASS_POS_SUBNORMAL,
    DEC_CLASS_POS_NORMAL,
    DEC_CLASS_POS_INF
    };
  extern decContext * decContextClearStatus(decContext *, uint32_t);
  extern decContext * decContextDefault(decContext *, int32_t);
  extern enum rounding decContextGetRounding(decContext *);
  extern uint32_t decContextGetStatus(decContext *);
  extern decContext * decContextRestoreStatus(decContext *, uint32_t, uint32_t);
  extern uint32_t decContextSaveStatus(decContext *, uint32_t);
  extern decContext * decContextSetRounding(decContext *, enum rounding);
  extern decContext * decContextSetStatus(decContext *, uint32_t);
  extern decContext * decContextSetStatusFromString(decContext *, const char *);
  extern decContext * decContextSetStatusFromStringQuiet(decContext *, const char *);
  extern decContext * decContextSetStatusQuiet(decContext *, uint32_t);
  extern const char * decContextStatusToString(const decContext *);
  extern int32_t decContextTestEndian(uint8_t);
  extern uint32_t decContextTestSavedStatus(uint32_t, uint32_t);
  extern uint32_t decContextTestStatus(decContext *, uint32_t);
  extern decContext * decContextZeroStatus(decContext *);
  typedef struct {
    int32_t digits;
    int32_t exponent;
    uint8_t bits;
    uint16_t lsu[((16 +3 -1)/3)];
    } decNumber;
  decNumber * decNumberFromInt32(decNumber *, int32_t);
  decNumber * decNumberFromUInt32(decNumber *, uint32_t);
  decNumber * decNumberFromString(decNumber *, const char *, decContext *);
  char * decNumberToString(const decNumber *, char *);
  char * decNumberToEngString(const decNumber *, char *);
  uint32_t decNumberToUInt32(const decNumber *, decContext *);
  int32_t decNumberToInt32(const decNumber *, decContext *);
  uint8_t * decNumberGetBCD(const decNumber *, uint8_t *);
  decNumber * decNumberSetBCD(decNumber *, const uint8_t *, uint32_t);
  decNumber * decNumberAbs(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberAdd(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberAnd(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberCompare(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberCompareSignal(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberCompareTotal(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberCompareTotalMag(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberDivide(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberDivideInteger(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberExp(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberFMA(decNumber *, const decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberInvert(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberLn(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberLogB(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberLog10(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberMax(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberMaxMag(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberMin(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberMinMag(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberMinus(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberMultiply(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberNormalize(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberOr(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberPlus(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberPower(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberQuantize(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberReduce(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberRemainder(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberRemainderNear(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberRescale(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberRotate(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberSameQuantum(decNumber *, const decNumber *, const decNumber *);
  decNumber * decNumberScaleB(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberShift(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberSquareRoot(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberSubtract(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberToIntegralExact(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberToIntegralValue(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberXor(decNumber *, const decNumber *, const decNumber *, decContext *);
  enum decClass decNumberClass(const decNumber *, decContext *);
  const char * decNumberClassToString(enum decClass);
  decNumber * decNumberCopy(decNumber *, const decNumber *);
  decNumber * decNumberCopyAbs(decNumber *, const decNumber *);
  decNumber * decNumberCopyNegate(decNumber *, const decNumber *);
  decNumber * decNumberCopySign(decNumber *, const decNumber *, const decNumber *);
  decNumber * decNumberNextMinus(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberNextPlus(decNumber *, const decNumber *, decContext *);
  decNumber * decNumberNextToward(decNumber *, const decNumber *, const decNumber *, decContext *);
  decNumber * decNumberTrim(decNumber *);
  const char * decNumberVersion(void);
  decNumber * decNumberZero(decNumber *);
  int32_t decNumberIsNormal(const decNumber *, decContext *);
  int32_t decNumberIsSubnormal(const decNumber *, decContext *);
typedef int wchar_t;
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
extern int mkstemp (char *__template) __attribute__ ((__nonnull__ (1))) ;
extern int mkstemps (char *__template, int __suffixlen) __attribute__ ((__nonnull__ (1))) ;
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
  typedef long int LI;
  extern const uint8_t DECSTICKYTAB[10];
  extern const uint32_t DECPOWERS[10];
  extern const uint16_t DPD2BIN[1024];
  extern const uint16_t BIN2DPD[1000];
  extern const uint32_t DPD2BINK[1024];
  extern const uint32_t DPD2BINM[1024];
  extern const uint8_t DPD2BCD8[4096];
  extern const uint8_t BIN2BCD8[4000];
  extern const uint16_t BCD2DPD[2458];
  extern const uint8_t d2utable[49 +1];
  typedef struct {
    uint8_t *msd;
    uint8_t *lsd;
    uint32_t sign;
    int32_t exponent;
    } bcdnum;
  extern const uint32_t DECCOMBMSD[64];
  extern const uint32_t DECCOMBFROM[48];
  typedef struct {
    uint8_t bytes[8];
    } decimal64;
  decimal64 * __dpd64FromString(decimal64 *, const char *, decContext *);
  char * __dpd64ToString(const decimal64 *, char *);
  char * __dpd64ToEngString(const decimal64 *, char *);
  decimal64 * __dpd64FromNumber(decimal64 *, const decNumber *,
      decContext *);
  decNumber * __dpd64ToNumber(const decimal64 *, decNumber *);
  uint32_t decimal64IsCanonical(const decimal64 *);
  decimal64 * decimal64Canonical(decimal64 *, const decimal64 *);
extern const uint32_t COMBEXP[32], COMBMSD[32];
extern const uint16_t DPD2BIN[1024];
extern const uint16_t BIN2DPD[1000];
extern const uint8_t BIN2CHAR[4001];
extern void decDigitsFromDPD(decNumber *, const uint32_t *, int32_t);
extern void decDigitsToDPD(const decNumber *, uint32_t *, int32_t);
const uint16_t BIN2DPD[1000]={ 0, 1, 2, 3, 4, 5, 6, 7,
    8, 9, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 32,
   33, 34, 35, 36, 37, 38, 39, 40, 41, 48, 49, 50, 51,
   52, 53, 54, 55, 56, 57, 64, 65, 66, 67, 68, 69, 70,
   71, 72, 73, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
   96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 112, 113, 114,
  115, 116, 117, 118, 119, 120, 121, 10, 11, 42, 43, 74, 75,
  106, 107, 78, 79, 26, 27, 58, 59, 90, 91, 122, 123, 94,
   95, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 144, 145,
  146, 147, 148, 149, 150, 151, 152, 153, 160, 161, 162, 163, 164,
  165, 166, 167, 168, 169, 176, 177, 178, 179, 180, 181, 182, 183,
  184, 185, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 208,
  209, 210, 211, 212, 213, 214, 215, 216, 217, 224, 225, 226, 227,
  228, 229, 230, 231, 232, 233, 240, 241, 242, 243, 244, 245, 246,
  247, 248, 249, 138, 139, 170, 171, 202, 203, 234, 235, 206, 207,
  154, 155, 186, 187, 218, 219, 250, 251, 222, 223, 256, 257, 258,
  259, 260, 261, 262, 263, 264, 265, 272, 273, 274, 275, 276, 277,
  278, 279, 280, 281, 288, 289, 290, 291, 292, 293, 294, 295, 296,
  297, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 320, 321,
  322, 323, 324, 325, 326, 327, 328, 329, 336, 337, 338, 339, 340,
  341, 342, 343, 344, 345, 352, 353, 354, 355, 356, 357, 358, 359,
  360, 361, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 266,
  267, 298, 299, 330, 331, 362, 363, 334, 335, 282, 283, 314, 315,
  346, 347, 378, 379, 350, 351, 384, 385, 386, 387, 388, 389, 390,
  391, 392, 393, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409,
  416, 417, 418, 419, 420, 421, 422, 423, 424, 425, 432, 433, 434,
  435, 436, 437, 438, 439, 440, 441, 448, 449, 450, 451, 452, 453,
  454, 455, 456, 457, 464, 465, 466, 467, 468, 469, 470, 471, 472,
  473, 480, 481, 482, 483, 484, 485, 486, 487, 488, 489, 496, 497,
  498, 499, 500, 501, 502, 503, 504, 505, 394, 395, 426, 427, 458,
  459, 490, 491, 462, 463, 410, 411, 442, 443, 474, 475, 506, 507,
  478, 479, 512, 513, 514, 515, 516, 517, 518, 519, 520, 521, 528,
  529, 530, 531, 532, 533, 534, 535, 536, 537, 544, 545, 546, 547,
  548, 549, 550, 551, 552, 553, 560, 561, 562, 563, 564, 565, 566,
  567, 568, 569, 576, 577, 578, 579, 580, 581, 582, 583, 584, 585,
  592, 593, 594, 595, 596, 597, 598, 599, 600, 601, 608, 609, 610,
  611, 612, 613, 614, 615, 616, 617, 624, 625, 626, 627, 628, 629,
  630, 631, 632, 633, 522, 523, 554, 555, 586, 587, 618, 619, 590,
  591, 538, 539, 570, 571, 602, 603, 634, 635, 606, 607, 640, 641,
  642, 643, 644, 645, 646, 647, 648, 649, 656, 657, 658, 659, 660,
  661, 662, 663, 664, 665, 672, 673, 674, 675, 676, 677, 678, 679,
  680, 681, 688, 689, 690, 691, 692, 693, 694, 695, 696, 697, 704,
  705, 706, 707, 708, 709, 710, 711, 712, 713, 720, 721, 722, 723,
  724, 725, 726, 727, 728, 729, 736, 737, 738, 739, 740, 741, 742,
  743, 744, 745, 752, 753, 754, 755, 756, 757, 758, 759, 760, 761,
  650, 651, 682, 683, 714, 715, 746, 747, 718, 719, 666, 667, 698,
  699, 730, 731, 762, 763, 734, 735, 768, 769, 770, 771, 772, 773,
  774, 775, 776, 777, 784, 785, 786, 787, 788, 789, 790, 791, 792,
  793, 800, 801, 802, 803, 804, 805, 806, 807, 808, 809, 816, 817,
  818, 819, 820, 821, 822, 823, 824, 825, 832, 833, 834, 835, 836,
  837, 838, 839, 840, 841, 848, 849, 850, 851, 852, 853, 854, 855,
  856, 857, 864, 865, 866, 867, 868, 869, 870, 871, 872, 873, 880,
  881, 882, 883, 884, 885, 886, 887, 888, 889, 778, 779, 810, 811,
  842, 843, 874, 875, 846, 847, 794, 795, 826, 827, 858, 859, 890,
  891, 862, 863, 896, 897, 898, 899, 900, 901, 902, 903, 904, 905,
  912, 913, 914, 915, 916, 917, 918, 919, 920, 921, 928, 929, 930,
  931, 932, 933, 934, 935, 936, 937, 944, 945, 946, 947, 948, 949,
  950, 951, 952, 953, 960, 961, 962, 963, 964, 965, 966, 967, 968,
  969, 976, 977, 978, 979, 980, 981, 982, 983, 984, 985, 992, 993,
  994, 995, 996, 997, 998, 999, 1000, 1001, 1008, 1009, 1010, 1011, 1012,
 1013, 1014, 1015, 1016, 1017, 906, 907, 938, 939, 970, 971, 1002, 1003,
  974, 975, 922, 923, 954, 955, 986, 987, 1018, 1019, 990, 991, 12,
   13, 268, 269, 524, 525, 780, 781, 46, 47, 28, 29, 284, 285,
  540, 541, 796, 797, 62, 63, 44, 45, 300, 301, 556, 557, 812,
  813, 302, 303, 60, 61, 316, 317, 572, 573, 828, 829, 318, 319,
   76, 77, 332, 333, 588, 589, 844, 845, 558, 559, 92, 93, 348,
  349, 604, 605, 860, 861, 574, 575, 108, 109, 364, 365, 620, 621,
  876, 877, 814, 815, 124, 125, 380, 381, 636, 637, 892, 893, 830,
  831, 14, 15, 270, 271, 526, 527, 782, 783, 110, 111, 30, 31,
  286, 287, 542, 543, 798, 799, 126, 127, 140, 141, 396, 397, 652,
  653, 908, 909, 174, 175, 156, 157, 412, 413, 668, 669, 924, 925,
  190, 191, 172, 173, 428, 429, 684, 685, 940, 941, 430, 431, 188,
  189, 444, 445, 700, 701, 956, 957, 446, 447, 204, 205, 460, 461,
  716, 717, 972, 973, 686, 687, 220, 221, 476, 477, 732, 733, 988,
  989, 702, 703, 236, 237, 492, 493, 748, 749, 1004, 1005, 942, 943,
  252, 253, 508, 509, 764, 765, 1020, 1021, 958, 959, 142, 143, 398,
  399, 654, 655, 910, 911, 238, 239, 158, 159, 414, 415, 670, 671,
  926, 927, 254, 255};
const uint16_t DPD2BIN[1024]={ 0, 1, 2, 3, 4, 5, 6, 7,
    8, 9, 80, 81, 800, 801, 880, 881, 10, 11, 12, 13, 14,
   15, 16, 17, 18, 19, 90, 91, 810, 811, 890, 891, 20, 21,
   22, 23, 24, 25, 26, 27, 28, 29, 82, 83, 820, 821, 808,
  809, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 92, 93,
  830, 831, 818, 819, 40, 41, 42, 43, 44, 45, 46, 47, 48,
   49, 84, 85, 840, 841, 88, 89, 50, 51, 52, 53, 54, 55,
   56, 57, 58, 59, 94, 95, 850, 851, 98, 99, 60, 61, 62,
   63, 64, 65, 66, 67, 68, 69, 86, 87, 860, 861, 888, 889,
   70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 96, 97, 870,
  871, 898, 899, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
  180, 181, 900, 901, 980, 981, 110, 111, 112, 113, 114, 115, 116,
  117, 118, 119, 190, 191, 910, 911, 990, 991, 120, 121, 122, 123,
  124, 125, 126, 127, 128, 129, 182, 183, 920, 921, 908, 909, 130,
  131, 132, 133, 134, 135, 136, 137, 138, 139, 192, 193, 930, 931,
  918, 919, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 184,
  185, 940, 941, 188, 189, 150, 151, 152, 153, 154, 155, 156, 157,
  158, 159, 194, 195, 950, 951, 198, 199, 160, 161, 162, 163, 164,
  165, 166, 167, 168, 169, 186, 187, 960, 961, 988, 989, 170, 171,
  172, 173, 174, 175, 176, 177, 178, 179, 196, 197, 970, 971, 998,
  999, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 280, 281,
  802, 803, 882, 883, 210, 211, 212, 213, 214, 215, 216, 217, 218,
  219, 290, 291, 812, 813, 892, 893, 220, 221, 222, 223, 224, 225,
  226, 227, 228, 229, 282, 283, 822, 823, 828, 829, 230, 231, 232,
  233, 234, 235, 236, 237, 238, 239, 292, 293, 832, 833, 838, 839,
  240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 284, 285, 842,
  843, 288, 289, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259,
  294, 295, 852, 853, 298, 299, 260, 261, 262, 263, 264, 265, 266,
  267, 268, 269, 286, 287, 862, 863, 888, 889, 270, 271, 272, 273,
  274, 275, 276, 277, 278, 279, 296, 297, 872, 873, 898, 899, 300,
  301, 302, 303, 304, 305, 306, 307, 308, 309, 380, 381, 902, 903,
  982, 983, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 390,
  391, 912, 913, 992, 993, 320, 321, 322, 323, 324, 325, 326, 327,
  328, 329, 382, 383, 922, 923, 928, 929, 330, 331, 332, 333, 334,
  335, 336, 337, 338, 339, 392, 393, 932, 933, 938, 939, 340, 341,
  342, 343, 344, 345, 346, 347, 348, 349, 384, 385, 942, 943, 388,
  389, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 394, 395,
  952, 953, 398, 399, 360, 361, 362, 363, 364, 365, 366, 367, 368,
  369, 386, 387, 962, 963, 988, 989, 370, 371, 372, 373, 374, 375,
  376, 377, 378, 379, 396, 397, 972, 973, 998, 999, 400, 401, 402,
  403, 404, 405, 406, 407, 408, 409, 480, 481, 804, 805, 884, 885,
  410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 490, 491, 814,
  815, 894, 895, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429,
  482, 483, 824, 825, 848, 849, 430, 431, 432, 433, 434, 435, 436,
  437, 438, 439, 492, 493, 834, 835, 858, 859, 440, 441, 442, 443,
  444, 445, 446, 447, 448, 449, 484, 485, 844, 845, 488, 489, 450,
  451, 452, 453, 454, 455, 456, 457, 458, 459, 494, 495, 854, 855,
  498, 499, 460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 486,
  487, 864, 865, 888, 889, 470, 471, 472, 473, 474, 475, 476, 477,
  478, 479, 496, 497, 874, 875, 898, 899, 500, 501, 502, 503, 504,
  505, 506, 507, 508, 509, 580, 581, 904, 905, 984, 985, 510, 511,
  512, 513, 514, 515, 516, 517, 518, 519, 590, 591, 914, 915, 994,
  995, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529, 582, 583,
  924, 925, 948, 949, 530, 531, 532, 533, 534, 535, 536, 537, 538,
  539, 592, 593, 934, 935, 958, 959, 540, 541, 542, 543, 544, 545,
  546, 547, 548, 549, 584, 585, 944, 945, 588, 589, 550, 551, 552,
  553, 554, 555, 556, 557, 558, 559, 594, 595, 954, 955, 598, 599,
  560, 561, 562, 563, 564, 565, 566, 567, 568, 569, 586, 587, 964,
  965, 988, 989, 570, 571, 572, 573, 574, 575, 576, 577, 578, 579,
  596, 597, 974, 975, 998, 999, 600, 601, 602, 603, 604, 605, 606,
  607, 608, 609, 680, 681, 806, 807, 886, 887, 610, 611, 612, 613,
  614, 615, 616, 617, 618, 619, 690, 691, 816, 817, 896, 897, 620,
  621, 622, 623, 624, 625, 626, 627, 628, 629, 682, 683, 826, 827,
  868, 869, 630, 631, 632, 633, 634, 635, 636, 637, 638, 639, 692,
  693, 836, 837, 878, 879, 640, 641, 642, 643, 644, 645, 646, 647,
  648, 649, 684, 685, 846, 847, 688, 689, 650, 651, 652, 653, 654,
  655, 656, 657, 658, 659, 694, 695, 856, 857, 698, 699, 660, 661,
  662, 663, 664, 665, 666, 667, 668, 669, 686, 687, 866, 867, 888,
  889, 670, 671, 672, 673, 674, 675, 676, 677, 678, 679, 696, 697,
  876, 877, 898, 899, 700, 701, 702, 703, 704, 705, 706, 707, 708,
  709, 780, 781, 906, 907, 986, 987, 710, 711, 712, 713, 714, 715,
  716, 717, 718, 719, 790, 791, 916, 917, 996, 997, 720, 721, 722,
  723, 724, 725, 726, 727, 728, 729, 782, 783, 926, 927, 968, 969,
  730, 731, 732, 733, 734, 735, 736, 737, 738, 739, 792, 793, 936,
  937, 978, 979, 740, 741, 742, 743, 744, 745, 746, 747, 748, 749,
  784, 785, 946, 947, 788, 789, 750, 751, 752, 753, 754, 755, 756,
  757, 758, 759, 794, 795, 956, 957, 798, 799, 760, 761, 762, 763,
  764, 765, 766, 767, 768, 769, 786, 787, 966, 967, 988, 989, 770,
  771, 772, 773, 774, 775, 776, 777, 778, 779, 796, 797, 976, 977,
  998, 999};
const uint8_t BIN2CHAR[4001]={
 '\0','0','0','0', '\1','0','0','1', '\1','0','0','2', '\1','0','0','3', '\1','0','0','4',
 '\1','0','0','5', '\1','0','0','6', '\1','0','0','7', '\1','0','0','8', '\1','0','0','9',
 '\2','0','1','0', '\2','0','1','1', '\2','0','1','2', '\2','0','1','3', '\2','0','1','4',
 '\2','0','1','5', '\2','0','1','6', '\2','0','1','7', '\2','0','1','8', '\2','0','1','9',
 '\2','0','2','0', '\2','0','2','1', '\2','0','2','2', '\2','0','2','3', '\2','0','2','4',
 '\2','0','2','5', '\2','0','2','6', '\2','0','2','7', '\2','0','2','8', '\2','0','2','9',
 '\2','0','3','0', '\2','0','3','1', '\2','0','3','2', '\2','0','3','3', '\2','0','3','4',
 '\2','0','3','5', '\2','0','3','6', '\2','0','3','7', '\2','0','3','8', '\2','0','3','9',
 '\2','0','4','0', '\2','0','4','1', '\2','0','4','2', '\2','0','4','3', '\2','0','4','4',
 '\2','0','4','5', '\2','0','4','6', '\2','0','4','7', '\2','0','4','8', '\2','0','4','9',
 '\2','0','5','0', '\2','0','5','1', '\2','0','5','2', '\2','0','5','3', '\2','0','5','4',
 '\2','0','5','5', '\2','0','5','6', '\2','0','5','7', '\2','0','5','8', '\2','0','5','9',
 '\2','0','6','0', '\2','0','6','1', '\2','0','6','2', '\2','0','6','3', '\2','0','6','4',
 '\2','0','6','5', '\2','0','6','6', '\2','0','6','7', '\2','0','6','8', '\2','0','6','9',
 '\2','0','7','0', '\2','0','7','1', '\2','0','7','2', '\2','0','7','3', '\2','0','7','4',
 '\2','0','7','5', '\2','0','7','6', '\2','0','7','7', '\2','0','7','8', '\2','0','7','9',
 '\2','0','8','0', '\2','0','8','1', '\2','0','8','2', '\2','0','8','3', '\2','0','8','4',
 '\2','0','8','5', '\2','0','8','6', '\2','0','8','7', '\2','0','8','8', '\2','0','8','9',
 '\2','0','9','0', '\2','0','9','1', '\2','0','9','2', '\2','0','9','3', '\2','0','9','4',
 '\2','0','9','5', '\2','0','9','6', '\2','0','9','7', '\2','0','9','8', '\2','0','9','9',
 '\3','1','0','0', '\3','1','0','1', '\3','1','0','2', '\3','1','0','3', '\3','1','0','4',
 '\3','1','0','5', '\3','1','0','6', '\3','1','0','7', '\3','1','0','8', '\3','1','0','9',
 '\3','1','1','0', '\3','1','1','1', '\3','1','1','2', '\3','1','1','3', '\3','1','1','4',
 '\3','1','1','5', '\3','1','1','6', '\3','1','1','7', '\3','1','1','8', '\3','1','1','9',
 '\3','1','2','0', '\3','1','2','1', '\3','1','2','2', '\3','1','2','3', '\3','1','2','4',
 '\3','1','2','5', '\3','1','2','6', '\3','1','2','7', '\3','1','2','8', '\3','1','2','9',
 '\3','1','3','0', '\3','1','3','1', '\3','1','3','2', '\3','1','3','3', '\3','1','3','4',
 '\3','1','3','5', '\3','1','3','6', '\3','1','3','7', '\3','1','3','8', '\3','1','3','9',
 '\3','1','4','0', '\3','1','4','1', '\3','1','4','2', '\3','1','4','3', '\3','1','4','4',
 '\3','1','4','5', '\3','1','4','6', '\3','1','4','7', '\3','1','4','8', '\3','1','4','9',
 '\3','1','5','0', '\3','1','5','1', '\3','1','5','2', '\3','1','5','3', '\3','1','5','4',
 '\3','1','5','5', '\3','1','5','6', '\3','1','5','7', '\3','1','5','8', '\3','1','5','9',
 '\3','1','6','0', '\3','1','6','1', '\3','1','6','2', '\3','1','6','3', '\3','1','6','4',
 '\3','1','6','5', '\3','1','6','6', '\3','1','6','7', '\3','1','6','8', '\3','1','6','9',
 '\3','1','7','0', '\3','1','7','1', '\3','1','7','2', '\3','1','7','3', '\3','1','7','4',
 '\3','1','7','5', '\3','1','7','6', '\3','1','7','7', '\3','1','7','8', '\3','1','7','9',
 '\3','1','8','0', '\3','1','8','1', '\3','1','8','2', '\3','1','8','3', '\3','1','8','4',
 '\3','1','8','5', '\3','1','8','6', '\3','1','8','7', '\3','1','8','8', '\3','1','8','9',
 '\3','1','9','0', '\3','1','9','1', '\3','1','9','2', '\3','1','9','3', '\3','1','9','4',
 '\3','1','9','5', '\3','1','9','6', '\3','1','9','7', '\3','1','9','8', '\3','1','9','9',
 '\3','2','0','0', '\3','2','0','1', '\3','2','0','2', '\3','2','0','3', '\3','2','0','4',
 '\3','2','0','5', '\3','2','0','6', '\3','2','0','7', '\3','2','0','8', '\3','2','0','9',
 '\3','2','1','0', '\3','2','1','1', '\3','2','1','2', '\3','2','1','3', '\3','2','1','4',
 '\3','2','1','5', '\3','2','1','6', '\3','2','1','7', '\3','2','1','8', '\3','2','1','9',
 '\3','2','2','0', '\3','2','2','1', '\3','2','2','2', '\3','2','2','3', '\3','2','2','4',
 '\3','2','2','5', '\3','2','2','6', '\3','2','2','7', '\3','2','2','8', '\3','2','2','9',
 '\3','2','3','0', '\3','2','3','1', '\3','2','3','2', '\3','2','3','3', '\3','2','3','4',
 '\3','2','3','5', '\3','2','3','6', '\3','2','3','7', '\3','2','3','8', '\3','2','3','9',
 '\3','2','4','0', '\3','2','4','1', '\3','2','4','2', '\3','2','4','3', '\3','2','4','4',
 '\3','2','4','5', '\3','2','4','6', '\3','2','4','7', '\3','2','4','8', '\3','2','4','9',
 '\3','2','5','0', '\3','2','5','1', '\3','2','5','2', '\3','2','5','3', '\3','2','5','4',
 '\3','2','5','5', '\3','2','5','6', '\3','2','5','7', '\3','2','5','8', '\3','2','5','9',
 '\3','2','6','0', '\3','2','6','1', '\3','2','6','2', '\3','2','6','3', '\3','2','6','4',
 '\3','2','6','5', '\3','2','6','6', '\3','2','6','7', '\3','2','6','8', '\3','2','6','9',
 '\3','2','7','0', '\3','2','7','1', '\3','2','7','2', '\3','2','7','3', '\3','2','7','4',
 '\3','2','7','5', '\3','2','7','6', '\3','2','7','7', '\3','2','7','8', '\3','2','7','9',
 '\3','2','8','0', '\3','2','8','1', '\3','2','8','2', '\3','2','8','3', '\3','2','8','4',
 '\3','2','8','5', '\3','2','8','6', '\3','2','8','7', '\3','2','8','8', '\3','2','8','9',
 '\3','2','9','0', '\3','2','9','1', '\3','2','9','2', '\3','2','9','3', '\3','2','9','4',
 '\3','2','9','5', '\3','2','9','6', '\3','2','9','7', '\3','2','9','8', '\3','2','9','9',
 '\3','3','0','0', '\3','3','0','1', '\3','3','0','2', '\3','3','0','3', '\3','3','0','4',
 '\3','3','0','5', '\3','3','0','6', '\3','3','0','7', '\3','3','0','8', '\3','3','0','9',
 '\3','3','1','0', '\3','3','1','1', '\3','3','1','2', '\3','3','1','3', '\3','3','1','4',
 '\3','3','1','5', '\3','3','1','6', '\3','3','1','7', '\3','3','1','8', '\3','3','1','9',
 '\3','3','2','0', '\3','3','2','1', '\3','3','2','2', '\3','3','2','3', '\3','3','2','4',
 '\3','3','2','5', '\3','3','2','6', '\3','3','2','7', '\3','3','2','8', '\3','3','2','9',
 '\3','3','3','0', '\3','3','3','1', '\3','3','3','2', '\3','3','3','3', '\3','3','3','4',
 '\3','3','3','5', '\3','3','3','6', '\3','3','3','7', '\3','3','3','8', '\3','3','3','9',
 '\3','3','4','0', '\3','3','4','1', '\3','3','4','2', '\3','3','4','3', '\3','3','4','4',
 '\3','3','4','5', '\3','3','4','6', '\3','3','4','7', '\3','3','4','8', '\3','3','4','9',
 '\3','3','5','0', '\3','3','5','1', '\3','3','5','2', '\3','3','5','3', '\3','3','5','4',
 '\3','3','5','5', '\3','3','5','6', '\3','3','5','7', '\3','3','5','8', '\3','3','5','9',
 '\3','3','6','0', '\3','3','6','1', '\3','3','6','2', '\3','3','6','3', '\3','3','6','4',
 '\3','3','6','5', '\3','3','6','6', '\3','3','6','7', '\3','3','6','8', '\3','3','6','9',
 '\3','3','7','0', '\3','3','7','1', '\3','3','7','2', '\3','3','7','3', '\3','3','7','4',
 '\3','3','7','5', '\3','3','7','6', '\3','3','7','7', '\3','3','7','8', '\3','3','7','9',
 '\3','3','8','0', '\3','3','8','1', '\3','3','8','2', '\3','3','8','3', '\3','3','8','4',
 '\3','3','8','5', '\3','3','8','6', '\3','3','8','7', '\3','3','8','8', '\3','3','8','9',
 '\3','3','9','0', '\3','3','9','1', '\3','3','9','2', '\3','3','9','3', '\3','3','9','4',
 '\3','3','9','5', '\3','3','9','6', '\3','3','9','7', '\3','3','9','8', '\3','3','9','9',
 '\3','4','0','0', '\3','4','0','1', '\3','4','0','2', '\3','4','0','3', '\3','4','0','4',
 '\3','4','0','5', '\3','4','0','6', '\3','4','0','7', '\3','4','0','8', '\3','4','0','9',
 '\3','4','1','0', '\3','4','1','1', '\3','4','1','2', '\3','4','1','3', '\3','4','1','4',
 '\3','4','1','5', '\3','4','1','6', '\3','4','1','7', '\3','4','1','8', '\3','4','1','9',
 '\3','4','2','0', '\3','4','2','1', '\3','4','2','2', '\3','4','2','3', '\3','4','2','4',
 '\3','4','2','5', '\3','4','2','6', '\3','4','2','7', '\3','4','2','8', '\3','4','2','9',
 '\3','4','3','0', '\3','4','3','1', '\3','4','3','2', '\3','4','3','3', '\3','4','3','4',
 '\3','4','3','5', '\3','4','3','6', '\3','4','3','7', '\3','4','3','8', '\3','4','3','9',
 '\3','4','4','0', '\3','4','4','1', '\3','4','4','2', '\3','4','4','3', '\3','4','4','4',
 '\3','4','4','5', '\3','4','4','6', '\3','4','4','7', '\3','4','4','8', '\3','4','4','9',
 '\3','4','5','0', '\3','4','5','1', '\3','4','5','2', '\3','4','5','3', '\3','4','5','4',
 '\3','4','5','5', '\3','4','5','6', '\3','4','5','7', '\3','4','5','8', '\3','4','5','9',
 '\3','4','6','0', '\3','4','6','1', '\3','4','6','2', '\3','4','6','3', '\3','4','6','4',
 '\3','4','6','5', '\3','4','6','6', '\3','4','6','7', '\3','4','6','8', '\3','4','6','9',
 '\3','4','7','0', '\3','4','7','1', '\3','4','7','2', '\3','4','7','3', '\3','4','7','4',
 '\3','4','7','5', '\3','4','7','6', '\3','4','7','7', '\3','4','7','8', '\3','4','7','9',
 '\3','4','8','0', '\3','4','8','1', '\3','4','8','2', '\3','4','8','3', '\3','4','8','4',
 '\3','4','8','5', '\3','4','8','6', '\3','4','8','7', '\3','4','8','8', '\3','4','8','9',
 '\3','4','9','0', '\3','4','9','1', '\3','4','9','2', '\3','4','9','3', '\3','4','9','4',
 '\3','4','9','5', '\3','4','9','6', '\3','4','9','7', '\3','4','9','8', '\3','4','9','9',
 '\3','5','0','0', '\3','5','0','1', '\3','5','0','2', '\3','5','0','3', '\3','5','0','4',
 '\3','5','0','5', '\3','5','0','6', '\3','5','0','7', '\3','5','0','8', '\3','5','0','9',
 '\3','5','1','0', '\3','5','1','1', '\3','5','1','2', '\3','5','1','3', '\3','5','1','4',
 '\3','5','1','5', '\3','5','1','6', '\3','5','1','7', '\3','5','1','8', '\3','5','1','9',
 '\3','5','2','0', '\3','5','2','1', '\3','5','2','2', '\3','5','2','3', '\3','5','2','4',
 '\3','5','2','5', '\3','5','2','6', '\3','5','2','7', '\3','5','2','8', '\3','5','2','9',
 '\3','5','3','0', '\3','5','3','1', '\3','5','3','2', '\3','5','3','3', '\3','5','3','4',
 '\3','5','3','5', '\3','5','3','6', '\3','5','3','7', '\3','5','3','8', '\3','5','3','9',
 '\3','5','4','0', '\3','5','4','1', '\3','5','4','2', '\3','5','4','3', '\3','5','4','4',
 '\3','5','4','5', '\3','5','4','6', '\3','5','4','7', '\3','5','4','8', '\3','5','4','9',
 '\3','5','5','0', '\3','5','5','1', '\3','5','5','2', '\3','5','5','3', '\3','5','5','4',
 '\3','5','5','5', '\3','5','5','6', '\3','5','5','7', '\3','5','5','8', '\3','5','5','9',
 '\3','5','6','0', '\3','5','6','1', '\3','5','6','2', '\3','5','6','3', '\3','5','6','4',
 '\3','5','6','5', '\3','5','6','6', '\3','5','6','7', '\3','5','6','8', '\3','5','6','9',
 '\3','5','7','0', '\3','5','7','1', '\3','5','7','2', '\3','5','7','3', '\3','5','7','4',
 '\3','5','7','5', '\3','5','7','6', '\3','5','7','7', '\3','5','7','8', '\3','5','7','9',
 '\3','5','8','0', '\3','5','8','1', '\3','5','8','2', '\3','5','8','3', '\3','5','8','4',
 '\3','5','8','5', '\3','5','8','6', '\3','5','8','7', '\3','5','8','8', '\3','5','8','9',
 '\3','5','9','0', '\3','5','9','1', '\3','5','9','2', '\3','5','9','3', '\3','5','9','4',
 '\3','5','9','5', '\3','5','9','6', '\3','5','9','7', '\3','5','9','8', '\3','5','9','9',
 '\3','6','0','0', '\3','6','0','1', '\3','6','0','2', '\3','6','0','3', '\3','6','0','4',
 '\3','6','0','5', '\3','6','0','6', '\3','6','0','7', '\3','6','0','8', '\3','6','0','9',
 '\3','6','1','0', '\3','6','1','1', '\3','6','1','2', '\3','6','1','3', '\3','6','1','4',
 '\3','6','1','5', '\3','6','1','6', '\3','6','1','7', '\3','6','1','8', '\3','6','1','9',
 '\3','6','2','0', '\3','6','2','1', '\3','6','2','2', '\3','6','2','3', '\3','6','2','4',
 '\3','6','2','5', '\3','6','2','6', '\3','6','2','7', '\3','6','2','8', '\3','6','2','9',
 '\3','6','3','0', '\3','6','3','1', '\3','6','3','2', '\3','6','3','3', '\3','6','3','4',
 '\3','6','3','5', '\3','6','3','6', '\3','6','3','7', '\3','6','3','8', '\3','6','3','9',
 '\3','6','4','0', '\3','6','4','1', '\3','6','4','2', '\3','6','4','3', '\3','6','4','4',
 '\3','6','4','5', '\3','6','4','6', '\3','6','4','7', '\3','6','4','8', '\3','6','4','9',
 '\3','6','5','0', '\3','6','5','1', '\3','6','5','2', '\3','6','5','3', '\3','6','5','4',
 '\3','6','5','5', '\3','6','5','6', '\3','6','5','7', '\3','6','5','8', '\3','6','5','9',
 '\3','6','6','0', '\3','6','6','1', '\3','6','6','2', '\3','6','6','3', '\3','6','6','4',
 '\3','6','6','5', '\3','6','6','6', '\3','6','6','7', '\3','6','6','8', '\3','6','6','9',
 '\3','6','7','0', '\3','6','7','1', '\3','6','7','2', '\3','6','7','3', '\3','6','7','4',
 '\3','6','7','5', '\3','6','7','6', '\3','6','7','7', '\3','6','7','8', '\3','6','7','9',
 '\3','6','8','0', '\3','6','8','1', '\3','6','8','2', '\3','6','8','3', '\3','6','8','4',
 '\3','6','8','5', '\3','6','8','6', '\3','6','8','7', '\3','6','8','8', '\3','6','8','9',
 '\3','6','9','0', '\3','6','9','1', '\3','6','9','2', '\3','6','9','3', '\3','6','9','4',
 '\3','6','9','5', '\3','6','9','6', '\3','6','9','7', '\3','6','9','8', '\3','6','9','9',
 '\3','7','0','0', '\3','7','0','1', '\3','7','0','2', '\3','7','0','3', '\3','7','0','4',
 '\3','7','0','5', '\3','7','0','6', '\3','7','0','7', '\3','7','0','8', '\3','7','0','9',
 '\3','7','1','0', '\3','7','1','1', '\3','7','1','2', '\3','7','1','3', '\3','7','1','4',
 '\3','7','1','5', '\3','7','1','6', '\3','7','1','7', '\3','7','1','8', '\3','7','1','9',
 '\3','7','2','0', '\3','7','2','1', '\3','7','2','2', '\3','7','2','3', '\3','7','2','4',
 '\3','7','2','5', '\3','7','2','6', '\3','7','2','7', '\3','7','2','8', '\3','7','2','9',
 '\3','7','3','0', '\3','7','3','1', '\3','7','3','2', '\3','7','3','3', '\3','7','3','4',
 '\3','7','3','5', '\3','7','3','6', '\3','7','3','7', '\3','7','3','8', '\3','7','3','9',
 '\3','7','4','0', '\3','7','4','1', '\3','7','4','2', '\3','7','4','3', '\3','7','4','4',
 '\3','7','4','5', '\3','7','4','6', '\3','7','4','7', '\3','7','4','8', '\3','7','4','9',
 '\3','7','5','0', '\3','7','5','1', '\3','7','5','2', '\3','7','5','3', '\3','7','5','4',
 '\3','7','5','5', '\3','7','5','6', '\3','7','5','7', '\3','7','5','8', '\3','7','5','9',
 '\3','7','6','0', '\3','7','6','1', '\3','7','6','2', '\3','7','6','3', '\3','7','6','4',
 '\3','7','6','5', '\3','7','6','6', '\3','7','6','7', '\3','7','6','8', '\3','7','6','9',
 '\3','7','7','0', '\3','7','7','1', '\3','7','7','2', '\3','7','7','3', '\3','7','7','4',
 '\3','7','7','5', '\3','7','7','6', '\3','7','7','7', '\3','7','7','8', '\3','7','7','9',
 '\3','7','8','0', '\3','7','8','1', '\3','7','8','2', '\3','7','8','3', '\3','7','8','4',
 '\3','7','8','5', '\3','7','8','6', '\3','7','8','7', '\3','7','8','8', '\3','7','8','9',
 '\3','7','9','0', '\3','7','9','1', '\3','7','9','2', '\3','7','9','3', '\3','7','9','4',
 '\3','7','9','5', '\3','7','9','6', '\3','7','9','7', '\3','7','9','8', '\3','7','9','9',
 '\3','8','0','0', '\3','8','0','1', '\3','8','0','2', '\3','8','0','3', '\3','8','0','4',
 '\3','8','0','5', '\3','8','0','6', '\3','8','0','7', '\3','8','0','8', '\3','8','0','9',
 '\3','8','1','0', '\3','8','1','1', '\3','8','1','2', '\3','8','1','3', '\3','8','1','4',
 '\3','8','1','5', '\3','8','1','6', '\3','8','1','7', '\3','8','1','8', '\3','8','1','9',
 '\3','8','2','0', '\3','8','2','1', '\3','8','2','2', '\3','8','2','3', '\3','8','2','4',
 '\3','8','2','5', '\3','8','2','6', '\3','8','2','7', '\3','8','2','8', '\3','8','2','9',
 '\3','8','3','0', '\3','8','3','1', '\3','8','3','2', '\3','8','3','3', '\3','8','3','4',
 '\3','8','3','5', '\3','8','3','6', '\3','8','3','7', '\3','8','3','8', '\3','8','3','9',
 '\3','8','4','0', '\3','8','4','1', '\3','8','4','2', '\3','8','4','3', '\3','8','4','4',
 '\3','8','4','5', '\3','8','4','6', '\3','8','4','7', '\3','8','4','8', '\3','8','4','9',
 '\3','8','5','0', '\3','8','5','1', '\3','8','5','2', '\3','8','5','3', '\3','8','5','4',
 '\3','8','5','5', '\3','8','5','6', '\3','8','5','7', '\3','8','5','8', '\3','8','5','9',
 '\3','8','6','0', '\3','8','6','1', '\3','8','6','2', '\3','8','6','3', '\3','8','6','4',
 '\3','8','6','5', '\3','8','6','6', '\3','8','6','7', '\3','8','6','8', '\3','8','6','9',
 '\3','8','7','0', '\3','8','7','1', '\3','8','7','2', '\3','8','7','3', '\3','8','7','4',
 '\3','8','7','5', '\3','8','7','6', '\3','8','7','7', '\3','8','7','8', '\3','8','7','9',
 '\3','8','8','0', '\3','8','8','1', '\3','8','8','2', '\3','8','8','3', '\3','8','8','4',
 '\3','8','8','5', '\3','8','8','6', '\3','8','8','7', '\3','8','8','8', '\3','8','8','9',
 '\3','8','9','0', '\3','8','9','1', '\3','8','9','2', '\3','8','9','3', '\3','8','9','4',
 '\3','8','9','5', '\3','8','9','6', '\3','8','9','7', '\3','8','9','8', '\3','8','9','9',
 '\3','9','0','0', '\3','9','0','1', '\3','9','0','2', '\3','9','0','3', '\3','9','0','4',
 '\3','9','0','5', '\3','9','0','6', '\3','9','0','7', '\3','9','0','8', '\3','9','0','9',
 '\3','9','1','0', '\3','9','1','1', '\3','9','1','2', '\3','9','1','3', '\3','9','1','4',
 '\3','9','1','5', '\3','9','1','6', '\3','9','1','7', '\3','9','1','8', '\3','9','1','9',
 '\3','9','2','0', '\3','9','2','1', '\3','9','2','2', '\3','9','2','3', '\3','9','2','4',
 '\3','9','2','5', '\3','9','2','6', '\3','9','2','7', '\3','9','2','8', '\3','9','2','9',
 '\3','9','3','0', '\3','9','3','1', '\3','9','3','2', '\3','9','3','3', '\3','9','3','4',
 '\3','9','3','5', '\3','9','3','6', '\3','9','3','7', '\3','9','3','8', '\3','9','3','9',
 '\3','9','4','0', '\3','9','4','1', '\3','9','4','2', '\3','9','4','3', '\3','9','4','4',
 '\3','9','4','5', '\3','9','4','6', '\3','9','4','7', '\3','9','4','8', '\3','9','4','9',
 '\3','9','5','0', '\3','9','5','1', '\3','9','5','2', '\3','9','5','3', '\3','9','5','4',
 '\3','9','5','5', '\3','9','5','6', '\3','9','5','7', '\3','9','5','8', '\3','9','5','9',
 '\3','9','6','0', '\3','9','6','1', '\3','9','6','2', '\3','9','6','3', '\3','9','6','4',
 '\3','9','6','5', '\3','9','6','6', '\3','9','6','7', '\3','9','6','8', '\3','9','6','9',
 '\3','9','7','0', '\3','9','7','1', '\3','9','7','2', '\3','9','7','3', '\3','9','7','4',
 '\3','9','7','5', '\3','9','7','6', '\3','9','7','7', '\3','9','7','8', '\3','9','7','9',
 '\3','9','8','0', '\3','9','8','1', '\3','9','8','2', '\3','9','8','3', '\3','9','8','4',
 '\3','9','8','5', '\3','9','8','6', '\3','9','8','7', '\3','9','8','8', '\3','9','8','9',
 '\3','9','9','0', '\3','9','9','1', '\3','9','9','2', '\3','9','9','3', '\3','9','9','4',
 '\3','9','9','5', '\3','9','9','6', '\3','9','9','7', '\3','9','9','8', '\3','9','9','9', '\0'};
decimal64 * __dpd64FromNumber(decimal64 *d64, const decNumber *dn,
    decContext *set) {
  uint32_t status=0;
  int32_t ae;
  decNumber dw;
  decContext dc;
  uint32_t comb, exp;
  uint32_t uiwork;
  uint32_t targar[2]={0, 0};
  ae=dn->exponent+dn->digits-1;
  if (dn->digits>16
   || ae>384
   || ae<-383) {
    decContextDefault(&dc, 64);
    dc.round=set->round;
    decNumberPlus(&dw, dn, &dc);
    dw.bits|=dn->bits&0x80;
    status=dc.status;
    dn=&dw;
    }
  if (dn->bits&(0x40|0x20|0x10)) {
    if (dn->bits&0x40) targar[1]=0x78<<24;
     else {
      if ((*dn->lsu!=0 || dn->digits>1)
       && (dn->digits<16)) {
 decDigitsToDPD(dn, targar, 0);
 }
      if (dn->bits&0x20) targar[1]|=0x7c<<24;
       else targar[1]|=0x7e<<24;
      }
    }
   else {
    if ((*(dn)->lsu==0 && (dn)->digits==1 && (((dn)->bits&(0x40|0x20|0x10))==0))) {
      if (dn->exponent<-398) {
 exp=0;
 status|=0x00000400;
 }
       else {
 exp=dn->exponent+398;
 if (exp>(384 +398 -16 +1)) {
   exp=(384 +398 -16 +1);
   status|=0x00000400;
   }
 }
      comb=(exp>>5) & 0x18;
      }
     else {
      uint32_t msd;
      int32_t pad=0;
      exp=(uint32_t)(dn->exponent+398);
      if (exp>(384 +398 -16 +1)) {
 pad=exp-(384 +398 -16 +1);
 exp=(384 +398 -16 +1);
 status|=0x00000400;
 }
      if (3==3 && pad==0) {
 uint32_t dpd[6]={0,0,0,0,0,0};
 uint32_t i;
 int32_t d=dn->digits;
 for (i=0; d>0; i++, d-=3) dpd[i]=BIN2DPD[dn->lsu[i]];
 targar[0] =dpd[0];
 targar[0]|=dpd[1]<<10;
 targar[0]|=dpd[2]<<20;
 if (dn->digits>6) {
   targar[0]|=dpd[3]<<30;
   targar[1] =dpd[3]>>2;
   targar[1]|=dpd[4]<<8;
   }
 msd=dpd[5];
 }
       else {
 decDigitsToDPD(dn, targar, pad);
 msd=targar[1]>>18;
 targar[1]&=0x0003ffff;
 }
      if (msd>=8) comb=0x18 | ((exp>>7) & 0x06) | (msd & 0x01);
      else comb=((exp>>5) & 0x18) | msd;
      }
    targar[1]|=comb<<26;
    targar[1]|=(exp&0xff)<<18;
    }
  if (dn->bits&0x80) targar[1]|=0x80000000;
  if (1) {
    (uiwork=(targar[0]), memcpy(d64->bytes, (void *)&uiwork, 4), uiwork);
    (uiwork=(targar[1]), memcpy(d64->bytes+4, (void *)&uiwork, 4), uiwork);
    }
   else {
    (uiwork=(targar[1]), memcpy(d64->bytes, (void *)&uiwork, 4), uiwork);
    (uiwork=(targar[0]), memcpy(d64->bytes+4, (void *)&uiwork, 4), uiwork);
    }
  if (status!=0) decContextSetStatus(set, status);
  return d64;
  }
decNumber * __dpd64ToNumber(const decimal64 *d64, decNumber *dn) {
  uint32_t msd;
  uint32_t exp;
  uint32_t comb;
  int32_t need;
  uint32_t uiwork;
  uint32_t sourar[2];
  if (1) {
    sourar[0]=(memcpy((void *)&uiwork, d64->bytes, 4), uiwork);
    sourar[1]=(memcpy((void *)&uiwork, d64->bytes+4, 4), uiwork);
    }
   else {
    sourar[1]=(memcpy((void *)&uiwork, d64->bytes, 4), uiwork);
    sourar[0]=(memcpy((void *)&uiwork, d64->bytes+4, 4), uiwork);
    }
  comb=(sourar[1]>>26)&0x1f;
  decNumberZero(dn);
  if (sourar[1]&0x80000000) dn->bits=0x80;
  msd=COMBMSD[comb];
  exp=COMBEXP[comb];
  if (exp==3) {
    if (msd==0) {
      dn->bits|=0x40;
      return dn;
      }
    else if (sourar[1]&0x02000000) dn->bits|=0x10;
    else dn->bits|=0x20;
    msd=0;
    }
   else {
    dn->exponent=(exp<<8)+((sourar[1]>>18)&0xff)-398;
    }
  sourar[1]&=0x0003ffff;
  if (msd) {
    sourar[1]|=msd<<18;
    need=6;
    }
   else {
    if (!sourar[1]) {
      if (!sourar[0]) return dn;
      need=3;
      if (sourar[0]&0xc0000000) need++;
      }
     else {
      need=4;
      if (sourar[1]&0x0003ff00) need++;
      }
    }
  decDigitsFromDPD(dn, sourar, need);
  return dn;
  }
char * __dpd64ToEngString(const decimal64 *d64, char *string){
  decNumber dn;
  __dpd64ToNumber(d64, &dn);
  decNumberToEngString(&dn, string);
  return string;
  }
char * __dpd64ToString(const decimal64 *d64, char *string){
  uint32_t msd;
  int32_t exp;
  uint32_t comb;
  char *cstart;
  char *c;
  const uint8_t *u;
  char *s, *t;
  int32_t dpd;
  int32_t pre, e;
  uint32_t uiwork;
  uint32_t sourar[2];
  if (1) {
    sourar[0]=(memcpy((void *)&uiwork, d64->bytes, 4), uiwork);
    sourar[1]=(memcpy((void *)&uiwork, d64->bytes+4, 4), uiwork);
    }
   else {
    sourar[1]=(memcpy((void *)&uiwork, d64->bytes, 4), uiwork);
    sourar[0]=(memcpy((void *)&uiwork, d64->bytes+4, 4), uiwork);
    }
  c=string;
  if (((int32_t)sourar[1])<0) *c++='-';
  comb=(sourar[1]>>26)&0x1f;
  msd=COMBMSD[comb];
  exp=COMBEXP[comb];
  if (exp==3) {
    if (msd==0) {
      strcpy(c, "Inf");
      strcpy(c+3, "inity");
      return string;
      }
    if (sourar[1]&0x02000000) *c++='s';
    strcpy(c, "NaN");
    c+=3;
    if (sourar[0]==0 && (sourar[1]&0x0003ffff)==0) return string;
    exp=0; msd=0;
    }
   else exp=(exp<<8)+((sourar[1]>>18)&0xff)-398;
  cstart=c;
  if (msd) *c++='0'+(char)msd;
  dpd=(sourar[1]>>8)&0x3ff;
  u=&BIN2CHAR[DPD2BIN[dpd]*4]; if (c!=cstart) {memcpy(c, u+1, 4); c+=3;} else if (*u) {memcpy(c, u+4-*u, 4); c+=*u;};
  dpd=((sourar[1]&0xff)<<2) | (sourar[0]>>30);
  u=&BIN2CHAR[DPD2BIN[dpd]*4]; if (c!=cstart) {memcpy(c, u+1, 4); c+=3;} else if (*u) {memcpy(c, u+4-*u, 4); c+=*u;};
  dpd=(sourar[0]>>20)&0x3ff;
  u=&BIN2CHAR[DPD2BIN[dpd]*4]; if (c!=cstart) {memcpy(c, u+1, 4); c+=3;} else if (*u) {memcpy(c, u+4-*u, 4); c+=*u;};
  dpd=(sourar[0]>>10)&0x3ff;
  u=&BIN2CHAR[DPD2BIN[dpd]*4]; if (c!=cstart) {memcpy(c, u+1, 4); c+=3;} else if (*u) {memcpy(c, u+4-*u, 4); c+=*u;};
  dpd=(sourar[0])&0x3ff;
  u=&BIN2CHAR[DPD2BIN[dpd]*4]; if (c!=cstart) {memcpy(c, u+1, 4); c+=3;} else if (*u) {memcpy(c, u+4-*u, 4); c+=*u;};
  if (c==cstart) *c++='0';
  if (exp==0) {
    *c='\0';
    return string;
    }
  e=0;
  pre=c-cstart+exp;
  if (exp>0 || pre<-5) {
    e=pre-1;
    pre=1;
    }
  s=c-1;
  if (pre>0) {
    char *dotat=cstart+pre;
    if (dotat<c) {
      t=c;
      for (; s>=dotat; s--, t--) *t=*s;
      *t='.';
      c++;
      }
    if (e!=0) {
      *c++='E';
      *c++='+';
      if (e<0) {
 *(c-1)='-';
 e=-e;
 }
      u=&BIN2CHAR[e*4];
      memcpy(c, u+4-*u, 4);
      c+=*u;
      }
    *c='\0';
    return string;
    }
  t=c+1-pre;
  *(t+1)='\0';
  for (; s>=cstart; s--, t--) *t=*s;
  c=cstart;
  *c++='0';
  *c++='.';
  for (; pre<0; pre++) *c++='0';
  return string;
  }
decimal64 * __dpd64FromString(decimal64 *result, const char *string,
    decContext *set) {
  decContext dc;
  decNumber dn;
  decContextDefault(&dc, 64);
  dc.round=set->round;
  decNumberFromString(&dn, string, &dc);
  __dpd64FromNumber(result, &dn, &dc);
  if (dc.status!=0) {
    decContextSetStatus(set, dc.status);
    }
  return result;
  }
uint32_t decimal64IsCanonical(const decimal64 *d64) {
  decNumber dn;
  decimal64 canon;
  decContext dc;
  decContextDefault(&dc, 64);
  __dpd64ToNumber(d64, &dn);
  __dpd64FromNumber(&canon, &dn, &dc);
  return memcmp(d64, &canon, 8)==0;
  }
decimal64 * decimal64Canonical(decimal64 *result, const decimal64 *d64) {
  decNumber dn;
  decContext dc;
  decContextDefault(&dc, 64);
  __dpd64ToNumber(d64, &dn);
  __dpd64FromNumber(result, &dn, &dc);
  return result;
  }
const uint32_t COMBEXP[32]={0, 0, 0, 0, 0, 0, 0, 0,
   1, 1, 1, 1, 1, 1, 1, 1,
   2, 2, 2, 2, 2, 2, 2, 2,
   0, 0, 1, 1, 2, 2, 3, 3};
const uint32_t COMBMSD[32]={0, 1, 2, 3, 4, 5, 6, 7,
   0, 1, 2, 3, 4, 5, 6, 7,
   0, 1, 2, 3, 4, 5, 6, 7,
   8, 9, 8, 9, 8, 9, 0, 1};
static const uint32_t multies[]={131073, 26215, 5243, 1049, 210};
void decDigitsToDPD(const decNumber *dn, uint32_t *targ, int32_t shift) {
  int32_t cut;
  int32_t n;
  int32_t digits=dn->digits;
  uint32_t dpd;
  uint32_t bin;
  uint32_t *uout=targ;
  uint32_t uoff=0;
  const uint16_t *inu=dn->lsu;
  uint16_t uar[((34 +3 -1)/3)];
  if (shift!=0) {
    const uint16_t *source;
    uint16_t *target, *first;
    uint32_t next=0;
    source=dn->lsu+((digits)<=49?d2utable[digits]:((digits)+3 -1)/3)-1;
    target=uar+((digits)<=49?d2utable[digits]:((digits)+3 -1)/3)-1+((shift)<=49?d2utable[shift]:((shift)+3 -1)/3);
    cut=3 -((shift)-(((shift)<=49?d2utable[shift]:((shift)+3 -1)/3)-1)*3);
    if (cut==0) {
      for (; source>=dn->lsu; source--, target--) *target=*source;
      }
     else {
      first=uar+((digits+shift)<=49?d2utable[digits+shift]:((digits+shift)+3 -1)/3)-1;
      for (; source>=dn->lsu; source--, target--) {
   uint32_t quot=((((uint32_t)(*source)>>(cut))*multies[cut])>>17);
   uint32_t rem=*source-quot*DECPOWERS[cut];
   next+=quot;
 if (target<=first) *target=(uint16_t)next;
 next=rem*DECPOWERS[3 -cut];
 }
      }
    for (; target>=uar; target--) {
      *target=(uint16_t)next;
      next=0;
      }
    digits+=shift;
    inu=uar;
    }
  for(n=0; digits>0; n++) {
      bin=*inu;
      digits-=3;
      inu++;
    dpd=BIN2DPD[bin];
    *uout|=dpd<<uoff;
    uoff+=10;
    if (uoff<32) continue;
    uout++;
    uoff-=32;
    *uout|=dpd>>(10-uoff);
    }
  return;
  }
void decDigitsFromDPD(decNumber *dn, const uint32_t *sour, int32_t declets) {
  uint32_t dpd;
  int32_t n;
  uint16_t *uout=dn->lsu;
  uint16_t *last=uout;
  const uint32_t *uin=sour;
  uint32_t uoff=0;
  for (n=declets-1; n>=0; n--) {
    dpd=*uin>>uoff;
    uoff+=10;
    if (uoff>32) {
      uin++;
      uoff-=32;
      dpd|=*uin<<(10-uoff);
      }
    dpd&=0x3ff;
    if (dpd==0) *uout=0;
     else {
      *uout=DPD2BIN[dpd];
      last=uout;
      }
    uout++;
    }
  dn->digits=(last-dn->lsu)*3 +1;
  if (*last<10) return;
  dn->digits++;
  if (*last<100) return;
  dn->digits++;
  return;
  }
typedef unsigned int UINT32;
typedef unsigned long long UINT64;
typedef struct { UINT64 w[2]; } UINT128;
void _bid_to_dpd32 (UINT32 *, UINT32 *);
void _dpd_to_bid32 (UINT32 *, UINT32 *);
void _bid_to_dpd64 (UINT64 *, UINT64 *);
void _dpd_to_bid64 (UINT64 *, UINT64 *);
void _bid_to_dpd128 (UINT128 *, UINT128 *);
void _dpd_to_bid128 (UINT128 *, UINT128 *);
decimal64 *decimal64FromString (decimal64 *, const char *, decContext *);
char *decimal64ToString (const decimal64 *, char *);
char *decimal64ToEngString (const decimal64 *, char *);
decimal64 *decimal64FromNumber (decimal64 *, const decNumber *, decContext *);
decNumber *decimal64ToNumber (const decimal64 *, decNumber *);
void __host_to_ieee_64 (UINT64 in, decimal64 *out);
void __ieee_to_host_64 (decimal64 in, UINT64 *out);
decimal64 *
decimal64FromNumber (decimal64 *d64, const decNumber *dn,
        decContext *set)
{
  union
    {
      UINT64 _Dec;
      decimal64 dec;
    } u;
  __dpd64FromNumber (d64, dn, set);
  __ieee_to_host_64 (*d64, &u._Dec);
  _dpd_to_bid64 (&u._Dec, &u._Dec);
  __host_to_ieee_64 (u._Dec, &u.dec);
  *d64 = u.dec;
  return d64;
}
decNumber *
decimal64ToNumber (const decimal64 *bid64, decNumber *dn)
{
  union
    {
      UINT64 _Dec;
      decimal64 dec;
    } u;
  __ieee_to_host_64 (*bid64, &u._Dec);
  _bid_to_dpd64 (&u._Dec, &u._Dec);
  __host_to_ieee_64 (u._Dec, &u.dec);
  return __dpd64ToNumber (&u.dec, dn);
}
char *
decimal64ToString (const decimal64 *d64, char *string)
{
  decNumber dn;
  decimal64ToNumber (d64, &dn);
  decNumberToString (&dn, string);
  return string;
}
char *
decimal64ToEngString (const decimal64 *d64, char *string)
{
  decNumber dn;
  decimal64ToNumber (d64, &dn);
  decNumberToEngString (&dn, string);
  return string;
}
decimal64 *
decimal64FromString (decimal64 *result, const char *string,
        decContext *set)
{
  decContext dc;
  decNumber dn;
  decContextDefault (&dc, 64);
  dc.round = set->round;
  decNumberFromString (&dn, string, &dc);
  decimal64FromNumber (result, &dn, &dc);
  if (dc.status != 0)
    {
      decContextSetStatus (set, dc.status);
    }
  return result;
}
