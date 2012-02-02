//===-- asan_interceptors.cc ------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is a part of AddressSanitizer, an address sanity checker.
//
// Intercept various libc functions.
//===----------------------------------------------------------------------===//
#include "asan_interceptors.h"

#include "asan_allocator.h"
#include "asan_interface.h"
#include "asan_internal.h"
#include "asan_mac.h"
#include "asan_mapping.h"
#include "asan_stack.h"
#include "asan_stats.h"
#include "asan_thread_registry.h"

#include <new>
#include <ctype.h>

#ifndef _WIN32
#include <dlfcn.h>
#include <pthread.h>
#endif

// To replace weak system functions on Linux we just need to declare functions
// with same names in our library and then obtain the real function pointers
// using dlsym(). This is not so on Mac OS, where the two-level namespace makes
// our replacement functions invisible to other libraries. This may be overcomed
// using the DYLD_FORCE_FLAT_NAMESPACE, but some errors loading the shared
// libraries in Chromium were noticed when doing so.
// Instead we use mach_override, a handy framework for patching functions at
// runtime. To avoid possible name clashes, our replacement functions have
// the "wrap_" prefix on Mac.
//
// After interception, the calls to system functions will be substituted by
// calls to our interceptors. We store pointers to system function f()
// in __asan::real_f().
#if defined(__APPLE__)
// Include the declarations of the original functions.
#include <signal.h>
#include <string.h>
#include <strings.h>

#include "mach_override/mach_override.h"

#define OVERRIDE_FUNCTION(oldfunc, newfunc)                                   \
  do {CHECK(0 == __asan_mach_override_ptr_custom((void*)(oldfunc),            \
                                                 (void*)(newfunc),            \
                                                 (void**)&real_##oldfunc,     \
                                                 __asan_allocate_island,      \
                                                 __asan_deallocate_island));  \
  CHECK(real_##oldfunc != NULL);   } while (0)

#define OVERRIDE_FUNCTION_IF_EXISTS(oldfunc, newfunc)               \
  do { __asan_mach_override_ptr_custom((void*)(oldfunc),            \
                                       (void*)(newfunc),            \
                                       (void**)&real_##oldfunc,     \
                                       __asan_allocate_island,      \
                                       __asan_deallocate_island);   \
  } while (0)

#define INTERCEPT_FUNCTION(func)                                        \
  OVERRIDE_FUNCTION(func, WRAP(func))

#define INTERCEPT_FUNCTION_IF_EXISTS(func)                              \
  OVERRIDE_FUNCTION_IF_EXISTS(func, WRAP(func))

#elif defined(_WIN32)
// TODO(timurrrr): change these macros once we decide how to intercept
// functions on Windows.
#define INTERCEPT_FUNCTION(func)                                        \
  do { } while (0)

#define INTERCEPT_FUNCTION_IF_EXISTS(func)                              \
  do { } while (0)

#else  // __linux__
#define INTERCEPT_FUNCTION(func)                                        \
  CHECK((real_##func = (func##_f)dlsym(RTLD_NEXT, #func)));

#define INTERCEPT_FUNCTION_IF_EXISTS(func)                              \
  do { real_##func = (func##_f)dlsym(RTLD_NEXT, #func); } while (0)
#endif

namespace __asan {

// Instruments read/write access to a single byte in memory.
// On error calls __asan_report_error, which aborts the program.
__attribute__((noinline))
static void AccessAddress(uintptr_t address, bool isWrite) {
  if (__asan_address_is_poisoned((void*)address)) {
    GET_BP_PC_SP;
    __asan_report_error(pc, bp, sp, address, isWrite, /* access_size */ 1);
  }
}

// We implement ACCESS_MEMORY_RANGE, ASAN_READ_RANGE,
// and ASAN_WRITE_RANGE as macro instead of function so
// that no extra frames are created, and stack trace contains
// relevant information only.

// Instruments read/write access to a memory range.
// More complex implementation is possible, for now just
// checking the first and the last byte of a range.
#define ACCESS_MEMORY_RANGE(offset, size, isWrite) do { \
  if (size > 0) { \
    uintptr_t ptr = (uintptr_t)(offset); \
    AccessAddress(ptr, isWrite); \
    AccessAddress(ptr + (size) - 1, isWrite); \
  } \
} while (0)

#define ASAN_READ_RANGE(offset, size) do { \
  ACCESS_MEMORY_RANGE(offset, size, false); \
} while (0)

#define ASAN_WRITE_RANGE(offset, size) do { \
  ACCESS_MEMORY_RANGE(offset, size, true); \
} while (0)

// Behavior of functions like "memcpy" or "strcpy" is undefined
// if memory intervals overlap. We report error in this case.
// Macro is used to avoid creation of new frames.
static inline bool RangesOverlap(const char *offset1, size_t length1,
                                 const char *offset2, size_t length2) {
  return !((offset1 + length1 <= offset2) || (offset2 + length2 <= offset1));
}
#define CHECK_RANGES_OVERLAP(name, _offset1, length1, _offset2, length2) do { \
  const char *offset1 = (const char*)_offset1; \
  const char *offset2 = (const char*)_offset2; \
  if (RangesOverlap(offset1, length1, offset2, length2)) { \
    Report("ERROR: AddressSanitizer %s-param-overlap: " \
           "memory ranges [%p,%p) and [%p, %p) overlap\n", \
           name, offset1, offset1 + length1, offset2, offset2 + length2); \
    PRINT_CURRENT_STACK(); \
    ShowStatsAndAbort(); \
  } \
} while (0)

#define ENSURE_ASAN_INITED() do { \
  CHECK(!asan_init_is_running); \
  if (!asan_inited) { \
    __asan_init(); \
  } \
} while (0)

size_t internal_strlen(const char *s) {
  size_t i = 0;
  while (s[i]) i++;
  return i;
}

size_t internal_strnlen(const char *s, size_t maxlen) {
#ifndef __APPLE__
  if (real_strnlen != NULL) {
    return real_strnlen(s, maxlen);
  }
#endif
  size_t i = 0;
  while (i < maxlen && s[i]) i++;
  return i;
}

void* internal_memchr(const void* s, int c, size_t n) {
  const char* t = (char*)s;
  for (size_t i = 0; i < n; ++i, ++t)
    if (*t == c)
      return (void*)t;
  return NULL;
}

int internal_memcmp(const void* s1, const void* s2, size_t n) {
  const char* t1 = (char*)s1;
  const char* t2 = (char*)s2;
  for (size_t i = 0; i < n; ++i, ++t1, ++t2)
    if (*t1 != *t2)
      return *t1 < *t2 ? -1 : 1;
  return 0;
}

char *internal_strstr(const char *haystack, const char *needle) {
  // This is O(N^2), but we are not using it in hot places.
  size_t len1 = internal_strlen(haystack);
  size_t len2 = internal_strlen(needle);
  if (len1 < len2) return 0;
  for (size_t pos = 0; pos <= len1 - len2; pos++) {
    if (internal_memcmp(haystack + pos, needle, len2) == 0)
      return (char*)haystack + pos;
  }
  return 0;
}

char *internal_strncat(char *dst, const char *src, size_t n) {
  size_t len = internal_strlen(dst);
  size_t i;
  for (i = 0; i < n && src[i]; i++)
    dst[len + i] = src[i];
  dst[len + i] = 0;
  return dst;
}

int internal_strcmp(const char *s1, const char *s2) {
  while (true) {
    unsigned c1 = *s1;
    unsigned c2 = *s2;
    if (c1 != c2) return (c1 < c2) ? -1 : 1;
    if (c1 == 0) break;
    s1++;
    s2++;
  }
  return 0;
}

}  // namespace __asan

// ---------------------- Wrappers ---------------- {{{1
using namespace __asan;  // NOLINT

#define OPERATOR_NEW_BODY \
  GET_STACK_TRACE_HERE_FOR_MALLOC;\
  return asan_memalign(0, size, &stack);

#ifdef ANDROID
void *operator new(size_t size) { OPERATOR_NEW_BODY; }
void *operator new[](size_t size) { OPERATOR_NEW_BODY; }
#else
void *operator new(size_t size) throw(std::bad_alloc) { OPERATOR_NEW_BODY; }
void *operator new[](size_t size) throw(std::bad_alloc) { OPERATOR_NEW_BODY; }
void *operator new(size_t size, std::nothrow_t const&) throw()
{ OPERATOR_NEW_BODY; }
void *operator new[](size_t size, std::nothrow_t const&) throw()
{ OPERATOR_NEW_BODY; }
#endif

#define OPERATOR_DELETE_BODY \
  GET_STACK_TRACE_HERE_FOR_FREE(ptr);\
  asan_free(ptr, &stack);

void operator delete(void *ptr) throw() { OPERATOR_DELETE_BODY; }
void operator delete[](void *ptr) throw() { OPERATOR_DELETE_BODY; }
void operator delete(void *ptr, std::nothrow_t const&) throw()
{ OPERATOR_DELETE_BODY; }
void operator delete[](void *ptr, std::nothrow_t const&) throw()
{ OPERATOR_DELETE_BODY;}

static void *asan_thread_start(void *arg) {
  AsanThread *t = (AsanThread*)arg;
  asanThreadRegistry().SetCurrent(t);
  return t->ThreadStart();
}

#ifndef _WIN32
INTERCEPTOR(int, pthread_create, pthread_t *thread,
                                 const pthread_attr_t *attr,
                                 void *(*start_routine)(void*), void *arg) {
  GET_STACK_TRACE_HERE(kStackTraceMax);
  int current_tid = asanThreadRegistry().GetCurrentTidOrMinusOne();
  AsanThread *t = AsanThread::Create(current_tid, start_routine, arg, &stack);
  asanThreadRegistry().RegisterThread(t);
  return real_pthread_create(thread, attr, asan_thread_start, t);
}

INTERCEPTOR(void*, signal, int signum, void *handler) {
  if (!AsanInterceptsSignal(signum)) {
    return real_signal(signum, handler);
  }
  return NULL;
}

INTERCEPTOR(int, sigaction, int signum, const struct sigaction *act,
                            struct sigaction *oldact) {
  if (!AsanInterceptsSignal(signum)) {
    return real_sigaction(signum, act, oldact);
  }
  return 0;
}
#endif  // _WIN32


static void UnpoisonStackFromHereToTop() {
  int local_stack;
  AsanThread *curr_thread = asanThreadRegistry().GetCurrent();
  CHECK(curr_thread);
  uintptr_t top = curr_thread->stack_top();
  uintptr_t bottom = ((uintptr_t)&local_stack - kPageSize) & ~(kPageSize-1);
  PoisonShadow(bottom, top - bottom, 0);
}

INTERCEPTOR(void, longjmp, void *env, int val) {
  UnpoisonStackFromHereToTop();
  real_longjmp(env, val);
}

INTERCEPTOR(void, _longjmp, void *env, int val) {
  UnpoisonStackFromHereToTop();
  real__longjmp(env, val);
}

INTERCEPTOR(void, siglongjmp, void *env, int val) {
  UnpoisonStackFromHereToTop();
  real_siglongjmp(env, val);
}

#if ASAN_HAS_EXCEPTIONS == 1
#ifdef __APPLE__
extern "C" void __cxa_throw(void *a, void *b, void *c);
#endif  // __APPLE__

INTERCEPTOR(void, __cxa_throw, void *a, void *b, void *c) {
  CHECK(&real___cxa_throw);
  UnpoisonStackFromHereToTop();
  real___cxa_throw(a, b, c);
}
#endif

// intercept mlock and friends.
// Since asan maps 16T of RAM, mlock is completely unfriendly to asan.
// All functions return 0 (success).
static void MlockIsUnsupported() {
  static bool printed = 0;
  if (printed) return;
  printed = true;
  Printf("INFO: AddressSanitizer ignores mlock/mlockall/munlock/munlockall\n");
}

extern "C" {
INTERCEPTOR_ATTRIBUTE
int mlock(const void *addr, size_t len) {
  MlockIsUnsupported();
  return 0;
}

INTERCEPTOR_ATTRIBUTE
int munlock(const void *addr, size_t len) {
  MlockIsUnsupported();
  return 0;
}

INTERCEPTOR_ATTRIBUTE
int mlockall(int flags) {
  MlockIsUnsupported();
  return 0;
}

INTERCEPTOR_ATTRIBUTE
int munlockall(void) {
  MlockIsUnsupported();
  return 0;
}
}  // extern "C"

static inline int CharCmp(unsigned char c1, unsigned char c2) {
  return (c1 == c2) ? 0 : (c1 < c2) ? -1 : 1;
}

static inline int CharCaseCmp(unsigned char c1, unsigned char c2) {
  int c1_low = tolower(c1);
  int c2_low = tolower(c2);
  return c1_low - c2_low;
}

INTERCEPTOR(int, memcmp, const void *a1, const void *a2, size_t size) {
  ENSURE_ASAN_INITED();
  unsigned char c1 = 0, c2 = 0;
  const unsigned char *s1 = (const unsigned char*)a1;
  const unsigned char *s2 = (const unsigned char*)a2;
  size_t i;
  for (i = 0; i < size; i++) {
    c1 = s1[i];
    c2 = s2[i];
    if (c1 != c2) break;
  }
  ASAN_READ_RANGE(s1, Min(i + 1, size));
  ASAN_READ_RANGE(s2, Min(i + 1, size));
  return CharCmp(c1, c2);
}

INTERCEPTOR(void*, memcpy, void *to, const void *from, size_t size) {
  // memcpy is called during __asan_init() from the internals
  // of printf(...).
  if (asan_init_is_running) {
    return real_memcpy(to, from, size);
  }
  ENSURE_ASAN_INITED();
  if (FLAG_replace_intrin) {
    if (to != from) {
      // We do not treat memcpy with to==from as a bug.
      // See http://llvm.org/bugs/show_bug.cgi?id=11763.
      CHECK_RANGES_OVERLAP("memcpy", to, size, from, size);
    }
    ASAN_WRITE_RANGE(from, size);
    ASAN_READ_RANGE(to, size);
  }
  return real_memcpy(to, from, size);
}

INTERCEPTOR(void*, memmove, void *to, const void *from, size_t size) {
  ENSURE_ASAN_INITED();
  if (FLAG_replace_intrin) {
    ASAN_WRITE_RANGE(from, size);
    ASAN_READ_RANGE(to, size);
  }
  return real_memmove(to, from, size);
}

INTERCEPTOR(void*, memset, void *block, int c, size_t size) {
  // memset is called inside INTERCEPT_FUNCTION on Mac.
  if (asan_init_is_running) {
    return real_memset(block, c, size);
  }
  ENSURE_ASAN_INITED();
  if (FLAG_replace_intrin) {
    ASAN_WRITE_RANGE(block, size);
  }
  return real_memset(block, c, size);
}

INTERCEPTOR(char*, strchr, const char *str, int c) {
  ENSURE_ASAN_INITED();
  char *result = real_strchr(str, c);
  if (FLAG_replace_str) {
    size_t bytes_read = (result ? result - str : real_strlen(str)) + 1;
    ASAN_READ_RANGE(str, bytes_read);
  }
  return result;
}

#ifndef __APPLE__
INTERCEPTOR(void*, index, const char *string, int c)
  __attribute__((alias(WRAPPER_NAME(strchr))));
#else
DEFINE_REAL(void*, index, const char *string, int c);
#endif

INTERCEPTOR(int, strcasecmp, const char *s1, const char *s2) {
  ENSURE_ASAN_INITED();
  unsigned char c1, c2;
  size_t i;
  for (i = 0; ; i++) {
    c1 = (unsigned char)s1[i];
    c2 = (unsigned char)s2[i];
    if (CharCaseCmp(c1, c2) != 0 || c1 == '\0') break;
  }
  ASAN_READ_RANGE(s1, i + 1);
  ASAN_READ_RANGE(s2, i + 1);
  return CharCaseCmp(c1, c2);
}

INTERCEPTOR(char*, strcat, char *to, const char *from) {  // NOLINT
  ENSURE_ASAN_INITED();
  if (FLAG_replace_str) {
    size_t from_length = real_strlen(from);
    ASAN_READ_RANGE(from, from_length + 1);
    if (from_length > 0) {
      size_t to_length = real_strlen(to);
      ASAN_READ_RANGE(to, to_length);
      ASAN_WRITE_RANGE(to + to_length, from_length + 1);
      CHECK_RANGES_OVERLAP("strcat", to, to_length + 1, from, from_length + 1);
    }
  }
  return real_strcat(to, from);
}

INTERCEPTOR(int, strcmp, const char *s1, const char *s2) {
  if (!asan_inited) {
    return internal_strcmp(s1, s2);
  }
  unsigned char c1, c2;
  size_t i;
  for (i = 0; ; i++) {
    c1 = (unsigned char)s1[i];
    c2 = (unsigned char)s2[i];
    if (c1 != c2 || c1 == '\0') break;
  }
  ASAN_READ_RANGE(s1, i + 1);
  ASAN_READ_RANGE(s2, i + 1);
  return CharCmp(c1, c2);
}

INTERCEPTOR(char*, strcpy, char *to, const char *from) {  // NOLINT
  // strcpy is called from malloc_default_purgeable_zone()
  // in __asan::ReplaceSystemAlloc() on Mac.
  if (asan_init_is_running) {
    return real_strcpy(to, from);
  }
  ENSURE_ASAN_INITED();
  if (FLAG_replace_str) {
    size_t from_size = real_strlen(from) + 1;
    CHECK_RANGES_OVERLAP("strcpy", to, from_size, from, from_size);
    ASAN_READ_RANGE(from, from_size);
    ASAN_WRITE_RANGE(to, from_size);
  }
  return real_strcpy(to, from);
}

INTERCEPTOR(char*, strdup, const char *s) {
  ENSURE_ASAN_INITED();
  if (FLAG_replace_str) {
    size_t length = real_strlen(s);
    ASAN_READ_RANGE(s, length + 1);
  }
  return real_strdup(s);
}

INTERCEPTOR(size_t, strlen, const char *s) {
  // strlen is called from malloc_default_purgeable_zone()
  // in __asan::ReplaceSystemAlloc() on Mac.
  if (asan_init_is_running) {
    return real_strlen(s);
  }
  ENSURE_ASAN_INITED();
  size_t length = real_strlen(s);
  if (FLAG_replace_str) {
    ASAN_READ_RANGE(s, length + 1);
  }
  return length;
}

INTERCEPTOR(int, strncasecmp, const char *s1, const char *s2, size_t n) {
  ENSURE_ASAN_INITED();
  unsigned char c1 = 0, c2 = 0;
  size_t i;
  for (i = 0; i < n; i++) {
    c1 = (unsigned char)s1[i];
    c2 = (unsigned char)s2[i];
    if (CharCaseCmp(c1, c2) != 0 || c1 == '\0') break;
  }
  ASAN_READ_RANGE(s1, Min(i + 1, n));
  ASAN_READ_RANGE(s2, Min(i + 1, n));
  return CharCaseCmp(c1, c2);
}

INTERCEPTOR(int, strncmp, const char *s1, const char *s2, size_t size) {
  // strncmp is called from malloc_default_purgeable_zone()
  // in __asan::ReplaceSystemAlloc() on Mac.
  if (asan_init_is_running) {
    return real_strncmp(s1, s2, size);
  }
  unsigned char c1 = 0, c2 = 0;
  size_t i;
  for (i = 0; i < size; i++) {
    c1 = (unsigned char)s1[i];
    c2 = (unsigned char)s2[i];
    if (c1 != c2 || c1 == '\0') break;
  }
  ASAN_READ_RANGE(s1, Min(i + 1, size));
  ASAN_READ_RANGE(s2, Min(i + 1, size));
  return CharCmp(c1, c2);
}

INTERCEPTOR(char*, strncpy, char *to, const char *from, size_t size) {
  ENSURE_ASAN_INITED();
  if (FLAG_replace_str) {
    size_t from_size = Min(size, internal_strnlen(from, size) + 1);
    CHECK_RANGES_OVERLAP("strncpy", to, from_size, from, from_size);
    ASAN_READ_RANGE(from, from_size);
    ASAN_WRITE_RANGE(to, size);
  }
  return real_strncpy(to, from, size);
}

#ifndef __APPLE__
INTERCEPTOR(size_t, strnlen, const char *s, size_t maxlen) {
  ENSURE_ASAN_INITED();
  size_t length = real_strnlen(s, maxlen);
  if (FLAG_replace_str) {
    ASAN_READ_RANGE(s, Min(length + 1, maxlen));
  }
  return length;
}
#endif

// ---------------------- InitializeAsanInterceptors ---------------- {{{1
namespace __asan {
void InitializeAsanInterceptors() {
#ifndef __APPLE__
  INTERCEPT_FUNCTION(index);
#else
  OVERRIDE_FUNCTION(index, WRAP(strchr));
#endif
  INTERCEPT_FUNCTION(memcmp);
  INTERCEPT_FUNCTION(memmove);
#ifdef __APPLE__
  // Wrap memcpy() on OS X 10.6 only, because on 10.7 memcpy() and memmove()
  // are resolved into memmove$VARIANT$sse42.
  // See also http://code.google.com/p/address-sanitizer/issues/detail?id=34.
  // TODO(glider): need to check dynamically that memcpy() and memmove() are
  // actually the same function.
  if (GetMacosVersion() == MACOS_VERSION_SNOW_LEOPARD) {
    INTERCEPT_FUNCTION(memcpy);
  } else {
    real_memcpy = real_memmove;
  }
#else
  // Always wrap memcpy() on non-Darwin platforms.
  INTERCEPT_FUNCTION(memcpy);
#endif
  INTERCEPT_FUNCTION(memset);
  INTERCEPT_FUNCTION(strcasecmp);
  INTERCEPT_FUNCTION(strcat);  // NOLINT
  INTERCEPT_FUNCTION(strchr);
  INTERCEPT_FUNCTION(strcmp);
  INTERCEPT_FUNCTION(strcpy);  // NOLINT
  INTERCEPT_FUNCTION(strdup);
  INTERCEPT_FUNCTION(strlen);
  INTERCEPT_FUNCTION(strncasecmp);
  INTERCEPT_FUNCTION(strncmp);
  INTERCEPT_FUNCTION(strncpy);

  INTERCEPT_FUNCTION(sigaction);
  INTERCEPT_FUNCTION(signal);
  INTERCEPT_FUNCTION(longjmp);
  INTERCEPT_FUNCTION(_longjmp);
  INTERCEPT_FUNCTION_IF_EXISTS(__cxa_throw);
  INTERCEPT_FUNCTION(pthread_create);

#ifdef __APPLE__
  INTERCEPT_FUNCTION(dispatch_async_f);
  INTERCEPT_FUNCTION(dispatch_sync_f);
  INTERCEPT_FUNCTION(dispatch_after_f);
  INTERCEPT_FUNCTION(dispatch_barrier_async_f);
  INTERCEPT_FUNCTION(dispatch_group_async_f);
  // We don't need to intercept pthread_workqueue_additem_np() to support the
  // libdispatch API, but it helps us to debug the unsupported functions. Let's
  // intercept it only during verbose runs.
  if (FLAG_v >= 2) {
    INTERCEPT_FUNCTION(pthread_workqueue_additem_np);
  }
  // Normally CFStringCreateCopy should not copy constant CF strings.
  // Replacing the default CFAllocator causes constant strings to be copied
  // rather than just returned, which leads to bugs in big applications like
  // Chromium and WebKit, see
  // http://code.google.com/p/address-sanitizer/issues/detail?id=10
  // Until this problem is fixed we need to check that the string is
  // non-constant before calling CFStringCreateCopy.
  INTERCEPT_FUNCTION(CFStringCreateCopy);
#else
  // On Darwin siglongjmp tailcalls longjmp, so we don't want to intercept it
  // there.
  INTERCEPT_FUNCTION(siglongjmp);
#endif

#ifndef __APPLE__
  INTERCEPT_FUNCTION(strnlen);
#endif
  if (FLAG_v > 0) {
    Printf("AddressSanitizer: libc interceptors initialized\n");
  }
}

}  // namespace __asan
