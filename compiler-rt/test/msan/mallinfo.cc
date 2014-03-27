// RUN: %clangxx_msan -m64 -O0 -g %s -o %t && %t

#include <assert.h>
#include <malloc.h>

#include <sanitizer/msan_interface.h>

int main(void) {
  struct mallinfo mi = mallinfo();
  assert(__msan_test_shadow(&mi, sizeof(mi)) == -1);
  return 0;
}
