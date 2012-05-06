// RUN: %dragonegg -S %s -o - | FileCheck %s
#include <stdint.h>
char *a = (void*)(uintptr_t)(void*)&a;
// CHECK: @a = global i8* bitcast (i8** @a to i8*)
