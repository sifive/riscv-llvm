// RUN: %dragonegg -S %s -o - | FileCheck %s

static void function_weakref(void) __attribute__ ((weakref("foo")));
void *use_function = (void *)function_weakref;

// CHECK: @use_function = unnamed_addr global void ()* @foo
// CHECK: declare extern_weak void @foo()
