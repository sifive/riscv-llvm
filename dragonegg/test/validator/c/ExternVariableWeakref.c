// RUN: %dragonegg -S %s -o - | FileCheck %s

static int variable_weakref __attribute__ ((weakref("bar")));
int *use_variable = &variable_weakref;

// CHECK: @use_variable = unnamed_addr global i32* @bar
// CHECK: @bar = extern_weak global i32
