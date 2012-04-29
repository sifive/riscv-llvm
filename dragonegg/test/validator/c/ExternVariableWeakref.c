// RUN: %dragonegg -S %s -o - | FileCheck %s

static int variable_weakref __attribute__ ((weakref("bar")));
int *use_variable = &variable_weakref;

// CHECK: @bar = extern_weak global i32
// CHECK: @variable_weakref = alias weak i32* @bar
