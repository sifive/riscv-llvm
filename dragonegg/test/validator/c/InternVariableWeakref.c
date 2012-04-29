// RUN: %dragonegg -S %s -o - | FileCheck %s

int bar;
static int variable_weakref __attribute__ ((weakref("bar")));
int *use_variable = &variable_weakref;

// CHECK: @variable_weakref = alias weak i32* @bar
