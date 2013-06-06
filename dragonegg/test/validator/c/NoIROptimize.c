// RUN: %dragonegg -S -O3 -fplugin-arg-dragonegg-llvm-ir-optimize=0 -fplugin-arg-dragonegg-debug-pass-arguments %s 2>&1 | FileCheck %s

// CHECK-NOT: inline
