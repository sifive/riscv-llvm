// RUN: %dragonegg -S -o - %s -fstack-protector --param ssp-buffer-size=1 | FileCheck --check-prefix=SP %s
// RUN: %dragonegg -S -o - %s | FileCheck --check-prefix=NP %s
void foo(void) {}
// SP: attributes {{.*}} "stack-protector-buffer-size"="1"
// NP-NOT: stack-protector
