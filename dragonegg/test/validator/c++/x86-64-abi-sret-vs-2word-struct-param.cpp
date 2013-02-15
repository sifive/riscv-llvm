// RUN: %dragonegg -S %s -o - | FileCheck %s
// XFAIL: i386, i486, i586, i686

struct S {
    void* data[3];
};

struct T {
    void* data[2];
};

extern "C" S fail(int, int, int, int, T t, void* p) {
// CHECK: %struct.T* byval align 8
    S s;
    s.data[0] = t.data[0];
    s.data[1] = t.data[1];
    s.data[2] = p;
    return s;
}

extern "C" S* succeed(S* sret, int, int, int, int, T t, void* p) {
// CHECK: %struct.T* byval align 8
    sret->data[0] = t.data[0];
    sret->data[1] = t.data[1];
    sret->data[2] = p;
    return sret;
}
