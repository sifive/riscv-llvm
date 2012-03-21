// RUN: %dragonegg %s -S -o - | FileCheck %s

struct QVectorTypedData {
    int size;
    unsigned int sharable : 1;
    unsigned short array[1];
};

void foo(QVectorTypedData *X) {
  X->array[0] = 123;
}
// CHECK-NOT: i32 6
