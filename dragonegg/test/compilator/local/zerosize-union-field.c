typedef unsigned int Foo __attribute__((aligned(32)));
typedef union{Foo:0;char b;}a;
typedef union{int x; Foo:0;}b;

a A;
b B;
