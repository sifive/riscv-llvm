// RUN: %dragonegg -S -o /dev/null %s -O1
// GCC PR c++/11878

struct A
{
    virtual ~A();
};

template<typename T> struct B
{
    T t;
};

void foo() { throw B<A>().t; }
