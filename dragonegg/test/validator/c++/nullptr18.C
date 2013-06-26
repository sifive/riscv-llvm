// RUN: %dragonegg -xc++ -S -std=c++0x -o /dev/null %s
// XFAIL: gcc-4.5
// PR14777
template <typename T, typename U> struct tType_equal;
template <typename T> struct tType_equal<T, T> { typedef void type; };

template <typename T, typename U>
inline typename tType_equal<T, U>::type
type_equal(U) { }

char* j( char* );
bool j(  bool );

void test_j()
{
  type_equal<char*>(j(nullptr));
  decltype(nullptr) mynull = 0;
  type_equal<char*>(j(mynull));
}
