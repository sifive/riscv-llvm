#include <string>
void g(std::string &X);
void bof(const char *c, size_t d) {
  std::string X(c, d);
  g(X);
}
