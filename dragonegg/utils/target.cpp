#include <cstring>
#include <iostream>

#include <llvm/ADT/Triple.h>

using namespace llvm;

static void PrintTriple(Triple &T) {
  std::cout << T.getTriple() << "\n";
}
static void PrintArchName(Triple &T) {
  std::cout << T.getArchName().str() << "\n";
}
static void PrintVendorName(Triple &T) {
  std::cout << T.getVendorName().str() << "\n";
}
static void PrintOSTypeName(Triple &T) {
  std::cout << T.getOSTypeName(T.getOS()) << "\n";
}
static void PrintEnvironmentName(Triple &T) {
  std::cout << T.getEnvironmentName().str() << "\n";
}
static void PrintOSAndEnvironmentName(Triple &T) {
  std::cout << T.getOSAndEnvironmentName().str() << "\n";
}
static void PrintArchTypePrefix(Triple &T) {
  std::cout << T.getArchTypePrefix(T.getArch()) << "\n";
}

struct Option {
  const char *Name;
  void (*Action)(Triple &);
};

static Option Options[] = {
  { "-t", PrintTriple },
  { "-a", PrintArchName },
  { "-v", PrintVendorName },
  { "-o", PrintOSTypeName },
  { "-e", PrintEnvironmentName },
  { "-oe", PrintOSAndEnvironmentName },
  { "-p", PrintArchTypePrefix },
  { NULL, NULL }
};

int main(int argc, char **argv) {
  Triple T(TARGET_NAME);

  for (int i = 1; i < argc; ++i) {
    bool Found = false;
    for (Option *O = Options; O->Name; ++O)
      if (!strcmp(argv[i], O->Name)) {
        Found = true;
        O->Action(T);
        break;
      }
    if (!Found) {
      std::cerr << "Unknown option \"" << argv[i] << "\"\n";
      std::cerr << "Usage: " << argv[0];
      for (Option *O = Options; O->Name; ++O)
        std::cerr << " " << O->Name;
      std::cerr << "\n";
      return 1;
    }
  }

  return 0;
}
