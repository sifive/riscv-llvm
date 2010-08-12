#include <cstring>
#include <iostream>

#include <llvm/ADT/Triple.h>

using namespace llvm;

static void PrintTriple(Triple &T) {
  std::cout << T.getTriple() << "\n";
}
static void PrintArchName(Triple &T) {
  std::cout << T.getArchTypeName(T.getArch()) << "\n";
}
static void PrintVendorName(Triple &T) {
  std::cout << T.getVendorTypeName(T.getVendor()) << "\n";
}
static void PrintOSName(Triple &T) {
  std::cout << T.getOSTypeName(T.getOS()) << "\n";
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
  { "-o", PrintOSName },
  { "-p", PrintArchTypePrefix },
  { NULL, NULL }
};

int main(int argc, char **argv) {
  Triple T(Triple::normalize(TARGET_TRIPLE));

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
