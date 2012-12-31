//===----- TargetInfo.cpp - Utility for getting info about the target -----===//
//
// Copyright (C) 2009 to 2013  Duncan Sands.
//
// This file is part of DragonEgg.
//
// DragonEgg is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later version.
//
// DragonEgg is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// DragonEgg; see the file COPYING.  If not, write to the Free Software
// Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA.
//
//===----------------------------------------------------------------------===//
// Utility program for getting information about the system that GCC targets.
//===----------------------------------------------------------------------===//

#include <llvm/ADT/Triple.h>
#include <iostream>

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
