// LLVM headers
#include "llvm/Constant.h"
#include "llvm/Value.h"

// System headers
#include <gmp.h>

// GCC headers
#undef VISIBILITY_HIDDEN

extern "C" {
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
}

using namespace llvm;

bool flag_odr = false;

int ix86_regparm;

extern "C" bool contains_aligned_value_p (tree type) {
abort();
}
