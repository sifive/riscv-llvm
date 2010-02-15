#include <stdlib.h>

bool flag_odr = false;

int ix86_regparm;

union tree_node;

extern "C" bool contains_aligned_value_p (union tree_node *type) {
  abort();
}
