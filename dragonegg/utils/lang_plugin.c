#include "gcc-plugin.h"

int plugin_is_GPL_compatible;

/* Use a symbol defined inside gcc.  */
extern int toplev_main (int argc, char **argv);
int (*use) (int argc, char **argv) = toplev_main;

int plugin_init(struct plugin_name_args *plugin_info,
                struct plugin_gcc_version *version) {
  return 0;
}
