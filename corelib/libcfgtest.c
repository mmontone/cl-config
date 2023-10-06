#include <stdio.h>
#include "libcfg.h"

int main (int argc, const char **argv) {

  cfg_load_lib(argc, argv);
  long my_config = cfg_create("My config");
  cfg_set(my_config, "host", "localhost");
  cfg_set(my_config, "name", "app");
  cfg_print(my_config);

  char* host = cfg_get(my_config, "host");
  printf("%s", host);
  
  return 0;
}
