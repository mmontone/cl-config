#include <stdio.h>
#include "libcfg.h"

int main (int argc, const char **argv) {

  cfg_load_lib(argc, argv);

  long my_config = cfg_create("My config");
  long my_schema = cfg_create_schema("My config schema");

  cfg_set_schema_doc(my_schema, "This is a config schema");

  cfg_set(my_config, "host", "localhost");
  cfg_set(my_config, "name", "app");
  cfg_print(my_config);

  char* host = cfg_get(my_config, "host");
  printf("%s", host);

  cfg_cli_help(my_schema);
 
  return 0;
}
