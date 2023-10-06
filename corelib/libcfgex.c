#include "libcfg.h"

int main () {
  
  long my_config = cfg_create("My config");
  cfg_set(my_config, "host", "localhost");
  cfg_print(my_config);
  
  return 0;
}
