#include <stdio.h>
#include "libcfg.h"

int main (int argc, const char **argv) {

  cfg_load_lib(argc, argv);

  long my_schema = cfg_create_schema("My config schema");
  cfg_set_schema_doc(my_schema, "This is a config schema");

  long host_s = cfg_add_string_setting(my_schema, "host");
  cfg_setting_set_doc(host_s, "The host.");
  cfg_setting_set_string_default(host_s,"localhost");

  long port_s = cfg_add_integer_setting(my_schema, "port");
  cfg_setting_set_doc(port_s, "The web port");
  //cfg_setting_set_integer_default(port_s, 80);

  char* choices[] = {"foo", "bar", "baz"};
  long choices_s = cfg_add_choice_setting(my_schema, "choices", choices);
  cfg_setting_set_doc(choices_s, "Choice one of foo, bar or baz.");


  long my_config = cfg_create("My config");
  cfg_set(my_config, "host", "localhost");
  cfg_set(my_config, "name", "app");
  cfg_print(my_config);

  if (cfg_validate_with_schema(my_config, my_schema) != 0) {
    puts("Not valid");
    return 1;
  } else {
    puts("Valid");
    return 0;
  }
}
