/* gcc -o libtest libtest.c libscm.a -ldl -lm -lc */
/*#include "scm.h"*/
#include "cfg.h"
/* include patchlvl.h for SCM's INIT_FILE_NAME. */
#include "patchlvl.h"

/*
char* scm2str(SCM str) {
  sizet len = 1 + LENGTH(str);
  char *dst = (char *)must_malloc((long)len, s_string);
  return CHARS(CAR(args));
}
*/

long cfg_create (char* name) {
  return make_config(makfrom0str(name));
}

char* cfg_name (long config) {
  return CHARS(apply(GLOBAL(config_name),config,listofnull));
}

char* cfg_get (long config, char* key) {
  return CHARS(config_get(config, makfrom0str(key)));
}

void cfg_set(long config, char* key, char* value) {
  config_set_excl_(config, makfrom0str(key), makfrom0str(value));
}

void cfg_print(long config) {
  print_config(config);
}

void cfg_load_lib (int argc, const char **argv)
{
  scm_init_from_argv(argc, argv, 0L, 0, 0);

  init_cfg();

  /* final_scm(!0);*/  
}
