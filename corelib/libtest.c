/* gcc -o libtest libtest.c libscm.a -ldl -lm -lc */
/*#include "scm.h"*/
#include "cfg.h"
/* include patchlvl.h for SCM's INIT_FILE_NAME. */
#include "patchlvl.h"


void libtest_init_user_scm()
{
  fputs("This is libtest_init_user_scm\n", stderr); fflush(stderr);
  sysintern("*the-string*", makfrom0str("hello world\n"));
}

SCM user_main()
{
  static int done = 0;
  SCM config;
  if (done++) return MAKINUM(EXIT_FAILURE);
  //scm_ldstr("(display 12345)");
  //scm_ldstr("(hello-world)");
  //config = scm_ldstr("(make-config \"myconfig\")");
  
  return MAKINUM(EXIT_SUCCESS);
}

int main(argc, argv)
     int argc;
     const char **argv;
{
  SCM retval;
  /*char *implpath, *execpath;

  init_user_scm = libtest_init_user_scm;
  execpath = dld_find_executable(argv[0]);
  fprintf(stderr, "dld_find_executable(%s): %s\n", argv[0], execpath);
  implpath = find_impl_file(execpath, "scm", INIT_FILE_NAME, dirsep);
  fprintf(stderr, "implpath: %s\n", implpath);*/

  scm_init_from_argv(argc, argv, 0L, 0, 0);

  init_cfg();

  hello_world();

  SCM config = make_config(makfrom0str("My config!"));
  config_set_excl_(config, makfrom0str("host"), makfrom0str("localhost"));
  print_config(config);

  /*retval = scm_top_level(implpath, user_main);*/

  final_scm(!0);
  return (int)INUM(retval);
}
