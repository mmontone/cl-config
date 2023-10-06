#include "cfgcli.h"

SCM cfgcli_colon_parse(arguments,schema)
SCM arguments,schema;
{
  return apply(GLOBAL(error),const_1,listofnull);
}

SCM cfgcli_colon_help(schema)
SCM schema;
{
  return apply(GLOBAL(error),const_2,listofnull);
}

SCM cfgcli_colon_help_setting(schema,setting)
SCM schema,setting;
{
  return apply(GLOBAL(error),const_3,listofnull);
}

SCM cfgcli_colon_apropos(schema,setting)
SCM schema,setting;
{
  return apply(GLOBAL(error),const_4,listofnull);
}

SCM top_actions_1_cfgcli()
{
  make_subr((char *)"cfgcli:parse",tc7_subr_2,cfgcli_colon_parse);
  make_subr((char *)"cfgcli:help",tc7_subr_1,cfgcli_colon_help);
  make_subr((char *)"cfgcli:help-setting",tc7_subr_2,cfgcli_colon_help_setting);
  return make_subr((char *)"cfgcli:apropos",tc7_subr_2,cfgcli_colon_apropos);
}

SCM top_actions_cfgcli()
{
  return top_actions_1_cfgcli();
}

SCM init_1_cfgcli()
{
  error=&CDR(intern((char *)"error",5));
  const_1=scm_gc_protect(makfromstr((char *)"TODO",4));
  const_2=scm_gc_protect(makfromstr((char *)"TODO",4));
  const_3=scm_gc_protect(makfromstr((char *)"TODO",4));
  const_4=scm_gc_protect(makfromstr((char *)"TODO",4));
  top_actions_cfgcli();
}

SCM init_cfgcli()
{
  no_symhash_gc=1;
  init_1_cfgcli();
}

