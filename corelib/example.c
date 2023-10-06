#include "example.h"

SCM make_config(name)
SCM name;
{
  return cons(cons(name_symb,name),cons(cons(attributes_symb,EOL),cons(cons(options_symb,EOL),EOL)));
}

SCM config_name(config)
SCM config;
{
  return CDR(assoc(name_symb,config));
}

SCM attributes(config)
SCM config;
{
  return CDR(assoc(attributes_symb,config));
}

SCM assoc_set_excl_(ls,key,value)
SCM ls,key,value;
{
  SCM ass;

  ass=assoc(key,ls);
  if (NFALSEP(ass)) {
    SET_CDR(ass,value);
    return ls;
  }
  else
    return append2(ls,cons(cons(key,value),EOL));
}

SCM set_attribute_excl_(config,attribute_name,value)
SCM config,attribute_name,value;
{
  return assoc_set_excl_(config,attributes_symb,assoc_set_excl_(attributes(config),attribute_name,value));
}

SCM print_config(config)
SCM config;
{
  scm_display(config_name(config),cur_output_port());
  scm_newline(cur_output_port());
  scm_newline(cur_output_port());
  return for_each1(print_config_fn2,attributes(config));
}

SCM print_config_fn2(attr)
SCM attr;
{
  scm_display(CAR(attr),cur_output_port());
  scm_display(const_1,cur_output_port());
  scm_display(CDR(attr),cur_output_port());
  return scm_newline(cur_output_port());
}

SCM hello_world()
{
  return scm_display(const_2,cur_output_port());
}

SCM top_actions_1_example()
{
  make_subr((char *)"make-config",tc7_subr_1,make_config);
  make_subr((char *)"config-name",tc7_subr_1,config_name);
  GLOBAL(attributes_clproc0)=make_subr((char *)"attributes_clproc0",tc7_subr_1,attributes);
  make_subr((char *)"assoc-set!",tc7_subr_3,assoc_set_excl_);
  make_subr((char *)"set-attribute!",tc7_subr_3,set_attribute_excl_);
  make_subr((char *)"print-config",tc7_subr_1,print_config);
  return make_subr((char *)"hello-world",tc7_subr_0,hello_world);
}

SCM top_actions_example()
{
  return top_actions_1_example();
}

SCM for_each1(fn,lst)
SCM (*fn) ();
SCM lst;
{
  for (;PAIR_P(lst);) {
    (*fn)(CAR(lst));
    lst=CDR(lst);
  }
  return UNSPECIFIED;
}

SCM init_1_example()
{
  attributes_clproc0=&CDR(intern((char *)"attributes",10));
  name_symb=scm_gc_protect(CAR(intern((char *)"name",4)));
  attributes_symb=scm_gc_protect(CAR(intern((char *)"attributes",10)));
  options_symb=scm_gc_protect(CAR(intern((char *)"options",7)));
  const_1=scm_gc_protect(makfromstr((char *)": ",2));
  const_2=scm_gc_protect(makfromstr((char *)"hello world",11));
  top_actions_example();
}

SCM init_example()
{
  no_symhash_gc=1;
  init_1_example();
}

