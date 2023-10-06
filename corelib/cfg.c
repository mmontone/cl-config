#include "cfg.h"

SCM make_config(name)
SCM name;
{
  return cons(cons(name_symb,name),cons(cons(parent_symb,BOOL_F),cons(cons(settings_symb,EOL),cons(cons(schema_symb,BOOL_F),cons(cons(options_symb,EOL),EOL)))));
}

SCM config_name(config)
SCM config;
{
  return CDR(assoc(name_symb,config));
}

SCM settings(config)
SCM config;
{
  return CDR(assoc(settings_symb,config));
}

SCM assoc_set_excl_(lst,key,value)
SCM lst,key,value;
{
  SCM ass;

  ass=assoc(key,lst);
  if (NFALSEP(ass)) {
    SET_CDR(ass,value);
    return lst;
  }
  else
    return append2(lst,cons(cons(key,value),EOL));
}

SCM assoc_get(lst,key)
SCM lst,key;
{
  return CDR(assoc(key,lst));
}

SCM config_set_excl_(config,attribute_name,value)
SCM config,attribute_name,value;
{
  return assoc_set_excl_(config,settings_symb,assoc_set_excl_(settings(config),attribute_name,value));
}

SCM config_get(config,name)
SCM config,name;
{
  return assoc_get(settings(config),name);
}

SCM print_config(config)
SCM config;
{
  scm_display(config_name(config),cur_output_port());
  scm_newline(cur_output_port());
  scm_newline(cur_output_port());
  return for_each1(print_config_fn2,settings(config));
}

SCM print_config_fn2(attr)
SCM attr;
{
  scm_display(CAR(attr),cur_output_port());
  scm_display(const_1,cur_output_port());
  scm_display(CDR(attr),cur_output_port());
  return scm_newline(cur_output_port());
}

SCM cfg_colon_validate(config,schema)
SCM config,schema;
{
  SCM new_var1,schema__1,new_var2;

  new_var1=schema;
  if (NFALSEP(new_var1))
    schema__1=new_var1;
  else {
    new_var2=config_schema(config);
    if (NFALSEP(new_var2))
      schema__1=new_var2;
    else
      schema__1=apply(GLOBAL(error),const_2,listofnull);
  }
  return apply(GLOBAL(error),const_3,listofnull);
}

SCM config_schema(config)
SCM config;
{
  return assoc_get(config,schema_symb);
}

SCM config_parent(config)
SCM config;
{
  return assoc_get(config,parent_symb);
}

SCM set_parent_excl_(config,parent)
SCM config,parent;
{
  return assoc_set_excl_(config,parent_symb,parent);
}

SCM register_config(config)
SCM config;
{
  GLOBAL(configs_global)=assoc_set_excl_(GLOBAL(configs_global),config_name(config),config);
  return UNSPECIFIED;
}

SCM find_config(name)
SCM name;
{
  return assoc_get(GLOBAL(configs_global),name);
}

SCM cfg_colon_save(config)
SCM config;
{
  return apply(GLOBAL(error),const_4,listofnull);
}

SCM cfg_colon_load(source)
SCM source;
{
  return apply(GLOBAL(error),const_5,listofnull);
}

SCM hello_world()
{
  return scm_display(const_6,cur_output_port());
}

SCM top_actions_1_cfg()
{
  GLOBAL(schemas_global)=EOL;
  GLOBAL(configs_global)=EOL;
  make_subr((char *)"make-config",tc7_subr_1,make_config);
  make_subr((char *)"config-name",tc7_subr_1,config_name);
  GLOBAL(settings_clproc0)=make_subr((char *)"settings_clproc0",tc7_subr_1,settings);
  make_subr((char *)"assoc-set!",tc7_subr_3,assoc_set_excl_);
  make_subr((char *)"assoc-get",tc7_subr_2,assoc_get);
  make_subr((char *)"config-set!",tc7_subr_3,config_set_excl_);
  make_subr((char *)"config-get",tc7_subr_2,config_get);
  make_subr((char *)"print-config",tc7_subr_1,print_config);
  make_subr((char *)"cfg:validate",tc7_subr_2,cfg_colon_validate);
  make_subr((char *)"config-schema",tc7_subr_1,config_schema);
  make_subr((char *)"config-parent",tc7_subr_1,config_parent);
  make_subr((char *)"set-parent!",tc7_subr_2,set_parent_excl_);
  make_subr((char *)"register-config",tc7_subr_1,register_config);
  make_subr((char *)"find-config",tc7_subr_1,find_config);
  make_subr((char *)"cfg:save",tc7_subr_1,cfg_colon_save);
  make_subr((char *)"cfg:load",tc7_subr_1,cfg_colon_load);
  return make_subr((char *)"hello-world",tc7_subr_0,hello_world);
}

SCM top_actions_cfg()
{
  return top_actions_1_cfg();
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

SCM init_1_cfg()
{
  error=&CDR(intern((char *)"error",5));
  settings_clproc0=&CDR(intern((char *)"settings",8));
  configs_global=&CDR(intern((char *)"*configs*",9));
  schemas_global=&CDR(intern((char *)"*schemas*",9));
  name_symb=scm_gc_protect(CAR(intern((char *)"name",4)));
  parent_symb=scm_gc_protect(CAR(intern((char *)"parent",6)));
  settings_symb=scm_gc_protect(CAR(intern((char *)"settings",8)));
  schema_symb=scm_gc_protect(CAR(intern((char *)"schema",6)));
  options_symb=scm_gc_protect(CAR(intern((char *)"options",7)));
  const_1=scm_gc_protect(makfromstr((char *)": ",2));
  const_2=scm_gc_protect(makfromstr((char *)"No config schema",16));
  const_3=scm_gc_protect(makfromstr((char *)"TODO",4));
  const_4=scm_gc_protect(makfromstr((char *)"TODO",4));
  const_5=scm_gc_protect(makfromstr((char *)"TODO",4));
  const_6=scm_gc_protect(makfromstr((char *)"hello world",11));
  top_actions_cfg();
}

SCM init_cfg()
{
  no_symhash_gc=1;
  init_1_cfg();
}

