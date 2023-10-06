#include "cfg.h"

SCM call_with_optional_args(args,defaults,proc)
SCM args,defaults,proc;
{
  SCM i;

  if (length(args)>length(defaults))
    apply(GLOBAL(error),const_1,listofnull);
  i=MAKINUM(0);
  return apply(GLOBAL(apply_nonkeyword),proc,cons(map1_inst1(&i,args,call_with_optional_args_fn2,defaults),listofnull));
}

SCM call_with_optional_args_fn2(i,args,default_nonkeyword)
SCM *i;
SCM args,default_nonkeyword;
{
  SCM val;

  if (apply(GLOBAL(nonum_prefix_1_plus_),*i,listofnull)<=length(args))
    val=list_ref(args,*i);
  else
    val=default_nonkeyword;
  *i=apply(GLOBAL(nonum_prefix_1_plus_),*i,listofnull);
  return val;
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

SCM assoc_get(lst,key,default_nonkeyword)
SCM lst,key,default_nonkeyword;
{
  SCM pair;

  pair=assoc(key,lst);
  if (PAIR_P(pair))
    return apply(GLOBAL(values),CDR(pair),cons(BOOL_T,listofnull));
  else
    return apply(GLOBAL(values),((!(NULL_P(default_nonkeyword))&&NFALSEP(procedurep(CAR(default_nonkeyword)))) ? apply(CAR(default_nonkeyword),EOL,EOL) : BOOL_F),cons(BOOL_F,listofnull));
}

SCM make_config(name)
SCM name;
{
  SCM cfg;

  cfg=apply(GLOBAL(_percent_make_config),name,listofnull);
  apply(GLOBAL(set_config_doc_excl_),cfg,cons(BOOL_F,listofnull));
  apply(GLOBAL(set_config_schema_excl_),cfg,cons(BOOL_F,listofnull));
  apply(GLOBAL(set_config_settings_excl_),cfg,cons(EOL,listofnull));
  apply(GLOBAL(set_config_parent_excl_),cfg,cons(BOOL_F,listofnull));
  apply(GLOBAL(set_config_options_excl_),cfg,cons(EOL,listofnull));
  return cfg;
}

SCM make_schema(name)
SCM name;
{
  return cons(cons(type_symb,cfg_colon_schema_symb),cons(cons(name_symb,name),cons(cons(settings_symb,EOL),EOL)));
}

SCM schema_pred_(x)
SCM x;
{
  if (NFALSEP(listp(x)))
    return SBOOL(assoc_get(x,type_symb,EOL)==cfg_colon_schema_symb);
  else
    return BOOL_F;
}

SCM make_setting(name,type,options)
SCM name,type,options;
{
  return cons(name,cons(type,cons(options,EOL)));
}

SCM config_set_excl_(config,attribute_name,value)
SCM config,attribute_name,value;
{
  return apply(GLOBAL(set_config_settings_excl_),config,cons(assoc_set_excl_(apply(GLOBAL(settings),config,listofnull),attribute_name,value),listofnull));
}

SCM config_get(config,name)
SCM config,name;
{
  SCM newclosure;

  return apply(GLOBAL(call_with_values),(newclosure=makcclo(config_get_cl1_clproc0,3),VECTOR_SET(newclosure,MAKINUM(1),config),VECTOR_SET(newclosure,MAKINUM(2),name),newclosure),cons(config_get_cl2_clproc0,listofnull));
}

SCM config_get_cl2(x,y)
SCM x,y;
{
  return x;
}

SCM config_get_cl1(closurearg_0)
SCM closurearg_0;
{
  SCM closurearg_car_0,config,name;

  closurearg_car_0=CAR(closurearg_0);
  config=VECTOR_REF(closurearg_car_0,MAKINUM(1));
  name=VECTOR_REF(closurearg_car_0,MAKINUM(2));
  return assoc_get(apply(GLOBAL(settings),config,listofnull),name,cons(config_get_cl1_cl3_clproc0,EOL));
}

SCM config_get_cl1_cl3()
{
  return apply(GLOBAL(error),const_2,listofnull);
}

SCM print_config(config)
SCM config;
{
  scm_display(apply(GLOBAL(config_name),config,listofnull),cur_output_port());
  scm_newline(cur_output_port());
  scm_newline(cur_output_port());
  return for_each1(print_config_fn4,apply(GLOBAL(settings),config,listofnull));
}

SCM print_config_fn4(attr)
SCM attr;
{
  scm_display(CAR(attr),cur_output_port());
  scm_display(const_3,cur_output_port());
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
    new_var2=apply(GLOBAL(config_schema),config,listofnull);
    if (NFALSEP(new_var2))
      schema__1=new_var2;
    else
      schema__1=apply(GLOBAL(error),const_4,listofnull);
  }
  return apply(GLOBAL(error),const_5,listofnull);
}

SCM register_config(config)
SCM config;
{
  GLOBAL(configs_global)=assoc_set_excl_(GLOBAL(configs_global),apply(GLOBAL(config_name),config,listofnull),config);
  return UNSPECIFIED;
}

SCM find_config(name)
SCM name;
{
  return assoc_get(GLOBAL(configs_global),name,EOL);
}

SCM cfg_colon_save(config,destination)
SCM config,destination;
{
  return apply(GLOBAL(error),const_6,listofnull);
}

SCM cfg_colon_load(source)
SCM source;
{
  return apply(GLOBAL(error),const_7,listofnull);
}

SCM hello_world()
{
  return scm_display(const_8,cur_output_port());
}

SCM top_actions_1_cfg()
{
  GLOBAL(schemas_global)=EOL;
  GLOBAL(configs_global)=EOL;
  make_subr((char *)"call-with-optional-args",tc7_subr_3,call_with_optional_args);
  make_subr((char *)"assoc-set!",tc7_subr_3,assoc_set_excl_);
  make_subr((char *)"assoc-get",tc7_lsubr,assoc_get_wrapper);
  apply(GLOBAL(define_record_type),GLOBAL(_less_config_grtr_),cons(apply(GLOBAL(_percent_make_config),GLOBAL(name),listofnull),cons(GLOBAL(config_pred_),cons(apply(GLOBAL(name),GLOBAL(config_name),cons(GLOBAL(set_config_name_excl_),listofnull)),cons(apply(GLOBAL(doc),GLOBAL(config_doc),cons(GLOBAL(set_config_doc_excl_),listofnull)),cons(apply(GLOBAL(schema),GLOBAL(config_schema),cons(GLOBAL(set_config_schema_excl_),listofnull)),cons(apply(GLOBAL(settings),GLOBAL(settings),cons(GLOBAL(set_config_settings_excl_),listofnull)),cons(apply(GLOBAL(parent),GLOBAL(config_parent),cons(GLOBAL(set_config_parent_excl_),listofnull)),cons(apply(GLOBAL(options),GLOBAL(config_options),cons(GLOBAL(set_config_options_excl_),listofnull)),listofnull)))))))));
  make_subr((char *)"make-config",tc7_subr_1,make_config);
  make_subr((char *)"make-schema",tc7_subr_1,make_schema);
  make_subr((char *)"schema?",tc7_subr_1,schema_pred_);
  make_subr((char *)"make-setting",tc7_lsubr,make_setting_wrapper);
  make_subr((char *)"config-set!",tc7_subr_3,config_set_excl_);
  make_subr((char *)"config-get",tc7_subr_2,config_get);
  make_subr((char *)"print-config",tc7_subr_1,print_config);
  make_subr((char *)"cfg:validate",tc7_subr_2,cfg_colon_validate);
  make_subr((char *)"register-config",tc7_subr_1,register_config);
  make_subr((char *)"find-config",tc7_subr_1,find_config);
  make_subr((char *)"cfg:save",tc7_subr_2,cfg_colon_save);
  make_subr((char *)"cfg:load",tc7_subr_1,cfg_colon_load);
  return make_subr((char *)"hello-world",tc7_subr_0,hello_world);
}

SCM top_actions_cfg()
{
  return top_actions_1_cfg();
}

SCM map1(fn,lst)
SCM (*fn) ();
SCM lst;
{
  SCM res,res_end;

  res=EOL;
  res_end=res;
  for (;PAIR_P(lst);) {
    if (NULL_P(res)) {
      res=cons((*fn)(CAR(lst)),EOL);
      res_end=res;
    }
    else {
      SET_CDR(res_end,cons((*fn)(CAR(lst)),EOL));
      res_end=CDR(res_end);
    }
    lst=CDR(lst);
  }
  return res;
}

SCM map1_inst1(npar__1,npar__2,fn,lst)
SCM *npar__2;
SCM (*fn) ();
SCM npar__1,lst;
{
  SCM res,res_end;

  res=EOL;
  res_end=res;
  for (;PAIR_P(lst);) {
    if (NULL_P(res)) {
      res=cons((*fn)(npar__1,npar__2,CAR(lst)),EOL);
      res_end=res;
    }
    else {
      SET_CDR(res_end,cons((*fn)(npar__1,npar__2,CAR(lst)),EOL));
      res_end=CDR(res_end);
    }
    lst=CDR(lst);
  }
  return res;
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

SCM make_setting_wrapper(x)
SCM x;
{
  return make_setting(CAR(x),CAR(CDR(x)),CDR(CDR(x)));
}

SCM assoc_get_wrapper(x)
SCM x;
{
  return assoc_get(CAR(x),CAR(CDR(x)),CDR(CDR(x)));
}

SCM init_1_cfg()
{
  config_get_cl1_cl3_clproc0=make_subr((char *)"config-get_cl1_cl3",tc7_subr_0,config_get_cl1_cl3);
  config_get_cl2_clproc0=make_subr((char *)"config-get_cl2",tc7_subr_2,config_get_cl2);
  config_get_cl1_clproc0=make_subr((char *)"config-get_cl1_clproc0",tc7_lsubr,config_get_cl1);
  define_record_type=&CDR(intern((char *)"define-record-type",18));
  options=&CDR(intern((char *)"options",7));
  config_options=&CDR(intern((char *)"config-options",14));
  parent=&CDR(intern((char *)"parent",6));
  config_parent=&CDR(intern((char *)"config-parent",13));
  schema=&CDR(intern((char *)"schema",6));
  doc=&CDR(intern((char *)"doc",3));
  config_doc=&CDR(intern((char *)"config-doc",10));
  set_config_name_excl_=&CDR(intern((char *)"set-config-name!",16));
  config_pred_=&CDR(intern((char *)"config?",7));
  name=&CDR(intern((char *)"name",4));
  _less_config_grtr_=&CDR(intern((char *)"<config>",8));
  config_schema=&CDR(intern((char *)"config-schema",13));
  config_name=&CDR(intern((char *)"config-name",11));
  call_with_values=&CDR(intern((char *)"call-with-values",16));
  settings=&CDR(intern((char *)"settings",8));
  set_config_options_excl_=&CDR(intern((char *)"set-config-options!",19));
  set_config_parent_excl_=&CDR(intern((char *)"set-config-parent!",18));
  set_config_settings_excl_=&CDR(intern((char *)"set-config-settings!",20));
  set_config_schema_excl_=&CDR(intern((char *)"set-config-schema!",18));
  set_config_doc_excl_=&CDR(intern((char *)"set-config-doc!",15));
  _percent_make_config=&CDR(intern((char *)"%make-config",12));
  values=&CDR(intern((char *)"values",6));
  nonum_prefix_1_plus_=&CDR(intern((char *)"1+",2));
  apply_nonkeyword=&CDR(intern((char *)"apply",5));
  error=&CDR(intern((char *)"error",5));
  configs_global=&CDR(intern((char *)"*configs*",9));
  schemas_global=&CDR(intern((char *)"*schemas*",9));
  type_symb=scm_gc_protect(CAR(intern((char *)"type",4)));
  cfg_colon_schema_symb=scm_gc_protect(CAR(intern((char *)"cfg:schema",10)));
  name_symb=scm_gc_protect(CAR(intern((char *)"name",4)));
  settings_symb=scm_gc_protect(CAR(intern((char *)"settings",8)));
  const_1=scm_gc_protect(makfromstr((char *)"Bad arguments",13));
  const_2=scm_gc_protect(makfromstr((char *)"Invalid setting",15));
  const_3=scm_gc_protect(makfromstr((char *)": ",2));
  const_4=scm_gc_protect(makfromstr((char *)"No config schema",16));
  const_5=scm_gc_protect(makfromstr((char *)"TODO",4));
  const_6=scm_gc_protect(makfromstr((char *)"TODO",4));
  const_7=scm_gc_protect(makfromstr((char *)"TODO",4));
  const_8=scm_gc_protect(makfromstr((char *)"hello world",11));
  top_actions_cfg();
}

SCM init_cfg()
{
  no_symhash_gc=1;
  init_1_cfg();
}

