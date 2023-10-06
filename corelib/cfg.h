#include "scmhob.h"


SCM make_config();
SCM config_name();
SCM settings();
SCM assoc_set_excl_();
SCM assoc_get();
SCM config_set_excl_();
SCM config_get();
SCM print_config();
SCM print_config_fn2();
SCM cfg_colon_validate();
SCM config_schema();
SCM config_parent();
SCM set_parent_excl_();
SCM register_config();
SCM find_config();
SCM cfg_colon_save();
SCM cfg_colon_load();
SCM hello_world();
SCM top_actions_1_cfg();
SCM top_actions_cfg();
SCM for_each1();
SCM init_1_cfg();
SCM init_cfg();
SCM *error;
SCM *configs_global;
SCM *schemas_global;
SCM *settings_clproc0;
static SCM name_symb;
static SCM parent_symb;
static SCM settings_symb;
static SCM schema_symb;
static SCM options_symb;
static SCM const_1;
static SCM const_2;
static SCM const_3;
static SCM const_4;
static SCM const_5;
static SCM const_6;

