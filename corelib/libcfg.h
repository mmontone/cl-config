extern long cfg_create(char *setting_name);
extern char* cfg_name(long config);
extern char* cfg_get(long config, char* key);
extern void cfg_set(long config, char* key, char* value);
extern void cfg_print(long config);
extern void cfg_load_lib(int argc, const char **argv);
extern long cfg_create_schema(char *setting_name);
extern void cfg_set_schema_doc(long schema, char* doc);
extern void cfg_cli_help(long schema);
extern int cfg_validate(long config);
extern int cfg_validate_with_schema(long config, long schema);
extern void cfg_setting_set_doc(long setting, char* doc);
extern long cfg_add_string_setting(long schema, char* setting_name);
extern long cfg_add_integer_setting(long schema, char* setting_name);
extern long cfg_add_choice_setting(long schema, char* setting_name, char* choices[]);
extern void cfg_setting_set_default(long setting, void *val);
extern void cfg_setting_set_string_default(long setting, char* val);
extern void cfg_setting_set_integer_default(long setting, short int val);
extern int cfg_is_set(long config, char* setting_name);
