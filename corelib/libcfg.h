#ifndef cfg_h__
#define cfg_h__

extern long cfg_create(char *name);
extern char* cfg_name(long config);
extern char* cfg_get(long config, char* key);
extern void cfg_set(long config, char* key, char* value);
extern void cfg_print(long config);
extern void cfg_load_lib(int argc, const char **argv);
extern long cfg_create_schema(char *name);
extern void cfg_set_schema_doc(long schema, char* doc);
extern void cfg_cli_help(long schema);
extern int cfg_validate(long config);
extern int cfg_validate_with_schema(long config, long schema);

#endif // cfg_h__
