#ifndef cfg_h__
#define cfg_h__

extern long cfg_create(char *name);
extern char* cfg_name(long config);
extern char* cfg_get(long config, char* key);
extern void cfg_set(long config, char* key, char* value);
extern void cfg_print(long config);
extern void cfg_load_lib(int argc, const char **argv);

#endif // cfg_h__
