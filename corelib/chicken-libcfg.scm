(module libcfg
  (cfg_create
   cfg_name
   cfg_get
   cfg_set
   cfg_print
   cfg_create_schema
   cfg_set_schema_doc
   cfg_load_lib
   cfg_validate
   cfg_validate_with_schema)
  (import (chicken foreign))
  (import bind)

  (bind* "#include \"libcfg.h\"")
  (bind-file "libcfg.h"))
