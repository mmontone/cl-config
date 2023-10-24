(load "./chicken-libcfg.so")

(import libcfg)

(define schema (cfg_create "My schema"))

(cfg_set_schema_doc schema "This is a schema test")

(define config (cfg_create "My config"))

(cfg_validate_with_schema config schema)
