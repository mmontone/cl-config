(define web-schema
  (make-schema-from-spec
   "Web"
   '(#f
     ((host string :default "localhost")
      (port integer :default 80)))))

(define web-config (make-config "Web config"))

(cfgcli:parse "--host foohost" web-schema web-config)

(settings web-config)
(config-get web-config 'host)

(cfgcli:parse "--hosts foohost" web-schema web-config)
