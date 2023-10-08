(define web-schema
  (make-schema-from-spec
   "Web"
   '(#f
     ((host string :default "localhost"
            :doc "The host where to start the service.")
      (port integer :default 80
            :doc "The port where the service listens."))
     :doc "Web configuration")))

(define web-config (make-config "Web config"))

(cfgcli:parse "--host foohost" web-schema web-config)

(settings web-config)
(config-get web-config 'host)

(cfgcli:parse "--hosts foohost" web-schema web-config)

(cfgcli:print-help web-schema "web")
