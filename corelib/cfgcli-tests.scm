(define web-schema
  (make-schema-from-spec
   "Web"
   '(#f
     ((host string :default "localhost"
            :summary "The host."
            :doc "The host where to start the service.")
      (port integer :default 80
            :summary "The port."
            :doc "The port where the service listens."))
     :doc "Web configuration")))

(define web-config (make-config "Web config"))

(cfgcli:parse "--host foohost" web-schema web-config)

;; (settings web-config)
;; (config-get web-config 'host)

;; (cfgcli:parse "--hosts foohost" web-schema web-config)

;; (cfgcli:print-help web-schema "web")

;; (cfg:save web-config "/home/marian/web-config.config")

;; (cfg:load "/home/marian/web-config.config")

;; (print-config (cfg:load "/home/marian/web-config.config"))
