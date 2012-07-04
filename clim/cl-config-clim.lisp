(in-package :cfg.clim)

(define-application-frame configuration-schema-viewer ()
  ((configuration-schema
    :initarg :configuration-schema
    :accessor configuration-schema
    :initform (error "Provide the configuration schema")))
  (:pane
   (flet ((label (string)
            (make-pane 'label-pane :label string)))
     (with-slots (configuration-schema) *application-frame*
       (labelling (:label (title configuration-schema))
         (make-pane 'vrack-pane :contents
                    (append
                     (list
                      (labelling (:label "Overview")
                        (label (cfg::documentation* configuration-schema))))
                     (loop for section being the hash-values of
                          (cfg::sections configuration-schema)
                          collect
                          (labelling (:label (title section))
                            (make-pane 'table-pane
                                       :contents
                                       (loop for option-schema being the hash-values of (cfg::direct-options section)
                                          collect
                                          (mapcar #'label
                                                  (list
                                                   (prin1-to-string (cfg::name option-schema))
                                                   (title option-schema)
                                                   (if (cfg::optional option-schema)
                                                       "Yes" "No")
                                                   (if (slot-boundp option-schema 'cfg::default)
                                                       (prin1-to-string (cfg::default option-schema))
                                                       "--")
                                                   (if (cfg::advanced option-schema)
                                                       "Yes" "No")
                                                   (or (cfg::documentation* option-schema) ""))))))))))))))
                 
              

(define-application-frame configuration-editor ()
  ((configuration :initarg :configuration
                  :accessor configuration
                  :initform (error "Provide the configuration")))
  (:pane
   (flet ((label (string)
            (make-pane 'label-pane :label string)))
     (with-slots (configuration) *application-frame*
       (scrolling (:width 1000 :height 600)
         (make-pane 'vrack-pane :contents
                    (append
                     (list
                      (labelling (:label "Overview")
                        (label (cfg::documentation* configuration))))
                     (loop for section being the hash-values of
                          (cfg::sections (cfg::configuration-schema configuration))
                          collect
                          (labelling (:label (title section))
                            (make-configuration-section-pane configuration section))))))))))

(defun make-configuration-section-pane (configuration section)
  (let
      ((direct-options (cfg::direct-options-list section
                                            :exclude-advanced t)))
    (make-pane 'table-pane
               :contents
               (loop for option in direct-options
                  collect
                    (list
                     (make-pane 'label-pane :label (title option))
                     (make-configuration-option-editor configuration section option))))))
           
(defun make-configuration-option-editor (configuration section option)
  (multiple-value-bind (value option-instance section-instance origin)
	(cfg::get-option-value
	  (list (cfg::name section)
		(cfg::name option))
	  configuration nil)
    (make-configuration-option-editor-valued (cfg::option-type option)
                                             option
                                             option-instance
                                             value)))

(defgeneric make-configuration-option-editor-valued (type option-schema option value))

(defmethod make-configuration-option-editor-valued ((type (eql 'cfg::text-configuration-schema-option-type))
                                                    option-schema
                                                    option
                                                    value)
  (make-pane 'text-field
             :value value))

(defmethod make-configuration-option-editor-valued ((type cfg::integer-configuration-schema-option-type)
                                                    option-schema
                                                    option
                                                    value)
  ;; (make-pane 'slider :min-value 0 :max-value 100 :value value)
  (make-pane 'text-field :value value)
  )

(defmethod make-configuration-option-editor-valued ((type cfg::one-of-configuration-schema-option-type)
                                                    option-schema
                                                    option
                                                    value)
  (make-pane 'option-pane
             :items (cfg::options type)
             :value (find value (cfg::options type) :key #'cfg::name)
             :name-key #'cfg::title))

(defmethod make-configuration-option-editor-valued ((type cfg::list-configuration-schema-option-type)
                                                    option-schema
                                                    option
                                                    value)
   (make-pane 'list-pane
              :mode :nonexclusive
              :items (cfg::options type)
              :name-key #'cfg::title
              :value (find value (cfg::options type) :key #'cfg::name)))

(defmethod make-configuration-option-editor-valued ((type cfg::boolean-configuration-schema-option-type)
                                                    option-schema
                                                    option
                                                    value)
  (make-pane 'toggle-button :value value))

(defmethod make-configuration-option-editor-valued ((type cfg::pathname-configuration-schema-option-type)
                                                    option-schema
                                                    option
                                                    value)
  (make-configuration-option-editor-valued 'cfg::text-configuration-schema-option-type
                                           option-schema
                                           option
                                           value))
                                                    
(defmethod make-configuration-option-editor-valued ((type cfg::email-configuration-schema-option-type)
                                                    option-schema
                                                    option
                                                    value)
  (make-configuration-option-editor-valued 'cfg::text-configuration-schema-option-type
                                           option-schema
                                           option
                                           value))

(defmethod make-configuration-option-editor-valued ((type cfg::url-configuration-schema-option-type)
                                                    option-schema
                                                    option
                                                    value)
  (make-configuration-option-editor-valued 'cfg::text-configuration-schema-option-type
                                           option-schema
                                           option
                                           value))

(defun run-configuration-editor ()
  (run-frame-top-level
   (make-application-frame 'configuration-editor)))



