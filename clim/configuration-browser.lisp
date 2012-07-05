(in-package :cfg.clim)

(define-application-frame configuration-schema-browser ()
  ((configuration-schemas :accessor configuration-schemas
                          :initarg :configuration-schemas
                          :initform (list (find-configuration-schema 'cfg::standard-configuration))))
  (:pointer-documentation t)
  (:panes
   (app :application
        :width 3000
        :height 2000
        :scroll-bars nil
        :incremental-redisplay t
        :display-function 'configuration-schema-browser-display)
   (io :interactor
       :height 50))
  (:layouts
   (default
       (vertically (:width 800 :min-width 100 :max-width +fill+)
         (:fill
          (horizontally ()
            (scrolling (:scroll-bars t)
              app)))
         io))))

(defvar *expanded* nil)

(define-presentation-type node ())

(define-configuration-schema-browser-command toggle-node ((node 'node :gesture :select))
  (if (member node *expanded*)
      (setf *expanded* (remove node *expanded*))
      (push node *expanded*)))

(defun configuration-schema-browser-display (app pane)
  app pane
  (let ((*standard-output* pane))
    (format-graph-from-roots (configuration-schemas *application-frame*)
                             #'(lambda (node *standard-output*)
                                 (let ((*print-case* :downcase))
                                   (surrounding-output-with-border
                                       (*standard-output* :shape :drop-shadow)
                                     (with-text-style (t (make-text-style :sans-serif nil nil))
                                       (with-output-as-presentation (t node 'node)
                                         (with-text-style (t (make-text-style :sans-serif :bold :large))
                                           (princ (cfg::title node))))
                                       (terpri)
                                       (with-drawing-options (*standard-output* :ink +red4+)
                                         (princ (clean-docu-string (or (cfg::documentation* node) ""))))
                                       (terpri)
                                       (terpri)
                                       (loop for section being the hash-values of (cfg::sections node)
                                          do
                                          (progn
                                            (with-text-style (t (make-text-style :sans-serif :bold :normal))
                                              (princ (cfg::title section)))
                                            (terpri)
                                            (formatting-table ()
                                              (loop for option-schema being the hash-values of (cfg::direct-options section)
                                                 do
                                                 (formatting-row ()
                                                   (formatting-cell (t :align-y :top)
                                                     (princ (cfg::title option-schema))
                                                     (princ " ")
                                                     (with-text-style (t (make-text-style :sans-serif :italic :small))
                                                       (format t "(~A)" (cfg::title (cfg::option-type option-schema)))))
                                                   ;; (formatting-cell (t :align-y :top)
                                                   ;;   (with-drawing-options (*standard-output* :ink +red4+)
                                                   ;;     (princ (clean-docu-string (or (cfg::documentation* option-schema) "")))))
                                                   )))
                                            (terpri)))
                                       (terpri)))))
                             #'(lambda (node)
                                 (if (member node *expanded*)
                                     ;;(pcl:class-direct-subclasses node)
                                     (mapcar #'find-configuration-schema (cfg::parents node))
                                     nil))
                             :cutoff-depth nil
                             :graph-type :tree
                             :merge-duplicates t
                             :arc-drawer #'climi::arrow-arc-drawer
                             :arc-drawing-options (list :ink +gray66+ :line-thickness 1)
                             :generation-separation 30)
    (terpri)))

(defun clean-docu-string (string)
  (with-output-to-string (bag)
    (let ((last-was-nl nil))
      (loop for c across string do
            (cond ((eql c #\newline)
                   (princ c bag)
                   (setf last-was-nl t))
                  ((member c '(#\space #\tab))
                   (if last-was-nl
                       nil
                       (princ c bag)))
                  (t
                   (setf last-was-nl nil)
                   (princ c bag)))))))

(run-frame-top-level
 (make-application-frame 'configuration-schema-browser))