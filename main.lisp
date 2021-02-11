(in-package #:enhanced-defclass)

(defun %extract-metaclass (options &optional env)
  (let ((metaclass (getf options :metaclass)))
    (when metaclass
      (find-class metaclass t env))))

(defgeneric enhanced-defclass:passthroughp (metaclass-prototype name direct-superclasses direct-slots env)
  (:method ((prototype cl:standard-class) name direct-superclasses direct-slots env)
    (declare (ignore name direct-superclasses direct-slots env))
    (eq (class-of prototype) (load-time-value (find-class 'c2mop:standard-class)))))

(defun %determine-metaclass (name direct-superclasses direct-slots options
                             &optional env (default-metaclass (load-time-value (find-class 'c2mop:standard-class))))
  (let (maybe-raw-initargs)
    (values (or (%extract-metaclass options env)
                (%guess-metaclass (list* :name name
                                         (setf maybe-raw-initargs
                                               (list* :direct-superclasses direct-superclasses
                                                      :direct-slots direct-slots
                                                      options))))
                default-metaclass)
            maybe-raw-initargs)))

(defmacro enhanced-defclass:defclass (&whole whole name direct-superclasses direct-slots &rest options &environment env)
  (etypecase (first options)
    (cons `(cl:defclass ,@(cdr whole))) ; Heavyweight options.
    (symbol                             ; Lightweight options, or ambiguous if no options.
     (multiple-value-bind (metaclass maybe-raw-initargs)
         (%determine-metaclass name direct-superclasses direct-slots options env)
       (let ((metaclass-prototype (c2mop:class-prototype (c2mop:ensure-finalized metaclass))))
         (if (and (not options)
                  (enhanced-defclass:passthroughp metaclass-prototype name direct-superclasses direct-slots env))
             `(cl:defclass ,@(cdr whole)) ; No options meant heavyweight options, so passthrough.
             `(progn
                (evaled-when:evaled-when (:compile-toplevel)
                  (cl:defclass ,name ,direct-superclasses
                    ()
                    ,@(unless (eq metaclass (load-time-value (find-class 'c2mop:standard-class)))
                        (list `(:metaclass ,(class-name metaclass))))))
                (c2mop:ensure-class ',name
                                    :metaclass ',metaclass
                                    ,@(apply #'enhanced-defclass:canonicalize-initargs
                                             metaclass-prototype
                                             (or maybe-raw-initargs
                                                 (list* :direct-superclasses direct-superclasses
                                                        :direct-slots direct-slots
                                                        options)))))))))))
