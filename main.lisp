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

(defun %generate-compile-time-defclass (metaclass name direct-superclasses direct-slots)
  (list* 'cl:defclass name direct-superclasses
         (mapcar (lambda (slot)
                   (cons (car slot) (%mappcon (lambda (key value)
                                                (when (member key '(:initarg :initform :type
                                                                    :reader :writer :accessor
                                                                    :allocation :documentation)
                                                              :test #'eq)
                                                  (list key value)))
                                              (cdr slot))))
                 direct-slots)
         (unless (eq metaclass (load-time-value (find-class 'c2mop:standard-class)))
           (list `(:metaclass ,(class-name metaclass))))))


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
                ,@(when (enhanced-defclass:check-superclasses-p metaclass)
                    (list `(%declare-class-metaclass ,name ,(class-name metaclass))))
                (evaled-when:evaled-when (:compile-toplevel)
                  ,(%generate-compile-time-defclass metaclass name direct-superclasses direct-slots))
                (c2mop:ensure-class ',name
                                    :metaclass ',metaclass
                                    ,@(apply #'enhanced-defclass:canonicalize-initargs
                                             metaclass-prototype
                                             (or maybe-raw-initargs
                                                 (list* :direct-superclasses direct-superclasses
                                                        :direct-slots direct-slots
                                                        options)))))))))))
