(in-package #:enhanced-defclass)

(defun %extract-metaclass (options &optional env)
  (let ((metaclass (getf options :metaclass)))
    (when metaclass
      (find-class metaclass t env))))

(defgeneric enhanced-defclass:strip-defclass (metaclass-prototype defclass-form)
  (:method ((metaclass-prototype c2mop:standard-class) defclass-form)
    (destructuring-bind (operator name direct-superclasses direct-slots &rest options) defclass-form
      (list* operator name direct-superclasses
             (mapcar (lambda (slot)
                       (cons (car slot) (%mappcon (lambda (key value)
                                                    (when (member key '(:initarg :initform :type
                                                                        :reader :writer :accessor
                                                                        :allocation :documentation)
                                                                  :test #'eq)
                                                      (list key value)))
                                                  (cdr slot))))
                     direct-slots)
             (mapcan (lambda (option)
                       (when (member (car option) '(:metaclass :default-initargs :documentation)
                                     :test #'eq)
                         (list option)))
                     options)))))

(defmacro enhanced-defclass:defclass (name direct-superclasses direct-slots &rest options &environment env)
  (let ((explicit-metaclass (%extract-metaclass options env))
        (defclass-form `(cl:defclass ,name ,direct-superclasses ,direct-slots
                          ,@(%mappcon (lambda (name value)
                                        (list (if (member name '(:metaclass :documentation) :test #'eq)
                                                  (list name value)
                                                  (cons name value))))
                                      options)))
        (class-standard-class (load-time-value (find-class 'c2mop:standard-class))))
    (if (eq explicit-metaclass class-standard-class)
        defclass-form
        (let* ((raw-initargs (list* :direct-superclasses direct-superclasses
                                    :direct-slots direct-slots
                                    options))
               (metaclass (or explicit-metaclass
                              (%guess-metaclass (list* :name name raw-initargs))
                              class-standard-class)))
          (if (eq metaclass class-standard-class)
              defclass-form
              (let ((metaclass-prototype (c2mop:class-prototype (c2mop:ensure-finalized metaclass))))
                `(progn
                   (evaled-when:evaled-when (:compile-toplevel)
                     ,(strip-defclass metaclass-prototype defclass-form))
                   (c2mop:ensure-class ',name
                                       :metaclass ',metaclass
                                       ,@(apply #'enhanced-defclass:canonicalize-initargs
                                                metaclass-prototype
                                                raw-initargs)))))))))
