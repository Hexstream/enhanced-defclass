(in-package #:enhanced-defclass)

(cl:defclass enhanced-defclass:autoclass (compatible-metaclasses:standard-class)
  ()
  (:metaclass compatible-metaclasses:standard-metaclass))

(defgeneric enhanced-defclass:check-superclasses-p (class)
  (:method ((class cl:class))
    nil))

(cl:defclass enhanced-defclass:standard-autoclass (enhanced-defclass:autoclass)
  ((%slot-options :reader enhanced-defclass:slot-options
                  :type list)
   (%class-options :reader enhanced-defclass:class-options
                   :type list)
   (%check-superclasses-p :reader enhanced-defclass:check-superclasses-p
                          :type boolean
                          :initform nil))
  (:metaclass compatible-metaclasses:standard-metaclass))

(defgeneric enhanced-defclass:compute-slot-options (class-prototype)
  (:method-combination nconc)
  (:method nconc ((class-prototype cl:class))
    nil))

(defgeneric enhanced-defclass:compute-class-options (class-prototype)
  (:method-combination nconc)
  (:method nconc ((class-prototype cl:class))
    nil))

(defmethod c2mop:finalize-inheritance :after ((class enhanced-defclass:standard-autoclass))
  (let ((prototype (c2mop:class-prototype class)))
    (setf (slot-value class '%slot-options)
          (enhanced-defclass:compute-slot-options prototype)
          (slot-value class '%class-options)
          (enhanced-defclass:compute-class-options prototype))))

(defvar *%autoclass-name-to-prototype* (make-hash-table :test 'eq))

(defun %%declare-class-metaclass (class-name metaclass-name)
  (unless (find-class class-name nil)
    (let ((metaclass (find-class metaclass-name)))
      (when (typep metaclass 'enhanced-defclass:standard-autoclass)
        (setf (gethash class-name *%autoclass-name-to-prototype*)
              (c2mop:class-prototype (c2mop:ensure-finalized metaclass)))))))

(defmacro %declare-class-metaclass (class-name metaclass-name)
  `(eval-when (:compile-toplevel)
     (%%declare-class-metaclass ',class-name ',metaclass-name)))

(defmethod simple-guess:inquire ((manager enhanced-defclass:metaclass-manager) (class enhanced-defclass:standard-autoclass)
                                 &rest initargs &key direct-superclasses direct-slots
                                                  (metaclass-strategy '(:slot-options :class-options :superclasses)))
  (when (some (lambda (what-to-check)
                (ecase what-to-check
                  (:slot-options
                   (let ((slot-options (enhanced-defclass:slot-options class)))
                     (and slot-options
                          (some (lambda (slot-spec)
                                  (get-properties (cdr slot-spec) slot-options))
                                direct-slots))))
                  (:class-options
                   (let ((class-options (enhanced-defclass:class-options class)))
                     (and class-options
                          (get-properties initargs class-options))))
                  (:superclasses
                   (when (enhanced-defclass:check-superclasses-p class)
                     (let ((visited-superclasses (make-hash-table :test 'eq)))
                       (labels ((recurse (superclasses)
                                  (some (lambda (superclass)
                                          (unless (gethash superclass visited-superclasses)
                                            (setf (gethash superclass visited-superclasses) t)
                                            (or (typep superclass class)
                                                (recurse (c2mop:class-direct-superclasses superclass)))))
                                        superclasses)))
                         (block nil
                           (let ((autoclass-name-to-prototype *%autoclass-name-to-prototype*))
                             (recurse (mapcan (lambda (superclass-name)
                                                (let ((superclass (find-class superclass-name nil)))
                                                  (if superclass
                                                      (list superclass)
                                                      (let ((prototype (gethash superclass-name
                                                                                autoclass-name-to-prototype)))
                                                        (when (typep prototype class)
                                                          (return t))))))
                                              direct-superclasses))))))))))
              metaclass-strategy)
    class))
