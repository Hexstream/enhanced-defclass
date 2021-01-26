(in-package #:enhanced-defclass)

(cl:defclass enhanced-defclass:autoclass (compatible-metaclasses:standard-class)
  ()
  (:metaclass compatible-metaclasses:standard-metaclass))

(cl:defclass enhanced-defclass:standard-autoclass (enhanced-defclass:autoclass)
  ((%slot-options :initarg :slot-options
                  :reader enhanced-defclass:slot-options
                  :type list
                  :initform nil)
   (%class-options :initarg :class-options
                   :reader enhanced-defclass:class-options
                   :type list
                   :initform nil))
  (:metaclass compatible-metaclasses:standard-metaclass))

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
                   (let ((visited-superclasses (make-hash-table :test 'eq)))
                     (labels ((recurse (superclasses)
                                (some (lambda (superclass)
                                        (unless (gethash superclass visited-superclasses)
                                          (setf (gethash superclass visited-superclasses) t)
                                          (or (eq superclass class)
                                              (recurse (c2mop:class-direct-superclasses superclass)))))
                                      superclasses)))
                       (recurse (mapcan (lambda (superclass-name)
                                          (let ((superclass (find-class superclass-name nil)))
                                            (when superclass
                                              (list superclass))))
                                        direct-superclasses)))))))
              metaclass-strategy)
    class))
