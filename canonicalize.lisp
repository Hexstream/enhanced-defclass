(in-package #:enhanced-defclass)

(defun %every-other (function)
  (let ((processp t))
    (lambda (key value)
      (prog1 (when processp
               (funcall function key value))
        (setf processp (not processp))))))

(defun %mappc (function plist)
  (mapc (%every-other function) plist (cdr plist)))

(defun %mappcon (function plist)
  (mapcan (%every-other function) plist (cdr plist)))


(defgeneric enhanced-defclass:make-slot-initargs-canonicalizer (metaclass-prototype)
  (:method ((metaclass-prototype c2mop:standard-class))
    (let (initform initformp initargs readers writers
          type typep allocation allocationp documentation documentationp already-finished-p)
      (values (macrolet ((input-single (value-var)
                           (let ((valuep-var (intern (format nil "~A~A" value-var '#:p))))
                             `(if ,valuep-var
                                  (error "Duplicate ~S for slot." key)
                                  (setf ,value-var value
                                        ,valuep-var t))))
                         (push-symbol (place)
                           `(progn
                              (check-type value symbol)
                              (push value ,place))))
                (lambda (key value)
                  (check-type key symbol)
                  (ecase key
                    (:initform
                     (input-single initform))
                    (:initarg
                     (push-symbol initargs))
                    (:reader
                     (push-symbol readers))
                    (:writer
                     (push-symbol writers))
                    (:accessor
                     (push-symbol readers)
                     (push `(setf ,value) writers))
                    (:type
                     (input-single type))
                    (:allocation
                     (input-single allocation))
                    (:documentation
                     (input-single documentation)))))
              (lambda ()
                (if already-finished-p
                    (error "Accumulation was already finished.")
                    (progn
                      (setf already-finished-p t)
                      (nconc (when initformp
                               (list :initform `',initform
                                     :initfunction `(lambda () ,initform)))
                             (flet ((list-of (key value)
                                      (when value
                                        (list key `',(nreverse value)))))
                               (nconc (list-of :initargs initargs)
                                      (list-of :readers readers)
                                      (list-of :writers writers)))
                             (when typep
                               (list :type `',type))
                             (when allocationp
                               (list :allocation `',allocation))
                             (when documentationp
                               (list :documentation documentation))))))))))

(defgeneric enhanced-defclass:canonicalize-slot-specification (metaclass-prototype spec)
  (:method ((metaclass-prototype c2mop:standard-class) spec)
    (multiple-value-bind (accumulate finish)
        (enhanced-defclass:make-slot-initargs-canonicalizer metaclass-prototype)
      (%mappc accumulate (cdr spec))
      (list* 'list :name `',(car spec) (funcall finish)))))

(defgeneric enhanced-defclass:make-initargs-canonicalizer (metaclass-prototype)
  (:method ((metaclass-prototype c2mop:standard-class))
    (lambda (name value)
      (when (symbolp name)
        (case name
          (:direct-superclasses
           (list name `',value))
          (:direct-slots
           (list name `(list ,@(mapcar (lambda (slot-specification)
                                         (enhanced-defclass:canonicalize-slot-specification
                                          metaclass-prototype slot-specification))
                                       value))))
          (:default-initargs
           (list :direct-default-initargs
                 `(list ,@(%mappcon (lambda (name initform)
                                      (list `(list ',name ',initform (lambda () ,initform))))
                                    value)))))))))

(defgeneric enhanced-defclass:canonicalize-initargs (metaclass-prototype &rest initargs)
  (:method ((metaclass-prototype c2mop:standard-class) &rest initargs)
    (let ((canonicalizer (enhanced-defclass:make-initargs-canonicalizer metaclass-prototype)))
      (%mappcon (lambda (name value)
                  (unless (eq name :metaclass)
                    (or (funcall canonicalizer name value)
                        (list name value))))
                initargs))))
