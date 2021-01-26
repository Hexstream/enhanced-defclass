(in-package #:enhanced-defclass)

(cl:defclass enhanced-defclass:metaclass-manager (simple-guess:manager)
  ())

(cl:defclass standard-metaclass-manager (enhanced-defclass:metaclass-manager)
  ())

(defvar *metaclass-manager* (make-instance 'standard-metaclass-manager))

(defvar *metaclass-advisor* (simple-guess:constant-advisor nil))

(shared-prefs:define-defaults
  (default-metaclass-manager *metaclass-manager*)
  (default-metaclass-advisor *metaclass-advisor*))

(defun %advising-package (name)
  (when (and name (symbolp name))
    (let ((home-package (symbol-package name))
          (current-package *package*))
      (if home-package
          (prog1 home-package
            (unless (eq home-package current-package)
              (error "The home package of ~S (~S) is not the current package (~S)."
                     name home-package current-package)))
          current-package))))

(defun %guess-metaclass (raw-initargs &optional (preferences (shared-prefs:preferences *package*)))
  (let ((manager (enhanced-defclass:default-metaclass-manager preferences))
        (advisor (enhanced-defclass:default-metaclass-advisor preferences)))
    (apply #'simple-guess:inquire manager advisor raw-initargs)))
