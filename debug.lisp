(in-package #:enhanced-defclass)

(defvar *query-number* 0)

(defmethod simple-guess:inquire :around ((manager standard-metaclass-manager) advisor &rest query)
  (let ((number (incf *query-number*)))
    (format t "~2%Query ~A: ~S ~S" number advisor query)
    (let ((answer (call-next-method)))
      (format t "~2%Answer ~A: ~S" number answer)
      answer)))
