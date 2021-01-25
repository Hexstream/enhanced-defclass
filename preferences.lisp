(in-package #:enhanced-defclass)

(shared-prefs:define-settings enhanced-defclass:preferences ()
  (;; See guess-metaclass.lisp for defaults.
   enhanced-defclass:default-metaclass-manager
   enhanced-defclass:default-metaclass-advisor))
