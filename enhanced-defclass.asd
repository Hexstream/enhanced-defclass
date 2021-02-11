(asdf:defsystem #:enhanced-defclass

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "Provides a truly extensible version of DEFCLASS that can accurately control the expansion according to the metaclass and automatically detect the suitable metaclass by analyzing the DEFCLASS form."

  :depends-on ("closer-mop"
               "evaled-when"
               "simple-guess"
               "shared-preferences"
               "compatible-metaclasses"
               "enhanced-eval-when"
               "enhanced-find-class")

  :version "2.0"
  :serial cl:t
  :components ((:file "package")
               (:file "preferences")
               (:file "guess-metaclass")
               (:file "autoclass")
               (:file "canonicalize")
	       (:file "main"))

  :in-order-to ((asdf:test-op (asdf:test-op #:enhanced-defclass_tests))))
