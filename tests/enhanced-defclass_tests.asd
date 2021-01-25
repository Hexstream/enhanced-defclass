(asdf:defsystem #:enhanced-defclass_tests

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "enhanced-defclass unit tests."

  :depends-on ("enhanced-defclass"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:enhanced-defclass_tests)))
