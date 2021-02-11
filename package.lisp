(cl:defpackage #:enhanced-defclass
  (:use #:cl)
  (:shadowing-import-from #:enhanced-find-class #:find-class)
  (:shadow #:defclass)
  (:export #:preferences

           #:metaclass-manager
           #:default-metaclass-manager
           #:default-metaclass-advisor

           #:autoclass
           #:standard-autoclass
           #:slot-options
           #:class-options
           #:compute-slot-options
           #:compute-class-options

           #:canonicalize-initargs
           #:make-initargs-canonicalizer
           #:canonicalize-slot-specification
           #:make-slot-initargs-canonicalizer

           #:defclass
           #:passthroughp))
