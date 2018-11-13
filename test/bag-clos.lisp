;;;; tests the bag CLOS layer
#-asdf3.1 (error "ACCRETIONS/TEST/BAG-CLOS requires ASDF 3.1.2 or later")

(defpackage #:accretions/test/bag-clos
  (:use #:cl #:fiveam)
  (:export #:bag #:make #:copy #:size #:emptyp #:add #:add-many))
(in-package #:accretions/test/bag-clos)

(uiop:define-package #:bag		; glue
    (:use-reexport #:accretions/src/bag))

(def-suite bag-clos
    :description "All bag CLOS tests.")
(in-suite bag-clos)
