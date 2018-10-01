;;;; tests the bag implementation
#-asdf3.1 (error "ACCRETIONS/BAG-TEST requires ASDF 3.1.2 or later")

(defpackage #:accretions/bag-test
  (:use #:cl))

(in-package #:accretions/bag-test)

(uiop:define-package #:bag
  (:use-reexport #:accretions/bag))
