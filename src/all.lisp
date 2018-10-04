;;;; the actual accretions package
#-asdf3.1 (error "ACCRETIONS requires ASDF 3.1")

(uiop:define-package #:accretions/src/all
  (:nicknames #:accretions)
  (:use :common-lisp)
  (:shadow map)
  (:use-reexport #:accretions/src/bag
		 #:accretions/src/generics
		 #:accretions/src/bag-clos))
(in-package #:accretions/src/all)
