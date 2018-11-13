;;;; the actual accretions package
#-asdf3.1 (error "ACCRETIONS/CLOS/ALL requires ASDF 3.1.2")

(uiop:define-package :accretions/clos/all
  (:use :common-lisp)
  (:use-reexport :accretions/clos/generics))
(in-package :accretions/clos/all)
