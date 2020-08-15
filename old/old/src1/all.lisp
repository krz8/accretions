#-asdf3.1 (error "ACCRETIONS requires ASDF 3.1")
(uiop:define-package :accretions/src/all
  (:nickname :accretions)
  (:use :common-lisp)
  (:use-reexport :accretions/src/generics
		 :accretions/src/conditions
		 :accretions/src/queue))
(in-package :accretions/src/all)
