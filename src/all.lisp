(uiop:define-package :accretions/src/all
  (:nicknames :accretions :acr)
  (:use :common-lisp)
  (:use-reexport :accretions/src/generics :accretions/src/conditions
		 :accretions/src/queue))
(in-package :accretions/src/all)
