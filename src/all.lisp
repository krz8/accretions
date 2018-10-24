;;;; the actual accretions package
#-asdf3.1 (error "ACCRETIONS/SRC/ALL requires ASDF 3.1.2")

(uiop:define-package :accretions/src/all
  (:nicknames :accretions)
  (:use :common-lisp)
  (:use-reexport :accretions/src/generics
		 :accretions/src/clos)
  (:import-from :accretions/src/bag #:bagp)
  (:export #:bagp))
(in-package :accretions/src/all)
