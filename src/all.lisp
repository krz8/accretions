;;;; the actual accretions package
#-asdf3.1 (error "ACCRETIONS/SRC/ALL requires ASDF 3.1.2")

;; (eval-when (:compile-toplevel)
;;   (print "compiling all"))
;; (eval-when (:load-toplevel)
;;   (print "loading all"))
;; (eval-when (:execute)
;;   (print "executing all"))

(uiop:define-package :accretions/src/all
  (:nicknames :accretions)
  (:use :common-lisp)
  (:use-reexport :accretions/src/generics
		 :accretions/src/bagclos)
  (:import-from :accretions/src/bag #:bagp)
  (:export #:bagp))
(in-package :accretions/src/all)
