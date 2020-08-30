;;;; the actual accretions package
#-asdf3.1 (error "ACCRETIONS/ALL requires ASDF 3.1.2 or later")

(eval-when (:compile-toplevel)
  (print "compiling all"))
(eval-when (:load-toplevel)
  (print "loading all"))
(eval-when (:execute)
  (print "executing all"))

(uiop:define-package :accretions/all
  (:nicknames :accretions)
  (:use :common-lisp)
;  (:use-reexport :accretions/clos/generics :accretions/clos/bag)
  (:import-from :accretions/spv #:sparse-vector-p)
  (:export #:sparse-vector-p))
(in-package :accretions/all)
