;;;; tests the actual accretions package
#-asdf3.1 (error "ACCRETIONS/TEST/ALL requires ASDF 3.1.2")

12(eval-when (:compile-toplevel)
  (print "compiling test/all"))
(eval-when (:load-toplevel)
  (print "loading test/all"))
(eval-when (:execute)
  (print "executing test/all"))

(uiop:define-package :accretions/test/all
  (:use :common-lisp :fiveam)
  (:export #:all-tests #:test-all)
  (:reexport #:accretions/test/bag))
(in-package :accretions/test/all)

(def-suite all-tests :description "All Accretions tests")
(in-suite all-tests)

(defun test-all ()
  (run! 'all-tests))
