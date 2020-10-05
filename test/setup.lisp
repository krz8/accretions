;;;; setup for testing accretions

(eval-when (:compile-toplevel) (print "compiling test/setup"))
(eval-when (:load-toplevel)    (print "loading test/setup"))
(eval-when (:execute)          (print "executing test/setup"))

(defpackage :accretions/test/setup
  (:use :cl :fiveam)
  (:export :all))

(def-suite all
  :description "All tests for Accretions")
