;;;; test suites for all accretions cases

(eval-when (:compile-toplevel) (princ "compiling test/suites") (fresh-line))
(eval-when (:load-toplevel)    (princ "loading test/suites")   (fresh-line))
(eval-when (:execute)          (princ "executing test/suites") (fresh-line))

;;; Note that this file is loaded *before* all the other test packages
;;; in Accretions.  This is the opposite of other uses of "all", which
;;; go at the end in order to tie everything together.  Here, we have
;;; nothing to "tie together" after the test cases are spec'd, but we
;;; put "all" first in order to define the "all" test suite, which
;;; holds all the other test suites.

(uiop:define-package :accretions/test/suites
  (:use :cl :fiveam)
  (:export #:all #:misc #:spv #:try)
  (:nicknames :accretions/test))
(in-package :accretions/test/suites)

(def-suite all
  :description "accretions tests")

(def-suite misc
  :description "miscellaneous tests"
  :in all)

(def-suite spv
  :description "sparse vector tests"
  :in all)

(defun try (&optional (test-suite 'all))
  "Runs all the tests in TEST-SUITE.  Just a convenience function that
  helps ASDF a bit.  TEST-SUITE is a symbol in any package; it is
  converted to a symbol in the suites package."
  (run! (intern (symbol-name test-suite) :accretions/test/suites)))
