;;;; tests for accretions

(eval-when (:compile-toplevel) (princ "compiling test/all") (fresh-line))
(eval-when (:load-toplevel)    (princ "loading test/all")   (fresh-line))
(eval-when (:execute)          (princ "executing test/all") (fresh-line))

;;; Note that this file is loaded *before* all the other test packages
;;; in Accretions.  This is the opposite of other uses of "all", which
;;; typically go at the end in order to tie everything together.

(defpackage :accretions/test/all
  (:use :cl :fiveam)
  (:export #:all #:try))
(in-package :accretions/test/all)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite all
    :description "All tests for Accretions"))

(defun try ()
  "Run all tests in the Accretions system.  This is a convenience
  function, nothing more."
  (run! 'all))
