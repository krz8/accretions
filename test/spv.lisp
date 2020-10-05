;;;; tests for sparse vectors

(eval-when (:compile-toplevel) (princ "compiling test/spv") (fresh-line))
(eval-when (:load-toplevel)    (princ "loading test/spv")   (fresh-line))
(eval-when (:execute)          (princ "executing test/spv") (fresh-line))

(defpackage :accretions/test/spv
  (:use :cl :fiveam :accretions :accretions/spv :accretions/test/suites)
  (:export #:spv #:try))
(in-package :accretions/test/spv)

(in-suite accretions/test:spv)

(defun try ()
  "Run all sparse vector tests in the Accretions system.  This is a
  convenience function, nothing more."
  (run! 'spv))

(test creation
  (let ((sv (make-sparse-vector '(2 3 4 5) :element-type 'short-float :initial-element 42s0)))
    (is (typep sv 'sparse-vector))
    (is (= 120 (accretions/spv::spv-size sv)))
    (is (equal '(2 3 4 5) (accretions/spv::spv-splay sv)))
    (is (equal '(60 20 5) (accretions/spv::spv-divisors sv)))
    (is (eq 'short-float (accretions/spv::spv-element-type sv)))
    (is (= 42s0 (accretions/spv::spv-initial-element sv)))
    (is (null (accretions/spv::spv-tree sv)))
    (is (= 42s0 (funcall (accretions/spv::spv-get sv) 0)))))
