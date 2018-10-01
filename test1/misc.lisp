(defpackage :accretions/test/misc
  (:use :common-lisp))
(in-package :accretions/test/misc)

;;; The macros in src/misc are very well understood and generally
;;; don't need testing.  This file exists mostly to give us something
;;; to start fleshing out a test skeleton for the rest of Accretions.
;;; FiveAM is simple as anything, but package-inferred systems might
;;; throw us a curveball.

(def-suite misc :description "all petulant tests" :in all)
(in-suite misc)

(test awhen
  (let ((n 0)
	(x 0))
    ;; sanity checks
    (is-true (zerop n))
    (is-true (zerop x))
    ;; ensure we don't evaluate more than once
    (accretions/src/misc:awhen (incf x)
      (incf n))
    (is-true (= 1 n))
    (is-true (= 1 x))
    ;; ensure IT binds as expected
    (accretions/src/misc:awhen (incf x)
      (is-true (= it x))
      (is-true (= 2 x))
      (is-true (= 1 n)))))
