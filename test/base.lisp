#-asdf3.1 (error "ACCRETIONS requires ASDF 3.1")

;;; Found this approach to solving a minor annoyance in Fare's Lisp
;;; Interface Library.
;;;
;;; The problem is this.  We're using 5am for testing, and we want a
;;; test suite simply named "all" that includes all the other test
;;; suites.  The common idiom in 5am is to declare a test suite "all"
;;; and then, as the other test suites are defined, they are annotated
;;; as belonging to "all".
;;;
;;; The trouble is that the "all" test suite doesn't exist yet unless
;;; you were careful to always run all tests.  You could no longer run
;;; just a subset of tests without losing the dependency.
;;; Soooooooo... "all.lisp" references all the test suites, and all
;;; the test suites reference a base testing package, which defines
;;; the "all" test suite.
;;;
;;; I know, I know.  But it works for now.

(defpackage :accretions/test/base
  (:export #:all))
(in-package :accretions/test/base)

(def-suite all :description "all tests for Accretions")
