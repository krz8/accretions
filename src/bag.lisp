;;;; a bag is an unordered collection of items

;; (eval-when (:compile-toplevel)
;;   (print "compiling bag"))
;; (eval-when (:load-toplevel)
;;   (print "loading bag"))
;; (eval-when (:execute)
;;   (print "executing bag"))

(defpackage :accretions/src/bag
  (:use #:cl)
  (:export #:bag #:make #:copy #:bagp
	   #:head #:size
	   #:emptyp #:add #:mapfun))
(in-package :accretions/src/bag)

;;; BAG is a package, as we're following a "one package one file"
;;; coding style, as well as a symbol (the structure) within that
;;; package.  This renders the standard automagically-generated
;;; function names (for constructors and the like) a bit needlessly
;;; wordy.  Hence, the DEFSTRUCT options below.

(defstruct (bag (:conc-name nil) (:constructor make) (:copier nil)
		(:predicate bagp))
  "An unordered collection of items.  Unlike queues and other
collections, bags only accumulate values and do not support deletion."
  (head nil :type list)
  (size 0 :type unsigned-byte))

(defun copy (bag)
  "Creates and returns a shallow copy of the supplied BAG.  New values
may be added to the returned BAG without affecting the source BAG, but
existing values are shared between the two."
  (make :head (copy-list (head bag))
	:size (size bag)))

(defun emptyp (bag)
  "Return T if the supplied BAG contains zero items; else, return NIL."
  (null (head bag)))

;;; We used to test that the CONS succeeded before modifying the list;
;;; the idea was that CONS is really the only thing that could fail in
;;; ADD.  In practice, though, any situation that leads to a CONS
;;; failure should raise an error condition of some kind.  In other
;;; words, CONS doesn't have a failure return.  For that reason, ADD
;;; no longer tests anything, on the assumption that if an error is
;;; not raised, the operation is successful.

(defun add (bag value)
  "Add the supplied VALUE to the BAG, returning that BAG.  Duplicate
VALUE are supported; calling (ADD NIL) three times yields a BAG with
three NIL values."
  (setf (head bag) (cons value (head bag)))
  (incf (size bag))
  bag)

(defun mapfun (bag function)
  "For every value in the BAG, call the supplied FUNCTION designator
with that value as an argument.  Returns T."
  (mapc function (head bag)))
