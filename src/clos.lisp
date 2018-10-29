;;;; CLOS methods, mostly just dispatchers for other functionality
#-asdf3.1 (error "ACCRETIONS/SRC/CLOS requires ASDF 3.1.2 or later")

;; (eval-when (:compile-toplevel)
;;   (print "compiling clos"))
;; (eval-when (:load-toplevel)
;;   (print "loading clos"))
;; (eval-when (:execute)
;;   (print "executing clos"))

(defpackage :accretions/src/clos
  (:use #:cl #:accretions/src/generics)
  (:export #:make #:copy #:add #:emptyp #:size #:mapfun))
(in-package :accretions/src/clos)

;;; Assumes that the other src packages are already loaded (via the
;;; asd).

(defmethod make ((kind (eql :bag)))
  "Creates a new empty BAG and returns it."
  (accretions/src/bag:make))

(defmethod copy ((bag accretions/src/bag:bag))
  "Creates a shallow copy of the supplied BAG and returns it.  The new
bag shares values with the source bag, but has its own structure and
can be further modified."
  (accretions/src/bag:copy bag))

(defmethod add ((bag accretions/src/bag:bag) item)
  "Adds the supplied ITEM to the BAG."
  (accretions/src/bag:add bag item))

(defmethod size ((bag accretions/src/bag:bag))
  "Returns the number of items in the supplied BAG."
  (accretions/src/bag:size bag))

(defmethod emptyp ((bag accretions/src/bag:bag))
  "Returns a true value if the supplied BAG contains no items; else, NIL."
  (accretions/src/bag:emptyp bag))

(defmethod mapfun ((bag accretions/src/bag:bag) function)
  (accretions/src/bag:mapfun bag function))
