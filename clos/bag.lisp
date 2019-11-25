;;;; CLOS methods, mostly just dispatchers for other functionality
#-asdf3.1 (error "ACCRETIONS/CLOS/BAG requires ASDF 3.1.2 or later")

(eval-when (:compile-toplevel)
  (print "compiling clos/bag"))
(eval-when (:load-toplevel)
  (print "loading clos/bag"))
(eval-when (:execute)
  (print "executing clos/bag"))

(defpackage :accretions/clos/bag
  (:use #:cl #:accretions/clos/generics)
  (:export #:make ; #:copy #:add #:emptyp #:size #:mapfun
	   ))
(in-package :accretions/clos/bag)

(defmethod make ((kind (eql :bag)))
  "Creates a new empty BAG and returns it."
  (accretions/bag:make))

#|
(defmethod copy ((bag accretions/bag:bag))
  "Creates a shallow copy of the supplied BAG and returns it.  The new
bag shares values with the source bag, but has its own structure and
can be further modified."
  (accretions/bag:copy bag))

(defmethod add ((bag accretions/bag:bag) value)
  "Adds the supplied VALUE to the BAG."
  (accretions/bag:add bag item))

(defmethod size ((bag accretions/bag:bag))
  "Returns the number of items in the supplied BAG."
  (accretions/bag:size bag))

(defmethod emptyp ((bag accretions/bag:bag))
  "Returns a true value if the supplied BAG contains no items; else, NIL."
  (accretions/bag:emptyp bag))

(defmethod mapfun ((bag accretions/bag:bag) function)
  (accretions/bag:mapfun bag function))
|#
