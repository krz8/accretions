;;;; generics offered throughout Accretions

(defpackage :accretions/src/generics
  (:use :common-lisp)
  (:export #:make #:copy #:add #:size #:mapfun #:emptyp))
(in-package :accretions/src/generics)

(defgeneric make (kind)
  (:documentation "Returns a new Accretions collection according to
the KIND argument."))

(defgeneric copy (collection)
  (:documentation "Returns a shallow copy of an Accretions collection.
The structure of the collection is distinct, but its keys and values
are shared with the source collection."))

(defgeneric add (collection item)
  (:documentation "Add the supplied ITEM to the COLLECTION."))

(defgeneric size (collection)
  (:documentation "Returns the number of items currently held within
the supplied COLLECTION."))

(defgeneric mapfun (collection function)
  (:documentation "For every item in the supplied COLLECTION, the
supplied function designator is invoked with an item.  An empty
collection results in no calls to FUNCTION.  Return values of FUNCTION
are ignored; use your own blocks and returns if you need to abort
early \(for error processing or whatever\)."))

(defgeneric emptyp (collection)
  (:documentation "Returns a true value if the supplied COLLECTION
contains zero items; else, it returns NIL."))
