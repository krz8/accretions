;;;; generics offered throughout Accretions

(defpackage :accretions/src/generics
  (:use :common-lisp)
  (:shadow map)
  (:export #:add #:size #:map #:make #:emptyp))
(in-package :accretions/src/generics)

(defgeneric add (collection item)
  (:documentation "Add the supplied ITEM to the COLLECTION."))

(defgeneric size (collection)
  (:documentation "Returns the number of items currently held within
  the supplied COLLECTION."))

(defgeneric map (collection function)
  (:documentation "For every item in the supplied COLLECTION, the
  supplied function designator is invoked with an item.  An empty
  collection results in no calls to FUNCTION.  Return values of
  FUNCTION are ignored; use your own blocks and returns if you need to
  abort early \(for error processing or whatever\)."))

(defgeneric make (kind)
  (:documentation "Create and return a new container, according to the
  supplied keyword argument.  Currently supported container keywords
  are :BAG."))

(defgeneric emptyp (collection)
  (:documentation "Returns a true value if the supplied COLLECTION
  contains zero items; else, it returns NIL."))
