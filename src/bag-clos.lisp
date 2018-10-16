;;;; CLOS methods for operating on bags
#-asdf3.1 (error "ACCRETIONS/SRC/BAG-CLOS requires ASDF 3.1.2 or later")

(defpackage #:accretions/src/bag-clos
  (:use #:cl #:accretions/src/generics)
  (:export #:add #:emptyp #:size #:map))
(in-package #:accretions/src/bag-clos)

(defmethod add ((bag accretions/src/bag:bag) item)
  "Adds the supplied ITEM to the BAG."
  (accretions/src/bag:add bag item))

(defmethod size ((bag accretions/src/bag:bag))
  "Returns the number of items in the supplied BAG."
  (accretions/src/bag:size bag))

(defmethod emptyp ((bag accretions/src/bag:bag))
  "Returns a true value if the supplied BAG contains no items; else, NIL."
  (accretions/src/bag:emptyp bag))

(defmethod mapcoll ((bag accretions/src/bag:bag) fun)
  (accretions/src/bag:mapcoll bag fun))
