;;;; generic function declarations
(in-package #:accretions)

;;; Normally, I don't put generics in their own file; it usually makes
;;; more sense to maintain context and place them near their various
;;; method definitions.  But, in the case of Accretions, we want to share
;;; the same family of generic functions across as many different types
;;; as we can.

(defgeneric emptyp (collection)
  (:documentation "Returns true when COLLECTION is empty, containing
  nothing."))

(defgeneric add (collection &key item key value tail)
  (:documentation "Adds something to COLLECTION, returning a true value
  unless there was a problem.  When adding to a COLLECTION of single
  items \(such as a BAG\), use the :ITEM keyword to specify the item
  being added to the collection.  When adding to a COLLECTION of key
  and value pairs, use the keywords :KEY and :VALUE accordingly.
  When the COLLECTION maintains a \"head\" and \"tail\", use the
  :TAIL keyword with a non-NIL value to specify addition at the tail
  of the collection instead of its head."))

(defgeneric mapfun (function collection)
  (:documentation "Calls FUNCTION once for every item present within
  COLLECTION.  When MAPFUN is called for a collection of items,
  FUNCTION should expect to be called with a single argument.  When
  MAPFUN is invoked for a collection of key/value pairs, FUNCTION
  should expect to be called with two arguments, a key value followed
  by its associated value.  The return value of FUNCTION is ignored,
  and MAPFUN returns T so long as no errors occur."))

(defgeneric containsp (collection &key item key value)
  (:documentation "Returns T when COLLECTION has at least one instance
  of the supplied ITEM, KEY, or VALUE, or both; otherwise returns NIL.
  When searching collections of items, use the :ITEM keyword to
  specify the item to be sought in the collection.  When searching
  collections of key/value pairs, use :KEY or :VALUE to search; you
  can supply both to test for pairs instead of just one or the
  other."))

(defgeneric size (collection)
  (:documentation "Returns the number of items currently held within
  the supplied COLLECTION.  Not all collections necessarily support
  SIZE."))
