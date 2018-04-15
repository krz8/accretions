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

(defgeneric add (collection &key &allow-other-keys)
  (:documentation "Adds something to COLLECTION, specified by keywords
  specific to each method, returning a true value unless there was a
  problem."))

(defgeneric mapfun (function collection)
  (:documentation "Calls FUNCTION once for every item present within
  COLLECTION.  The expected lambda list for FUNCTION varies depending
  on which method of MAPFUN is invoked.  The return value of FUNCTION
  is ignored, and MAPFUN returns T so long as no errors occur."))

(defgeneric containsp (collection &key &allow-other-keys)
  (:documentation "Searches the supplied COLLECTION for some specific
  item, key, value, or key/value pair \(the exact keywords are unique
  to each method of CONTAINSP\), returning T when a match is found."))

(defgeneric size (collection)
  (:documentation "Returns the number of items currently held within
  the supplied COLLECTION.  Not all collections necessarily support
  SIZE."))
