;;;; generic function declarations
(in-package #:accretions)

;;; Normally, I don't put generics in their own file; it usually makes
;;; more sense to maintain context and place them near their various
;;; method definitions.  But, in the case of Accretions, we want to share
;;; the same family of generic functions across as many different types
;;; as we can, so these all live over here.

(defgeneric add (collection &key &allow-other-keys)
  (:documentation "To the supplied COLLECTION, add something specified
  by keywords specific to each method, returning the number of things
  added unless there was a problem.  Typically, errors are reported
  via a condition, but if conditions are suppressed by the caller,
  this function will return zero.  Note that unlike most other generic
  functions in Accretions, ADD and DEL have integer return values,
  using 0 to indicate failures."))

;; (defgeneric containsp (collection &key &allow-other-keys)
;;   (:documentation "Searches the supplied COLLECTION for some specific
;;   item, key, value, or key/value pair \(the exact keywords are unique
;;   to each method of CONTAINSP\), returning T when a match is found.
;;   If you are looking for \"search-and-something\" functionality, in
;;   order to perform some operation on one or more matches, consider the
;;   combination of MAPFUN with a function that uses RETURN-FROM;
;;   CONTAINSP is just a shortcut for a common idiom."))

;; (defgeneric copy (collection)
;;   (:documentation "Creates a functional copy of the supplied
;;   COLLECTION, returnining it.  The new collection may share structure
;;   with the original COLLECTION, or it may only share datums, depending
;;   on the method of COPY that is invoked.  Likewise, methods are
;;   permitted to return new collections that do not share the underlying
;;   organization of the original COLLECTION \(e.g., trees may be
;;   rebalanced\).  All that will be promised is that the contents of the
;;   COLLECTION, and any ordering thereof, are unchanged.  Errors will be
;;   reported via conditions, and if suppressed COPY will then return
;;   NIL; otherwise, the new collection is returned."))

;; (defgeneric count (collection &key &allow-other-keys)
;;   (:documentation "Returns a non-negative integer indicating the
;;   number of times that the supplied COLLECTION contains some thing
;;   that matches search criteria \(determined by keywords specific to
;;   different methods\)."))

;; (defgeneric del (collection &key &allow-other-keys)
;;   (:documentation "Deletes from a COLLECTION some number of things,
;;   determined by the supplied arguments.  This is a destructive
;;   operation.  A number of different arguments may be presented in
;;   order to control aspects of the deletion; be sure to consult the
;;   method of DEL that is specific to your COLLECTION.  The number of
;;   items deleted from the collection is returned.  Note that unlike
;;   most other generic functions in Accretions, ADD and DEL have integer
;;   return values, using 0 to indicate failures."))

;; ;; secondary return value from del could be a list of the deleted things

;; (defgeneric emptyp (collection)
;;   (:documentation "Returns T when COLLECTION is empty, containing
;;   nothing; otherwise, it returns NIL."))

;; (defgeneric mapfun (collection function)
;;   (:documentation "Calls FUNCTION once for every item present within
;;   COLLECTION.  The expected lambda list for FUNCTION varies depending
;;   on which method of MAPFUN is invoked.  The return value of FUNCTION
;;   is ignored, and MAPFUN returns T so long as no errors occur.  Do
;;   note that to preserve consistency with the rest of Accretions, the
;;   arguments of MAPFUN are reversed from the typical MAP and MAPC
;;   functions defined by Common Lisp."))

;; (defgeneric size (collection)
;;   (:documentation "Returns the number of items currently held within
;;   the supplied COLLECTION."))
