;;;; generic function declarations
(in-package #:accretions)

;;; Normally, I don't put generics in their own file; it usually makes
;;; more sense to maintain context and place them near their various
;;; method definitions.  But, in the case of Accretions, we want to share
;;; the same family of generic functions across as many different trees
;;; as we can.

(defgeneric emptyp (collection)
  (:documentation "Returns true when COLLECTION is empty, containing
  nothing."))

(defgeneric add (item collection)
  (:documentation "Adds ITEM to COLLECTION, returning a true value unless
  there was a problem."))

(defgeneric add* (item collection)
  (:documentation "Adds ITEM to COLLECTION in some alternative way,
  returning a true value unless there was a problem.  Where ADD might
  add an item to the front of a deque, for example, ADD* might add it
  to the rear."))

(defgeneric contains (item collection &key test)
  (:documentation "Returns T when COLLECTION has \(at least one\)
  instance of ITEM; otherwise returns NIL.  The function used to
  compare ITEM with items in the COLLECTION is typically EQUAL, but
  any other function can be supplied via the TEST keyword argument."))

(defgeneric mapfun (function collection)
  (:documentation "Calls FUNCTION once for every item present within
  COLLECTION."))

(defgeneric make-iterator (collection)
  (:documentation "Returns a function that iterates over the contents
  of the supplied COLLECTION.  Each call to the returned function will
  return two values: one of the items in COLLECTION, and T.
  After all items have been returned, further calls to the iterating
  function will return the values NIL and NIL.  The second value, then,
  indicates whether the first value is from the COLLECTION or not.

  Some iterators accept an optional keyword as their argument, modifying
  their behavior.  Not all iterators support them, but when they do:

  :PEEK can be used to return values without advancing the iterator.

  :RESET can be used to move the iterator back to its initial state."))

(defmacro with-iterator ((name collection) &body body)
  "Creates an iterator for COLLECTION, binds it to NAME as a function,
  and evaluates BODY in that new lexical context.  See MAKE-ITERATOR
  for details on the iterator."
  (let ((fun (gensym))
	(args (gensym)))
    `(let ((,fun (make-iterator ,collection)))
       (labels ((,name (&rest ,args) (apply ,fun ,args)))
	 ,@body))))

(defgeneric size (collection)
  (:documentation "Returns the number of items currently held within
  the supplied COLLECTION."))
