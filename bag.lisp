;;;; bag support
(in-package #:accretions)

;;; A bag is just a collection of items that you can later review;
;;; also known as a multiset in some circles.  There is no explicit
;;; order within a bag.  More, when working through the contents of a
;;; bag, there is no promised order to the items returned (our
;;; implementation here is based on a simple list and so is a LIFO
;;; queue, but you probably shouldn't rely on that).
;;;
;;; We'll implement a bag as a singly linked list.  You'll probably
;;; never really need it, as the Common Lisp macro PUSH is more than
;;; handy enough.  However, every once in a while you'd like to pass a
;;; list as a simple argument to be modified by another function
;;; without list surgery or macrology, and this is where the slight
;;; overhead of a CLOS instance helps us out.
;;;
;;; However, I won't deny that the bag class exists mostly to test
;;; some aspects of the Accretions implementation.

(defclass bag ()
  ((items :accessor items :initform nil
	  :documentation "The actual list maintained as the collection
	   of items in the bag."))
  (:documentation "A bag is a simple collection of items, implemented
  here using a plain list."))

(defun make-bag ()
  "Returns a new bag object to which items can be added, and whose
  contents can be reviewed."
  (make-instance 'bag))

(defmethod emptyp ((bag bag))
  "Return T if the supplied bag contains nothing."
  (null (items bag)))

(defmethod add (item (bag bag))
  "Adds an ITEM to the supplied BAG, returning true on success."
  (setf (items bag) (cons item (items bag)))
  t)

(defmethod mapfun (function (bag bag))
  "Invoke FUNCTION once for every item in the supplied BAG, passing
  that item as its single argument.  Always returns true."
  (mapc function (items bag))
  t)

(defmethod make-iterator (bag)
  "Returns a function that iterates over the contents of the supplied
  THING.  Each call to the returned function will return two values:
  one of the items in THING, and T.  After all items have been
  returned, further calls to the iterating function will return the
  values NIL and NIL.  The second value, then, indicates whether the
  first value is from the THING or not.

  The returned iterating function also accepts one of two optional
  keyword arguments:

  :PEEK can be used to return values without advancing the iterator.

  :RESET can be used to move the iterator back to its initial state."
  (let ((cursor (items bag)))
    (dlambda
     (:peek () (values (car cursor) (and cursor t)))
     (:reset () (setf cursor (items bag))
	        t)
     (t () (let ((old cursor))
	      (setf cursor (cdr cursor))
	      (values (car old) (and old t)))))))

;; We allow the user to store NIL in the bag.  If we didn't, CONTAIN
;; would be a simple call to
;; 
;;    (find item (items bag) :test test)
;;
;; But that won't work if the user stores NIL in the bag and later
;; looks for it.  So, we'll do this in a slightly slower way, but one
;; that allows us to always return a useful true or false value.

(defmethod contains (item (bag bag) &key (test #'equal))
  "Returns T if the BAG contains ITEM, according to TEST; otherwise,
  returns NIL.  The default test function is EQUAL unless overriden
  with the :TEST keyword argument."
  (do ((x (items bag) (cdr x)))
      ((null x) nil)
    (when (funcall test (car x) item)
      (return-from contains t))))

(defclass counted-bag (bag counted)
  ()
  (:documentation "Like a simple BAG, but also supports returning the
  number of objects contained via SIZE."))

(defun make-counted-bag ()
  "Returns a new counted bag object to which items can be added, and
  whose contents can be reviewed.  Unlike a plain bag, the counted
  bag also supports the SIZE generic function, returning the number of
  items in the bag at any point in time."
  (make-instance 'counted-bag))
