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

(defclass bag (item-collection counted)
  ((items :accessor items :initform nil
	  :documentation "The actual list maintained as the collection
	   of items in the bag."))
  (:documentation "A bag is a simple unordered collection of items."))

(defun make-bag (&key (test #'equal))
  "Returns a new bag object to which items can be added, and whose
  contents can be reviewed.  By defaut, items in the returned bag
  will be compared with EQUAL during operations such as CONTAINSP,
  but another function can be specified with the :TEST keyword."
  (make-instance 'bag :test test))

(defmethod emptyp ((bag bag))
  "Return T if the supplied bag contains nothing."
  (null (items bag)))

(defmethod add ((bag bag) &key (item nil itemp))
  "Adds an item to the supplied BAG, specified by the :ITEM keyword,
  returning true on success."
  (cond
    (itemp
     (push item (items bag))
     t)
    (t
     (error 'missing-item :gfname 'add :collection bag)
     nil)))

(defmethod mapfun (function (bag bag))
  "Invoke FUNCTION once for every item in the supplied BAG, passing
  an item as its single argument each time.  Always returns true."
  (mapc function (items bag))
  t)

;; We allow the user to store NIL in the bag.  If we didn't, CONTAINSP
;; could be a simple call to FIND.  We could use POSITION, but that function
;; typically incurs a tiny penalty in tracking an index as it progresses.
;; So, we'll DO it ourselves.

(defmethod containsp ((bag bag) &key (item nil itemp))
  "Returns T if the BAG contains the item specified by the :ITEM
  keyword argument."
  (if itemp
      (let ((test (testfn bag)))
	(do ((x (items bag) (cdr x)))
	    ((null x) nil)
	  (when (funcall test (car x) item)
	    (return-from containsp t))))
      (error 'missing-item
	     :gfname 'containsp
	     :collection bag))
  nil)
