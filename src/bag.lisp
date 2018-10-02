;;;; a bag is an unordered collection of items

(defpackage #:accretions/src/bag
  (:use #:cl)
  (:export #:bag #:make #:add #:emptyp #:size))

(in-package #:accretions/src/bag) 

;;; We'll use a simple cons list to represent our bag type.  This
;;; will imply an ordering to its contents, but that's not a big
;;; deal; no one should rely on any ordering in a bag.

(defclass bag ()
  ((head :initarg :head :accessor head
	 :documentation "Points to the head of the list of items in the bag.")
   (sz :initarg :sz :accessor sz :type (integer 0 *)
       :documentation "Tracks the number of items in the bag."))
  (:default-initargs :head nil :sz 0)
  (:documentation "An unordered collection of items.  Unlike queues
  and other collections, individual items in a bag cannot be deleted
  \(the bag itself can be deleted, of course, which removes the
  references to all items within it\)."))

(defun make ()
  "Create and return a new empty bag object."
  (make-instance 'bag))

(defun size (bag)
  "Return the number of items currently in the supplied BAG."
  (sz bag))

(defun emptyp (bag)
  "Return T if the supplied BAG contains zero items; else, return NIL."
  (null (head bag)))

;;; We used to test that the CONS succeeded before modifying the list
;;; and returning true; the idea was that CONS is really the only
;;; thing that could fail in ADD.  In practice, though, any situation
;;; that leads to a CONS failure will raise an error condition of some
;;; kind.  For that reason, ADD doesn't actually test anything; it
;;; assumes that if an error is not raised, the operation is
;;; successful.

(defun add (bag item)
  "Add ITEM to the supplied BAG, returning that BAG so that more
operations could be chained on it."
  (setf (head bag) (cons item (head bag)))
  (incf (sz bag))
  bag)
