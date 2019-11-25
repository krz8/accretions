;;;; double ended queue
(in-package #:accretions)

;;; Just a classic implementation of a double ended queue.  This
;;; really exists for use by some of the functions over in
;;; ternary.lisp, so that we can support both kinds of sequences
;;; (arrays and lists).  It's useful enough, though, that it makes
;;; sense to create a clean implementation and offer it through the
;;; usual API.

(defclass deque (item-collection counted)
  ((forw :accessor forw :initarg :forw
	 :documentation "Links to the \"next\" node in a double ended
	 queue.")
   (back :accessor back :initarg :back
	 :documentation "Links to the \"prev\" node in a double ended
	 queue."))
  (:default-initargs :forw nil :back nil)
  (:documentation "The root of a double ended queue \(deque\).  When
  either the forward or backward links of the node point to itself,
  the deque is considered empty."))

(defclass deque-node (deque counted)
  ((item :accessor item :initarg :item
	 :documentation "The actual item at this location in the deque."))
  (:default-initargs :item nil)
  (:documentation "A node in a double ended queue \(deque\)."))

(defun make-deque (&key (test #'equal))
  "Returns a new deque \(double ended queue\) object, to which items
  can be added, and whose contents can be reviewed. By default, items
  in the deque will be compared with EQUAL during operations such as
  CONTAINSP, but another function can be specified with the :TEST
  keyword."
  (make-instance 'deque :test test))

(defmethod initialize-instance :after ((deque deque) &key &allow-other-keys)
  "When a new deque node is created and either link slot is NIL, both
  slots are rewritten such that the node is an empty deque of its own,
  not part of another deque.  In this way, an initarg of e.g., :FORW
  NIL can be used to indicate a new empty deque.  On the other hand,
  if both slots are not NIL, it's assumed this is a new node to appear
  in some other deque, and no changes are made."
  (when (or (null (forw deque))
	    (null (back deque)))
    (setf (forw deque) deque
	  (back deque) deque)))

(defmethod emptyp ((deque deque))
  "Return T if the supplied double ended queue is empty."
  (format t "~&~%emptyp deque ~s~%forw ~s~%back ~s~%~%"
	  deque (forw deque) (back deque))
  (eql (forw deque) (back deque)))

(defmethod add ((deque deque) &key (item nil itemp) head)
  "Adds a new item, specified by the :ITEM keyword, to the tail end of
  the supplied deque, returning true on success.  If the :HEAD keyword
  is supplied and non-NIL, the new item is added to the front of the
  queue."
  (cond
    ((null itemp)
     (error 'missing-item :gfname 'add :collection deque)
     nil)
    (head
     (let ((new (make-instance 'deque-node :back deque :forw (acr::forw deque)
			       :item item)))
       (setf (back (forw deque)) new
	     (forw deque) new))
     t)
    (t
     (let ((new (make-instance 'deque-node :forw deque :back (acr::back deque)
			       :item item)))
       (setf (forw (back deque)) new
	     (back deque) new))
     t))
  )
