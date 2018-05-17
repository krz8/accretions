(uiop:define-package :accretions/src/queue
  (:use :common-lisp :accretions/src/generics :accretions/src/collections)
  (:export :queue))
(in-package :accretions/src/queue)

(defclass queue (accretions/src/collections:item-collection)
  ((head :accessor qhead :initform nil
	 :documentation "The actual list containing all the items
	 added to the QUEUE collection.")
   (tail :accessor qtail :initform nil
	 :documentation "A cached pointer to the last item in the list
	 at HEAD."))
  (:documentation "QUEUE is the singly-linked list in Accretions.
  It's really not much more than a wrapper around a proper list,
  caching an extra pointer to the tail of the list for fast queuing
  operations via ADD \(otherwise, you'd have to choose between a fast
  enqueue or a fast dequeue operation, as one of them would have to be
  reversed; with the cached tail, both operations can be relatively
  fast\)."))

(defmethod add ((queue queue) &key (item nil itemp))
  "Adds a new item, specified by the :ITEM keyword, to the supplied
  QUEUE.  Returns a true value on success, and raises an error
  condition on failure."
  (let ((head (qhead queue))
	(tail (qtail queue))
	(new (cons item nil)))
    (cond
      ((not itemp)
       (error 'accretions/src/conditions:missing-item
	      :gfname 'add :collection queue))
      ((null head)
       (setf (qhead queue) new
	     (qtail queue) new))
      (t
       (setf (cdr tail) new
	     (qtail queue) new)))))
