;;;; counted mixin
(in-package #:accretions)

;;; At the expense of a little space and a little time, most
;;; containers can be adapted to track their size with a simple mixin
;;; class.

(defclass counted ()
  ((sz :accessor sz :initarg :sz :initform 0 :type integer
       :documentation "A simple counter, incremented on ADD operations."))
  (:documentation "A mixin that adds sizes (counts) to various
  collections.  When ADD or ADD* is successful for such a collection,
  the count of items therein is incremented."))

(defmethod size ((collection counted))
  "Returns the number of items held within the supplied COLLECTION."
  (n collection))

(defmethod add :around (item (collection counted))
  (unless (call-next-method item collection)
    (return-from add nil))
  (incf (sz collection))
  t)

(defmethod add* :around (item (collection counted))
  (unless (call-next-method item collection)
    (return-from add* nil))
  (incf (sz collection))
  t)
