;;;; counted mixin
(in-package #:accretions)

;;; At the expense of a little space and a little time, most
;;; containers can be adapted to track their size with a simple mixin
;;; class.

(defclass counted ()
  ((sz :accessor sz :initarg :sz :type integer
       :documentation "A simple counter, incremented on ADD operations."))
  (:default-initargs :sz 0)
  (:documentation "A mixin that adds sizes (counts) to various
  collections.  When ADD is successful for such a collection, the
  count of items therein is incremented."))

(defmethod size ((collection counted))
  "Returns the number of items held within the supplied COLLECTION."
  (sz collection))

#+nil
(defmethod add :after ((collection counted) &key &allow-other-keys)
  (incf (sz collection)))

(defmethod add :around ((collection counted) &key &allow-other-keys)
  (when (call-next-method)
    (incf (sz collection))))
