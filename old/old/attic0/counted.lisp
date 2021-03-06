;;;; counted mixin
(in-package #:accretions)

;;; At the expense of a little space and a little time, most
;;; containers can be adapted to track their size with a simple mixin
;;; class.

(defclass counted ()
  ((cnt :accessor cnt :initform 0 :type integer
	:documentation "A simple counter, incremented and decremented
	on ADD and DEL operations, respectively."))
  (:documentation "A mixin that adds counts (sizes) to various
  collections.  When ADD is successful for such a collection, the
  count therein is incremented; similarly, DEL may reduce the count of
  a collection."))

(defmethod count ((collection counted))
  "Returns the number of items held within the supplied COLLECTION."
  (count collection))

(defmethod add :around ((collection counted) &key &allow-other-keys)
  "Increments the count of things in the supplied collection by the
  number of things added via the primary ADD method."
  (let ((n (call-next-method)))
    (incf (count collection) n)
    n))

(defmethod del :around ((collection counted) &key &allow-other-keys)
  "This wrapper method decrements the count of things in the supplied
  COLLECTION based on the primary DEL method's result."
  (let ((n (call-next-method)))
    (decf (count collection) n)
    n))
