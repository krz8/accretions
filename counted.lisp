;;;; counted mixin
(in-package #:accretions)

;;; At the expense of a little space and a little time, most
;;; containers can be adapted to track their size with a simple mixin
;;; class.

(defclass counted ()
  ((sz :accessor sz :initarg :sz :initform 0 :type integer
       :documentation "A simple counter, incremented on ADD operations."))
  (:documentation "A mixin that adds sizes (counts) to various
  collections.  When ADD is successful for such a collection, the
  count of items therein is incremented."))

(defmethod size ((collection counted))
  "Returns the number of items held within the supplied COLLECTION."
  (sz collection))

;; It's okay for these to be after methods and not around methods.  If
;; the primary ADD method fails, raising some error, the after method
;; will never be triggered and the count shouldn't be off.  So, be
;; sure that the primary ADD methods signal failure and don't simply
;; return NIL.

(defmethod add :after ((collection counted) &key &allow-other-keys)
  (incf (sz collection)))
