;;;; sparse vectors

(eval-when (:compile-toplevel)
  (print "compiling spv"))
(eval-when (:load-toplevel)
  (print "loading spv"))
(eval-when (:execute)
  (print "executing spv"))

(defpackage :accretions/spv
  (:use #:cl)
  (:export #:sparse-vector #:make-sparse-vector))
(in-package :accretions/spv)

(defclass sparse-vector ()
  ((slicesize :type unsigned-byte
	      :initarg :slicesize
	      :documentation "The conceptual vector represented by a
	      SPARSE-VECTOR is divided up into slices, each slice
	      being an actual vector holding up to SLICESIZE elements.
	      Consider this a tunable parameter, where different
	      applications using sparse vectors might choose greater
	      or lesser granularity.")
   (default :initarg :default
            :documentation "Until they are explicitly set, all
            elements of all slices are implied to have DEFAULT as
            their value.")
   (type :initarg :type
	 :documentation "The type of elements in the sparse vector.
	 This can simply be T for sparse vectors of indeterminate
	 objects, of course.  But, for sparse vectors of known types
	 \(e.g., double-float\), specifying this can make the
	 underlying slices more efficient.")
   (slices :initform (make-array 0 :adjustable t)
	   :documentation "Each element of this vector is a slice of
	   the entire sparse vector. The index of the element in the
	   slice, multiplied by the maximum slice size, gives the
	   address of that element in that slice.  SLICES is simply
	   the top level vector containing all these slices."))
  (:default-initargs :slicesize 1024 :type t)
  (:documentation "A sparse vector can be thought of as a virtual
  vector divided into SLICES, each containing SLICESIZE elements.
  Each of these slices can be NIL, or can be a vector of up to
  SLICESIZE elements, containing values explicitly set by the caller.
  When not yet set, all elements in all slices are implied to have
  the value DEFAULT."))

(defun make-sparse-vector (&key (slice 1024) (type t) default)
  "Just a convenience function for creating sparse vectors, slightly
more readable than a call to MAKE-INSTANCE.  By default, a sparse
vector is returned, divided into slices of 1024 elements of any type
with a default element value of NIL. These defaults can be changed
with the keyword arguments :SLICE, :TYPE, and :DEFAULT.

.Creates a sparse vector useful for numerical recipes.
  \(make-sparse-vector :type 'double-float :default 0.0d0\)"
  (make-instance 'sparse-vector :slicesize slice :default default
		 :type type))
