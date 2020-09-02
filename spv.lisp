;;;; sparse vectors

(eval-when (:compile-toplevel)
  (print "compiling spv"))
(eval-when (:load-toplevel)
  (print "loading spv"))
(eval-when (:execute)
  (print "executing spv"))

(defpackage :accretions/spv
  (:use #:cl)
  (:export #:sparse-vector #:make-sparse-vector #:spvref))
(in-package :accretions/spv)

(defparameter *error* *error-output*
  "Names a stream that errors are sent to in the SPARSE-VECTOR
  package.  Functions in this package tend to return NIL on error, and
  an error message is sent to the stream named by *ERROR* \(the
  standard *ERROR-OUTPUT* stream is default\).  When *ERROR* is NIL, a
  string describing the error is returned by the function detecting
  the error.  Manipulating *ERROR* does not change the meaning of the
  secondary success value returned by functions in this package.")

(defclass sparse-vector ()
  ((size :initarg :size
	 :type unsigned-byte
	 :initform 0
	 :documentation "The total number of elements represented by
	 this SPARSE-VECTOR.  Unlike traditional vectors, this value
	 may be larger than MOST-POSITIVE-FIXNUM.")
   (default :initarg :default
            :documentation "Every element in this SPARSE-VECTOR is
            implied to start with this value.  Unless explicitly
            initialized, this slot is set to zero when the ELTYPE slot
            is some sort of NUMBER, otherwise it is set to NIL.")
   (slicesz :initarg :slicesz
	    :type fixnum
	    :documentation "Each slice of the SPARSE-VECTOR contains
	    this many elements \(except the last slice, which contains
	    up to this many elements\).  With care, this value can
	    \"tune\" the granularity of the SPARSE-VECTOR.")
   (eltype :initarg :eltype
	   :documentation "Declares the types of the elements of this
	   SPARSE-VECTOR.  This can be T for a SPARSE-VECTOR of
	   indeterminate or \"any\" type, or it can be something
	   specific \(e.g., DOUBLE-FLOAT\) which may help with the
	   efficiency of the SPARSE-VECTOR.")
   (slices :type simple-vector
	   :documentation "Each element of this vector is, itself, a
	   vector of elements makng up one slice of the
	   SPARSE-VECTOR."))
  (:default-initargs :slicesz 1024 :eltype t)
  (:documentation "Sparse vectors can be implemented in a variety of
  ways, including lists, hashes, and tables.  Each has tradeoffs in
  computation and space.  This implementation uses a less common
  approach involving subvectors, here called \"slices\".  The sparse
  vector is divided into slices, each containing SLICESZ elements of
  the conceptual sparse vector.  The advantage is that both reading
  and writing is provided in constant time, involving two array
  accesses \(via SVREF\) per call.  The disadvantage is space, with
  the pathological case being a client that writes one value to each
  slice of the sparse vector, resulting in the instantiation of all
  slices.  For that reason, this implementation of sparse vectors is
  best suited for clients that will work with relatively localized
  elements."))

(defmethod initialize-instance :after
    ((spv sparse-vector) &rest initargs)
  "Complete the initialization of a SPARSE-VECTOR, based on the
initialization arguments already processed.  This method can signal
TYPE-ERROR conditions.  In addition to completing the initializations,
this function also checks a number of types and bounds, ensuring that
the instantiated SPARSE-VECTOR is sane and safe.  The SPVREF and
similar functions, then, can safely make a number of assumptions about
the SPARSE-VECTOR, supporting optimizations and faster code.

The initializations completed here are:
. Unless a :DEFAULT element value was explicitly provided for
  the SPARSE-VECTOR, make the default element value zero if the
  element type specified via :ELTYPE was a NUMBER, otherwise NIL.
. Ensure that a valid :SIZE was provided for the SPARSE-VECTOR.
. Ensure that the SLICESZ, which is used to set the length of
  each slice, is small enough to be a fixnum.
. Ensure that the SLICESZ is large enough, with respect to SIZE,
  that the top level vector is indexed by a fixnum.
. Ensure that the default value has a type that matches ELTYPE.
. Create the top level vector of slices."
  (declare (ignore initargs))
  (with-slots (size eltype slicesz slices default) spv
    (unless (slot-boundp spv 'default)
      (setf default (and (subtypep eltype 'number) (coerce 0 eltype))))
    (cond
      ((or (not (integerp size)) (not (plusp size)))
       (format *error* "The SIZE of a SPARSE-VECTOR must be a ~
                        positive integer, not ~W." size)
       (values nil nil))
      ((or (not (typep slicesz 'fixnum)) (not (plusp slicesz)))
       (format *error* "The SLICESZ of a SPARSE-VECTOR must be a ~
                        positive fixnum, not ~W." size)
       (values nil nil)))
    ;; This works, but not all users will recognize (integer 1 *) so
    ;; it's better to explicitly test the two conditions below.
    ;; (check-type size (integer 1 *) "a positive integer")
    (unless (typep default eltype)
      (error 'type-error "DEFAULT element value is not of type ELTYPE"
	     :datum default))
    (let ((nslices (ceiling size slicesz)))
      (if (<= nslices most-positive-fixnum)
	  (setf slices (make-array nslices :initial-element nil))
	  (error 'type-error "SLICESZ is too small for the given SIZE"
		 :datum slicesz)))))

(defmacro make-sparse-vector (&rest args)
  "Just a convenience for instantiating SPARSE-VECTOR objects."
  `(make-instance 'sparse-vector ,@args))

(defun spvref (spv index)
  "Returns the value of the INDEXth element in the supplied
SPARSE-VECTOR."
  (declare (type integer index))
  (with-slots (slices slicesz default size) spv
      (declare (type fixnum slicesz)
	       (type (integer 1 *) size)
	       (type simple-vector slices))
      (unless (< index size)
	(error 'index-error :spv spv :index index))
      (multiple-value-bind (i j)
	  (truncate index slicesz)
	(declare (type fixnum i j))
	(let ((s (svref slices i)))
	  (or (and s (svref s j))
	      default)))))

;; (defun make-spv (&key (slicesize 1024) (type t) default)
;;   "Just a convenience function for creating sparse vectors, slightly
;; more readable than a call to MAKE-INSTANCE.  By default, a sparse
;; vector is returned, divided into slices of 1024 elements of any type
;; with a default element value of NIL. These defaults can be changed
;; with the keyword arguments :SLICESIZE, :TYPE, and :DEFAULT.

;; .Creates a sparse vector useful for numerical recipes.
;;   \(make-sparse-vector :type 'double-float :default 0.0d0\)"
;;   (declare (type fixnum slicesize))
;;   (make-instance 'sparse-vector :slicesize slicesize :default default
;; 		 :type type))

;; (defun spvref (spv index)
;;   "Returns the element of the sparse vector SPV indicated by the INDEX
;; \(a non-negative integer\).  This value might be taken from an
;; existing slice of the sparse vector, or it may be the declared default
;; value for that sparse vector.  This function is non-destructive and
;; will not introduce any changes into the sparse vector or its slices."
;;   (declare (type unsigned-byte index))
;;   (with-slots (slicesize default slices) spv
;;     (declare (type fixnum slicesize) (type vector slices))
;;     (multiple-value-bind (s j)
;; 	(truncate index slicesize)
;;       (declare (type fixnum s j))
;;       (when (< s (length slices))
;; 	(let ((slice (aref slices s)))
;; 	  (declare (type vector slice))
;; 	  (when (and slice (< j (length slice)))
;; 	    (return-from spvref (aref slice j))))))
;;     default))

;; (defsetf spvref (spv index value)
;;     "Sets the element of the sparse vector SPV indicated by the INDEX
;; \(a non-negative integer\) to VALUE.  The corresponding slice of the
;; sparse vector may be extended or created as necessary to accomodate
;; the element.  The corresponding element is set to VALUE, and VALUE is
;; returned."
;;   (declare (type unsigned-byte index))
;;   (multiple-value-bind (s j)
;;       (truncate index (slicesize spv))
;;     (declare (type unsigned-byte s j))
;;     (when (>= s (length (slices spv)))
;;       (adjust-array (slices spv) (1+ s)))
;;     (unless (aref (slices spv) s)
;;       (setf (aref (slices spv) s) (make-array 0 :adjustable t
;; 					      :element-type (spvtype spv))))
    
;;     ))
