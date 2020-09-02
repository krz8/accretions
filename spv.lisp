;;;; sparse vectors

(eval-when (:compile-toplevel)
  (print "compiling spv"))
(eval-when (:load-toplevel)
  (print "loading spv"))
(eval-when (:execute)
  (print "executing spv"))

(defpackage :accretions/spv
  (:use #:cl)
  (:export #:*error* #:sparse-vector #:make-sparse-vector #:make-spv
	   #:spvref))
(in-package :accretions/spv)

(defparameter *error* *error-output*
  "Names a stream to which descriptions of errors in the SPARSE-VECTOR
  package are sent.  When NIL, no error messages are generated.")

(defun err (nvals &rest args)
  "Mostly just a wrapper around FORMAT, except that setting *ERROR* to
NIL won't generate a string, but instead is the way to muzzle errors
in here.  Returns NVALS multiple values, all NIL."
  (when *error*
    (write-sequence "SPARSE-VECTOR Error: " *error*)
    (apply #'format *error* args))
  (apply #'values (make-list nvals)))

(defstruct (sparse-vector (:conc-name spv-) (:constructor %make-spv)
			  (:predicate spvp))
  "Sparse vectors can be implemented in a variety of ways, including
lists, hashes, and tables.  Each has tradeoffs in computation and
space.  This implementation uses a less common approach involving
subvectors, here called \"slices\".  The sparse vector is divided into
slices, each containing SLICESZ elements of the conceptual sparse
vector.  The advantage is that both reading and writing is provided in
constant time, involving two array accesses \(via SVREF\) per call.
The disadvantage is space, with the pathological case being a client
that writes one value to each slice of the sparse vector, resulting in
the instantiation of all slices.  For that reason, this implementation
of sparse vectors is best suited for clients that will work with
relatively localized elements through the sparse vector, or just plain
very sparse elements."
  ;; The total number of elements represented by this SPARSE-VECTOR.
  ;; Unlike traditional vectors, this value may be larger than
  ;; MOST-POSITIVE-FIXNUM.
  (size 1 :type (integer 1 *))
  ;; Every element in this SPARSE-VECTOR is implied to start with this
  ;; value.  Unless explicitly initialized, this slot is set to zero
  ;; when the ELTYPE slot is some sort of NUMBER, otherwise it is set
  ;; to NIL.
  default
  ;; Each slice of the SPARSE-VECTOR contains this many elements
  ;; (except the last slice, which contains up to this many elements).
  ;; With care, this value can "tune" the granularity of the
  ;; SPARSE-VECTOR.
  (slicesz 0 :type fixnum)
  ;; Declares the types of the elements of this SPARSE-VECTOR.  This
  ;; can be T for a SPARSE-VECTOR of indeterminate or "any" elements,
  ;; or it can be something specific (e.g., DOUBLE-FLOAT) which may
  ;; help with the efficiency of the SPARSE-VECTOR.
  (eltype t)
  ;; Each element of this vector is, itself, a vector of elements
  ;; makng up one slice of the SPARSE-VECTOR.
  (slices nil :type simple-vector))

(defun make-sparse-vector (size &key (eltype t) (slicesz 1024)
			   (default nil defaultp))
  "Creates and initializes a new SPARSE-VECTOR, returning it on
success, or NIL on error.  This function also checks a number of types
and bounds, ensuring that the instantiated SPARSE-VECTOR is sane and
safe.  The SPVREF and similar functions, then, can safely make a
number of assumptions about the SPARSE-VECTOR, supporting
optimizations and faster code.

In addition to the expected initializations, extra steps taken here
include:
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
  (cond
    ((or (not (integerp size)) (not (plusp size)))
     (err 1 "The SIZE of a SPARSE-VECTOR cannot be ~W, it must be a ~
             positive integer." size))
    ((or (not (typep slicesz 'fixnum)) (not (plusp slicesz)))
     (err 1 "The SLICESZ of a SPARSE-VECTOR cannot be ~W, it must be ~
             a positive fixnum." slicesz))
    ((not (typep default eltype))
     (err 1 "The DEFAULT element value of a SPARSE-VECTOR must be of ~
             type ELTYPE."))
    (t
     (let ((nslices (ceiling size slicesz)))
       (cond
	 ((> nslices most-positive-fixnum)
	  (err 1 "SLICESZ is too small for the given SIZE of a ~
                  SPARSE-VECTOR."))
	 (t
	  (%make-spv :size size
		     :slicesz slicesz
		     :eltype eltype
		     :default (and defaultp
				   (subtypep eltype 'number)
				   (coerce 0 eltype))
		     :slices (make-array nslices :initial-element nil))))))))

(defmacro make-spv (&rest args)
  "Just a convenience for creating new SPARSE-VECTOR."
  `(make-sparse-vector ,@args))

(defun spvref (spv index)
  "Accesses the element of the supplied SPARSE-VECTOR indicated by the
subscript INDEX, or NIL if an error occurs.  A second value is also
returned, reflecting the success of the access."
  (declare (type sparse-vector spv)
	   (type integer index))
  (let ((size (spv-size spv))
	(slicesz (spv-slicesz spv)))
    ;; This is longer than it needs to be, because we're trying to
    ;; help the optimizer as much as we can to employ fast fixnum
    ;; math.  Question: would unboxed integers be faster?
    (declare (type integer size)
	     (type fixnum slicesz))
    (cond
      ((or (not (integerp index)) (not (plusp index)))
       (err 2 "AnINDEX of a SPARSE-VECTOR must be a positive integer."))
      ((>= index size)
       (err 2 "An INDEX, ~W, of a SPARSE-VECTOR must be less than ~
               its SIZE, ~W." index (spv-size spv)))
      (t
       (multiple-value-bind (i j)
	   (truncate index slicesz)
	 (declare (type fixnum i j))
	 (let ((s (svref (spv-slices spv) i)))
	   (or (and s (svref s j))
	       (spv-default spv))))))))

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
