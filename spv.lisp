;;;; sparse vectors

(eval-when (:compile-toplevel)
  (print "compiling spv"))
(eval-when (:load-toplevel)
  (print "loading spv"))
(eval-when (:execute)
  (print "executing spv"))

(defpackage :accretions/spv
  (:use #:cl #:alexandria)
  (:export #:*error*
	   ;; #:sparse-vector #:make-sparse-vector #:make-spv #:spvref
	   ))
(in-package :accretions/spv)

;; (defparameter *max-size* (* array-dimension-limit array-dimension-limit)
;;   "The maximum size of any sparse vector.  In practice, there are
;;   other limitations \(memory, swap, address bits\) that will limit the
;;   size of a sparse vector, but if none apply, this is the hard limit
;;   of our design.")

(defparameter *max-vector-size* 64	; array-dimension-limit
  "This defines the maximum size of any vector or slice at any level
  contained within a sparse vector.Normally, this is
  ARRAY-DIMENSION-LIMIT, but for testing purposes you might use a much
  lower number here to artifically induce deeper nesting in the sparse
  vector.")

(defparameter *non-sparse-threshold* 12
  "If a sparse vector is requested of this size or smaller, the result
  is a sparse vector that has no nesting: its depth is forced 1, and
  its index size and slice size are ignored.  I don't know what the
  \"right\" value is here, I'm just taking a guess.")

(defparameter *error* *error-output*
  "Names a stream to which descriptions of errors in the SPARSE-VECTOR
  package are sent.  When NIL, no error messages are generated.")

(defun err (control-string &rest args)
  "If *ERROR* is not NIL, calls FORMAT on the supplied control string
  and any arguments, sending the message to the stream *ERROR*. Mostly
  just a wrapper around FORMAT, except that setting *ERROR* to NIL
  won't generate a string, but instead is the way to muzzle errors.
  Always returns NIL."
  (when *error*
    (apply #'format *error*
	   (concatenate 'string "~&Error: " control-string "~%")
	   args))
  nil)

(defun warn (control-string &rest args)
  "Like ERR, except noting that this is a warning, and always
  returning T."
    (apply #'format *error*
	   (concatenate 'string "~&Warning: " control-string "~%")
	   args))
  t)

(defmacro all-plus-p (&rest args)
  "Expands into PLUSP called for every argument, chained together with
  an AND operator.  0 is our \"unset\" value in a sparse vector's
  attributes, so once you've established that fields are numeric and
  not negative, you can use this to test for combinations of \"set\"
  fields."
  (let ((tests))
    (dolist (a args)
      (push `(plusp ,a) tests))
    `(and ,@tests)))

(defun iroot (x n)
  "Returns the nearest integer equal to OR GREATER THAN the actual Nth
  root of X.  A secondary return value is the difference between the
  returned integer and the actual root."
  (declare (type integer x n))		; does this help?
  (ceiling (expt x (/ 1 n))))

(defstruct (sparse-vector (:conc-name spv-) (:constructor make-spv))
  ;; The total number of elements represented by this SPARSE-VECTOR.
  ;; Unlike traditional vectors, this value may be (much) larger than
  ;; either ARRAY-DIMENSION-LIMIT or MOST-POSITIVE-FIXNUM.  This DOES
  ;; NOT represent the size of the sparse vector in memory in any way.
  (size 0 :type integer)
  ;; The "depth" of a sparse vector represents its nesting depth,
  ;; describing how many levels of vectors there are until you get to
  ;; an actual element.
  (depth 0 :type fixnum)
  ;; The top level index vector, and any instantiated middle vectors,
  ;; are of this size.
  (index-size 0 :type fixnum)
  ;; The number of actual elements in each slice ("bottom" vector).
  (slice-size 0 :type fixnum)
  ;; Like the ELEMENT-TYPE of a traditional vector, this can be set to
  ;; help the Lisp engine pack slices into memory more efficiently.
  (element-type t)
  ;; As with a traditional array or vector, we have an initial element
  ;; whose value is assumed for all elements in the sparse vector
  ;; until otherwise set.
  (initial-element nil)
  ;; The tree of vectors.
  (tree nil :type (or null simple-vector)))

(defun ensure-types (size depth indexsz slicesz element-type)
  "Ensure that the values of certain attributes of a sparse vector to
  be created are of valid types.  This doesn't validate the values
  themselves, but simply that what the user supplied conforms.
  Returns T if everything looks plausible, or NIL if the sparse vector
  cannot be instantiated."
  (macrolet ((tst (var)
	       `((not (integerp ,var))
		 (err "The ~A of a sparse vector must be an integer."
		      ,(string var)))))
    (cond
      (tst size)
      (tst depth)
      (tst indexsz)
      (tst slicesz)
      ((null element-type)
       (err "The element type of a sparse vector must not be NIL."))
      (t
       t))))

;; The depth, index size, and slice size given to us by a user are
;; strong suggestions.  If they work, we leave them alone.  If they
;; don't work, we'll first modify the slice size.  If that doesn't
;; work, we'll change the index sizes.  If *that* doesn't work, then
;; we change the depth.
;;
;; If a size attribute (depth, index size, or slice size) is 0, that
;; represents an "unset" value from the user.  We do this, rather than
;; NIL, to keep the type specifiers short, and to quickly ensure all
;; values can be tested with ZEROP or PLUSP as needed.
;;
;; All the functions that work with testing or setting the size
;; attributes of a sparse vector have one of three return values. NIL
;; means that the tests didn't apply, no changes were made, and the
;; caller should continue trying other functions. T and 'FAIL both
;; mean further testing should stop; T indicating success, and 'FAIL
;; meaning just that.

(defun check-sizes-tiny (spv)
  "We need to draw the line somewhere; under a certain size, there is
  just no point to a sparse vector at all.  Maybe we should assert an
  error when this happens?  Anyway, returns T when leaving the sparse
  vector set with a depth of 1, or NIL when testing should continue."
  (declare (type sparse-vector spv))
  (when (<= (spv-size spv) *non-sparse-threshold*)
    (setf (spv-depth spv) 1
	  (spv-index-size spv) 0
	  (spv-slice-size spv) size)
    (warn "The requested size of the sparse vector is not greater ~
          than *NON-SPARSE-THRESHOLD*, so a sparse vector of DEPTH ~
          1 will be created, ignoring any supplied values for DEPTH, ~
          INDEX-SIZE, or SLICE-SIZE.")))

(defun check-sizes-all (spv)
  "If the user specified everything, see if the resulting sparse
  vector fits.  If not, try relaxing one or more attributes until a
  good sparse vector is possible.  Returns T if everything is good and
  the sparse vector can be created as-is, returns NIL if testing should
  continue \(after possibly zeroing one or more attributes\), or
  returns 'FAIL if there's just no chance of making this sparse vector
  work."
  (declare (type sparse-vector spv))
  (let ((size (spv-size spv)) (depth (spv-depth spv))
	(indexsz (spv-index-size spv)) (slicesz (spv-slice-size spv)))
    (when (all-plus-p size depth indexsz slicesz)
      (cond
	((<= size (* slicesz (expt indexsz (1- depth))))
	 t)
	((<= size (* array-dimension-limit (expt indexsz (1- depth))))
	 (setf (spv-slice-size spv) 0)
	 (warn "The specified SLICE-SIZE, ~W, of the requested sparse ~
               vector has been disregarded in order to satisfy the ~
               requested SIZE, ~W." slicesz size)
	 nil)
	((<= size (* array-dimension-limit
		     (expt array-dimension-limit (1- depth))))
	 (setf (spv-index-size spv) 0
	       (spv-depth-size spv) 0)
	 (warn "The specified INDEX-SIZE, ~W, and SLICE-SIZE, ~W, of ~
               the requested sparse vector have been disregarded in ~
               order to satisfy the requested SIZE, ~W." indexsz slicesz size)
	 nil)
	(t
	 (setf (spv-depth spv) 0
	       (spv-index-size spv) 0
	       (spv-depth-size spv) 0)
	 (warn "The specified DEPTH, INDEX-SIZE, and SLICE-SIZE of ~
               the requested sparse vector have all been disregarded in ~
               order to satisfy the requested SIZE, ~W." size)
	 nil)))))

;; (do* ((level 2 (1+ level))
;; 	   (total (expt slicesz level) (expt slicesz level)))
;; 	  ((>= total req-size)
;; 	   (values level slicesz))))))

(defun check-sizes-slice (spv)
  "If we've been given a SLICE-SIZE but nothing else, find a DEPTH and
  INDEX that will satisfy the requested SIZE for the sparse vector.
  Returns T if we've found it, or NIL if nothing obvious works."
  (declare (type sparse-vector spv))
  (let ((size (spv-size spv)) (depth (spv-depth spv))
	(indexsz (spv-index-size spv)) (slicesz (spv-slice-size spv)))
    (when (and (plusp slizesz) (zerop depth) (zerop indexsz))
      (let ((indexes (ceiling size slicesz)))
	(do* ((depth 2 (1+ depth)))
	     ((x (()))))))))

(defun check-sizes-none (spv)
  "If none of DEPTH, INDEX SIZE, or SLICE SIZE were set, determine a
  minimal sparse vector by taking roots of the requested size.  Note
  that it is just my hypotheis that this gives the smallest sparse
  vector, I have not actually proven it.  Returns T if the depth,
  index, and slice sizes have been set to something that will yield a
  working sparse vector, or NIL if nothing could be done."
  (let ((size (spv-size spv)) (depth (spv-depth spv))
	(indexsz (spv-index-size spv)) (slicesz (spv-slice-size spv)))
    (unless (or (plusp depth) (plusp indexsz) (plusp slicesz))
      (do* ((depth 2 (1+ depth))
	    (x (iroot size depth) (iroot size depth)))
	   ((< x *max-vector-size*)
	    (setf (spv-depth spv) depth
		  (spv-index-size spv) x
		  (spv-slice-size spv) x)
	    t)))))

;;   It is only my hypothesis that this function returns a particularly
;;   good small sparse vector for the given size; I have not, in fact,
;;   tried to prove it.  For a faster sparse vector, at the price of
;;   space, specify some attributes to MAKE-SPARSE-VECTOR \(which will
;;   cause a different function than this one to be called\)."
;;   (do* ((level 2 (1+ level))
;; 	(slicesz (iroot req-size level) (iroot req-size level)))
;;        ((< slicesz *max-vector-size*)
;; 	(list level slicesz slicesz))))


(defun check-sizes (spv)
  (declare (type sparse-vector spv))
  (let ((x (or (check-sizes-tiny spv)
	       (check-sizes-all spv)
	       (check-sizes-none spv))))
    ;; If 'FAIL, we can be reasonably certain an error message was
    ;; already sent. If NIL, then nothing worked and we need to say
    ;; something.  If T, that's a success and nothing needs to be
    ;; sent.
    (case x
      ('fail nil)
      (nil (err "A sparse vector of size ~W could not be created."
		(spv-size spv)))
      (t t))))

(defun check-sizes (spv)
  "Examine the combinations of size and other attributes given for the
  sparse vector.  Missing attributes are determined, others are
  adjusted if necessary.  A true value is returned if the resulting
  sparse vector can be used, otherwise NIL."
  (macrolet ((chk (var)
	       `((not (and (integerp ,var) (1 < ,var array-dimension-limit)))
		 (err "When specified, the ~A, ~W, of a sparse vector ~
                      must be a positive integer between 1 and ~
                      ARRAY-DIMENSION-LIMIT." ,(string var) ,var))))
    (let ((size (spv-size spv)) (depth (spv-depth spv))
	  (index-size (spv-index-size spv)) (slice-size (spv-slice-size spv)))
      (cond
	((not (typep size '(integer 1 *)))
	 (err "The SIZE, ~W, of a sparse vector must be a positive integer."
	      size))
	(chk depth)
	(chk index-size)
	(chk slice-size)
	((handle-sizes spv) t)))))

(defun fix-initel (initel initel-p eltype)
  "Returns the initial-element we should use for a sparse vector.  If
  the caller names a specific initial-element, ensure that it respects
  the element-type.  Otherwise, if the caller specifies an element
  type that is some kind of number, make our initial element zero.
  Otherwise, set the initial-element to NIL.

  Returns a list of initial element and element type when successful,
  or NIL on an error."
  (cond
    ((not initel-p)
     (list (and (subtypep eltype 'number) (coerce 0 eltype)) eltype))
    ((typep initel eltype)
     (list initel eltype))
    (t
     (err "The initial element, ~W, of a sparse vector must be of the ~
          element type, ~W." initel eltype))))

(defun init-spv (spv)
  "Returns SPV on success, or NIL."
  (declare (type sparse-vector spv))
  (setf (spv-tree spv) (make-array (spv-index-size spv)))
  spv)

;; We could get away with &ALLOW-OTHER-KEYS here, but specifying the
;; exact keywords gives us a chance to work with their values before
;; instantiating the structure. A compiler like SBCL will check types
;; and everything for us, but others might not.  So we'll do it here
;; explicitly for everyone.
(defun make-sparse-vector (size &key (depth 2) index-size slice-size
				  (element-type t)
				  (initial-element nil initial-element-p))
  "Constructor for a SPARSE-VECTOR.  SIZE requests the total number of
  elements contained in the vector; this value may be larger than
  ARRAY-DIMENSION-LIMIT or MOST-POSITIVE-FIXNUM.  INITIAL-ELEMENT and
  ELEMENT-TYPE are as MAKE-ARRAY.  DEPTH, INDEX-SIZE, and SLICE-SIZE
  are all optional; for whichever parameters aren't specified, values
  will be chosen that satisfy the parameters given.  Returns a new
  SPARSE-VECTOR, or NIL on some error."
  (let ((spv (make-spv :size size :depth depth :index-size index-size
		       :slice-size slice-size :element-type eltype
		       :initial-element initial-element)))
    (cond
      ((and (handle-sizes spv)
	    (handle-initel spv initial-element-p))
       (setf (spv-tree spv) (make-array (spv-index-size spv)))
       spv)
      (t
       nil))))

;; (defun validate-attrs (size depth index-size slice-size)
;;   "When constructing a sparse vector of the specified SIZE, check that
;;   the depth of the slices, the maximum size of the index vectors, and
;;   the maximum size of the slices, are all viable. Returns a list of
;;   three values: the depth. index size, and slice size.  If the values
;;   specified cannot create a working sparse vector, or would not
;;   satisfy SIZE, or on any other error is encountered, a message is
;;   sent to *ERROR* and NIL is returned."
;;   (and (plausible-size-p size)
;;        (plausible-depth-p depth)
;;        (plausible-index-or-slice-p index-size)
;;        (plausible-index-or-slice-p slice-size t)
;;        (or (>= (* (expt index-size (1- depth)) slice-size) size)
;; 	   (err "The supplied attributes of a sparse vector will ~
;;                 not satisfy the requested size, ~W." size))
;;        (list depth index-size slice-size)))

;; (defun attrs-from-size (req-size)
;;   "Given the requested size for a sparse vector, determine the
;;   attributes necessary for a sparse vector taking up minimal size in
;;   memory, returning a list three values: the depth, the index size,
;;   and the slice size.  On an error, a message is sent to *ERROR* and
;;   NIL is returned.  The global *MAX-VECTOR-SIZE* provides a cap to the
;;   index size and slice size of the proposed sparse vector.

;;   It is only my hypothesis that this function returns a particularly
;;   good small sparse vector for the given size; I have not, in fact,
;;   tried to prove it.  For a faster sparse vector, at the price of
;;   space, specify some attributes to MAKE-SPARSE-VECTOR \(which will
;;   cause a different function than this one to be called\)."
;;   (do* ((level 2 (1+ level))
;; 	(slicesz (iroot req-size level) (iroot req-size level)))
;;        ((< slicesz *max-vector-size*)
;; 	(list level slicesz slicesz))))

;; (defun best-guess-slicesz (req-size slicesz)
;;   "Given a requested size for a sparse vector, and the desired slice
;;   size, return two values: the necessary nesting depth of the sparse
;;   vector, and the size of each vector nested within it.  On an error,
;;   the two values NIL NIL are returned.

;;   This function optimizes the sparse vector to use the specified slice
;;   size.  This can be made large or small, optimizing for speed or
;;   space.  The value must be kept under *MAX-VECTOR-SIZE*.  See
;;   BEST-GUESS as an alternate function."
;;   (cond
;;     ((> slicesz *max-vector-size*)
;;      (err 2 "The requested SLICESZ, ~W, for a sparse vector cannot be ~
;;             greater than *MAX-VECTOR-SIZE*, ~W." slicesz *max-vector-size*))
;;     (t
;;      (do* ((level 2 (1+ level))
;; 	   (total (expt slicesz level) (expt slicesz level)))
;; 	  ((>= total req-size)
;; 	   (values level slicesz))))))

;; (defun get-sizes (size slicesz)
;;   "Given the requested size of a sparse vector and its slice size,
;;   return two values: the number of slices needed for the sparse vector
;;   and the (possibly new) slice size.  On any error, two NIL values are
;;   returned.

;;   This function is constrained by the Common Lisp standard, under
;;   which a vector is indexed by fixnum values \(thus, the size of a
;;   vector is also constrained to a fixnum\).  The supplied SLICESZ may
;;   be NIL.  If SLICESZ is NIL, or not a FIXNUM, or too small to yield a
;;   workable number of slices, this function disregards it and chooses a
;;   new slice size."
;;   (cond
;;     ((not (and (integerp size) (plusp size)))
;;      (err 2 "A SIZE, ~W, was requested, but SPARSE-VECTOR sizes ~
;;             must be positive integers." size))
;;     ((>= size *max-size*)
;;      (err 2 "The requetsted SIZE, ~W, was requested, but SPARSE-VECTOR sizes ~
;;             are limited to ." size))
;;     (t
;;      )
;;     (return-from get-sizes
;;       ))
  
;;   (let ((slicesz (and (typep slicesz 'fixnum) slicesz)))
;;     )
;;   (cond
;;     ((not (typep size '(integer 1 *)))
;;      (values nil nil))
;;     ((not (typep slicesz 'fixnum))
;;      (values nil nil))
;;     (t
;;      (let ((nslices (ceiling size slicesz)))
;;        (cond
;; 	 ((typep nslices '(integer 1 *))
;; 	  nslices)
;; 	 (t
;; 	  ))))))

;; (defstruct (sparse-vector (:conc-name spv-) (:constructor %make-spv)
;; 			  (:predicate spvp))
;;   "Sparse vectors can be implemented in a variety of ways, including
;;   lists, hashes, and tables.  Each has tradeoffs in computation and
;;   space.  This implementation uses a less common approach involving
;;   subvectors, here called \"slices\".  The sparse vector is divided
;;   into slices, each containing SLICESZ elements of the conceptual
;;   sparse vector.  The advantage is that both reading and writing is
;;   provided in constant time, involving two array accesses \(via
;;   SVREF\) per call.  The disadvantage is space, with the pathological
;;   case being a client that writes one value to each slice of the
;;   sparse vector, resulting in the instantiation of all slices.  For
;;   that reason, this implementation of sparse vectors is best suited
;;   for clients that will work with relatively localized elements
;;   through the sparse vector, or just plain very sparse elements."
;;   ;; The total number of elements represented by this SPARSE-VECTOR.
;;   ;; Unlike traditional vectors, this value may be larger than
;;   ;; MOST-POSITIVE-FIXNUM.
;;   (size 1 :type (integer 1 *))
;;   ;; Every element in this SPARSE-VECTOR is implied to start with this
;;   ;; value.  Unless explicitly initialized, this slot is set to zero
;;   ;; when the ELEMENT-TYPE slot is some sort of NUMBER, otherwise it is set
;;   ;; to NIL.
;;   initial-element
;;   ;; Each slice of the SPARSE-VECTOR contains this many elements
;;   ;; (except the last slice, which contains up to this many elements).
;;   ;; With care, this value can "tune" the granularity of the
;;   ;; SPARSE-VECTOR.
;;   (slicesz 0 :type fixnum)
;;   ;; Declares the types of the elements of this SPARSE-VECTOR.  This
;;   ;; can be T for a SPARSE-VECTOR of indeterminate or "any" elements,
;;   ;; or it can be something specific (e.g., DOUBLE-FLOAT) which may
;;   ;; help with the efficiency of the SPARSE-VECTOR.
;;   (element-type t)
;;   ;; Each element of this vector is, itself, a vector of elements
;;   ;; makng up one slice of the SPARSE-VECTOR.
;;   (slices nil :type simple-vector))

;; (defun generate-slicesz (size slicesz)
;;   "Given the size of a sparse vector, and a specified slicesz (or NIL,
;; if not specified), return the best guess at a slice size.  If the
;; caller's preferred slicesz can be used, it will be preserved.

;; Per the standard, arrays (vectors) are indexed by fixnums.  Now, on a
;; modern 64 bit platform, most-positive-fixnum is pretty huge, so it's
;; not likely we'll violate this rule.  But we'll keep the checks in
;; place for 32 bit platforms and for \"very very sparse\" vectors. ;-)

;; If we need to derive a new slice size, what we'll do is take a nearby
;; square root of the total size, using that as a slicesz.  If the
;; resulting number of slices is also a fixnum, then our guess is good
;; enough and we'll run with it.  If not, just return NIL."
;;   (cond
;;     ())
;;   (let ((nslices (ceiling size slicesz)))
;;     (cond
;;       ((and (typep slicesz 'fixnum) (typep nslices 'fixnum))
;;        slicesz)
;;       ;; Nope, the supplied slicesz doesn't work; either it, or the
;;       ;; resulting number of slices, won't fit in a fixnum (according
;;       ;; to the standard, arrays (vector) must be indexed by fixnums.
;;       ;; Let's try making our own.  Use the square root of the size to
;;       ;; get at a possible slice size, and let's see if that gives us
;;       ;; a working number of slices.
;;       (t
;;        (let* ((slicesz (isqrt size))
;; 	      (nslices (ceiling size slicesz)))
;; 	 ())
;;        )
;; 	)))

;; (defun make-sparse-vector (size &key (element-type t) slicesz
;; 			   (initial-element nil initial-element-p))
;;   "Creates and initializes a new SPARSE-VECTOR, returning it on
;; success, or NIL on error.  This function also checks a number of types
;; and bounds, ensuring that the instantiated SPARSE-VECTOR is sane and
;; safe.  The SPVREF and similar functions, then, can safely make a
;; number of assumptions about the SPARSE-VECTOR, supporting
;; optimizations and faster code.

;; In addition to the expected initializations, extra steps taken here
;; include:
;; . Unless an :INITIAL-ELEMENT value was explicitly provided for the
;;   SPARSE-VECTOR, make the initial value of each element zero if the
;;   element type is a NUMBER, otherwise NIL.
;; . Ensure that a valid :SIZE was provided for the SPARSE-VECTOR.
;; . If SLICESZ isn't specified, try to choose a good value, based on
;;   SIZE.
;; . Ensure that the SLICESZ, which is used to set the length of
;;   each slice, is small enough to be a fixnum.
;; . Ensure that the SLICESZ is large enough, with respect to SIZE,
;;   that the top level vector is indexed by a fixnum.
;; . Ensure that the value of the initial element has a type that matches
;;   ELEMENT-TYPE.
;; . Create the top level vector of slices."
;;   (cond
;;     ((or (not (integerp size)) (not (plusp size)))
;;      (err 1 "The specified SIZE, ~W, of a SPARSE-VECTOR must be a ~
;;              positive integer." size))
;;     ((and slicesz (or (not (typep slicesz 'fixnum)) (not (plusp slicesz))))
;;      (err 1 "The specified SLICESZ, ~W, of a SPARSE-VECTOR must be a ~
;;              positive fixnum." slicesz))
;;     ((and initial-element-p (not (typep initial-element element-type)))
;;      (err 1 "The INITIAL-ELEMENT, ~W, of this SPARSE-VECTOR must be ~
;;              of type ~W." initial-element element-type))
;;     (t
;;      (let ((nslices (ceiling size slicesz)))
;;        (cond
;; 	 ((> nslices most-positive-fixnum)
;; 	  (err 1 "The SLICESZ ~W is too small for a SPARSE-VECTOR of ~
;;                   SIZE ~W." slicesz size))
;; 	 (t
;; 	  (%make-spv :size nslices
;; 		     :slicesz slicesz
;; 		     :element-type element-type
;; 		     :initial-element (if initial-element-p
;; 				  initial-element
;; 				  (and (subtypep element-type 'number)
;; 				       (coerce 0 element-type)))
;; 		     :slices (make-array nslices :initial-element nil))))))))

;; (defmacro make-spv (&rest args)
;;   "Just a convenience for creating new SPARSE-VECTOR."
;;   `(make-sparse-vector ,@args))

;; (defun spvref (spv index)
;;   "Accesses the element of the supplied SPARSE-VECTOR indicated by the
;; subscript INDEX, or NIL if an error occurs.  A second value is also
;; returned, reflecting the success of the access."
;;   ;; This is longer than it needs to be, because we're trying to help
;;   ;; the optimizer as much as we can to employ fast math.  Question:
;;   ;; would unboxed integers be faster?  Probably not significantly:
;;   ;; fixnum math is unboxed math, just with a leading mask operation
;;   ;; to clear the type bits.  I doubt the masking should take many
;;   ;; cycles at all.
;;   (declare (type sparse-vector spv)
;; 	   (type integer index))
;;   (let ((size (spv-size spv))
;; 	(slicesz (spv-slicesz spv)))
;;     (declare (type integer size)
;; 	     (type fixnum slicesz))
;;     (cond
;;       ((or (not (integerp index)) (not (plusp index)))
;;        (err 2 "The INDEX of a SPARSE-VECTOR must be a positive integer."))
;;       ((>= index size)
;;        (err 2 "The INDEX, ~W, of a SPARSE-VECTOR must be less than ~
;;                its SIZE, ~W." index (spv-size spv)))
;;       (t
;;        (multiple-value-bind (i j) (truncate index slicesz)
;; 	 (declare (type fixnum i j))
;; 	 (values (if-let (s (svref (spv-slices spv) i))
;; 		   (svref s j)
;; 		   (spv-initial-element spv))
;; 		 t))))))

;; ---------------------------------------

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
