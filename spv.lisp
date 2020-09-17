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

(defparameter *error* *error-output*
  "Names a stream to which descriptions of errors in the SPARSE-VECTOR
  package are sent.  When NIL, no error messages are generated.")

(defparameter *max-vector-sizes* `(,(* 1024    8   )
				   ,(* 1024 1024   )
				   ,(* 1024 1024  8)
				   ,(* 1024 1024 64))
  "When we solve a splay for a given size, this parameter represents
  the maximum size of any vector in our tree.  We have a set of
  values, corresponding to a :SPEED parameter of 0-3, where:

  . 0: Space is more important than speed.
  . 1: The default.
  . 2: Speed is more important than space.
  . 3: Speed is of utmost importance, space is irrelevant.

  It's important to realize how much smaller these numbers are than
  you might expect.  When you consider ARRAY-DIMENSION-LIMIT and
  MOST-POSITIVE-FIXNUM, it's easy to feel like a king.  The truth,
  though, is that simple arrays and simple vectors allocate a
  pointer's worth of space for each element, plus a tiny bit of
  overhead for the array itself.  Even taking into account this
  factor of four or eight bytes, typically, the practical limit
  imposed by a lisp environment is much lower.  You'll run out
  of heap LONG before you even get within magnitudes of the
  aforementioned constants.  No joke.

  Truth is, if this is really important to you?  Don't mess with
  these constants, just supply your own splay for the vector
  tree.")

(defparameter *depth-limit* 10
  "When solving the splay of the vector tree, this is the limit to how
  \"deep\" we'll allow the tree to get.  Typical trees have a depth of
  3.")

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

(defun wrn (control-string &rest args)
  "Like ERR, except noting that this is a warning, and always
  returning T."
  (apply #'format *error*
	 (concatenate 'string "~&Warning: " control-string "~%")
	 args)
  t)

(defun all-plus-p (&rest args)
  "Returns T if all arguments are positive numbers; else NIL.  If no
  arguments are supplied, returns T (maybe fix this?)."
  (every #'plusp args))

(defun list-of-posints-p (list)
  "Returns T if LIST contains integers that are all positive.  If LIST
  is empty, returns NIL."
  (cond
    ((null list) nil)
    ((notevery #'(lambda (x) (and (integerp x) (plusp x)))
	       list) nil)
    (t t)))

(defun mkstr (&rest args)
  "Turn all arguments into strings, and return the concatenation of
  them.  Unoriginal but handy."
  (with-output-to-string (s)
    (let ((*standard-output* s))
      (mapc #'princ args))))

(defun symb (&rest args)
  "Create and return a new symbol whose name is the concatenation of
  all arguments.  Yes, Alexandria has this, too (called symbolicate),
  but it only works on symbols.  This will work on any mix of symbols,
  strings, what have you.  Seen in Paul Graham."
  (values (intern (apply #'mkstr args))))

(defun iroot (x n)
  "Returns the nearest integer equal to OR GREATER THAN the actual Nth
  root of X.  A secondary return value is the difference between the
  returned integer and the actual root."
  (ceiling (expt x (/ 1 n))))

;; SPV-ARGS is an intentionally simple and small structure.  We create
;; them to capture the various settings from the caller to
;; MAKE-SPARSE-VECTOR, and then throw a bunch of functions at it,
;; refining or validating the structure as necessary.  From that, we
;; create the real sparse vector.  We don't assign types to the slots
;; here, since they come unqualified from the caller.  After we've
;; checked their types and values, we'll set up a real sparse vector
;; that contains properly tagged types.
(defstruct (spv-args (:conc-name spva-))
  ;; The total number of elements represented by a SPARSE-VECTOR.
  ;; Unlike traditional vectors, this value may be (much) larger than
  ;; either ARRAY-DIMENSION-LIMIT or MOST-POSITIVE-FIXNUM.  This DOES
  ;; NOT represent the size of a sparse vector in memory in any way.
  (size 0)
  ;; A list of sizes of the vectors at each depth, starting from the
  ;; top.
  (splay nil)
  ;; Like the ELEMENT-TYPE of a traditional vector, this can be set to
  ;; help the Lisp engine pack slices into memory more efficiently.
  (element-type t)
  ;; As with a traditional array or vector, we have an initial element
  ;; whose value is assumed for all elements in the sparse vector
  ;; until otherwise set.
  (initial-element nil)
  (initial-element-p nil)
  ;; The caller can tell us that speed is more important than space in
  ;; our sparse vector.  Generally, this will widen any splay tree we
  ;; compute, causing it to be less deep.  By default, we make things
  ;; as tiny as we can, in a simple-minded way, at the cost of speed
  ;; (but I still think we can beat list- and hash-based sparse
  ;; vectors).  0 <= speed <= 3
  (speed 1))

(defun confirm-splay (spva)
  "Returns SPVA when the vector tree splay specifiers make sense.
  Returns NIL on an error."
  (when spva
    (let ((splay (spva-splay spva)))
      (cond
	((notevery #'(lambda (x) (<= 2 x array-dimension-limit))
		   splay)
	 (err "Every value in the list describing the splay of a ~
              SPARSE-VECTOR must be in the range [2,~d]."
	      array-dimension-limit))
	(t
	 (setf (spva-size spva) (apply #'* splay))
	 spva)))))

(defun solve-splay (spva)
  "Returns SPVA on success, or NIL on failure.  Given just a desired
  sparse array size, and a hint regarding the space/speed tradeoffs,
  determine a splay arrangement for the sparse vector and return."
  (when spva
    (let ((max (nth (spva-speed spva) *max-vector-sizes*))
	  (size (spva-size spva)))
      (do ((depth 2 (1+ depth)))
	  ((> depth *depth-limit*)
	   (err "Unreasonable size, ~W, encountered in ~
                MAKE-SPARSE-VECTOR. If this size is intentional, ~
                bind *DEPTH-LIMIT* to a value higher than ~W."
		size *depth-limit*))
	(let ((x (iroot size depth)))
	  (when (<= x max)
	    (setf (spva-splay spva)
		  (make-list depth :initial-element x))
	    (return-from solve-splay spva)))
#+nil	(do ((width 64 (* 2 width)))
	    ((> width max)
	     nil)
	  (when (>= (expt width depth) size)
	    (setf (spva-splay spva)
		  (make-list depth :initial-element width))
	    (return-from solve-splay spva)))))))

(defun chk-splay (spva)
  "Checks a splay tree described in the supplied spv-args, or computes
  a new tree based on the size.  This is called after CHK-SIZE, so we
  make a few safe assumptions about the state of SPVA.  Either SIZE is
  zero and there is a splay tree of some kind, or SIZE is supplied and
  a splay tree needs to be generated.  SPVA is returned on success, or
  NIL is returned after noting the error."
  (cond
    ((null spva)
     nil)
    ((null (spva-splay spva))
     (solve-splay spva))
    (t
     (confirm-splay spva))))

(defun chk-size (spva)
  "The SIZE-OR-SPLAY argument to MAKE-SPARSE-VECTOR lands in the size
  slot of the SPVA.  Here. we identify if it is a SIZE or a SPLAY.  If
  it's a SPLAY, we just move it to SPLAY and leave SIZE at zero;
  that'll trigger CHK-SPLAY.  If it's a SIZE, we just perform some
  simple tests on it to ensure it's plausible.  Returns SPVA if
  processing should continue, or NIL if there's an error and the
  sparse vector cannot be created."
  (when spva
    (let ((ss (spva-size spva)))
      (cond
	((listp ss)
	 (setf (spva-size spva) 0
	       (spva-splay spva) ss)
	 spva)
	((not (and (integerp ss) (plusp ss)))
	 (err "The size of a SPARSE-VECTOR must be a positive integer."))
	(t
	 spva)))))

(defun chk-speed (spva)
  "Returns SPVA if the specified speed is plausible; else NIL."
  (when spva
    (let ((speed (spva-speed spva)))
      (if (and (integerp speed) (<= 0 speed 3))
	  spva
          (err "The SPEED specifier to MAKE-SPARSE-VECTOR must be an ~
               integer [0,3].")))))

(defun chk-initial-element (spva)
  "When the INITIAL-ELEMENT of a SPARSE-VECTOR is specified, ensure
  that its type matches ELEMENT-TYPE.  If not specified, set it to NIL
  or some form of zero, if the ELEMENT-TYPE is a number of some kind.
  Returns SPVA if it can still be used, or returns NIL on an error.
  Assumes that ELEMENT-TYPE has already been checked."
  (when spva
    (let ((ie (spva-initial-element spva)) (et (spva-element-type spva)))
      (cond
	((not (spva-initial-element-p spva))
	 (setf (spva-initial-element spva) (and (subtypep et 'number)
						(coerce 0 et)))
	 spva)
	((not (typep ie et))
	 (err "The INITIAL-ELEMENT, ~W, of a SPARSE-VECTOR must match ~
            the ELEMENT-TYPE, ~W, of that vector." ie et))
	(t
	 spva)))))

(defun chk-element-type (spva)
  "Check that the element type we've been given is valid.  Returns
  either the SPV-ARGS we were passed, or NIL after writing an error
  message.  This function makes no attempt to fix the element type,
  it only detects errors."
  (when spva
    (let ((et (spva-element-type spva)))
      (cond
	((null et)
	 (err "The ELEMENT-TYPE of a SPARSE-VECTOR must not be NIL."))
	((not (subtypep et t))
	 (err "The ELEMENT-TYPE must be a valid type; ~W is not a ~
         subtype of T." et))
	(t
	 spva)))))

(defun make-sparse-vector (size-or-splay
			   &key (element-type t) (speed 1)
			   (initial-element nil initial-element-p))
  "Creates and returns a new SPARSE-VECTOR according to the supplied
  arguments, or NIL if there is some error.  Errors are typically
  reported on the *ERROR* stream.

  SIZE-OR-SPLAY describes the size and, optionally, the internal
  layout of the sparse vector.  This argument can be just a simple
  positive integer; this is the typical case, and describes the
  total number of elements in this vector.  Unlike a traditional
  vector, this number can be a bignum.  In this case, the internal
  splay of the vector tree is determined silently, optmizing for
  a compact arrangements (trading speed for less space).

  Alternatively, a list of small positive fixnums may be supplied.
  This list represents the splay of the vector tree.  The first number
  is the size of the top level vector.  The next number is the size of
  each vector at the second level.  This continues until the last
  number, representing the size of the \"bottomost\" vectors in the
  tree.  Typically, this list has 2-4 elements in it. For example, if
  handed '(100 200 300), the resulting sparse vector supports six
  million indexes.  The top level vector in the internal tree has 100
  elements, each pointing to a middle vector of 200 elements.
  Each of those, in turn, points to a slice vector of 300 elements.

  By default, the vector can hold any value at any index.  However,
  just like traditional arrays and vectors in CL, the ELEMENT-TYPE and
  INITIAL-VALUE keywords can be used to change this, sometimes
  granting some space efficiency.  Note: unlike traditional vectors,
  if a sparse vector is created with some kind of number as the
  ELEMENT-TYPE, but an INITIAL-VALUE is not specified, a zero of
  ELEMENT-TYPE made the initial value."
  (chk-splay
   (chk-size
    (chk-speed
     (chk-initial-element
      (chk-element-type
       (make-spv-args :size size-or-splay
		      :speed speed
		      :element-type element-type
		      :initial-element initial-element
		      :initial-element-p initial-element-p)))))))

;; (defstruct (sparse-vector (:conc-name spv-) (:constructor make-spv))
;;   ;; Divisors is a list that describes how many elements are
;;   ;; represented by each element in the vector at that level.  It can
;;   ;; be derived on the fly, but we cache them here to make SPVREF
;;   ;; faster.
;;   (divisors nil :type (or nil list))
;;   ;; The tree of vectors:  "top index" #( - o - - - - - - - o - …)
;;   ;;                                        |               |
;;   ;;                        "other indexes" #(- - o - …)    #(…)
;;   ;;                                              |
;;   ;;                                     "slices" #(d d d d x d d …)
;;   ;; . - is a nil element
;;   ;; . o points to another vector (index or slice)
;;   ;; . d is the default element for the sparse vector
;;   ;; . x is an element that has been setf through spvref
;;   ;; . All vectors except the bottomost ("slice") vectors are
;;   ;;   simple vectors.
;;   (tree nil :type (or null vector)))

;; (defmacro with-spv ((&rest args) instance &body body)
;;   "Just a quickie to save typing below.  It's similar to WITH-SLOTS,
;;   but works with the spv structure.  It also supports type assertions
;;   on each accessor with an optional third argument in each slot
;;   entry (see d below).  Types have to be optional, the standard
;;   affords no inspection on structures that would let us do it
;;   ourselves (unlike CLOS (although most Lisps do support it, and in
;;   fact WITH-SLOTS works with structures, but this is nonstandard and
;;   implementation-dependent, which is why we're here in the first
;;   place)).  The optional type assertions might help some compilers,
;;   but should be harmless to others.

;;       (with-spv (size (dv divisors) (d depth fixnum)) *s*
;;         ...)
;;   yields
;;       (symbol-macrolet ((size (spv-size *s*)) (dv (spv-divisors *s*))
;;                         (d (the fixnum (spv-depth *s*))))
;; 	...)"
;;   (let (bindings)
;;     (dolist (a args)
;;       (cond
;; 	((listp a)
;; 	 (destructuring-bind (var slot &optional type) a
;;     	   (let ((acc (symb 'spv- slot)))
;; 	     (if typ
;; 		 (push `(,var (the ,type (,acc ,instance))) bindings)
;; 		 (push `(,var            (,acc ,instance) ) bindings)))))
;; 	(t
;; 	 (push `(,a (,(symb 'spv- a) ,instance)) bindings))))
;;     `(symbol-macrolet ,bindings
;; 		      ,@body)))

;; (defun init-spv (spv)
;;   "Returns SPV on success, or NIL."
;;   (with-spv ((d depth)) spv
;;     (setf (spv-tree spv) (make-array (funcall (if (> ) spv-index-size) spv))))
;;   spv)

;; ;; Different compilers implement different amounts of type checking
;; ;; and validation on slot initializing arguments.  To make sure no one
;; ;; is left out, we'll do validation here exolicitly, so that we can
;; ;; make the same safe assumptions no matter what Lisp engine we're on.
;; ;; These tests are important, because we're going to make a lot of
;; ;; assumptions in other functions, so we do all the checking up front
;; ;; to ensure those assumptions are safe.

;; (defun make-sparse-vector (size-or-list-of-splays
;; 			   &key (limit *max-vector-size*)
;; 			     (element-type t)
;; 			     (initial-element nil initial-element-p))
;;   "Constructor for a SPARSE-VECTOR.  The first argument is either a
;;   single positive integer giving the total number of elements
;;   represented by this sparse vector, or it is a list of positive
;;   integers, giving the size of the splay at each level of the vector
;;   tree.

;;   The LIMIT is used when we determine what the vector tree should look
;;   like.  It specifies the maximum splay, that is, the maximum size of
;;   any vector in our tree.

;;   ELEMENT-TYPE and INITIAL-ELEMENT are as they are in MAKE-ARRAY, with
;;   one exception:  INITIAL-ELEMENT, when not specified, is normally NIL.
;;   However, if ELEMENT-TYPE is a subtype of NUMBER, then INITIAL-ELEMENT
;;   is zero.

;;   Returns a new SPARSE-VECTOR, or NIL on some error."
;;   (and (check-element ))
;;   (flet ((bye (r) (return-from make-sparse-vector r)))
;;     (cond
;;       ((null element-type)
;;        (err "The ELEMENT-TYPE of a sparse vector cannot be NIL."))
;;       ((and initial-element-p (not (typep initial-element)))
;;        (err "The INITIAL-ELEMENT (~W) of a sparse vector must be of ~
;;           type ELEMENT-TYPE (~W)." initial-element element-type))
;;       (t
;;        (if (and (not initial-element-p)
;; 		)))))
  
;;   (let ((size 0) (splay nil))
;;     (symbol-macrolet ((ss size-or-list-of-splays))
;;       (cond
;; 	;; Just ensure no one tried :element nil
;; 	((not (null element-type))
;; 	 (err "The ELEMENT-TYPE of a sparse vector cannot be NIL."))
;; 	;; if we're given a splay list, then derive the size from it
;; 	((all-vector-size-p ss)
;; 	 (setf size (apply #'* ss)
;; 	       splay ss)
;; 	 t)
;; 	;; if we're given a size, guess at some kind of splay tree
;; 	((integerp ss)
;; 	 (setf size ss
;; 	       splay (generate-splay size))
;; 	 t)
;; 	))
;;     (make-spv :size size :splays )))

  
;;       (let ((size 0) (splays nil))
;; 	(symbol-macrolet ((ss size-or-list-of-splays))
;; 	  (cond
;; 	    ((not (or (integerp ss) (listp ss)))
;; 	     (err "Either an integer representing a total size, or a list ~
;;             of integers representing vector tree splay, must be ~
;;             provided as the first argument to MAKE-SPARSE-VECTOR; ~
;;             ~W is invalid." ss))
;; 	    ((not (or (not (listp ss))
;; 		      (apply #'all-plus-adim-p ss)))
;; 	     (err "The list of splays provided for a sparse vector must ~
;;               contain only positive integers less than *MAX-VECTOR-SIZE*, ~
;;               ~W; ~W is invalid." *max-vector-size* ss)
;; 	     )
;; 	    ))
;; 	)
;;       (cond
;; 	((not (typep size '(integer 1 *)))
;; 	 (err "The given SIZE, ~W, of a sparse vector must be a ~
;;           positive integer." size))
;; 	((not (typep depth '(integer 0 *)))
;; 	 (err "The given DEPTH, ~W, of a sparse vector must be a ~
;;           positive integer." depth))
;; 	((not (typep index-size `(integer 0 ,array-dimension-limit)))
;; 	 (err "The given INDEX-SIZE, ~W, of a sparse vector must be a ~
;;           positive integer less than ARRAY-DIMENSION-LIMIT." index-size))
;; 	((not (typep slice-size `(integer 0 ,array-dimension-limit)))
;; 	 (err "The given SLICE-SIZE, ~W, of a sparse vector must be a ~
;;           positive integer less than ARRAY-DIMENSION-LIMIT." slice-size))
;; 	((not (null element-type))
;; 	 (err "The given ELEMENT-TYPE, ~W, of a sparse vector must not be NIL."
;; 	      element-type))
;; 	((not (typep initial-element element-type))
;; 	 (err "The given INITIAL-ELEMENT, ~W, of a sparse vector must be ~
;;            of type ELEMENT-TYPE, ~W." initial-element element-type))
;; 	(t
;; 	 (init-spv
;; 	  (make-spv :size size :depth depth :index-size index-size
;; 		    :slice-size slice-size :element-type eltype
;; 		    :initial-element initial-element))))

;; ;; All the functions that work with testing or setting the size
;;     ;; attributes of a sparse vector have one of three return values. NIL
;;     ;; means that the tests didn't apply, no changes were made, and the
;;     ;; caller should continue trying other functions. T and 'FAIL both
;;     ;; mean further testing should stop; T indicating success, and 'FAIL
;;     ;; meaning just that.

;; (defun check-sizes-tiny (spv)
;;   "We need to draw the line somewhere; under a certain size, there is
;;   just no point to a sparse vector at all.  Maybe we should assert an
;;   error when this happens?  Anyway, returns T when leaving the sparse
;;   vector set with a depth of 1, or NIL when testing should continue."
;;   (with-spv (size) spv
;;     (when (<= size *non-sparse-threshold*)
;;       (setf (spv-depth spv) 1
;; 	    (spv-index-size spv) 0
;; 	    (spv-slice-size spv) size)
;;       (wrn "The requested size of the sparse vector is not greater ~
;;            than *NON-SPARSE-THRESHOLD*, so a sparse vector of DEPTH ~
;;            1 will be created, ignoring any supplied values for DEPTH, ~
;;            INDEX-SIZE, or SLICE-SIZE."))))

;; (defun check-sizes-all (spv)
;;   "If the user specified everything, see if the resulting sparse
;;   vector fits.  If not, try relaxing one or more attributes until a
;;   good sparse vector is possible.  Returns T if everything is good and
;;   the sparse vector can be created as-is, returns NIL if testing should
;;   continue \(after possibly zeroing one or more attributes\), or
;;   returns 'FAIL if there's just no chance of making this sparse vector
;;   work."
;;   (with-spv (size depth indexsz slicesz) spv
;;     (when (all-plus-p size depth indexsz slicesz)
;;       (cond
;; 	((<= size (* slicesz (expt indexsz (1- depth))))
;; 	 t)
;; 	((<= size (* array-dimension-limit (expt indexsz (1- depth))))
;; 	 (setf (spv-slice-size spv) 0)
;; 	 (wrn "The specified SLICE-SIZE, ~W, of the requested sparse ~
;;               vector has been disregarded in order to satisfy the ~
;;               requested SIZE, ~W." slicesz size)
;; 	 nil)
;; 	((<= size (* slicesz (expt array-dimension-limit (1- depth))))
;; 	 (setf (spv-index-size spv) 0)
;; 	 (wrn "The specified INDEX-SIZE, ~W, of the requested sparse ~
;;               vector has been disregarded in order to satisfy the ~
;;               requested SIZE, ~W." indexsz size)
;; 	 nil)
;; 	((<= size (expt array-dimension-limit depth))
;; 	 (setf (spv-index-size spv) 0
;; 	       (spv-depth spv) 0)
;; 	 (wrn "The specified INDEX-SIZE, ~W, and SLICE-SIZE, ~W, of ~
;;               the requested sparse vector have been disregarded in ~
;;               order to satisfy the requested SIZE, ~W." indexsz slicesz size)
;; 	 nil)
;; 	(t
;; 	 (setf (spv-depth spv) 0
;; 	       (spv-index-size spv) 0
;; 	       (spv-depth spv) 0)
;; 	 (wrn "The specified DEPTH, INDEX-SIZE, and SLICE-SIZE of ~
;;               the requested sparse vector have all been disregarded in ~
;;               order to satisfy the requested SIZE, ~W." size)
;; 	 nil)))))

;;     ;; (do* ((level 2 (1+ level))
;;     ;; 	   (total (expt slicesz level) (expt slicesz level)))
;;     ;; 	  ((>= total req-size)
;;     ;; 	   (values level slicesz))))))

;;     #+nil
;;     (defun check-sizes-slice (spv)
;;       "If we've been given a SLICE-SIZE but nothing else, find a DEPTH and
;;   INDEX that will satisfy the requested SIZE for the sparse vector.
;;   Returns T if we've found it, or NIL if nothing obvious works."
;;       (with-spv (size depth indexsz slicesz) spv
;; 	(when (and (plusp slicesz) (zerop depth) (zerop indexsz))
;; 	  (do ((depth 0 (1+ depth)))
;; 	      ((> depth 10) nil)    ; just too big, this isn't working
;; 	    )
      
;; 	  (let ((indexes (ceiling size slicesz)))
;; 	    (do* ((depth 2 (1+ depth)))
;; 		 ((x (()))))))))

;; (defun check-sizes-none (spv)
;;   "If none of DEPTH, INDEX SIZE, or SLICE SIZE were set, determine a
;;   minimal sparse vector by taking roots of the requested size.  Note
;;   that it is just my hypotheis that this gives the smallest sparse
;;   vector, I have not actually proven it.  Returns T if the depth,
;;   index, and slice sizes have been set to something that will yield a
;;   working sparse vector, or NIL if nothing could be done."
;;   (let ((size (spv-size spv)) (depth (spv-depth spv))
;; 	(indexsz (spv-index-size spv)) (slicesz (spv-slice-size spv)))
;;     (unless (or (plusp depth) (plusp indexsz) (plusp slicesz))
;;       (do* ((depth 2 (1+ depth))
;; 	    (x (iroot size depth) (iroot size depth)))
;; 	   ((< x *max-vector-size*)
;; 	    (setf (spv-depth spv) depth
;; 		  (spv-index-size spv) x
;; 		  (spv-slice-size spv) x)
;; 	    t)))))

;;     ;;   It is only my hypothesis that this function returns a particularly
;;     ;;   good small sparse vector for the given size; I have not, in fact,
;;     ;;   tried to prove it.  For a faster sparse vector, at the price of
;;     ;;   space, specify some attributes to MAKE-SPARSE-VECTOR \(which will
;;     ;;   cause a different function than this one to be called\)."
;;     ;;   (do* ((level 2 (1+ level))
;;     ;; 	(slicesz (iroot req-size level) (iroot req-size level)))
;;     ;;        ((< slicesz *max-vector-size*)
;;     ;; 	(list level slicesz slicesz))))

;; (defun check-sizes (spv)
;;   "If 'FAIL, we can be reasonably certain an error message was
;;   already sent. If NIL, then nothing worked and we need to say
;;   something.  If T, that's a success and nothing needs to be
;;   sent."
;;   (case (or (check-sizes-tiny spv)
;; 	    (check-sizes-all spv)
;; 	    (check-sizes-none spv))
;;     ('fail nil)
;;     ((nil) (err "A sparse vector of size ~W could not be created."
;; 		(spv-size spv)))
;;     (t t)))

;; (defun check-sizes (spv)
;;   "Examine the combinations of size and other attributes given for the
;;   sparse vector.  Missing attributes are determined, others are
;;   adjusted if necessary.  A true value is returned if the resulting
;;   sparse vector can be used, otherwise NIL."
;;   (macrolet ((chk (var)
;; 	       `((not (and (integerp ,var) (1 < ,var array-dimension-limit)))
;; 		 (err "When specified, the ~A, ~W, of a sparse vector ~
;;                       must be a positive integer between 1 and ~
;;                       ARRAY-DIMENSION-LIMIT." ,(string var) ,var))))
;;     (let ((size (spv-size spv)) (depth (spv-depth spv))
;; 	  (index-size (spv-index-size spv)) (slice-size (spv-slice-size spv)))
;;       (cond
;; 	((not (typep size '(integer 1 *)))
;; 	 (err "The SIZE, ~W, of a sparse vector must be a positive integer."
;; 	      size))
;; 	(chk depth)
;; 	(chk index-size)
;; 	(chk slice-size)
;; 	((handle-sizes spv) t)))))

;; (defun fix-initel (initel initel-p eltype)
;;   "Returns the initial-element we should use for a sparse vector.  If
;;   the caller names a specific initial-element, ensure that it respects
;;   the element-type.  Otherwise, if the caller specifies an element
;;   type that is some kind of number, make our initial element zero.
;;   Otherwise, set the initial-element to NIL.

;;   Returns a list of initial element and element type when successful,
;;   or NIL on an error."
;;   (cond
;;     ((not initel-p)
;;      (list (and (subtypep eltype 'number) (coerce 0 eltype)) eltype))
;;     ((typep initel eltype)
;;      (list initel eltype))
;;     (t
;;      (err "The initial element, ~W, of a sparse vector must be of the ~
;;           element type, ~W." initel eltype))))



;;     (flet ((chk-size (x) (unless (typep x '(integer 0 *))
;; 			   (err "The SIZE of a sparse vector must be a ~
;;                               positive integer.")
;; 			   (return)))))
;;     (check-types size depth index-size slice-size
;; 		 initial-element initial-element-p)
;;     (let ((spv (make-spv :size size :depth depth :index-size index-size
;; 			 :slice-size slice-size :element-type eltype
;; 			 :initial-element initial-element)))
;;       (cond
;; 	((and (handle-sizes spv)
;; 	      (handle-initel spv initial-element-p))
;; 	 (setf (spv-tree spv) (make-array (spv-index-size spv)))
;; 	 spv)
;; 	(t
;; 	 nil))))

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
