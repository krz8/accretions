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

(defparameter *max-vector-sizes* `(500 2000 8000 32000)
  "When we solve a splay for a given size, this parameter represents
  the maximum size of any vector in our tree.  We have a set of
  values, corresponding to a :SPEED parameter of 0-3, where:

  . 0: Space is much more important than speed.
  . 1: The default, where space is more important than speed.
  . 2: Speed is more important than space.
  . 3: Speed is of utmost importance, space is irrelevant.")

(defparameter *size-limit* (expt 10 67)
  "The largest sparse vector we'll support.  It's tempting to make
  this enormous, but this value is used to keep the depth of a vector
  tree from growing out of control, so try to keep it to something
  necessary.")

(defmacro until (expr &body body)
  "Create a DO loop that evaluates BODY so long as EXPR tests false.
  Returns NIL once EXPR passes.  EXPR is tested before BODY is
  evaluated on every loop.  UNTIL is the inverse of a traditional
  WHILE loop."
  (let ((e (gensym)))
    `(do ((,e ,expr ,expr))
         (,e)
       ,@body)))

(defun mkstr (&rest args)
  "Common function that turns all arguments into strings, concatenates
  them, and returns the new string."
  (with-output-to-string (s)
    (let ((*standard-output* s))
      (map nil #'princ args))))

(defun symb (&rest args)
  "Common function that returns a new symbol that is the concatenation
  of all arguments. (symb 'foo \"bar\" 12) → FOOBAR12"
  (values (intern (apply #'mkstr args))))

(defmacro with-ss (var-slot-pairs prefix obj &body body)
  "WITH-SLOTS is not specified to work with structure, per the
  standard.  So we have WITH-SS (\"structure slots\") instead.  Each
  VAR-SLOT pair is the name of a variable and the slot it should be
  bound to. PREFIX is the common prefix to all accessor functions.
  OBJ is the structure instance.  The variables are (unsurprisingly)
  bound via SYMBOL-MACROLET.

      (with-ss ((n name) (a age)) person- newbie
        (stuff) …)
      =>
      (symbol-macrolet ((n (person-name newbie))
                        (a (person-age newbie)))
        (stuff) …)"
  (do* ((vsp var-slot-pairs (cdr vsp))
        (b (car vsp) (car vsp))
        (bindings))
       ((null vsp)
        `(symbol-macrolet ,bindings ,@body))
    (destructuring-bind (var thing) b
      (push `(,var (,(symb prefix thing) ,obj)) bindings))))

(defmacro let-ss (var-slot-pairs prefix obj &body body)
  "Similar in concept to WITH-SS, this macro binds a set of variables
  to known slots from a structure via a LET form, tucking BODY inside
  that LET form.  Every element of VAR-SLOT-PAIRS has the form (VAR
  SLOT), naming a variable and the SLOT from which to take its value.
  PREFIX is whatever needs to be placed in front of SLOT to yield a
  reader function, and OBJ is the instance of the structure to use."
  (do* ((vsp var-slot-pairs (cdr vsp))
        (b (car vsp) (car vsp))
        (bindings))
       ((null vsp)
        `(let ,bindings ,@body))
    (destructuring-bind (var thing) b
      (push `(,var (,(symb prefix thing) ,obj)) bindings))))

;; I originally intentionally avoided conditions, but since we're
;; writing code to the CL standard, the condition system is already in
;; use, we might as well use it!  Now, since we're switching to
;; signalling conditions, we can get rid of chaining return values
;; from the various chk- functions.

(define-condition spverr (error)
  ((name :initarg :name :reader spverr-name)
   (value :initarg :value :reader spverr-value)
   (problem :initarg :problem :reader spverr-problem))
  (:default-initargs :value nil :name "n/a" :problem "n/a")
  (:report (lambda (condition stream)
             (format stream "~&SPARSE-VECTOR error: ~A, ~W, ~A."
                     (spverr-name condition)
                     (spverr-value condition)
                     (spverr-problem condition))))
  (:documentation "Represents error conditions detected within the
  SPARSE-VECTOR library."))

(defmacro err (place problem)
  "Signals SPVERR using PROBLEM directly, using PLACE as the VALUE,
  and the quoted name of PLACE as the NAME in the condition."
  `(error 'spverr :value ,place :name ',place :problem ,problem))

(defmacro check ((place problem) expr)
  "If EXPR fails, signal an SPVERR condition using PLACE and PROBLEM
  as described in ERR.  Kind of like a decorated ASSERT."
  `(unless ,expr
     (err ,place ,problem)))

(defmacro cerr (place problem)
  "Signal a correctable SPVERR condition, using the quoted and plain
  forms of PLACE as its NAME and VALUE, and using PROBLEM as its
  same."
  (let ((restart (mkstr "Supply a new " place ".")))
    `(cerror ,restart 'spverr :value ,place :name ',place :problem ,problem)))

(defmacro ccheck ((place problem &optional (readvar 'input)) expr &body body)
  "Used to ensure an expression, EXPR, is true.  Until EXPR is true,
  a condition describing the problem is raised via CERROR.  If the
  user chooses to continue past the condition, input is solicited to
  address the problem, BODY is evaluated to fix the condition, and
  EXPR is tested again.  This continues until the expression passes or
  the user chooses not to continue past the signalled condition.

  PLACE is a form that has the problem described in the string
  PROBLEM.  When a new expression is solicited from the user, it is
  bound to the variable named by READVAR; if omitted, INPUT is used by
  default (take care if crossing package boundaries).  With this
  binding in place, BODY is then evaluated \(typically to \"fix\" the
  problem\), and then the loop begins anew.

  .Example of using CCHECK
      (ccheck (foo \"must be even\")
          (evenp foo)
        (setf foo input))"
  `(until ,expr
     (cerr ,place ,problem)
     (fresh-line)
     (princ ,(mkstr "Enter a new " place ": "))
     (let ((,readvar (read)))
       (declare (ignorable ,readvar))
       (fresh-line)
       ,@body)))

(defun iroot (x n)
  "Returns the nearest integer equal to OR GREATER THAN the actual Nth
  root of X.  A secondary return value is the difference between the
  returned integer and the actual root.  Errors are trapped and zero
  is returned instead."
  (handler-case (ceiling (expt x (/ 1 n)))
    (error (condition) (declare (ignore condition)) (values 0 0.0))))

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

(defmacro with-spva (var-slot-pairs obj &body body)
  "Wraps WITH-SS with SPVA- as the prefix to all slot access
  functions.  This is just a shorthand."
  `(with-ss ,var-slot-pairs spva- ,obj ,@body))

(defun confirm-splay (spva)
  "Signals SPVERR condition when the vector tree splay specifiers make
  sense.  The return value is unimportant; if the function returns,
  processing can continue."
  (with-spva ((size size) (splay splay)) spva
    (flet ((good-element (x) (and (integerp x) (<= 2 x array-dimension-limit)))
           (good-size () (< (apply #'* splay) *size-limit*)))
      (ccheck (splay "must be a list of integers > 1")
          (and (consp splay) (every #'good-element splay))
        (setf splay input))
      (ccheck (splay "must yield a size < *SIZE-LIMIT*")
          (good-size)
        (setf splay input)))
    (setf size (apply #'* splay))))

(defun solve-splay (spva)
  "Given just a desired sparse array size, and a hint regarding the
  space/speed tradeoffs, determine a splay arrangement for the sparse
  vector.  The return value is unimportant; if the function returns,
  processing can continue."
  (with-spva ((splay splay) (size size) (speed speed)) spva
    (do* ((max   (nth speed *max-vector-sizes*))
	  (depth 2                              (1+ depth))
	  (x     (iroot size depth)             (iroot size depth)))
	 ((and (plusp x) (<= x max))	; the plusp catches errors
	  (setf splay (make-list depth :initial-element x))
	  spva))))

(defun chk-splay (spva)
  "Checks a splay tree described in the supplied spv-args, or computes
  a new tree based on the size.  This is called after CHK-SIZE, so we
  make a few safe assumptions about the state of SPVA.  Either SIZE is
  zero and there is a splay tree of some kind, or SIZE is supplied and
  a splay tree needs to be generated.  SPVA is returned on success, or
  NIL is returned after noting the error.  The return value is
  unimportant; if the function returns, processing can continue."
  (cond
    ((null spva)
     nil)
    ((null (spva-splay spva))
     (solve-splay spva))
    (t
     (confirm-splay spva))))

(defun chk-size (spva)
  "The SIZE-OR-SPLAY argument to MAKE-SPARSE-VECTOR first lands in the
  SIZE slot of the SPVA.  Here. we identify if it is really is a SIZE
  or instead a splay list.  If it's a list, we just move it to SPLAY
  slot and leave SIZE at zero; if it's a SIZE, we just perform some
  simple tests on it to ensure it's plausible.  CHK-SPLAY does the
  rest, based on what we determine here.  The return value is
  unimportant; if the function returns, processing can continue."
  (with-spva ((size size) (splay splay)) spva
    (if (listp size)
        (setf splay size
              size 0)
        (ccheck (size "must be a positive integer")
            (and (integerp size) (plusp size))
          (setf size input)))))

(defun chk-speed (spva)
  "Ensures that the SPEED supplied to MAKE-SPARSE-VECTOR was
  plausible.  The return value is unimportant; if the function
  returns, processing can continue."
  (with-spva ((speed speed)) spva
    (ccheck (speed "must be an integer [0,3]")
        (and (integerp speed) (<= 0 speed 3))
      (setf speed input))))

(defun chk-initial-element (spva)
  "When the INITIAL-ELEMENT of a SPARSE-VECTOR is specified, ensure
  that its type matches ELEMENT-TYPE.  If not specified, set it to NIL
  or some form of zero, if the ELEMENT-TYPE is a number of some kind.
  The return value is unimportant; if the function returns, processing
  can continue."
  (with-spva ((initial-element initial-element)
              (element-type    element-type)
              (iep             initial-element-p)) spva
    (if (not iep)
        (setf initial-element (and (subtypep element-type 'number)
                                   (coerce 0 element-type)))
        (ccheck (element-type "must match the ELEMENT-TYPE")
            (typep initial-element element-type)
          (setf initial-element input)))))

(defun chk-element-type (spva)
  "Check that the element type we've been given is valid.  This
  function makes no attempt to fix the element type, it only signals
  errors.  The return value is unimportant; if the function returns,
  processing can continue."
  (with-spva ((element-type element-type)) spva
    (ccheck (element-type "must be a plausible type specifier")
        (and element-type (subtypep element-type t))
      (setf element-type input))))

(defun spv-print (object stream)
  "Used when a SPARSE-VECTOR lands in the Printer's lap.  We don't do
  much: we just emit the structure name and some unique identifier
  \(typically, its address in memory\).  This way, we don't
  accidentally try to print the root of the vector tree, or
  ridiculously long size slots, and so on."
  (print-unreadable-object (object stream :type t :identity t)))

(defstruct (sparse-vector (:include spv-args) (:conc-name spv-)
                          (:constructor make-spv) (:print-object spv-print))
  ;; The tree of vectors:  "top index" #( - o - - - - - - - o - …)
  ;;                                        |               |
  ;;                        "other indexes" #(- - o - …)    #(…)
  ;;                                              |
  ;;                                     "slices" #(d d d d x d d …)
  ;; . - is a nil element
  ;; . o points to another vector (index or slice)
  ;; . d is the default element for the sparse vector
  ;; . x is an element that has been setf through spvref
  ;; . All vectors except the bottomost ("slice") vectors are
  ;;   simple vectors.
  (tree nil)
  ;; Kind of a complement to the splay list.  At each level in the
  ;; tree, the value in the splay list gives you the width of the
  ;; vector at that level; the corresponding value in the divisors
  ;; list gives you the number to divide your current remainder by,m
  ;; to obtain a new quotient (for indexing into the current vector)
  ;; and remainder (for use as an index at the next level).
  (divisors nil)
  ;; Fast functions compiled just for this particular sparse vector.
  ;; No loops, embedded context, straight line code.  Ugly and brute,
  ;; but it works.
  (getter nil)
  (setter nil))

#+nil
(defun spvref (spv index)
  (declare (type integer index) (type sparse-vector spv))
  (with-ss ((size size) (tree tree) (ie initial-element) (divisors divisors)
            (et element-type))
      spv- spv
    (check (index "must be a valid index of the sparse vector")
        (<= 0 index size))
    (do ((i index)
         (v tree)
         (d divisors (cdr d)))
        ((null d)
         (aref v i))
      (multiple-value-bind (q r) (truncate i (car d))
        (unless (setf i r
                      v (svref v q))
          (return-from spvref ie))))))

;; There are more declarations here than are needed; SBCL can follow
;; just a few type declarations through sub-expressions and derive the
;; types along the way.  But, we'll compute and add them anyway, it
;; might help other compilers too.

(defun gen-get (spv)
  ;; It isn't important how optimized gen-get is, it's the performance
  ;; of the code it generates that matters.  So, no matter how we're
  ;; compiling the rest of the package, gen-get gets a quiet and
  ;; conservative compilation.
  (declare (optimize (speed 1) (space 1) (safety 1)))
  (let-ss ((ie initial-element) (el element-type) (size size)
	   (divs divisors) (splay splay)) spv- spv
    (labels ((mvb (vec rem divs splay)
	       (let ((q (gensym)) (r (gensym)) ; (v (gensym))
		     )
		 `(let ((v (or ,vec (return ,ie))))
		    (multiple-value-bind (,q ,r)
			(truncate ,rem ,(car divs))
		      ;; probably redundant, sbcl could reason this out
		      ;; I suspect, but these decls could help other
		      ;; compilers
		      (declare (type (integer 0 ,(1- (car splay))) ,q)
			       (type (integer 0 ,(1- (car divs))) ,r))
		      ,(if (cdr divs)
			   (mvb `(svref v ,q) r (cdr divs) (cdr splay))
			   `(aref (the (simple-array ,el)
				       (or (svref v ,q) (return ,ie)))
				  ,r)))))))
      (let ((max (1- size)))
	`(lambda (index)
	   (check (index "must be an integer index of this sparse vector")
	       (typep index '(integer 0 ,max)))
	   (block nil
	     (locally
		 (declare (optimize (speed 3) (safety 0) (space 0))
			  (type (integer 0 ,max) index))
	       ,(mvb `(spv-tree ,spv) `index divs splay))))))))

(defun gen-set (spv)
  (declare (optimize (speed 1) (space 1) (safety 1)))
  (let-ss ((ie initial-element) (el element-type) (size size)
	   (divs divisors) (splay splay)) spv- spv
    (labels ((mvb (vec rem divs splay)
	       (let ((q (gensym)) (r (gensym)))
		 `(let ((v (ensure-array ,vec ,(car splay))
			   #+nil(or ,vec
				  (setf ,vec (make-index ,(car splay))))))
		    (multiple-value-bind (,q ,r)
			(truncate ,rem ,(car divs))
		      ;; probably redundant, sbcl could reason this out
		      ;; I suspect, but these decls could help other
		      ;; compilers
		      (declare (type (integer 0 ,(1- (car splay))) ,q)
			       (type (integer 0 ,(1- (car divs))) ,r))
		      ,(if (cdr divs)
			   (mvb `(svref v ,q) r (cdr divs) (cdr splay))
			   `(setf (aref (the (simple-array ,el)
					     (ensure-array (svref v ,q)
							   ,(cadr splay)
							   ',el ,ie))
					,r)
				  value)))))))
      (let ((max (1- size)))
	`(lambda (index value)
	   (macrolet ((mkarray (n el ie)
			`(make-array ,n :element-type ,el :initial-element ,ie))
		      (ensure-array (form n &optional (el t) (ie nil))
			`(or ,form (setf ,form (mkarray ,n ,el ,ie)))))
	     (check (index "must be an integer index of this sparse vector")
		 (typep index '(integer 0 ,max)))
	     ,(unless (eq el t)
		`(check (value "must be of the sparse vector element type")
		     (typep value ',el)))
	     (block nil
	       (locally
		   (declare (optimize (speed 3) (safety 0) (space 0))
			    (type (integer 0 ,max) index))
		 ,(mvb `(spv-tree ,spv) `index divs splay)))))))))

(defparameter *spva-checks* (list #'chk-element-type #'chk-initial-element
                                  #'chk-speed #'chk-size #'chk-splay)
  "A list of functions to be called to validate an SPV argument
  structure.  Each function is expected to signal an SPVERR condition
  when detecting an error.  Once all functions return, processing
  should continue, considering the SPV-ARGS structure they were passed
  to now be safe and valid.")

(defun init-spv (spv)
  "Given a new SPARSE-VECTOR, whose argument-driven fields have all
  been validated, finish the initialization by creating the divisors
  list and compiling the acccessor functions.  Returns the
  SPARSE-VECTOR, ready for use."
  (with-ss ((splay splay) (size size) (tree tree) (divs divisors)
	    (setter setter) (getter getter)) spv- spv
    (let ((sz size))
      (setf tree (make-array (car splay) :initial-element nil)
            divs (butlast (mapcar #'(lambda (len)
                                      (setf sz (ceiling (/ sz len))))
                                  splay))
	    getter (compile nil (gen-get spv))
	    setter (compile nil (gen-set spv)))))
  spv)

(defun spvref (spv idx)
  "Return the element at IDX in the supplied sparse vector."
  (funcall (spv-getter spv) idx))

(defun spvset (spv idx val)
  "Set the element at IDX in the supplied sparse vector to VAL.
  Returns VAL."
  (funcall (spv-setter spv) idx val))

(defsetf spvref spvset)

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
  (let ((spv (make-spv :size size-or-splay
                       :speed speed
                       :element-type element-type
                       :initial-element initial-element
                       :initial-element-p initial-element-p)))
    (map nil #'(lambda (f) (funcall f spv)) *spva-checks*)
    (init-spv spv)))

;; Local Variables:
;; eval: (put 'ccheck 'common-lisp-indent-function '(&lambda 4 &body))
;; eval: (put 'check 'common-lisp-indent-function '(&lambda 4))
;; End:
