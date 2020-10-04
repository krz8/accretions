;;;; sparse vectors

(eval-when (:compile-toplevel) (print "compiling spv"))
(eval-when (:load-toplevel)    (print "loading spv"))
(eval-when (:execute)          (print "executing spv"))

(defpackage :accretions/spv
  (:use #:cl #:cl-environments #:accretions/misc)
  (:export #:sparse-vector #:make-sparse-vector #:spvref #:spvset
	   #:spvp
	   #:gen-get #:gen-set #:*max-vector-sizes* #:*size-limit*))
(in-package :accretions/spv)

(defparameter *max-vector-sizes* '(25000 2500 250 25)
  "When we solve a splay for a given size, this parameter represents
  the maximum size of any vector in our tree.  We have a set of values
  that are used when MAKE-SPARSE-VECTOR is called.  The value is
  chosen at runtime, based on the current value of the SPACE attribute
  of the OPTIMIZE declaration in the environment; yes, that means
  sparse vectors with different runtime characteristics can be
  generated simply by changing the OPTIMIZE declaration in effect when
  MAKE-SPARSE-VECTOR is run (in other words, compile time has no
  bearing on the selection of index and slice sizes).")

(defparameter *size-limit* (expt 10 67)
  "The largest sparse vector we'll support.  It's tempting to make
  this enormous, but this value is used to keep the depth of a vector
  tree from growing out of control, so try to keep it to something
  necessary.")

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

(defun iroot (x n)
  "Returns the nearest integer equal to OR GREATER THAN the actual Nth
  root of X.  A secondary return value is the difference between the
  returned integer and the actual root.  Errors are trapped and zero
  is returned instead.  Both X and N must be positive integers."
  (declare (type (integer 1 *) x n))
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
  (initial-element-p nil))

(defmacro with-spva (var-slot-pairs obj &body body)
  "Wraps WITH-SS with SPVA- as the prefix to all slot access
  functions.  This is just a shorthand."
  `(with-ss ,var-slot-pairs spva- ,obj ,@body))

(defun confirm-splay (spva)
  "Signals ERROR condition unless the vector tree splay specifiers make
  sense.  The return value is unimportant; if the function returns,
  processing can continue."
  (with-spva ((size size) (splay splay)) spva
    (flet ((good-element (x) (and (integerp x) (<= 2 x array-dimension-limit))))
      (assert (and (consp splay)
		   (every #'good-element splay)
		   (< (apply #'* splay) *size-limit*))
	      (splay)
	      "The splay list, ~w, must be a list of integers, each ~
              [2,~d], such that the resulting total size is ~
              less than ~d." splay array-dimension-limit *size-limit*))
    (setf size (apply #'* splay))))

(defun solve-splay (spva)
  "Given just a desired sparse array size, and a hint regarding the
  space/speed tradeoffs, determine a splay arrangement for the sparse
  vector.  The return value is unimportant; if the function returns,
  processing can continue."
  (let ((space (get-opt 'space)))
    (with-spva ((splay splay) (size size)) spva
	    (do* ((max (nth space *max-vector-sizes*))
		  (depth 2 (1+ depth))
		  (x (iroot size depth) (iroot size depth)))
		 ;; the plusp catches iroot errors
		 ((and (plusp x) (<= x max))
		  (setf splay (make-list depth :initial-element x))
		  spva)))))

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
	(assert (and (integerp size) (plusp size)) (size)
		"The size, ~w, of a sparse vector must be a ~
                positive integer." size))))

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
	(assert (typep initial-element element-type)
		(initial-element)
		"The INITIAL-ELEMENT, ~w, must be of the ELEMENT-TYPE, ~
                ~w."
		initial-element element-type))))

(defun chk-element-type (spva)
  "Check that the element type we've been given is valid.  This
  function makes no attempt to fix the element type, it only signals
  errors.  The return value is unimportant; if the function returns,
  processing can continue."
  (with-spva ((element-type element-type)) spva
    (assert (and element-type (subtypep element-type t))
	    (element-type)
	    "The ELEMENT-TYPE, ~w, must be a plausible type specifier."
	    element-type)))

(defun spv-print (object stream)
  "Used when a SPARSE-VECTOR lands in the Printer's lap.  We don't do
  much: we just emit the structure name and some unique identifier
  \(typically, its address in memory\).  This way, we don't
  accidentally try to print the root of the vector tree, or
  ridiculously long size slots, and so on."
  (print-unreadable-object (object stream :type t :identity t)))

(declaim (inline spv-p))

(defstruct (sparse-vector (:include spv-args) (:constructor make-spv)
                          (:conc-name spv-) (:print-object spv-print))
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
  (get nil)
  (set nil))

;;; There are more declarations here than are needed in the various
;;; drill functions; SBCL can follow just a few type declarations
;;; through sub-expressions and derive the types along the way.  But,
;;; we'll compute and add them anyway, it might help other compilers
;;; too.

(defun make-drill% (vec remainder divisors splay)
  "Returns the main unrolled body of a function that accesses a sparse
  vector at runtime.  Call this with VEC as a form to access the root
  of the vector tree, REMAINDER as the initial INDEX into the sparse
  vector, and DIVISORS and SPLAY as the respective lists from the
  sparse vector itself."
  `(let* ((w ,(car splay)) (v (formx ,vec)))
     (declare (ignorable w))
     (multiple-value-bind (q r)
	 (truncate ,remainder ,(car divisors))
       ;; Probably redundant, sbcl could reason this out I suspect.
       ;; Still, these decls could help other compilers.
       (declare (type (integer 0 ,(1- (car splay))) q)
		(type (integer 0 ,(1- (car divisors))) r))
       ,(if (cdr divisors)
	    (make-drill% `(svref v q) 'r (cdr divisors) (cdr splay))
	    `(let* ((w ,(cadr splay)) (v (form1 (svref v q))))
	       (declare (ignorable w))
	       (form0 v r))))))

(defun make-drill (spv argform macros)
  "Returns a list that is a lambda expression which \"drills\" down
  into the vector tree of a supplied sparse vector.  ARGFORM is a list
  of one or two names, providing the argument list to this lambda.
  MACROS should be a list of macro definitions (as would be supplied
  to MACROLET) which support customization of the MAKE-DRILL% function
  for read or write access to the vector tree."
  (let* ((sz (spv-size spv)) (max (1- sz)))
    (destructuring-bind (idx &optional val &rest dummy) argform
      (declare (ignore dummy) (ignorable val))
      `(lambda ,argform
	 (macrolet ,macros
	   ,(when (plusp (get-opt 'safety))
	      `(assert (typep ,idx '(integer 0 ,max)) (,idx)
		       "The INDEX, ~w, to this sparse vector must be ~
                       an integer [0,~d]." ,idx ,sz))
	   (block nil
	     (locally
		 (declare (type (integer 0 ,max) ,idx))
	       ;; Divisors and splay lists are evaluated now, as the
	       ;; lambda is being built, because they're short.  We
	       ;; could do the same with the tree itself, capturing
	       ;; the tree into the environment instead of SPV itself,
	       ;; and instead of forcing a slot access at runtime.
	       ;; So, why do it this way?  It lets us keep the slot
	       ;; itself NIL until it's needed, allowing the first
	       ;; FORMX on (SPV-TREE x) can be setf like all the
	       ;; others.  If we evaluated it now, we'd just wind up
	       ;; with a NIL and not a vector.
	       ,(make-drill% `(spv-tree ,spv) idx (spv-divisors spv)
			     (spv-splay spv)))))))))

(defun gen-get (spv)
  "Returns a list that is a lambda expression embodying read access to
  the supplied SPARSE-VECTOR."
  (let* ((ie (spv-initial-element spv)) (el (spv-element-type spv))
	 (macros
	   ;; I will probably never get nested backquotes right, so
	   ;; I'll settle for string hacking for now.  This is only
	   ;; invoked during the creation of a sparse vector, and not
	   ;; during general use, so it's no significant performance
	   ;; hit.
	   (readfmt
	    "((~w (form)
                `(or ,form (return ~w)))
              (~w (form)
                `(or ,form (return ~w)))
              (~w (a i)
                `(aref (the (simple-array ~w) ,a) ,i)))"
	    'formx ie 'form1 ie 'form0 el)))
    (make-drill spv '(index) macros)))

(defun gen-set (spv)
  "Returns a list that is a lambda expression embodying write access
  to the supplied SPARSE-VECTOR."
  (let* ((ie (spv-initial-element spv)) (el (spv-element-type spv))	
	 (macros
	   ;; I will probably never get nested backquotes right, so
	   ;; I'll settle for string hacking for now.  This is only
	   ;; invoked during the creation of a sparse vector, and not
	   ;; during general use, so it's no significant performance
	   ;; hit.
	   (readfmt
	    "((~w (form)
                `(or ,form (setf ,form (make-array ~w))))
              (~w (form)
                `(or ,form (setf ,form (make-array ~w :element-type '~w
                                                :initial-element ~w))))
              (~w (a i)
                `(setf (aref (the (simple-array ~w) ,a) ,i) ~w)))"
	    'formx 'w 'form1 'w el ie 'form0 el 'value)))
    (make-drill spv '(index value) macros)))

(defparameter *spva-checks* (list #'chk-element-type
				  #'chk-initial-element #'chk-size
				  #'chk-splay)
  "A list of functions to be called to validate an SPV argument
  structure.  Each function is expected to signal an ERROR condition
  when detecting a problem.  Once all functions return, creation of a
  new sparse vector should continue, as SPV-ARGS is safe and valid.")

(defun init-spv (spv)
  "Given a new SPARSE-VECTOR, whose argument-driven fields have all
  been validated, finish the initialization by creating the divisors
  list and compiling the acccessor functions.  Returns the
  SPARSE-VECTOR, ready for use."
  (with-ss ((splay splay) (size size) (tree tree) (divs divisors)
	    (set set) (get get)) spv- spv
    (let ((sz size))
      (setf tree nil #+nil (make-array (car splay) :initial-element nil)
            divs (butlast (mapcar #'(lambda (len)
                                      (setf sz (ceiling (/ sz len))))
                                  splay))
	    get (compile nil (gen-get spv))
	    set (compile nil (gen-set spv)))))
  spv)

(defun make-sparse-vector (size-or-splay
                           &key (element-type t)
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
                       :element-type element-type
                       :initial-element initial-element
                       :initial-element-p initial-element-p)))
    (map nil #'(lambda (f) (funcall f spv)) *spva-checks*)
    (init-spv spv)))

(defun spvref (spv index)
  "Return the INDEXth element of the supplied sparse vector."
  (funcall (spv-get spv) index))

(defun spvset (spv index value)
  "Sets the INDEXth element of the supplied sparse vector to VALUE."
  (funcall (spv-set spv) index value))

(defsetf spvref spvset)

(defun spvp (thing)
  "Return a true value if THING is a sparse vector, else NIL."
  (declare (inline spv-p))
  (spv-p thing))
