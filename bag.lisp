;;;; a bag is an unordered collection of items

(eval-when (:compile-toplevel)
  (print "compiling bag"))
(eval-when (:load-toplevel)
  (print "loading bag"))
(eval-when (:execute)
  (print "executing bag"))

(defpackage :accretions/bag
  (:use #:cl)
  (:export #:bag #:make #:bagp #:emptyp #:hasp #:count #:add #:copy #:map)
  (:shadow #:count #:map))
(in-package :accretions/bag)

;;; We use native hash tables to implement the bag type.  This
;;; hopefully strikes a good balance between speed and size, with the
;;; caveat that performance on _small_ bags suffers compared to simple
;;; lists.  We may offer other bags that use lists instead of hash
;;; tables in the future, just in case there's a need for heavy use of
;;; small bags.
;;;
;;; However, one annoying "glitch" in the hash table specification of
;;; Common Lisp is that there are patterns where GETHASH is called
;;; twice for the same key in succession.  For example, one often must
;;; call GETHASH to look up a value, and then call it again when
;;; updating that value.  It would be helpful to somehow cache that
;;; lookup so that the update can reuse it without recomputing the
;;; hash.  (Aside: this is exactly what SBCL does; it hides a fun hack
;;; where the last GETHASH is cached, and if a new GETHASH uses a key
;;; that is EQ to the last key, the cached return is used again.  But,
;;; obviously, this is unique to SBCL, and it only works for the most
;;; recent lookup.)
;;;
;;; Extended hash table API, like SBCL's, are common among different
;;; lisp implementations; none of them are standard, however.  So,
;;; we'll take a _slightly_ unusual approach here, and store a CONS as
;;; the value for a given key in the hash.  We no longer increment a
;;; value in the hash, we increment a place whose address is stored in
;;; the hash.  In this common case, the hash and hash lookup is only
;;; performed once.  Of course, the first time a new value is seen by
;;; our bag will still yield the old pattern of calling GETHASH twice.
;;; So, at worst, we're no worse than the usual behavior, and at best,
;;; we avoid nearly half the calls to GETHASH (and SXHASH and
;;; friends).

;;; There's no easy way (that I know) to limit the DEFSETF forms
;;; created by DEFSTRUCT from being exported; that is, how can we
;;; export HEAD without also exporting the DEFSETF for HEAD?  It's
;;; something to be avoided, since its presence might suggest to a
;;; client that it's perfectly fine to (SETF (HEAD BAG) Y) which is a
;;; bad idea.  To discourage this, we'll borrow a trick from Kalle
;;; Olavi Niemitalo in comp.lang.lisp.
;;;
;;; Use DEFSTRUCT options so that the names of the forms implied by
;;; DEFSTRUCT are not the same as the names we're going to export.
;;; Also, mark them for inlining.  Then, we can create our own
;;; functions for export, inlining the generated code into them.
;;; Creating our own functions will ensure they do exactly what we
;;; want, no more or less, and that they serve the API we want.  Using
;;; inline forms ensures that we don't pay any sort of double function
;;; call penalty for doing this; it wouldn't matter for larger
;;; functions as it would just get "lost in the noise", but usually
;;; these are tiny accessor (getter) functions where it could be
;;; noticed.

(defvar *testfn* #'eql
  "The function used to test for equality between items in a
bag (i.e., keys in a bag's hash table).  This value is used only when
a bag is created anew, setting the key test function of the underlying
hash table.  This exists specifically to link the argument supplied to
MAKE with BAG's constructor (provided by DEFSTRUCT), which has no such
API.")

(declaim (inline %bag-make %bag-p %bag-head %bag-size))
(defstruct (bag (:conc-name "%BAG-") (:copier %bag-copy)
		(:constructor %bag-make) (:predicate %bag-p))
  "An unordered collection of items. Unlike queues and other
collections, bags only accumulate values and do not support deletion."
  (hash (make-hash-table :test *testfn*) :type hash-table)
  (size 0 :type unsigned-byte))

(defun make (&key (test #'eql))
  "Returns a newly created BAG containing no items.  Items are tested
for equality using EQL, unless the TEST keyword is used to supply a
different function."
  (declare (inline %bag-make))
  (let ((*testfn* test))
    (%bag-make)))

(defun bagp (thing)
  "Returns T if THING is a BAG, otherwise NIL."
  (declare (inline %bag-p))
  (%bag-p thing))

(defun emptyp (bag)
  "Return T if the supplied BAG contains zero items; else, return NIL."
  (declare (inline %bag-size))
  (zerop (%bag-size bag)))

(defun hasp (bag item)
  "Returns T if ITEM appears at least once in the supplied BAG;
otherwise, returns NIL."
  (declare (inline %bag-hash))
  (multiple-value-bind (v s) (gethash item (%bag-hash bag))
    (declare (ignore v))
    (and s t)))

(defun count (bag &optional (item nil itemp))
  "Returns a count of items in the bag as an unsigned integer.  By default,
the total number of items in the bag is returned.  However, if the
ITEM argument is provided, then the total count of just that item
appearing in the bag is returned."
  (declare (inline %bag-size %bag-hash))
  (if itemp
      (multiple-value-bind (value found) (gethash item (%bag-hash bag))
	(or (and found (car value)) 0))
      (%bag-size bag)))

(defun add (bag item)
  "Add the supplied ITEM to the BAG, returning that same BAG.
ITEM can be any legitimate value in the lisp environment.  Duplicate
items within the BAG are supported; calling \(ADD NIL\) three times
yields a BAG with \(at least\) three NIL items."
  (declare (inline %bag-hash %bag-size))
  (let ((table (%bag-hash bag)))
    (multiple-value-bind (value found) (gethash item table)
      (if found
	  (incf (car value))
	  (setf (gethash item table) (cons 1 nil)))))
  (incf (%bag-size bag))
  bag)

;; As we're using hash tables in our implementation, there isn't an
;; actually useful distinction of "deep" or "shallow" copies of a bag.

(defun copy (bag)
  "Returns a new bag that is a copy of the supplied BAG."
  (let* ((oldhash (%bag-hash bag))
	 (newhash (make-hash-table :test (hash-table-test oldhash)
				   :size (hash-table-size oldhash))))
    (maphash #'(lambda (k v) (setf (gethash k newhash) (cons (car v) nil)))
	     oldhash)
    (%bag-make :hash newhash :size (%bag-size bag))))

(defun map (bag function)
  "For every item in the BAG, call the supplied FUNCTION designator
with that item as its sole argument.  The return value of FUNCTION is
considered as a generalized boolean.  MAP continues callng FUNCTION
once for every item in the bag \(including duplicate items\), unless
FUNCTION returns a false value.  On a false return from FUNCTION, MAP
stops and returns NIL.  Otherwise, all items in the bag are seen by
FUNCTION and MAP returns T."
  (declare (inline %bag-hash))
  (maphash #'(lambda (k v)
	       (dotimes (i (car v))
		 (unless (funcall function k)
		   (return-from map nil))))
	   (%bag-hash bag))
  t)

(declaim (notinline %bag-make %bagp %bag-head %bag-size))
