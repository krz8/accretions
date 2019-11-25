;;;; a bag is an unordered collection of items

(eval-when (:compile-toplevel)
  (print "compiling bag"))
(eval-when (:load-toplevel)
  (print "loading bag"))
(eval-when (:execute)
  (print "executing bag"))

(defpackage :accretions/bag
  (:use #:cl)
  (:export #:bag #:make #:size #:emptyp #:add))
(in-package :accretions/bag)

;;; We use native hash tables to implement the bag type.  This
;;; hopefully strike a good balance between speed and size, with the
;;; caveat that performance on _small_ bags suffers compared to simple
;;; lists.  Other possibilities are simple lists, and associative
;;; lists (used similar to the hash table approach below.
;;;
;;; However, one annoying "glitch" in the hash table specification of
;;; Common Lisp is that there are patterns where GETHASH is called
;;; twice for the same key in succession.  For example, one often must
;;; call GETHASH to look up a value, and then call it again when
;;; updating the value.  It would be helpful to somehow cache that
;;; lookup so that the update can reuse it without recomputing the
;;; hash.  (Aside: this is exactly what SBCL does; it hides a fun hack
;;; where the last GETHASH is cached, and if a new GETHASH uses a key
;;; that is EQ to the last key, the cached return is used again.  But,
;;; obviously, this is unique to SBCL.)
;;;
;;; So, we'll take a _slightly_ unusual approach here, and store a
;;; CONS as the value for a given key in the hash.  We no longer
;;; increment a value in the hash, we increment a place whose address
;;; is stored in the hash.  In this common case, the hash and hash
;;; lookup is only performed once.  Of course, the first time a new
;;; value is seen by our bag, that will yield the old pattern of
;;; calling GETHASH twice.  So, at worst, we're no worse than the
;;; usual behavior, and at best, we avoid nearly half the calls to
;;; GETHASH (and SXHASH and friends).

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
;;; Creating our own functions ensures they do exactly what we want,
;;; no more or less, and that they serve the API we want.  Using
;;; inline forms ensures that we don't pay any sort of double function
;;; call penalty for doing this; it wouldn't matter for larger
;;; functions as it would just get "lost in the noise", but usually
;;; these are tiny accessor (getter) functions where it could be
;;; noticed.

(defvar *testfn* #'eql
  "The function used to test for equality between keys in a bag's hash
table.  This value is used only when a bag is created anew, setting
the key test of the underlying hash table.  This exists specifically
to link the argument supplied to MAKE with BAG's constructor, which
has no such API.")

(declaim (inline %bag-make %bag-p %bag-head %bag-size))
(defstruct (bag (:conc-name "%BAG-") (:copier %bag-copy)
		(:constructor %bag-make) (:predicate %bag-p))
  "An unordered collection of items. Unlike queues and other
collections, bags only accumulate values and do not support deletion."
  (hash (make-hash-table :test *testfn*) :type hash-table)
  (size 0 :type unsigned-byte))

(defun make (&key (test #'eql))
  "Returns a newly created BAG containing no values.  By default, the
hash table underlying the newly created bag uses EQL as its function
to test key equality; this can be changed by specifying a different to
the :TEST keyword argument."
  (declare (inline %bag-make))
  (let ((*testfn* test))
    (%bag-make)))

(defun size (bag)
  "Returns the number of values that have been added to a BAG."
  (declare (inline %bag-size))
  (%bag-size bag))

(defun emptyp (bag)
  "Return T if the supplied BAG contains zero items; else, return NIL."
  (declare (inline %bag-size))
  (zerop (%bag-size bag)))

;; move table and test into test/bag.lisp it's not such a big deal if
;; we lose the inline nature of the calls, this isn't client code

(defun table (bag)
  "Returns the hash table used in the BAG.  This is sometimes used in
test cases, but it should not be generally exported."
  (declare (inline %bag-hash))
  (%bag-hash bag))

(defun testfn (bag)
  "Returns the function used to test equality in the bag's underlying
hash table.  This is sometimes used in test cases, but it should not
be generally exported."
  (declare (inline %bag-hash))
  (hash-table-test (%bag-hash bag)))

(defun add (bag value)
  "Add the supplied VALUE to the BAG, returning that same BAG.
Duplicate VALUE within the BAG is supported; calling \(ADD NIL\) three
times yields a BAG with \(at least\) three NIL values."
  (declare (inline %bag-hash %bag-size))
  (let ((table (%bag-hash bag)))
    (multiple-value-bind (place found) (gethash value table)
      (if found
	  (incf (car place))
	  (setf (gethash value table) (cons 1 nil)))))
  (incf (%bag-size bag))
  bag)

#|

(defun head (bag)
  "Returns the list component of a BAG."
  (declare (inline %bag-head))
  (%bag-head bag))

(defun bagp (thing)
  "Returns T if THING is a BAG, otherwise NIL."
  (declare (inline %bagp))
  (%bagp thing))

(defun copy (bag)
  "Creates and returns a shallow copy of the supplied BAG.  New
values may be added to either BAG without affecting the other, but
any values present in the source BAG when COPY is called are
shared with the returned BAG."
  (%bag-make :head (%bag-head bag)
		 :size (%bag-size bag)))

;;; We used to test that the CONS succeeded before modifying the list;
;;; the idea was that CONS is really the only thing that could fail in
;;; ADD.  In practice, though, any situation that leads to a CONS
;;; failure should raise an error condition of some kind.  In other
;;; words, CONS doesn't have a failure return.  For that reason, ADD
;;; no longer tests anything, on the assumption that if an error is
;;; not raised, the operation is successful.

(defun add (bag value)
  "Add the supplied VALUE to the BAG, returning that same BAG.
Duplicate VALUE within the BAG supported; calling \(ADD NIL\)
three times yields a BAG with \(at least\) three NIL values."
  (declare (inline %bag-head %bag-size))
  (setf (%bag-head bag) (cons value (%bag-head bag)))
  (incf (%bag-size bag))
  bag)

(defun mapfun (bag function)
  "For every value in the BAG, call the supplied FUNCTION designator
with that value as an argument.  Always returns T, regardless of the
return values of FUNCTION or the size of the BAG."
  (declare (inline %bag-head))
  (mapc function (%bag-head bag))
  t)

(defun hasp (bag value &key (test #'eql))
  "Tests for the presence of VALUE in the BAG, returning two values.
The first value returned is VALUE if found in the BAG, else NIL.  The
second value is always T or NIL, reflecting whether the value was
found.  This mimics the Common Lisp GETHASH, maintaining a useful
idiom for the primary return value while resolving the ambiguity
surrounding a search for a NIL value.  The TEST keyword can be used to
change the test for equality from its default of EQL."
  (declare (inline %bag-head))
  (let ((x (member value (%bag-head bag) :test test)))
    (values (car x) (not (null x)))))

(defun cnt (bag value &key (test #'eql))
  "Return the number of tims that VALUE appears within the supplied
BAG.  The TEST keyword can be used to change the test for equality
from its default of EQL.  This function has different runtime
characteristics from HASP, hence its separate implementation."
  (declare (inline %bag-head))
  (count value (%bag-head bag) :test test))

|#

(declaim (notinline %bag-make %bagp %bag-head %bag-size))
