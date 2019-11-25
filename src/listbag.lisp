;;;; a listbag is an unordered collection of items implemented with a list

(defpackage :accretions/src/listbag
  (:use #:cl)
  (:export #:bag #:make #:copy #:listbagp #:size #:emptyp #:add #:mapfun
	   #:hasp))
(in-package :accretions/src/bag)

;;; End-users should be discouraged from using the SETF forms to
;;; modify the BAG structure.  Borrowing from Kalle Olavi Niemitalo in
;;; comp.lang.lisp, the structure's traditional functions are
;;; decorated with a % in their name.  Marked inline, we can then use
;;; them in the definitions of our own exported forms to allow
;;; precisely the interface we want to the BAG structure; being inline, 

;;; There's no easy way (that I know) to limit defsetf forms created
;;; by DEFSTRUCT from being exported.  Thus, in the structure below,
;;; exporting HEAD will not only expose the "getter" function, but the
;;; "setter" will also be exported as well.  Clients might then be
;;; tempted to use forms like (SETF (HEAD X) Y) which is a bad idea.
;;; To discourage this, we'll borrow a trick from Kalle Olavi
;;; Niemitalo in comp.lang.lisp.
;;;
;;; Mark the accessors such that they can be inlined, using a naming
;;; scheme distinct from the default.  Then, define only the functions
;;; that do exactly what we want (and no more than that), using those
;;; inline definitions to avoid overhead.  Our package exports no long
;;; expose functionality we don't want.  A sufficiently motivated
;;; client can read the source, see the actual functions, and reach
;;; behind the curtain using :: instead of :.  This is as it should
;;; be, I don't want to seal this functionality off, I just want to
;;; discourage its use, accidental or otherwise.

(declaim (inline %listbag-make %listbagp %listbag-head %listbag-size))
(defstruct (listbag (:conc-name "%LISTBAG-") (:copier nil)
		    (:constructor %listbag-make) (:predicate %listbagp))
  "An unordered collection of items, implemented using a list.  Unlike
queues and other collections, bags only accumulate values and do not
support deletion.  Use a HASHBAG if the bag is expected to hold many
values."
  (head nil :type list)
  (size 0 :type unsigned-byte))
(declaim (notinline %listbag-make %listbagp %listbag-head %listbag-size))

(defun make ()
  "Returns a newly created LISTBAG containing no values."
  (declare (inline %listbag-make))
  (%listbag-make))

(defun head (listbag)
  "Returns the list component of a LISTBAG."
  (declare (inline %listbag-head))
  (%listbag-head listbag))

(defun size (listbag)
  "Returns the number of values seen by a LISTBAG."
  (declare (inline %listbag-size))
  (%listbag-size listbag))

(defun listbagp (thing)
  "Returns T if THING is a LISTBAG, otherwise NIL."
  (declare (inline %listbagp))
  (%listbagp thing))

(defun copy (listbag)
  "Creates and returns a shallow copy of the supplied LISTBAG.  New
values may be added to either LISTBAG without affecting the other, but
any values present in the source LISTBAG when COPY is called are
shared with the returned LISTBAG."
  (%listbag-make :head (%listbag-head bag)
		 :size (%listbag-size bag)))

(defun emptyp (listbag)
  "Return T if the supplied LISTBAG contains zero items; else, return NIL."
  (null (%listbag-head bag)))

;;; We used to test that the CONS succeeded before modifying the list;
;;; the idea was that CONS is really the only thing that could fail in
;;; ADD.  In practice, though, any situation that leads to a CONS
;;; failure should raise an error condition of some kind.  In other
;;; words, CONS doesn't have a failure return.  For that reason, ADD
;;; no longer tests anything, on the assumption that if an error is
;;; not raised, the operation is successful.

(defun add (listbag value)
  "Add the supplied VALUE to the LISTBAG, returning that same LISTBAG.
Duplicate VALUE within the LISTBAG supported; calling \(ADD NIL\)
three times yields a LISTBAG with \(at least\) three NIL values."
  (declare (inline %listbag-head %listbag-size))
  (setf (%listbag-head listbag) (cons value (%listbag-head listbag)))
  (incf (%listbag-size listbag))
  listbag)

(defun mapfun (listbag function)
  "For every value in the BAG, call the supplied FUNCTION designator
with that value as an argument.  Always returns T, regardless of the
return values of FUNCTION or the size of the BAG."
  (declare (inline %listbag-head))
  (mapc function (%listbag-head listbag))
  t)

(defun hasp (bag value &key (test #'eql))
  "Tests for the presence of VALUE in the BAG, returning two values.
The first value returned is VALUE if found in the BAG, else NIL.  The
second value is always T or NIL, reflecting whether the value was
found.  This mimics the Common Lisp GETHASH, maintaining a useful
idiom for the primary return value while resolving the ambiguity
surrounding a search for a NIL value.  The TEST keyword can be used to
change the test for equality from its default of EQL."
  (declare (inline %listbag-head))
  (let ((x (member value (%listbag-head bag))))
    (values (car x) (not (null x)))))

(defun count (listbag value &key (test #'eql))
  "Return the number of tims that VALUE appears within the supplied
LISTBAG.  The TEST keyword can be used to change the test for equality
from its default of EQL.  This function has different runtime
characteristics from HASP, hence its separate implementation."
  (declare (type (unsigned-byte n)) (inline %listbag-head))
  (setf n 0)
  (mapc #'(lambda (x) (when (funcall test value x)
			(incf n)))
	(%listbag-head listbag))
  n)
