;;;; ternary search trees

(defpackage :accretions/src/tst
  (:use #:cl)
  (:export #:tst #:make #:copy #:tstp
	   #:size #:lokid #:hikid
	   #:emptyp #:add #:mapfun))
(in-package :accretions/src/tst)

;;; As described by Sedgewick and Bentley, Ternary Search Trees offer
;;; important advantages over both hash tables as well as binary
;;; search trees.  However, their [refimpl][] exploits some details
;;; of their target system that aren't necessarily portable to all
;;; systems.  Primarily, they encode the ASCII NUL string terminator
;;; as part of their keys; this has the advantage of making the words
;;; "a" and "at" distinct from each other; "a\0" is distinct from
;;; the "a" that is a part of "at\0".  Simply disregarding those
;;; nulls would make it impossible (well, very hard, anyway) to
;;; distinguish between an "a" that appears in the tree on the way to
;;; "at" and the simple word "a". Likewise, their implementation does
;;; not allow for the encoding of ASCII NUL characters within a string
;;; key.
;;;
;;; [refimpl]:  http://www.cs.princeton.edu/~rs/strings/
;;;             "Reference Implementation"
;;; 
;;; This implementation, therefore, deviates from theirs slightly in
;;; the following ways:
;;;
;;; - Each string can be made up of any character supported by the
;;;   underlying system.  This includes Unicode characters (on systems
;;;   where Unicode is properly supported) as well as the null
;;;   character (code point 0).  This is a huge win over traditional
;;;   trie implementations, which can't reasonably accommodate key
;;;   components much larger than 7 bit ASCII or 8 bit ISO 8859.
;;;
;;; - Not only strings, but actually any sequence of elements may be
;;;   stored in the tree.
;;;
;;; - The root of a ternary search tree caches information used
;;;   throughout the tree, such as key comparison functions.  We
;;;   also use the presence of a node to indicate that a given key
;;;   element actually exists, rather than overloading the meaning
;;;   of NULL, as in the C implementation.
;;;
;;; - We use a termp ("terminated here?") slot that is set non-NIL in
;;;   the tree when we're at a point that defines a specific key in
;;;   the trie.  In this way, on the path "ate", you can indicate that
;;;   "a" is a separate word in the tree (or not).

(defstruct node
  split lokid eqkid hikid termp value)

(defstruct (tst (:include node) (:predicate tstp) (:conc-name nil))
  "A Ternary Search Tree, providing trie-like functionality in a
space-efficient manner."
  (test< #'char< :type function)
  (test= #'char= :type function)
  (size 0 :type unsigned-byte))

(defparameter +tst-key-args+ '(ignore-case test= test< &key)
  "A constant list that is spliced into lambda lists specified in
DEFUN-TST forms.")

(defun defun-tst-arg-fixer (arglist)
  "Given an ordinary lambda list, as would appear in a DEFUN form,
return a new lambda list that includes +TST-KEY-ARGS+.  Care is taken
to obey the ordering in an ordinary lambda list, according to CLHS
3.4.1."
  (let (spliced newargs)
    (flet ((maybe-add-key-args ()
	     (unless spliced
	       ;; append, not nconc, because +tst-key-args+ is constant
	       (setf newargs (append +tst-key-args+ newargs)
		     spliced t))))
      (dolist (a arglist)
	(cond
	  ((eq a '&aux)
	   (maybe-add-key-args)
	   (push '&aux newargs))
	  ((eq a '&key)
	   (maybe-add-key-args))
	  (t (push a newargs))))
      (maybe-add-key-args))
    (nreverse newargs)))

(defmacro defun-tst (name (&rest args) &body body)
  "Much like a regular DEFUN, taking most of the same forms, this
macro adds :IGNORE-CASE, :TEST=, and :TEST< keyword arguments, and it
also wraps the body inside a LET that binds TEST< and TEST= variables
to the appropriate functions. With this macro, we can define any
number of functions taking any arguments, ensuring that they also
take :IGNORE-CASE :TEST< and :TEST= in a consistent manner."
  `(defun ,name ,(defun-tst-arg-fixer args)
     (let ((test< (or test< (and ignore-case #'char-lessp) #'char<))
	   (test= (or test= (and ignore-case #'char-equal) #'char=)))
       (declare (ignorable test< test=))
       ,@body)))

(defun-tst make ()
  "Returns a new (empty) ternary search tree.  By default, the TST is
set up to process its keys as strings in a case-sensitive manner, but
this can be modified by using the following keywords.

- :IGNORE-CASE with any non-NIL value sets the TST to process keys as
  strings in a case insensitive manner.  That is, the default use of
  CHAR= and CHAR< to compare key elements are switched to CHAR-EQUAL
  and CHAR-LESSP when this keyword is set.

- :TEST= supplies a function to be used to compare two elements of
  keys used in the TST for equality.  If you specify :TEST= you should
  also specify :TEST<.  If not specified, the default function used is
  CHAR=.  This option overrides the behavior of :IGNORE-CASE.

- :TEST< supplies a function to be used to compare two elements of
  keys used in the TST.  If you specify :TEST< you should also
  specify :TEST=.  If not specified, the default function used is
  CHAR<.  This option overrides the behavior of :IGNORE-CASE."
  (make-tst :test< test< :test= test=))

(defun emptyp (tst)
  "Return T if the supplied ternary search tree contains zero items; else, return NIL."
  (zerop (tst-size tst)))

(defun copy (tst)
  "Create and return a new Ternary Search Tree which shares the key
elements and values of the source TST, but has its own distinct
structure.  Modifications to the returned tree are independent of the
source tree, but stored values may be shared betwen the two."
  nil)
