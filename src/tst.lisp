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
;;; This implementation, therefore, deviates from theirs a little bit
;;; in the following ways:
;;;
;;; - Each string can be made up of any character supported by the
;;;   underlying system.  This includes Unicode characters (on systems
;;;   where Unicode is properly supported) as well as the null
;;;   character (code point 0).  This is a huge win over traditional
;;;   trie implementations, which can't reasonably accommodate key
;;;   components much larger than 7 bit ASCII or 8 bit ISO 8859.
;;;
;;; - Not only strings, but actually any sequence of elements may be
;;;   stored in the tree.  TEST= and TEST< functions need to be
;;;   supplied, according to the nature of the sequences being used
;;;   for keys.
;;;
;;; - The root of a ternary search tree caches information used
;;;   throughout the tree, such as key comparison functions.
;;;
;;; - An empty TST has NIL in all three children slots, and false for
;;;   TERMP.  But, because we cache a size in the TST root, we can
;;;   also just test that single slot for zero, faster than testing
;;;   four slots in a node.
;;;
;;; - A TST maps a key of length 0 (an empty sequence) to just the
;;;   root node with TERMP set true.
;;;
;;; - A TST with a single key of length 1 contains the key's first
;;;   (only) element in the SPLIT slot, and TERMP set in the
;;;   (otherwise empty) node attached to EQKID.
;;;
;;; - The rest of the TST builds on this pattern.  The TERMP slot is
;;;   used to indicate if this node is the "end" of the path to a
;;;   valid key in the tree.  TERMP -> "terminated?"  This flag allows
;;;   us to handle keys with overlapping paths in the tree (e.g., "a"
;;;   and "ate") while still handling NIL in key sequences and values.
;;;   The reference implementation, if you look, assumes strings and
;;;   that nulls terminate sequences; a straight translation to Lisp
;;;   can't, then, support a sequence that contains NIL.

(defparameter +test=+ #'char=
  "Default function to use when comparing elements of a key sequence
for equality.")

(defparameter +test<+ #'char<
  "Default function to use when comparing elements of a key sequence
for less-than.")

(defstruct node
  "Represents every node in a ternary search tree, including the root.
SPLIT is the element this node represents, EQKID is for further keys
continuing from this element, while LOKID and HIKID are the child
trees relating to elements lower or higher than SPLIT.  TERMP
indicates when this null is the terminating element of a key, and
VALUE is the value associated with the key that terminates here."
  split lokid eqkid hikid termp value)

(defstruct (tst (:include node) (:predicate tstp) (:conc-name nil))
  "A Ternary Search Tree \(TST\), providing trie-like functionality in
a space-efficient manner."
  (test< +test<+ :type function)
  (test= +test=+ :type function)
  (size 0 :type unsigned-byte))

(defparameter +tst-key-args+ (reverse `(&key ignore-case
					     (test= ,+test=+)
					     (test< ,+test<+)))
  "A constant list that is spliced into lambda lists specified in
DEFUN-TST forms.")

(defun defun-tst-arg-fixer (arglist)
  "Given an ordinary lambda list, as would appear in a DEFUN form,
return a new lambda list that includes the keyword arguments specified
in +TST-KEY-ARGS+.  Care is taken to obey the ordering in an ordinary
lambda list \(see CLHS 3.4.1\)."
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
  "Return T if the supplied ternary search tree contains zero items;
else, return NIL."
  (zerop (size tst)))

;;; We could probably halve the number of tests in EMPTY-NODE-P by
;;; omitting the tests on LOKID and HIKID.  However, I want to support
;;; in-tree deletions that don't immediately rebalance the tree, so
;;; it's conceivable that after a delete operation, you could have a
;;; LOKID or a HIKID but not an EQKID any longer.  On the other hand,
;;; it's likely I'll never get around to a post-delete tree re-balance
;;; operation, making this obnoxiously over-cautious.

(defun empty-node-p (node)
  "Return T if the supplied node of a ternary search tree (including
the root) is empty, devoid of children or terminations."
  (and (null (node-lokid node))
       (null (node-eqkid node))
       (null (node-hikid node))
       (not (node-termp node))))

(defun-tst add (tst key value)
  (labels ((insert (tst node key idx value)
	     (cond
	       ((= idx (length key))
		(setf (node-termp node) t
		      (node-value value value)))
	       ((empty-node-p node)
		))
	     (incf (size tst))
		t))
    (insert tst tst key 0 value)))

(defun copy (tst)
  "Create and return a new Ternary Search Tree which shares the key
elements and values of the source TST, but has its own distinct
structure.  Modifications to the returned tree are independent of the
source tree, but stored values may be shared betwen the two."
  nil)
