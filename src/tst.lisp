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
;;;   elements much wider than 7 bit ASCII or 8 bit ISO 8859.
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

;;; In a previous version, I had the root of a TST be a node itself,
;;; just with extra slots.  Now, I'm switch that to a more traditional
;;; tree of nodes that is hosted in a root

(defstruct node
  "Represents every node in a ternary search tree.  SPLIT is the
element this node represents, EQKID is for further keys continuing
from this element, while LOKID and HIKID are the child trees relating
to elements lower or higher than SPLIT.  TERMP indicates when this
null is the terminating element of a key, and VALUE is the value
associated with the key that terminates here."
  split lokid eqkid hikid termp value)

(defstruct (tst (:predicate tstp))
  "A Ternary Search Tree \(TST\), providing trie-like functionality in
a space-efficient manner."
  (root (make-node) :type (or null node))
  (test< +test<+ :type function)
  (test= +test=+ :type function)
  (size 0 :type unsigned-byte))

(defparameter +tst-key-args+ (reverse `(&key ignore-case test= test<))
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
macro adds :IGNORE-CASE, :TEST=, and :TEST< keyword arguments to
whatever function is being defined, and it also wraps the body inside
a LET that binds TEST< and TEST= variables to the appropriate
functions. With this macro, we can define any number of functions
taking any arguments, ensuring that they also take :IGNORE-CASE :TEST<
and :TEST= in a consistent manner.  Properly handles any Ordinary
Lambda List for ARGS, and an optional docstring as the first element
of BODY."
  (let ((args (defun-tst-arg-fixer args))
	(docp (stringp (car body))))
    ;; Use APPEND here to make empty lists disappear, so a missing
    ;; docstring doesn't cause a rogue NIL to appear in the resulting
    ;; DEFUN.
    (append `(defun ,name ,args)
	    (when docp (list (car body)))
	    `((let ((test< (or test< (and ignore-case #'char-lessp) +test<+))
		    (test= (or test= (and ignore-case #'char-equal) +test=+)))
		(declare (ignorable test< test=))
		,@(if docp (cdr body) body))))))

(defun-tst make ()
  "Returns a new (empty) ternary search tree.  By default, the TST is
set up to process its keys as strings in a case-sensitive manner, but
this can be modified by using the following keywords.

- :IGNORE-CASE with any true value sets the TST to process keys as
  strings in a case insensitive manner by default.  That is, the
  default use of CHAR= and CHAR< to compare key elements are switched
  to CHAR-EQUAL and CHAR-LESSP when this keyword is set.

- :TEST= supplies a function to be used to compare two elements of
  keys used in the TST for equality.  If you specify :TEST= you should
  also specify :TEST<.  If not specified, the default function used is
  CHAR=.  This option overrides the behavior of :IGNORE-CASE.

- :TEST< supplies a function to be used to compare two elements of
  keys used in the TST.  If you specify :TEST< you should also
  specify :TEST=.  If not specified, the default function used is
  CHAR<.  This option overrides the behavior of :IGNORE-CASE."
  (make-tst :test< test< :test= test=))

(defun size (tst)
  "Returns the number of values stored in the supplied Ternary Searh
Tree."
  (tst-size tst))

(defun emptyp (tst)
  "Return T if the supplied ternary search tree contains zero values;
else, return NIL.  Note that this does not mean the TST has no nodes,
but only that there are no values currently stored within it."
  (zerop (tst-size tst)))

(defun empty-node-p (node)
  "Return T when the supplied node in a TST contains no children."
  (and (null (node-lokid node))
       (null (node-eqkid node))
       (null (node-hikid node))))

(defun add% (node key idx test< test=)
  "Given a node somewhere in a Ternary Search Tree, follow nodes in
the tree that correspond to the elements of KEY starting from the
index IDX to the tree.  New nodes are added where necessary.  The
final node is marked as such (TERMP) in the path that represents KEY
is returned.  TEST< and TEST= are used to compare individual elements
of the key."
  ;; The ENSURE might look a little funny, but it allows us to write
  ;; ADD% in a way that is amenable to Tail Call Optimization
  ;; (specifically, to allow the compiler to recognize Tail Recursion).
  (macrolet ((ensure (place)
	       (let ((val (gensym)))
		 `(let ((,val ,place))
		    (or ,val (setf ,place (make-node)))))))
    (cond
      ((>= idx (length key))
       (setf (node-termp node) t)
       node)
      (t
       (let ((el (elt key idx)))
	 (cond
	   ((null (node-eqkid node))
	    (setf (node-split node) el)
	    (add% (ensure (node-eqkid node)) key (1+ idx) test< test=))
	   ((funcall test= el (node-split node))
	    (add% (node-eqkid node) key (1+ idx) test< test=))
	   ((funcall test< el (node-split node))
	    (add% (ensure (node-lokid node)) key idx test< test=))
	   (t
	    (add% (ensure (node-hikid node)) key idx test< test=))))))))

(defun-tst add (tst key value)
  "Add a VALUE to the supplied Ternary Search Tree associated with the
supplied KEY sequence.  Comparisons between elements of the KEY are
performed according to functions set when the TST was created, but
these can be overridden for a call through the :IGNORE-CASE :TEST<
and :TEST= keywords \(see the documentation for MAKE for a detailed
explanation\).  The TST is returned from ADD."
  (setf (node-value (add% (tst-root tst) key 0 test< test=)) value)
  (incf (tst-size tst))
  tst)


#|
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

(defun add% (tst node key idx value test< test=)
  (if (>= idx (length key))
      (setf (node-termp node) t
	    (size tst) (1+ (size tst))
	    (node-value node) value)
      (let ((new (make-node)))
	(cond
	  ((empty-node-p node)
	   (setf (node-split node) (elt key idx)
		 (node-eqkid node) new)
	   (add% tst new key (1+ idx) value test< test=))
	  ((funcall test= (elt key idx) (node-split node))
	   (setf (node-eqkid node) new)
	   (add% tst (node-eqkid node) key (1+ idx) value test< test=))
	  ((funcall test< (elt key idx) (node-split node))
	   (setf (node-lokid node) new)
	   (add% tst (node-lokid node) key idx value test< test=))
	  (t
	   (setf (node-hikid node) new)
	   (add% tst (node-hikid node) key idx value test< test=))))))

(defun-tst add (tst key value)
  (add% tst tst key 0 value test< test=))

(defun copy (tst)
  "Create and return a new Ternary Search Tree which shares the key
elements and values of the source TST, but has its own distinct
structure.  Modifications to the returned tree are independent of the
source tree, but stored values may be shared betwen the two."
  nil)
|#
