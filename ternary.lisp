;;;; ternary search trees
(in-package #:accretions)

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
;;;   character (code point 0).
;;;
;;; - Not only strings, but actually any sequence of elements may be
;;;   stored in the tree.
;;;
;;; - The root of a ternary search tree is not the top node in the
;;;   tree; instead, it is a special object that caches information
;;;   used throughout the tree (such as comparison functions for
;;;   elements of the keys).  We also use the presence of a node to
;;;   mark that a given key element actually exists (rather than
;;;   overloading the meaning of null, as in the C implementation).

(defclass tst-node ()
  ((split :accessor split :initarg :split :initform nil
	  :documentation "The element in some key sequence that this
	  instance represents in a ternary search tree, acting as the
	  split point for other trees that represent element less and
	  greater than this one.")
   (lokid :accessor lokid :initarg :lokid :initform nil
	  :documentation "The subtree of nodes less than SPLIT.")
   (eqkid :accessor eqkid :initarg :eqkid :initform nil
	  :documentation "The subtree of nodes that include SPLIT.")
   (hikid :accessor hikid :initarg :hikid :initform nil
	  :documentation "The subtree of nodes greater than SPLIT."))
  (:documentation "Implements every node in a ternary search tree."))

(defclass tst-root (tst-node)
  ((test< :accessor test< :initarg :test< :initform #'char<)
   (test= :accessor test= :initarg :test= :initform #'char=)
   (test> :accessor test> :initarg :test> :initform #'char>))
  (:documentation "The root of a ternary search tree contains extra
  information used throughout that tree, such as functions to compare
  elements of key sequences.  Use MAKE-TST to create new root nodes in
  client code \(that is, to create new ternary search trees\), rather
  than MAKE-INSTANCE."))

(defun make-tst (&key ignore-case test= test< test>)
  "Creates and returns a new \(empty\) ternary search tree.  By
  default, the tree expects strings to represent keys, and performs
  case sensitive comparisons between the characters of those strings.
  However, this behavior can be modified by using one of more of the
  following keywords:

- :IGNORE-CASE, when non-NIL, sets the tree to perform
  case-insensitive character comparisons on its keys.  This is exactly
  equivalent to specifying #'CHAR-EQUAL with :TEST= and #'CHAR-LESSP
  with :TEST<.

- :TEST=, when non-NIL, specifies a function that compares two
  elements of a key sequence, returning true when the first argument
  should be considered equal \(or at least equivalent\) with the
  other.  Use this, :TEST<, and :TEST>, when non-string keys are
  desired.  Overrides :IGNORE-CASE.

- :TEST<, when non-NIL, specifies a function that compares two
  elements of a key sequence, returning true when the first argument
  should be considered \"less\" than the other.  Use this, :TEST<,
  and :TEST= when non-string keys are desired.
  Overrides :IGNORE-CASE.

- :TEST>, when non-NIL, specifies a function that compares two
  elements of a key sequence, returning true when the first argument
  should be considered \"less\" than the other.  Use this, :TEST=,
  and :TEST<, when non-string keys are desired.
  Overrides :IGNORE-CASE."
  (make-instance 'tst-root
		 :test= (or test=
			    (and ignore-case #'char-equal)
			    #'char=)
		 :test< (or test<
			    (and ignore-case #'char-lessp)
			    #'char<)
		 :test> (or test>
			    (and ignore-case #'char-greaterp)
			    #'char>)))

(defmacro with-tst-context ((tstroot) &body body)
  "Common code that sets up a context used in several functions for
  operating on a ternary search tree.  Essentially, this takes
  information out of our root node, caching it in the local lexical
  environment."
  `(let ((test<fn (test< ,tstroot))
	 (test=fn (test= ,tstroot))
	 (test>fn (test> ,tstroot)))
     (declare (ignorable test=fn test<fn test>fn))
     (macrolet ((test< (x y) `(funcall test<fn ,x ,y))
		(test= (x y) `(funcall test=fn ,x ,y))
		(test> (x y) `(funcall test>fn ,x ,y)))
       ,@body)))

(defun tst-insert (key tst)
  "Insert the supplied KEY sequence into the ternary search tree TSTROOT."
  (with-tst-context (tst)
    (labels
	((insert (i node)
	   (let ((el (elt key i)))
	     (when (null node)
	       (setf node (make-instance 'tst-node :split el)))
	     (cond
	       ((test< el (split node))
		(setf (lokid node) (insert (lokid node) i)))
	       ((test> el (split node))
		(setf (hikid node) (insert (hikid node) i)))
	       ((< i (1- (length key)))
		(setf (eqkid node) (insert (eqkid node) (1+ i))))
	       (t t))
	     node)))
      (insert 0 tst))))

;; can't work we aren't handling the "root node" correctly

(defmethod emptyp ((tst tst-root))
  "Returns true when the ternary search tree TST is empty."
  (null (tree tst)))
