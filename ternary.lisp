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
  ((split :accessor split :initarg :split
	  :documentation "Every node in a TST is associated with one
          element from a key, the \"split\" point.  It implies the
          three subtrees at LOKID, EQKID, and HIKID.")
   (lokid :accessor lokid :initarg :lokid
	  :documentation "The subtree of nodes less than SPLIT.")
   (eqkid :accessor eqkid :initarg :eqkid
	  :documentation "The subtree of nodes that include SPLIT.")
   (hikid :accessor hikid :initarg :hikid
	  :documentation "The subtree of nodes greater than SPLIT.")
   (termp :accessor termp :initarg :termp
	  :documentation "Used to mark when a node terminates some
	  path through the TST.  A node where TERMP is true not only
	  marks a complete key in the trie, but it also implies that
	  VALUE, even if NIL, is current.")
   (value :accessor value :initarg :value
	  :documentation "The value that was stored in the trie,
	  associated with the key at this node."))
  (:default-initargs :split nil :lokid nil :eqkid nil :hikid nil
		     :termp nil :value nil)
  (:documentation "Implements every node in a ternary search tree."))

(defclass tst-root (pair-collection kvc-with-element-tests tst-node)
  ()
  (:documentation "The root of a ternary search tree is a node in that
  tree.  It also contains extra information used throughout that tree,
  such as functions to compare elements of key sequences.  Use
  MAKE-TST to create new root nodes in client code \(that is, to
  create new ternary search trees\), rather than MAKE-INSTANCE."))

(defun make-tst (&key ignore-case key-test value-test el-test-<
		   el-test-= el-test>)
  "Creates and returns a new \(empty\) ternary search tree.  With
  no options, MAKE-TST returns a tree that expects strings as keys,
  performs case-sensitive comparisons of key values, and uses EQUAL
  to compare values \(such as when searching for a particular value
  via the CONTAINSP generic function\).

  However, other key and value tests can be specified:

- :IGNORE-CASE, when non-NIL, sets the tree to perform
  case-insensitive character comparisons on its keys.  This is
  shorthand for specifying #'EQUALP for both :KEY-TEST
  and :VALUE-TEST, and for specifying #'CHAR-LESSP, #'CHAR-EQUAL, and
  #'CHAR-GREATERP as arguments to :EL-TEST-<, :EL-TEST-=,
  and :EL-TEST->, respectively.

- :KEY-TEST specifies a function for comparing whole keys in the tree.
  The default test is #'EQUAL.

- :TEST=, when non-NIL, specifies a function that compares two
  elements of a key sequence, returning true when the first argument
  should be considered equal \(or at least equivalent\) with the
  other.  Overrides :IGNORE-CASE.

- :TEST<, when non-NIL, specifies a function that compares two
  elements of a key sequence, returning true when the first argument
  should be considered \"less\" than the other.  Overrides :IGNORE-CASE.

- :TEST>, when non-NIL, specifies a function that compares two
  elements of a key sequence, returning true when the first argument
  should be considered \"less\" than the other.  Overrides :IGNORE-CASE.

- :VALUE-TEST specifies a function for comparing values \(not keys\)
  in the tree.  This happens rarely, but is crucial to generic
  functions such as CONTAINSP.  The default comparison is #'EQUAL.
  Using :IGNORE-CASE \(which this overrides\) sets this to #'EQUALP."
  (make-instance 'tst-root
		 :key-test (or key-test
			       (and ignore-case #'equalp)
			       #'equal)
		 :value-test (or value-test
				 (and ignore-case #'equalp)
				 #'equal)
		 :el-test-= (or el-test-=
				(and ignore-case #'char-equal)
				#'char=)
		 :el-test-< (or el-test-<
				(and ignore-case #'char-lessp)
				#'char<)
		 :el-test-> (or el-test->
				(and ignore-case #'char-greaterp)
				#'char>)))

(defmacro with-tst-context ((tstroot) &body body)
  "Common code that sets up a context used in several functions for
  operating on a ternary search tree.  Essentially, this takes
  information out of our root node, caching it in the local lexical
  environment."
  `(let ((el<fn (el-test-< ,tstroot))
	 (el=fn (el-test-= ,tstroot))
	 (el>fn (el-test-> ,tstroot))
	 (ktest (key-test ,tstroot))
	 (vtest (value-test ,tstroot)))
     (declare (ignorable el<fn el=fn el>fn ktest vtest))
     (macrolet ((el< (x y) `(funcall el<fn ,x ,y))
		(el= (x y) `(funcall el=fn ,x ,y))
		(el> (x y) `(funcall el>fn ,x ,y))
		(key= (x y) `(funcall ktest ,x ,y))
		(value= (x y) `(funcall vtest ,x ,y)))
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
