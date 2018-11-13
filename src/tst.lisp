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

(defstruct (tst (:include node) (:conc-name nil) (:constructor nil)
		(:copier nil) (:predicate tstp))
  "A Ternary Search Tree, providing trie-like functionality in a
space-efficient manner."
  (test< #'char< :type function)
  (test= #'char= :type function)
  (size 0 :type unsigned-byte))

(defun make (&key ignore-case test= test<)
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
  (make-tst :test< (or test< (and ignore-case #'char-lessp) #'char<)
	    :test= (or test= (and ignore-case #'char-equal) #'char=)))
