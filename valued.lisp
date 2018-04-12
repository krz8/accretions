;;;; mixin supporting values (for key/value storage)
(in-package #:accretions)

;;; The valued mixin is meant to be added to the actual nodes in a
;;; storage tree (not the root, as with the counted mixin).

(defclass valued ()
  ((value :accessor value :initarg :value :initform nil
	  :documentation "A caller-specified value of any type,
	  associated with a particular key in a collection of some
	  sort."))
  (:documentation "Provides the value of key-based storage.  Not all
  keyed collections necessarily store values, so this adds it to the
  ones that need it.  For example, a ternary search trie might be used
  to represent a dictionary; such a trie requires no values to be
  maintained beside its keys."))

