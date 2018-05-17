(uiop:define-package :accretions/src/collections
  (:use :common-lisp :accretions/src/misc)
  (:export #:item-collection #:test
	   #:kv-collection #:key-test #:value-test
	   #:kve-collection #:key-el-test< #:key-el-test= #:key-el-test>))
(in-package :accretions/src/collections)

(defclass item-collection ()
  ((test :reader test :initarg :test
	 :documentation "The function to use when testing two items
	 for equality in the collection.  EQUAL is used by default."))
  (:default-initargs :test #'equal)
  (:documentation "Collections of single items \(i.e., not key/value
  pairs\) use this as a superclass, defining a default function to use
  when testing equality between values in that collection."))

#+nil
(defmethod containsp ((collection item-collection)
		      &key (item nil itemp) test)
  "The default method for collections of items, based on MAPFUN.
  Returns T when at least one instance of the :ITEM keyword appears in
  the named COLLECTION.  Returns NIL if no such item could be found,
  or if an error arises and the condition is suppressed.  This is a
  default implementation, other item collections may provide their own
  specific methods.  Use the :TEST keyword to provide a function of
  two arguments to test equality instead of the default function used
  in COLLECTION."
  (if itemp
      (let ((test (or test (test collection))))
	(mapfun collection (lambda (x) (when (funcall test x item)
					 (return-from containsp t)))))
      (error 'missing-item :gfname 'containsp :collection collection))  
  nil)

(defclass kv-collection ()
  ((key-test :accessor key-test :initarg :key-test
	     :documentation "The function to use when comparing keys in
             the collection.")
   (value-test :accessor value-test :initarg :value-test
	       :documentation "The function to use when comparing values in
               the collection."))
  (:default-initargs :key-test #'string= :value-test #'equal)
  (:documentation "Collections of key/value pairs use this as a
  superclass, providing functions to test equality between keys and
  between values.  This assists in the implementation of default
  methods of certain generic functions, such as CONTAINSP."))

#+nil
(defmethod containsp ((collection kv-collection)
		      &key (key nil keyp) (value nil valuep))
  "The default method for collections of key/value pairs, based on MAPFUN.
  Better methods may be supplied that exploit aspects of specific
  collection types, but this method should do in a pinch for all of
  them."
  ;; Yes, this is slightly ugly.  However, by spelling it out like
  ;; this, we avoid unneccesary bindings to unused functions (e.g.,
  ;; setting up a value test function when we can determine we'll only
  ;; need to test keys).  The code determining FN's value used to be
  ;; right in the MAPFUN form itself, but when the caller gets the
  ;; keyword arguments wrong, that generates an error for each item in
  ;; the collection; binding its function value, instead, ensures we
  ;; only see the error once.
  (let* ((ktest (key-testfn collection))
	 (vtest (value-testfn collection))
	 (fn (if keyp
		(if valuep
		    (lambda (k v)
		      (when (and (funcall ktest k key)
				 (funcall vtest v value))
			(return-from containsp t)))
		    (lambda (k v)
		      (declare (ignore v))
		      (when (funcall ktest k key)
			(return-from containsp t))))
		(if valuep
		    (lambda (k v)
		      (declare (ignore k))
		      (when (funcall vtest v value)
			(return-from containsp t)))
		    (error 'missing-key-or-value
			   :gfname 'containsp
			   :collection collection)))))
    (mapfun fn collection))
  nil)

(defclass kve-collection (kv-collection)
  ((key-el-test< :accessor key-el-test< :initarg :key-el-test<
		 :documentation "Names a function used to compare elements
	         within a KEY sequence.")
   (key-el-test= :accessor key-el-test= :initarg :key-el-test=
		 :documentation "Names a function used to compare elements
	         within a KEY sequence.")
   (key-el-test> :accessor key-el-test> :initarg :key-el-test>
		 :documentation "Names a function used to compare elements
	         within a KEY sequence."))
  (:default-initargs :key-el-test< #'char< :key-el-test= #'char=
		     :key-el-test> #'char>)
  (:documentation "Adds comparison functions for individual elements
  of keys to the KV-COLLECTION class.  This is necessary to tree
  structures that need to break down keys by element.  Three
  functions, instead of simple equality, are defined here because if
  you're working with the individual elements of a key, you're almost
  certainly sorting or ordering those keys in some manner."))
