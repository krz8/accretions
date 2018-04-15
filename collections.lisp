;;;; definitions of general kinds of collections
(in-package #:accretions)

(defclass item-collection ()
  ((testfn :accessor testfn :initarg :test
	   :documentation "The function to use when testing two items for
           equality in the collection."))
  (:default-initargs :test #'equal)
  (:documentation "Collections of single items \(e.g., bags\) use this
  as a superclass.  This assists in the implementation of default
  methods of certain generic functions, such as CONTAINSP."))

(defmethod containsp ((collection item-collection)
		      &key (item nil itemp))
  "The default method for collections of items, based on MAPFUN."
  (if itemp
      (let ((test (testfn collection)))
	(mapfun (lambda (x) (when (funcall test x item)
			      (return-from containsp t)))
		collection))
      (error 'missing-item
	     :gfname 'containsp
	     :collection collection))  
  nil)

(defclass kv-collection ()
  ((key-testfn :accessor key-testfn :initarg :key-test
	       :documentation "The function to use when comparing keys in
               the collection.")
   (value-testfn :accessor value-testfn :initarg :value-test
		 :documentation "The function to use when comparing values in
                 the collection."))
  (:default-initargs :key-test #'string= :value-test #'equal)
  (:documentation "Collections of key/value pairs use this as a
  superclass.  This assists in the implementation of default methods
  of certain generic functions, such as CONTAINSP."))

<<<<<<< Updated upstream
(defmethod containsp ((collection pair-collection)
		      &key (key nil keyp) (value nil valuep))
=======
(defmethod containsp ((collection kv-collection)
		      &key (key nil keyp) (value nil valuep)
			&allow-other-keys)
>>>>>>> Stashed changes
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
	         within the KEY.")
   (key-el-test= :accessor key-el-test= :initarg :key-el-test=
		 :documentation "Names a function used to compare elements
	         within the KEY.")
   (key-el-test> :accessor key-el-test> :initarg :key-el-test>
		 :documentation "Names a function used to compare elements
	         within the KEY."))
  (:default-initargs :key-el-test< #'char< :key-el-test= #'char=
		     :key-el-test> #'char>)
  (:documentation "Adds comparison functions for individual elements
  of keys to the KV-COLLECTION class.  This is necessary to tree
  structures that need to break down keys by element."))
