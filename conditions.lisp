(define-condition index-error (type-error)
  ((spv :initarg :spv
	:documentation "The SPARSE-VECTOR for which this condition was
	signaled.")
   (index :initarg :index
	  :documentation "The index of the element that the caller
	  attempted to access in the SPARSE-VECTOR."))
  (:report
   (lambda (condition stream)
     (with-slots (spv index) condition
       (with-slots (size) spv
	 (format stream "Invalid index ~W into SPARSE-VECTOR ~W of size ~W"
		 index spv size)))))
  (:documentation "The condition INDEX-ERROR is raised when a
  SPARSE-VECTOR is access with an out-of-bounds index.  Such values
  are less than 1, or equal-to or greater-than the size of the
  SPARSE-VECTOR.  This INDEX-ERROR is a subtype of INDEX-ERROR,
  conforming to existing practice in Common Lisp."))

