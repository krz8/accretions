;;;; the few conditions used by Accretions
(in-package #:accretions)

(define-condition collection-error (error)
  ((collection :initarg :collection :reader collection
	       :documentation "The collection being manipulated when
	       the error occured."))
  (:default-initargs :collection nil)  
  (:documentation "Tracks a collection used in an error condition."))

(define-condition generic-function-error (error)
  ((gfname :initarg :gfname :reader gfname
	   :documentation "The name of the generic function in use
	   when the error occured."))
  (:default-initargs :gfname nil)  
  (:documentation "Tracks the name of a generic function used in an
  error condition."))

(define-condition missing-item (collection-error generic-function-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "An :ITEM keyword argument must be supplied ~
                            to the generic function ~a when operating on ~
                            the collection ~s."
		     (gfname condition) (collection condition)))))

(define-condition missing-key-value (collection-error generic-function-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "A :KEY or a :VALUE keyword argument (or both) ~
                            must be supplied to the generic function ~a ~
                            when operating on the collection ~s."
		     (gfname condition) (collection condition)))))


;; (define-condition base-error (error)
;;   ((fmtstr :initarg :fmtstr :reader fmtstr
;; 	   :documentation "A FORMAT control string describing the
;;            error that occurred.")
;;    (fmtargs :initarg :fmtargs :reader fmtargs
;; 	    :documentation "Other arguments referenced by FMTSTR."))
;;   (:documentation "Holds common formatting information for error conditions
;;                   in the Accretions system.")
;;   (:default-initargs :fmtstr nil :fmtargs nil)
;;   (:report (lambda (condition stream)
;; 	     (if (fmtstr condition)
;; 		 (apply #'format stream (fmtstr condition) (fmtargs condition))
;; 		 (princ "No further information was supplied." stream)))))

;; (define-condition collection-failure (base-error)
;;   ((collection :initarg :collection :reader collection
;; 	       :documentation "The collection to which the item was
;; 	       being added when the error occured."))
;;   (:default-initargs :collection nil)
;;   (:documentation "Tracks the collection that's present in more than
;;                   one of our error conditions."))

;; (define-condition add-item-failure (collection-failure)
;;   ((item :initarg :item :reader item
;; 	 :documentation "The item that was being added to the
;; 	 collection when the failure occurred."))
;;   (:default-initargs :item nil :collection nil)
;;   (:report (lambda (condition stream)
;; 	     (format stream
;; 		     "While attempting to add ~s to the Accretions ~
;;                      collection ~s, an error occurred.  "
;; 		     (item condition) (collection condition))
;; 	     (call-next-method)))
;;   (:documentation "Raised by an ADD method on a failure.  Even though
;;                   we don't print all slots via the report, we manage
;;                   them in a class instance so the developer can look
;;                   closer at just which collection had a problem."))

;; (define-condition add-pair-failure (collection-failure)
;;   ((key :initarg :key :reader key
;; 	:documentation "The key that was being added to the collection
;; 	when the failure occurred.")
;;    (value :initarg :value :reader value
;; 	:documentation "The value associated with the key being added
;; 	to the collection when the failure occurred."))
;;   (:default-initargs :key nil :value nil :collection nil)
;;   (:report (lambda (condition stream)
;; 	     (format stream
;; 		     "While attempting to add ~s to the Accretions ~
;;                      collection ~s, an error occurred.  "
;; 		     (key condition) (collection condition))
;; 	     (call-next-method)))
;;   (:documentation "Raised by an ADD method on a failure.  Even though
;;                   we don't print all slots via the report, we manage
;;                   them in a class instance so the developer can look
;;                   closer at just which collection had a problem."))
