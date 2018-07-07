#-asdf3.1 (error "ACCRETIONS requires ASDF 3.1")
(defpackage :accretions/src/conditions
  (:use :common-lisp)
  (:export #:base-error #:missing-item #:missing-key #:missing-key-or-value
	   #:key-value #:key-not-a-sequence-error #:bad-key-length
	   #:not-unique))
(in-package :accretions/src/conditions)

(define-condition base-error (error)
  ((collection :initarg :collection :reader collection
	       :documentation "The collection being manipulated when
	       the error occured.")
   (gfname :initarg :gfname :reader gfname
	   :documentation "The name of the generic function in use
	   when the error occured."))
  (:default-initargs :collection nil :gfname nil)
  (:documentation "Errors that arise in Accretions generally include
                  a collection and a generic function in their
                  context."))

(define-condition missing-item (base-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "An :ITEM keyword argument must be supplied ~
                            to the generic function ~a when operating on ~
                            the collection ~s."
		     (gfname condition) (collection condition)))))

(define-condition missing-key (base-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "An :KEY keyword argument must be supplied ~
                            to the generic function ~a when operating on ~
                            the collection ~s."
		     (gfname condition) (collection condition)))))

(define-condition missing-key-or-value (base-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "A :KEY or a :VALUE keyword argument (or both) ~
                            must be supplied to the generic function ~a ~
                            when operating on the collection ~s."
		     (gfname condition) (collection condition)))))

(define-condition key-error (base-error)
  ((key :initarg :key :reader key
	:documentation "Names the KEY that was supplied."))
  (:default-initargs :key nil)
  (:documentation "Adds a KEY slot to BASE-ERROR, that's all."))

(define-condition key-not-a-sequence-error (key-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "The key ~s was supplied to the generic ~
                            function ~a, but the collection ~s ~
                            requires its keys to be sequences."
		     (key condition) (gfname condition)
		     (collection condition)))))

(define-condition bad-key-length (key-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "The key ~s supplied to the generic function ~
	                    ~a for collection ~s must have a length of ~
                            at least 1."
		     (key condition) (gfname condition)
		     (collection condition)))))

(define-condition not-unique (key-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "They key ~s, supplied to the generic function ~a ~
                            already exists in the collection ~a, but that key ~
                            is expected to be unique." 
		     (key condition) (gfname condition) 
		     (collection condition)))))

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
