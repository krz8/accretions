;;;; miscellaneous support functionality
(in-package #:accretions)

;; The dlambda macro is due to Doug Hoyte.  The only difference
;; between this and his own implementation is that the g!args binding
;; has been replaced with an explicit gensym bound to args.

(defmacro dlambda (&rest dargs)
  "A wrapper around LAMBDA that implements dispatch functionality on
  its first argument."
  (let ((args (gensym)))
    `(lambda (&rest ,args)
       (case (car ,args)
	 ,@(mapcar
	    (lambda (d)
	      `(,(if (eq t (car d))
		     t
		     (list (car d)))
		 (apply (lambda ,@(cdr d))
			,(if (eq t (car d))
			     args
			     `(cdr ,args)))))
	    dargs)))))

(defmacro strcat (&rest strings)
  `(concatenate 'string ,@strings))
