(uiop:define-package :accretions/src/misc
  (:use :common-lisp)
  (:export #:strcat #:dlambda))
(in-package :accretions/src/misc)

(defmacro strcat (&rest strings)
  `(concatenate 'string ,@strings))

(defmacro dlambda (&rest dargs)
  "A wrapper around LAMBDA that implements dispatch functionality on
  its first argument.  This is based on Doug Hoyte's discussion and
  implementation from his book \"Let Over Lambda,\" which is the best
  reference for how to use this macro.  The only difference is that
  the G!ARGS binding has been replaced with an explicit gensym bound
  to ARGS \(thus saving us from other macros that would be necessary
  to use this\)."
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
