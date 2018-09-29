(defpackage :accretions/src/misc
  (:use :common-lisp)
  (:export #:strcat #:awhen #:dlambda))
(in-package :accretions/src/misc)

(defmacro strcat (&rest strings)
  `(concatenate 'string ,@strings))

(defmacro awhen (test &body body)
  "Anaphoric WHEN: Evaluates the TEST form once.  If true \(not NIL\),
  BODY forms are then evaluated with the result of TEST bound to the
  symbol IT."
  `(let ((it ,test))
     (when it
       ,@body)))

(defmacro dlambda (&rest dargs)
  "Bundles together multiple lambda forms into a single dispatching
  closure.  Dispatch is handled at runtime through the first argument
  \(typically defined via keywords\) supplied to the returned closure,
  and support for a \"default\" lambda is also supported.  Argument
  lists for each subform are processed such that those matching a
  specific dispatch have that first dispatching argument removed,
  while the default form removes no arguments.

  This is based on Doug Hoyte's discussion and implementation from his
  book \"Let Over Lambda,\" which is the best reference for how to use
  this macro \(and, let's face it, is just one of the best books in
  general on Lisp\).  The only difference here is that the G!ARGS
  binding has been replaced with an explicit gensym bound to ARGS
  \(thus saving us from other macros that would be necessary to use
  Doug's definition\).

  A far from complete example:

  \(let \(\(cnt 0\)\)
    \(dlambda
      \(:reset \(\)
        \(setf cnt 0\)\)
      \(:mult \(&optional \(n 2\)\)
        \(setf cnt \(* n cnt\)\)\)
      \(t \(&optional \(n 1\)\)        ; default
        \(incf cnt n\)\)\)\)

  Supports invocation arguments lists such as:

  \(\) => 1
  \(3\) => 4
  \(:mult 5\) => 20
  \(:reset\) => 0
  \(\) => 1"
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
