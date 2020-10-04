;;;; miscellaneous hacks

(eval-when (:compile-toplevel) (print "compiling misc"))
(eval-when (:load-toplevel)    (print "loading misc"))
(eval-when (:execute)          (print "executing misc"))

(defpackage :accretions/misc
  (:use #:cl #:cl-environments)
  (:export #:until #:mkstr #:symb #:readfmt #:get-opt))
(in-package :accretions/misc)

(defmacro until (expr &body body)
  "Create a DO loop that evaluates BODY so long as EXPR tests false.
  Returns NIL once EXPR passes.  EXPR is tested before BODY is
  evaluated on every loop.  UNTIL is the inverse of a traditional
  WHILE loop."
  (let ((e (gensym)))
    `(do ((,e ,expr ,expr))
         (,e)
       ,@body)))

(defun mkstr (&rest args)
  "Common function that turns all arguments into strings, concatenates
  them, and returns the new string."
  (with-output-to-string (s)
    (let ((*standard-output* s))
      (map nil #'princ args))))

(defun symb (&rest args)
  "Common function that returns a new symbol that is the concatenation
  of all arguments. (symb 'foo \"bar\" 12) → FOOBAR12"
  (values (intern (apply #'mkstr args))))

(defun readfmt (fmt &rest args)
  "READ a string created by calling the formatter on the supplied
  arguments."
  (read-from-string (apply #'format nil fmt args)))

(defun get-opt (which)
  "Returns the current optimization declared in the environment for
  WHAT, which may be one of SPEED SPACE SAFETY or DEBUG."
  (or (cadr (assoc which (cl-environments:declaration-information
			  'optimize)))
      1))
