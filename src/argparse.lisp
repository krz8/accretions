(defvar *front-p* nil
  "Set when :FRONT is seen in an argument list.")
(defvar *back-p* nil
  "Set when :BACK is seen is an argument list.")

(defun arg-parse (fname arglist)
  "Invoke this using the WITH-ARG-PARSE macro.  Work through the
ARGLIST, which is typically a &REST parameter from a client-invoked
function.  This looks for the presence of certain keywords in
ARGLIST, tagging special variables accordingly.  FNAME is just a string that names the function whose arguments we're parsing."
  (do ((a arglist (cdr a)))
      ((null a) t)
    (case (car a)
      (:front (setf *front-p* t))
      (:back (setf *back-p* t))
      (otherwise (error 'accretions/src/conditions:unknown-keyword
			:fname fname :collection collection)))))

(defmacro with-arg-parse (fname arglist &body body)
  (let ((*front-p* nil)
	(*back-p* nil))
    (arg-parse arglist)
    ,@body))
