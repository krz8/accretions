(defvar *front-p* nil
  "Set when :FRONT is seen in an Accretions argument list.")
(defvar *back-p* nil
  "Set when :BACK is seen is an Accretions argument list.")

(defmacro with-keyopts ((fname arglist) &body body)
  "Used by exported Accretion functions. BODY forms are evaluated
  within a context that binds *FRONT-P* and *BACK-P* to true values
  whenever :FRONT or :BACK (respectively) appear within the list
  ARGLIST.  ARGLIST typically appears as a REST parameter in a
  function definition to capture \"optional keywords\" \(not keyword
  arguments\).  FNAME is a string naming the function in which this
  form appears, used only in messages associated with error
  conditions."
  (let ((a (gensym)))
    `(let (*front-p* *back-p*)
       (do ((,a ,arglist (cdr ,a)))
	   ((null ,a) t)
	 (case (car ,a)
	   (:front (setf *front-p* t))
	   (:back (setf *back-p* t))
	   (otherwise (error 'accretions/src/conditions:unknown-keyword :fname ,fname))))
       ,@body)))

