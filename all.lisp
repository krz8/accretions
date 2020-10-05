;;;; the actual accretions package
#-asdf3.1 (error "ACCRETIONS/ALL requires ASDF 3.1.2 or later")

(eval-when (:compile-toplevel) (princ "compiling all") (fresh-line))
(eval-when (:load-toplevel)    (princ "loading all")   (fresh-line))
(eval-when (:execute)          (princ "executing all") (fresh-line))

(uiop:define-package :accretions
  (:use :common-lisp)
  (:use-reexport :accretions/spv))
(in-package :accretions)
