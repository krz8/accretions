;;;; misc tests

;;; These aren't fancy, but just in case we ever go hacking in
;;; misc.lisp, these test cases ensure we don't break the assumptions
;;; of the rest of the system.

(eval-when (:compile-toplevel) (princ "compiling test/misc") (fresh-line))
(eval-when (:load-toplevel)    (princ "loading test/misc")   (fresh-line))
(eval-when (:execute)          (princ "executing test/misc") (fresh-line))

(defpackage :accretions/test/misc
  (:use :cl :fiveam :accretions :accretions/misc :accretions/test/suites))
(in-package :accretions/test/misc)

(in-suite accretions/test:misc)

#+nil
(defun try ()
  "Run all miscellaneous tests in the Accretions system.  This is a
  convenience function, nothing more."
  (run! 'accretions/test:misc))

(test until
  (let ((x 0) res)
    (is (null (until (> x 9)
		(push x res)
		(incf x))))
    (is (= 10 x))
    (is (equal '(9 8 7 6 5 4 3 2 1 0) res))))

(test mkstr
  (is (string= "ABC123DEF456" (mkstr 'abc 123 :def "456")))
  (is (string= "ABC" (mkstr 'a "" 'b "" 'c))))

(test symb
  (is (eq 'ABC123DEF456 (symb 'abc 123 :def "456")))
  (is (eq 'ABC123DEF456 (symb 'abc "" 123 "" :def "" "456"))))

(test readfmt
  (is (equal '(FOO 123 BAR) (readfmt "(~w 123 ~w)" 'foo 'bar))))
