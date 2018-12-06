;;;; tests ternary search trees
#-asdf3.1 (error "ACCRETIONS/TEST/TST requires ASDF 3.1.2 or later")

(defpackage :accretions/test/tst
  (:use #:cl #:fiveam)
  (:export #:tst #:make #:size #:emptyp #:tstp #:add)
  #+nil (:export #:bag #:make #:copy #:size #:emptyp #:add #:add-many))
(in-package :accretions/test/tst)

(uiop:define-package #:tst		; glue
    (:use-reexport #:accretions/src/tst))

;; (defparameter +many+ 1000000
;;   "How many long doubles to add to a bag when testing many additions.")
;; (defparameter +chunk+ 100000
;;    "How many long doubles to add to a bag at a time.")

(def-suite tst
    :description "All ternary search tree tests.")
(in-suite tst)

(defmacro with-tst (x &body body)
  `(let ((,x (tst:make)))
     ,@body))

(test make
  "Tests TST creation."
  (with-tst t1
  (is (not (null t1)))
  (is (typep t1 'tst:tst))
  (with-tst t2
    (is (not (null t2)))
    (is (typep t2 'tst:tst))
    (is (not (equal t1 t2))))))

(test size
  "Tests TST size."
  (with-tst tt
    (is (zerop (tst:size tt)))
    (is (= 1 (tst:size (tst:add tt "one" 42))))
    (is (= 2 (tst:size (tst:add tt "two" t))))
    (is (= 3 (tst:size (tst:add tt "three" nil))))
    (is (= 4 (tst:size (tst:add tt "four" "foobar"))))))

(test emptyp
  "Tests empty TST detection."
  (with-tst t1
    (is (tst:emptyp t1))
    (is (not (tst:emptyp (tst:add t1 "a" #\a))))
    (is (not (tst:emptyp (tst:add t1 "b" nil)))))
  (with-tst t2
    (is (tst:emptyp t2))
    (is (not (tst:emptyp (tst:add t2 nil nil))))))

(test add
  "Tests adding items to a TST."
  (with-tst tt
    (is (tst:emptyp tt))
    (is (tst:add tt nil nil))
    (is (not (tst:emptyp tt)))
    (is (= 1 (tst:size tt)))
    (is (tst:add tt nil nil))		;overwrites, doesn't actually "add"
    (is (= 1 (tst:size tt)))
    (is (tst:add tt '(#\a #\b) nil))
    (is (= 2 (tst:size tt)))
    (is (tst:add tt "abc" nil))
    (is (= 3 (tst:size tt)))))

(test tstp
  "Tests TSTP predicate."
  (with-tst tt
    (is (not (tst:tstp nil)))
    (is (not (tst:tstp t)))
    (is (not (tst:tstp 42)))
    (is (tst:tstp tt))))

#|
(defun tst-to-sorted-list (tst)
  "Return a sorted list of items whose elements are strings from TST."
  (let (list)
    (tst:mapfun tst (lambda (x) (push x list)))
    (sort list #'string<)))

;; The copy test, to be thorough, uses special knowledge of the tst
;; implementation.  Not sure if I'll do this for other types, but right
;; now it keeps me honest.

(test copy
  "Tests shallow copy."
  (with-tst b1
    (dolist (x '("BC" "a" "" "def"))
      (tst:add b1 x))
    (is (equalp '("" "BC" "a" "def") (tst-to-sorted-list b1)))
    (is (= 4 (tst:size b1)))
    (let ((b2 (tst:copy b1)))
      (is (equalp b1 b2))
      (is (not (equal b1 b2)))
      (setf (cadr (tst:head b2)) "z")
      (is (equalp '("BC" "a" "def" "z") (tst-to-sorted-list b2)))
      (tst:add b2 "hello, world")
      (is (= 4 (tst:size b1)))
      (is (= 5 (tst:size b2)))
      (setf (caddr (tst:head b2)) "")
      (is (equal (tst:head b1) (cdr (tst:head b2)))))))

(defun add-a-bunch (tst n)
  "Add N randomly chosen doubles to the supplied TST, returning T
  only if all additions are non-NIL.  Any ADD returning a false value
  causes an immediate NIL return from ADD-A-BUNCH."
  (do ((i n (1- i)))
      ((zerop i) t)
    (unless (tst:add tst (random most-positive-double-float))
      (return))))

(test add-many
  "Tests adding +MANY+ items to a tst."
  (with-tst tt
    (do* ((i +many+ (- i j))
	  (j +chunk+ (min +chunk+ i)))
	 ((<= i 0) t)
      (is (add-a-bunch tt j)))
    (is (= +many+ (tst:size tt)))))

(test mapfun
  "Tests mapping a function across a tst."
  (with-tst tt
    (let ((expected 0))
      (do ((i +chunk+ (1- i)))
	  ((zerop i) t)
	(let ((x (random 999)))
	  (tst:add tt x)
	  (incf expected x)))
      (let* ((actual 0)
	     (f (lambda (x) (incf actual x))))
	(tst:mapfun tt f)
	(is (= expected actual)))))
  (with-tst tt
    (is-true (tst:emptyp tt))
    (let* ((runs 0)
	   (f (lambda (x) (declare (ignore x))
		          (incf runs))))
      (tst:mapfun tt f)      
      (is (zerop runs)))))
|#
