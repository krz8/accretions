;;;; tests the bag implementation
#-asdf3.1 (error "ACCRETIONS/TEST/LISTBAG requires ASDF 3.1.2 or later")

(defpackage :accretions/test/listbag
  (:use #:cl #:fiveam)
  (:export #:listbag #:make #:copy #:size #:emptyp #:add #:add-many))
(in-package :accretions/test/listbag)

(uiop:define-package #:bag		; glue
    (:use-reexport :accretions/src/listbag))

(defparameter +many+ 1000000
  "How many long doubles to add to a listbag when testing many additions.")
(defparameter +chunk+ 100000
   "How many long doubles to add to a listbag at a time.")

(def-suite listbag
    :description "All listbag tests.")
(in-suite listbag)

(defmacro with-listbag (x &body body)
  `(let ((,x (bag:make)))
     ,@body))

(test make
  "Tests listbag creation."
  (with-listbag b1
    (is (not (null b1)))
    (is (typep b1 'bag:listbag))
    (with-listbag b2
      (is (not (null b2)))
      (is (typep b2 'bag:listbag))
      (is (not (equal b1 b2))))))

(test size
  "Tests listbag size."
  (with-listbag b
    (is (zerop (bag:size b)))
    (is (= 1 (bag:size (bag:add b 42))))
    (is (= 2 (bag:size (bag:add b t))))
    (is (= 3 (bag:size (bag:add b nil))))
    (is (= 4 (bag:size (bag:add b "foobar"))))))

(test emptyp
  "Tests empty listbag detection."
  (with-listbag b
    (is (bag:emptyp b))
    (is (not (bag:emptyp (bag:add b #\a))))
    (is (not (bag:emptyp (bag:add b nil))))))

(test add
  "Tests adding items to a listbag."
  (with-listbag b
    (is (bag:emptyp b))
    (is (bag:add b nil))
    (is (not (bag:emptyp b)))
    (is (bag:add b nil))
    (is (not (bag:emptyp b)))
    (is (= 2 (bag:size b)))
    (is (bag:add b nil))
    (is (= 3 (bag:size b)))))

(test listbagp
  "Tests LISTBAGP predicate."
  (with-listbag b
    (is (not (bag:listbagp nil)))
    (is (not (bag:listbagp t)))
    (is (not (bag:listbagp 42)))
    (is (bag:listbagp b))))

(defun listbag-to-sorted-list (listbag)
  "Return a sorted list of items whose elements are strings from LISTBAG."
  (let (list)
    (bag:mapfun listbag (lambda (x) (push x list)))
    (sort list #'string<)))

;; The copy test, to be thorough, uses special knowledge of the listbag
;; implementation.  Not sure if I'll do this for other types, but right
;; now it keeps me honest.

(test copy
  "Tests shallow copy."
  (with-listbag b1
    (dolist (x '("BC" "a" "" "def"))
      (bag:add b1 x))
    (is (equalp '("" "BC" "a" "def") (listbag-to-sorted-list b1)))
    (is (= 4 (bag:size b1)))
    (let ((b2 (bag:copy b1)))
      (is (equalp b1 b2))
      (is (not (equal b1 b2)))
      (setf (cadr (bag:head b2)) "z")
      (is (equalp '("BC" "a" "def" "z") (listbag-to-sorted-list b2)))
      (bag:add b2 "hello, world")
      (is (= 4 (bag:size b1)))
      (is (= 5 (bag:size b2)))
      (setf (caddr (bag:head b2)) "")
      (is (equal (bag:head b1) (cdr (bag:head b2)))))))

(defun add-a-bunch (listbag n)
  "Add N randomly chosen doubles to the supplied LISTBAG, returning T
  only if all additions are non-NIL.  Any ADD returning a false value
  causes an immediate NIL return from ADD-A-BUNCH."
  (do ((i n (1- i)))
      ((zerop i) t)
    (unless (bag:add listbag (random most-positive-double-float))
      (return))))

(test add-many
  "Tests adding +MANY+ items to a listbag."
  (with-listbag b
    (do* ((i +many+ (- i j))
	  (j +chunk+ (min +chunk+ i)))
	 ((<= i 0) t)
      (is (add-a-bunch b j)))
    (is (= +many+ (bag:size b)))))

(test mapfun
  "Tests mapping a function across a listbag."
  (with-listbag b
    (let ((expected 0))
      (do ((i +chunk+ (1- i)))
	  ((zerop i) t)
	(let ((x (random 999)))
	  (bag:add b x)
	  (incf expected x)))
      (let* ((actual 0)
	     (f (lambda (x) (incf actual x))))
	(bag:mapfun b f)
	(is (= expected actual)))))
  (with-listbag b
    (is-true (bag:emptyp b))
    (let* ((runs 0)
	   (f (lambda (x) (declare (ignore x))
		          (incf runs))))
      (bag:mapfun b f)      
      (is (zerop runs)))))
