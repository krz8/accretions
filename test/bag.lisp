;;;; tests the bag implementation
#-asdf3.1 (error "ACCRETIONS/TEST/BAG requires ASDF 3.1.2 or later")

(defpackage #:accretions/test/bag
  (:use #:cl #:fiveam)
  (:export #:bag #:make #:copy #:size #:emptyp #:add #:add-many))
(in-package #:accretions/test/bag)

(uiop:define-package #:bag		; glue
    (:use-reexport #:accretions/src/bag))

(defparameter +many+ 1000000
  "How many long doubles to add to a bag when testing many additions.")
(defparameter +chunk+ 100000
   "How many long doubles to add to a bag at a time.")

(def-suite bag
    :description "All bag tests.")
(in-suite bag)

(defmacro with-bag (x &body body)
  `(let ((,x (bag:make)))
     ,@body))

(test make
  "Tests bag creation."
  (with-bag b1
  (is (not (null b1)))
  (is (typep b1 'bag:bag))
  (with-bag b2
    (is (not (null b2)))
    (is (typep b2 'bag:bag))
    (is (not (equal b1 b2))))))

(test size
  "Tests bag size."
  (with-bag b
    (is (zerop (bag:size b)))
    (is (= 1 (bag:size (bag:add b 42))))
    (is (= 2 (bag:size (bag:add b t))))
    (is (= 3 (bag:size (bag:add b nil))))
    (is (= 4 (bag:size (bag:add b "foobar"))))))

(test emptyp
  "Tests empty bag detection."
  (with-bag b
    (is (bag:emptyp b))
    (is (not (bag:emptyp (bag:add b #\a))))
    (is (not (bag:emptyp (bag:add b nil))))))

(test add
  "Tests adding items to a bag."
  (with-bag b
    (is (bag:emptyp b))
    (is (bag:add b nil))
    (is (not (bag:emptyp b)))
    (is (bag:add b nil))
    (is (not (bag:emptyp b)))
    (is (= 2 (bag:size b)))
    (is (bag:add b nil))
    (is (= 3 (bag:size b)))))

(test bagp
  "Tests BAGP predicate."
  (with-bag b
    (is (not (bag:bagp nil)))
    (is (not (bag:bagp t)))
    (is (not (bag:bagp 42)))
    (is (bag:bagp b))))

(defun bag-to-sorted-list (bag)
  "Return a sorted list of items whose elements are strings from BAG."
  (let (list)
    (bag:mapfun bag (lambda (x) (push x list)))
    (sort list #'string<)))

;; The copy test, to be thorough, uses special knowledge of the bag
;; implementation.  Not sure if I'll do this for other types, but right
;; now it keeps me honest.

(test copy
  "Tests shallow copy."
  (with-bag b1
    (dolist (x '("BC" "a" "" "def"))
      (bag:add b1 x))
    (is (equalp '("" "BC" "a" "def") (bag-to-sorted-list b1)))
    (is (= 4 (bag:size b1)))
    (let ((b2 (bag:copy b1)))
      (is (equalp b1 b2))
      (is (not (equal b1 b2)))
      (setf (cadr (bag:head b2)) "z")
      (is (equalp '("BC" "a" "def" "z") (bag-to-sorted-list b2)))
      (bag:add b2 "hello, world")
      (is (= 4 (bag:size b1)))
      (is (= 5 (bag:size b2)))
      (setf (caddr (bag:head b2)) "")
      (is (equal (bag:head b1) (cdr (bag:head b2)))))))

(defun add-a-bunch (bag n)
  "Add N randomly chosen doubles to the supplied BAG, returning T
  only if all additions are non-NIL.  Any ADD returning a false value
  causes an immediate NIL return from ADD-A-BUNCH."
  (do ((i n (1- i)))
      ((zerop i) t)
    (unless (bag:add bag (random most-positive-double-float))
      (return))))

(test add-many
  "Tests adding +MANY+ items to a bag."
  (with-bag b
    (do* ((i +many+ (- i j))
	  (j +chunk+ (min +chunk+ i)))
	 ((<= i 0) t)
      (is (add-a-bunch b j)))
    (is (= +many+ (bag:size b)))))

(test mapfun
  "Tests mapping a function across a bag."
  (with-bag b
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
  (with-bag b
    (is-true (bag:emptyp b))
    (let* ((runs 0)
	   (f (lambda (x) (declare (ignore x))
		          (incf runs))))
      (bag:mapfun b f)      
      (is (zerop runs)))))
