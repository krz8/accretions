;;;; tests the bag implementation
#-asdf3.1 (error "ACCRETIONS/BAG-TEST requires ASDF 3.1.2 or later")

(defpackage #:accretions/bag-test
  (:use #:cl #:fiveam)
  (:export #:run! #:bag))
(in-package #:accretions/bag-test)

(uiop:define-package #:bag		; glue
    (:use-reexport #:accretions/bag))

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
  (with-bag b
    (is (not (null b)))
    (is (typep b 'bag:bag))))

(test size
  "Tests bag size."
  (with-bag b
    (is (zerop (bag:size b)))
    (bag:add b 42)
    (is (= 1 (bag:size b)))
    (bag:add b t)
    (is (= 2 (bag:size b)))
    (bag:add b nil)
    (is (= 3 (bag:size b)))
    (bag:add b "foobar")
    (is (= 4 (bag:size b)))))

(test emptyp
  "Tests empty bag detection."
  (with-bag b
    (is (bag:emptyp b))
    (bag:add b #\a)
    (is (not (bag:emptyp b)))
    (bag:add b nil)
    (is (not (bag:emptyp b)))))

(test add
  "Tests adding items to a bag."
  (with-bag b
    (is-true (bag:emptyp b))
    (is-true (bag:add b nil))
    (is-false (bag:emptyp b))
    (is-true (bag:add b nil))
    (is-false (bag:emptyp b))
    (is (= 2 (bag:size b)))
    (is-true (bag:add b nil))
    (is (= 3 (bag:size b)))))

(defun add-a-bunch (bag n)
  "Add N randomly chosen doubles to the supplied BAG, returning T only
if all additions are non-NIL."
  (every #'identity
	 (loop :for i :from 1 :to n
	       :collecting (bag:add bag (random most-positive-double-float)))))

(test add-many
  "Tests adding +MANY+ items to a bag."
  (with-bag b
    (do* ((i +many+ (- i j))
	  (j +chunk+ (min +chunk+ i)))
	 ((<= i 0) t)
      (is-true (add-a-bunch b j)))
    (is (= +many+ (bag:size b)))))

