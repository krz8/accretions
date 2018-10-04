;;;; tests the bag implementation
#-asdf3.1 (error "ACCRETIONS/TEST/BAG requires ASDF 3.1.2 or later")

(defpackage #:accretions/test/bag
  (:use #:cl #:fiveam)
  (:export #:run! #:bag))
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
  (with-bag b
    (is (not (null b)))
    (is (typep b 'bag:bag))))

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

(test map
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
	(bag:map b f)
	(is (= expected actual)))))
  (with-bag b
    (is-true (bag:emptyp b))
    (let* ((runs 0)
	   (f (lambda (x) (declare (ignore x))
		          (incf runs))))
      (bag:map b f)      
      (is (zerop runs)))))
