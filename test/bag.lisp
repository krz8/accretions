;;;; tests the bag implementation
#-asdf3.1 (error "ACCRETIONS/TEST/BAG requires ASDF 3.1.2 or later")

(defpackage :accretions/test/bag
  (:use #:cl #:fiveam)
  (:export #:try #:bag-tests #:make #:size #:emptyp #:add
	   #:hasp #:count))
(in-package :accretions/test/bag)

(uiop:define-package #:bag		; glue
    (:use-reexport :accretions/bag))

(defparameter +many+ 1000000
  "How many long doubles to add to a bag when testing many additions.")
(defparameter +chunk+ 100000
   "How many long doubles to add to a bag at a time.")

(def-suite bag-tests)
(in-suite bag-tests)

(defun try (&optional (test-or-suite :bag-tests))
  "Run a test or test suite.  With no arguments, the full BAG-TESTS
suite is run; otherwise, the argument should be a keyword that names a
specific test case or suite within ACCRETIONS/TEST/BAG.

Examples:
* (ACCRETIONS/TEST/BAG:TRY)
* (ACCRETIONS/TEST/BAG:TRY :MAKE)"
  (fiveam:run! (uiop:find-symbol* test-or-suite :accretions/test/bag)))

(defmacro with-new-bag (name (&rest args) &body body)
  `(let ((,name (bag:make ,@args)))
     ,@body))

(defmacro with-bag (name &body body)
  `(with-new-bag ,name ()
     ,@body))

(defmacro bag-test-is (expected &rest args)
  `(with-new-bag b ,args
     (is (eq ,expected (testfn b)))))

(defun table (bag)
  "Returns the hash table used in the BAG."
  (accretions/bag::%bag-hash bag))

(defun testfn (bag)
  "Returns the function used to test equality in the bag's underlying
hash table."
  (hash-table-test (accretions/bag::%bag-hash bag)))

(test make
  "Tests bag creation."
  (with-bag b1
    (is (not (null b1)))
    (is (typep b1 'bag:bag))
    (with-bag b2
      (is (not (null b2)))
      (is (typep b2 'bag:bag))
      (is (not (equal b1 b2)))
      (is (equalp b1 b2))))
  ;; HASH-TABLE-TEST returns symbols that designate the test function,
  ;; no matter how the function was set when the hash table was
  ;; created
  (bag-test-is 'eql)
  (bag-test-is 'eq :test 'eq)
  (bag-test-is 'eq :test #'eq)
  (bag-test-is 'eql :test 'eql)
  (bag-test-is 'eql :test #'eql)
  (bag-test-is 'equal :test 'equal)
  (bag-test-is 'equal :test #'equal)
  (bag-test-is 'equalp :test 'equalp)
  (bag-test-is 'equalp :test #'equalp))

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
  (with-new-bag b ()
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

(defmacro with-abc-bag (name &body body)
  (let ((x (gensym)))
    `(with-bag ,name
       (dolist (,x '("abc" "def" "ghi" "jkl"))
	 (bag:add ,name ,x))
       (dolist (,x '("def" "ghi" "jkl"))
	 (bag:add ,name ,x))
       (dotimes (,x 4)
	 (bag:add ,name "abc"))
       ,@body)))

(test copy
  "Tests BAG copies, ensuring that the returned bag is mostly a deep
copy; i.e., that its item counts are mutable and unique from the
source bag."
  (with-bag b1
    (dolist (x '("abc" "def" "ghi" "jkl"))
      (bag:add b1 x))
    (dolist (x '("def" "ghi" "jkl"))
      (bag:add b1 x))
    (dotimes (x 4)
      (bag:add b1 "abc"))
    (is (= 4 (hash-table-count (table b1))))
    (is (= 11 (bag:size b1)))
    (let ((b2 (bag:copy b1)))
      (is (= 4 (hash-table-count (table b2))))
      (is (= 11 (bag:size b2)))
      (bag:add b2 "jkl")
      (bag:add b2 "mno")
      (is (= 4 (hash-table-count (table b1))))
      (is (= 11 (bag:size b1)))
      (is (= 5 (hash-table-count (table b2))))
      (is (= 13 (bag:size b2)))
      (multiple-value-bind (v s) (gethash "jkl" (table b1))
      	(is (equalp '(2) v))
      	(is-true s))
      (multiple-value-bind (v s) (gethash "jkl" (table b2))
      	(is (equalp '(3) v))
      	(is-true s))
      (multiple-value-bind (v s) (gethash "mno" (table b1))
      	(is (null v))
      	(is-false s))
      (multiple-value-bind (v s) (gethash "mno" (table b2))
      	(is (equalp '(1) v))
      	(is-true s)))))

(test hasp
  "Tests HASP for detecting an item's presence in a bag."
  (with-abc-bag b
    (is-true (bag:hasp b "abc"))
    (is-false (bag:hasp b "Abc"))
    (is-true (bag:hasp b "def"))
    (is-true (bag:hasp b "ghi"))
    (is-false (bag:hasp b "GHI"))
    (is-true (bag:hasp (bag:add b "GHI") "GHI"))
    (is-true (bag:hasp b "jkl"))
    (is-false (bag:hasp b #\a))
    (is-false (bag:hasp b t))
    (is-false (bag:hasp b nil))))

(test count
  "Tests HASP for detecting an item's presence in a bag."
  (with-abc-bag b
    (bag:add b "")
    (is (= 12 (bag:size b)))
    (is (= 5 (bag:count b "abc")))
    (is (= 2 (bag:count b "def")))
    (is (= 2 (bag:count b "ghi")))
    (is (= 2 (bag:count b "jkl")))
    (is (= 0 (bag:count b "mno")))
    (is (= 1 (bag:count (bag:add b "mno") "mno")))
    (is (= 13 (bag:size b)))
    (is (= 0 (bag:count b "pqrs")))
    (is (= 1 (bag:count b "")))))

#|
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
|#
