;;;; tests the bag implementation
#-asdf3.1 (error "ACCRETIONS/TEST/BAG requires ASDF 3.1.2 or later")

(defpackage :accretions/test/bag
  (:use #:cl #:fiveam)
  (:export #:try #:bag-tests #:make

	   #:size #:emptyp #:add
	   #:hasp #:count))
(in-package :accretions/test/bag)

(uiop:define-package #:bag		; glue
    (:use-reexport :accretions/bag))

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
  "Evaluate BODY forms with NAME bound to a new BAG created by
BAG:MAKE.  ARGS are passed to BAG:MAKE."
  `(let ((,name (bag:make ,@args)))
     ,@body))

(defmacro with-bag (name &body body)
  "Evaluate BODY forms with NAME bound to a new empty BAG."
  `(with-new-bag ,name ()
     ,@body))

(defmacro bag-test-is (expected &rest args)
  "Special purpose macro: creates a new BAG via BAG:MAKE, passing ARGS
along to MAKE.  Then, check that the test function in the underlying
hash table of the bag matches EXPECTED."
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
  ;; HASH-TABLE-TEST returns *symbols* that designate the test
  ;; function, no matter how the function was set when the hash table
  ;; was created.
  (bag-test-is 'eql)
  (bag-test-is 'eq :test 'eq)
  (bag-test-is 'eq :test #'eq)
  (bag-test-is 'eql :test 'eql)
  (bag-test-is 'eql :test #'eql)
  (bag-test-is 'equal :test 'equal)
  (bag-test-is 'equal :test #'equal)
  (bag-test-is 'equalp :test 'equalp)
  (bag-test-is 'equalp :test #'equalp))

(test bagp
  "Tests BAGP predicate."
  (with-bag b
    (is (not (bag:bagp nil)))
    (is (not (bag:bagp t)))
    (is (not (bag:bagp 42)))
    (is (bag:bagp b))))

(test emptyp
  "Tests empty bag detection."
  (with-bag b
    (is (bag:emptyp b))
    (is (not (bag:emptyp (bag:add b #\a))))
    (is (not (bag:emptyp (bag:add b nil))))))

(defmacro with-abc-bag (name &body body)
  "Bind NAME to a new BAG containing various strings."
  (let ((x (gensym)))
    `(with-bag ,name
       (dolist (,x '("abc" "def" "ghi" "jkl"))
	 (bag:add ,name ,x))
       (dolist (,x '("def" "ghi" "jkl"))
	 (bag:add ,name ,x))
       (dotimes (,x 4)
	 (bag:add ,name "abc"))
       ,@body)))

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
  (with-bag b
    (is (zerop (bag:count b)))
    (is (zerop (bag:count b "foo"))))
  (with-abc-bag b
    (bag:add b "")
    (is (= 12 (bag:count b)))
    (is (= 5 (bag:count b "abc")))
    (is (= 2 (bag:count b "def")))
    (is (= 2 (bag:count b "ghi")))
    (is (= 2 (bag:count b "jkl")))
    (is (= 0 (bag:count b "mno")))
    (is (= 1 (bag:count (bag:add b "mno") "mno")))
    (is (= 13 (bag:count b)))
    (is (= 0 (bag:count b "pqrs")))
    (is (= 1 (bag:count b "")))))

(test add
  "Tests adding items to a bag."
  (with-bag b
    (is (bag:emptyp b))
    (is (bag:add b nil))
    (is (not (bag:emptyp b)))
    (is (bag:add b nil))
    (is (not (bag:emptyp b)))
    (is (= 2 (bag:count b)))
    (is (bag:add b nil))
    (is (= 3 (bag:count b)))))

(test size
  "Tests bag size."
  (with-bag b
    (is (zerop (bag:count b)))
    (is (= 1 (bag:count (bag:add b 42))))
    (is (= 2 (bag:count (bag:add b t))))
    (is (= 3 (bag:count (bag:add b nil))))
    (is (= 4 (bag:count (bag:add b "foobar"))))))

(test copy
  "Tests BAG copies, ensuring that the returned bag is mostly a deep
copy; i.e., that its item counts are mutable and unique from the
source bag."
  (with-abc-bag b1
    (is (= 4 (hash-table-count (table b1))))
    (is (= 11 (bag:count b1)))
    (let ((b2 (bag:copy b1)))
      (is (= 4 (hash-table-count (table b2))))
      (is (= 11 (bag:count b2)))
      (bag:add b2 "jkl")
      (bag:add b2 "mno")
      (is (= 4 (hash-table-count (table b1))))
      (is (= 11 (bag:count b1)))
      (is (= 5 (hash-table-count (table b2))))
      (is (= 13 (bag:count b2)))
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
