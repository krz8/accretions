;;;; tests for the accretions library
(defpackage #:accretions-test
  (:use #:cl #:5am)
  (:export #:all #:bag #:tst))

(in-package #:accretions-test)
(def-suite all :description "all accretions tests")

(def-suite bags :description "bags" :in all)
(in-suite bags)

(test make-bag
  (let ((b (acr:make-bag)))
    (is (not (null b)))
    (is (typep b 'acr:bag))
    (is (eql (acr::testfn b) #'equal))))

(test (make-bag-test :depends-on make-bag)
  (let ((b (acr:make-bag :test #'eq)))
    (is (not (null b)))
    (is (typep b 'acr:bag))
    (is (eql (acr::testfn b) #'eq))))

(test (emptyp :depends-on make-bag)
  (let ((b (acr:make-bag)))
    (is-true (acr:emptyp b))
    (push 123 (acr::items b))		; haven't tested add yet
    (is-false (acr:emptyp b))))

(defmacro with-bag ((name) &body body)
  `(let ((,name (acr:make-bag)))
     (is-true (acr:add ,name :item 'a))
     (is-true (acr:add ,name :item 3))
     (is-true (acr:add ,name :item t))
     (is-true (acr:add ,name :item nil))
     (is-true (acr:add ,name :item "foobar"))
     ,@body))

(test (add :depends-on make-bag)
  (with-bag (b)
    (let ((l (acr::items b)))
      (is (equal (car l) "foobar"))
      (is (null (cadr l)))
      (is (eq (caddr l) t))
      (is (= (cadddr l) 3))
      (is (eq (car (cddddr l)) 'a))
      (is (null (cdr (cddddr l)))))))

(test (mapfun :depends-on add)
  (let ((b (acr:make-bag))
	(n 0))
    (acr:mapfun (lambda (x)
	      (declare (ignore x))
	      (incf n))
	    b)
    (is (zerop n)))
  (with-bag (b)
    (let (r)
      (acr:mapfun (lambda (x) (push x r)) b)
      (is (= (length r) 5))
      (is (equalp r '(a 3 t nil "foobar"))))))

(test (containsp :depends-on add)
  (with-bag (b)
    (is-true (acr:containsp b :item "foobar"))
    (is-true (acr:containsp b :item nil))
    (is-true (acr:containsp b :item t))
    (is-true (acr:containsp b :item 3))
    (is-true (acr:containsp b :item 'a))
    (is-false (acr:containsp b :item "FOOBAR"))
    (is-false (acr:containsp b :item 2))
    (is-false (acr:containsp b :item 'b)))
  (let ((b (acr:make-bag :test #'equalp)))
    (is-true (acr:add b :item "foobar"))
    (is-true (acr:containsp b :item "FOOBAR"))
    (is-false (acr:containsp b :item "")))
  (let ((b (acr:make-bag :test #'eq)))
    (is-true (acr:add b :item "foobar"))
    (is-false (acr:emptyp b))
    (is-false (acr:containsp b :item "FOOBAR"))
    (is-false (acr:containsp b :item ""))))

(test (add-oops :depends-on add)
  (let ((b (acr:make-bag)))
    (signals acr:missing-item
      (acr:add b))
    (signals acr:missing-item
      (acr:containsp b))))

(def-suite tst :description "ternary search trees" :in all)
(in-suite tst)
