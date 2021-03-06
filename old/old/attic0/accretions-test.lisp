;;;; tests for the accretions library
(defpackage #:accretions-test
  (:use #:cl #:5am)
  (:export #:all #:bag #:deque #:tst))

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

(test (bag-emptyp :depends-on make-bag)
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

(test (bag-add :depends-on make-bag)
  (with-bag (b)
    (let ((l (acr::items b)))
      (is (equal (car l) "foobar"))
      (is (null (cadr l)))
      (is (eq (caddr l) t))
      (is (= (cadddr l) 3))
      (is (eq (car (cddddr l)) 'a))
      (is (null (cdr (cddddr l)))))))

(test (bag-mapfun :depends-on bag-add)
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

(test (bag-containsp :depends-on bag-add)
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

(test (bag-add-oops :depends-on bag-add)
  (let ((b (acr:make-bag)))
    (signals acr:missing-item
      (acr:add b))
    (signals acr:missing-item
      (acr:containsp b))))

(def-suite deque :description "double ended queues" :in all)
(in-suite deque)

(test make-deque
  (let ((deq (acr:make-deque)))
    (is (not (null deq)))
    (is (typep deq 'acr:deque))
    (is (eql deq (acr::forw deq)))
    (is (eql deq (acr::back deq)))
    (is (eql #'equal (acr::testfn deq)))))

(test (make-deque-test :depends-on make-deque)
  (let ((deq (acr:make-deque :test #'eq)))
    (is (not (null deq)))
    (is (typep deq 'acr:deque))
    (is (eql (acr::testfn deq) #'eq))))

(test (deque-emptyp :depends-on make-deque)
  (let* ((deq (acr:make-deque))
	 (deq2 (make-instance 'acr::deque-node :forw deq :back deq)))
    (is-true (acr:emptyp deq))
    (setf (acr::forw deq) deq2
	  (acr::back deq) deq2)
    (is-false (acr:emptyp deq))))

(defmacro with-deque ((name) &body body)
  `(let ((,name (acr:make-deque)))
     (is-true (acr:add ,name :item 'a))
     (is-true (acr:add ,name :item 3))
     (is-true (acr:add ,name :item t))
     (is-true (acr:add ,name :item nil))
     (is-true (acr:add ,name :item "foobar"))
     ,@body))

(test (deque-add :depends-on make-deque)
  (with-deque (deq)
    (macrolet
	((f (x) `(acr::forw ,x))
	 (b (x) `(acr::back ,x)))
      (is (eq (b (f deq)) deq))
      (is (eq (f (b deq)) deq))
      (is (not (eq (f (f deq)) deq)))
      (is (not (eq (b (b deq)) deq)))
      (is (eq deq (f (f (f (f (f (f deq))))))))
      (is (eq deq (b (b (b (b (b (b deq))))))))
      (is (eq 'a (acr::item (f deq))))
      (is (= '3 (acr::item (f (f deq)))))
      (is (eq t (acr::item (f (f (f deq))))))
      (is (null (acr::item (f (f (f (f deq)))))))
      (is (equal "foobar" (acr::item (f (f (f (f (f deq))))))))
      (is (equal "foobar" (acr::item (b deq))))
      (is (null (acr::item (b (b deq)))))
      (is (eq t (acr::item (b (b (b deq))))))
      (is (= '3 (acr::item (b (b (b (b deq)))))))
      (is (eq 'a (acr::item (b (b (b (b (b deq)))))))))))

(test (deque-mapfun :depends-on deque-add)
  (let ((deq (acr:make-deque))
	(n 0))
    (acr:mapfun (lambda (x)
	      (declare (ignore x))
	      (incf n))
	    deq)
    (is (zerop n)))
  (with-deque (deq)
    (let (r)
      (acr:mapfun (lambda (x) (push x r)) deq)
      (is (= (length r) 5))
      (is (equalp r '(a 3 t nil "foobar"))))))

(test (deque-containsp :depends-on deque-add)
  (with-deque (deq)
    (is-true (acr:containsp deq :item "foobar"))
    (is-true (acr:containsp deq :item nil))
    (is-true (acr:containsp deq :item t))
    (is-true (acr:containsp deq :item 3))
    (is-true (acr:containsp deq :item 'a))
    (is-false (acr:containsp deq :item "FOOBAR"))
    (is-false (acr:containsp deq :item 2))
    (is-false (acr:containsp deq :item 'b)))
  (let ((deq (acr:make-deque :test #'equalp)))
    (is-true (acr:add deq :item "foobar"))
    (is-true (acr:containsp deq :item "FOOBAR"))
    (is-false (acr:containsp deq :item "")))
  (let ((deq (acr:make-deque :test #'eq)))
    (is-true (acr:add deq :item "foobar"))
    (is-false (acr:emptyp deq))
    (is-false (acr:containsp deq :item "FOOBAR"))
    (is-false (acr:containsp deq :item ""))))

(test (deque-add-oops :depends-on deque-add)
  (let ((deq (acr:make-deque)))
    (signals acr:missing-item
      (acr:add deq))
    (signals acr:missing-item
      (acr:containsp deq))))

;; (def-suite tst :description "ternary search trees" :in all)
;; (in-suite tst)

;; (test make-tst
;;   (let ((tst (acr:make-tst)))
;;     (is (not (null tst)))
;;     (is (typep tst 'acr:tst))
;;     (is (eql (acr::key-testfn tst) #'string=))
;;     (is (eql (acr::value-testfn tst) #'equal))
;;     (is (eql (acr::key-el-test= tst) #'char=))
;;     (is (eql (acr::key-el-test< tst) #'char<))
;;     (is (eql (acr::key-el-test> tst) #'char>))))

;; (test (make-tst-ignore-case :depends-on make-tst)
;;   (let ((tst (acr:make-tst :ignore-case t)))
;;     (is (not (null tst)))
;;     (is (typep tst 'acr:tst))
;;     (is (eql (acr::key-testfn tst) #'string-equal))
;;     (is (eql (acr::value-testfn tst) #'equalp))
;;     (is (eql (acr::key-el-test= tst) #'char-equal))
;;     (is (eql (acr::key-el-test< tst) #'char-lessp))
;;     (is (eql (acr::key-el-test> tst) #'char-greaterp))))

;; (test (make-tst-tests :depends-on make-tst)
;;   (let ((tst (acr:make-tst :key-test #'equal :value-test #'eql
;; 			   :key-el-test= #'eq :key-el-test< #'=
;; 			   :key-el-test> #'equalp)))
;;     (is (not (null tst)))
;;     (is (typep tst 'acr:tst))
;;     (is (eql (acr::key-testfn tst) #'equal))
;;     (is (eql (acr::value-testfn tst) #'eql))
;;     (is (eql (acr::key-el-test= tst) #'eq))
;;     (is (eql (acr::key-el-test< tst) #'=))
;;     (is (eql (acr::key-el-test> tst) #'equalp)))
;;   (let ((tst (acr:make-tst :ignore-case t :key-test #'equal :value-test #'eql
;; 			   :key-el-test= #'eq :key-el-test< #'=
;; 			   :key-el-test> #'equalp)))
;;     (is (not (null tst)))
;;     (is (typep tst 'acr:tst))
;;     (is (eql (acr::key-testfn tst) #'equal))
;;     (is (eql (acr::value-testfn tst) #'eql))
;;     (is (eql (acr::key-el-test= tst) #'eq))
;;     (is (eql (acr::key-el-test< tst) #'=))
;;     (is (eql (acr::key-el-test> tst) #'equalp))))

;; (test (tst-emptyp :depends-on make-tst)
;;   (let ((tst (acr:make-tst)))
;;     (is-true (acr:emptyp tst))
;;     (setf (acr::split tst) #\a
;; 	  (acr::termp tst) t)
;;     (is-false (acr:emptyp tst))))

;; (test (tst-add :depends-on tst-emptyp)
;;   (let ((tst (acr:make-tst)))
;;     (is-true (acr:add tst :key "foo" :value "bar"))
;;     (is (= 1 (acr:size tst)))))
