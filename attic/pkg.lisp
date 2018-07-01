(defpackage #:accretions
  (:use #:cl)
  (:nicknames #:acr)
  (:export #:emptyp #:add #:containsp #:mapfun #:size
	   #:collection-error #:collection #:generic-function-error #:gfname
	   #:missing-item #:missing-key-value
	   #:bag #:make-bag
	   #:deque #:make-deque
	   #:tst #:make-tst))
