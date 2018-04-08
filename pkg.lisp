(defpackage #:accretions
  (:use #:cl)
  (:nicknames #:acr)
  (:export #:emptyp #:add #:add* #:contains #:mapfun #:contains
	   #:make-iterator #:with-iterator
	   #:size
	   #:bag #:make-bag #:counted-bag #:make-counted-bag))
