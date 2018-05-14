(defpackage #:accretions-conds
  (:use #:cl)
  (:export #:missing-item #:missing-key #:missing-key-or-value
	   #:key-not-a-sequence-error #:bad-key-length #:non-unique)
  (:documentation "ACCRETIONS-CONDS exists simply to hide a few
  symbols from the rest of the ACCRETIONS package.  We don't want to
  mix the accessors defined in the conditions to be mixed in with the
  other methods for the same generic function provided for working
  with collections.  Strictly speaking, it's not a problem, but if a
  user is browsing available methods, they cound see a method, e.g.,
  KEY that applies to something that isn't a collection.  This other
  package is just an artifice to prevent that from ever happening; the
  only symbols exported from ACCRETIONS-CONDS should be just the
  conditions themselves.

  Executive summary?  Don't use ACCRETIONS-CONDS for your work, it
  exists only to paper over some inconvenient overlaps in the methods
  defined for collections and for conditions.  Just stick with
  ACCRETIONS."))

(defpackage #:accretions
  (:use #:cl #:accretions-conds)
  (:nicknames #:acr)
  (:export #:emptyp #:add #:containsp #:mapfun #:size
	   #:collection-error #:collection #:generic-function-error #:gfname
	   #:missing-item #:missing-key-value
	   #:bag #:make-bag
	   #:deque #:make-deque
	   #:tst #:make-tst))
