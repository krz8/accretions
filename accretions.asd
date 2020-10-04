#-asdf3.1 (error "ACCRETIONS requires ASDF 3.1.2 or later")
(asdf:defsystem "accretions"
  :description "Accretions is a collection of performant data
structures and algorithms."
  :version "0.1.0"
  :license "MIT"
  :author "Bob Krzaczek"
  :mailto "RobertKrzaczek+cl@gmail.com"
  :homepage "http://github.com/krz8/accretions/"
  :long-description "Accretions provides a growing set of data
  structures (e.g., ternary search tries, red-black trees, sparse
  arrays and vectors) used in various applications that aren't present
  in the Common Lisp standard.  Some of these are provided by various
  CL implementations, certainly, but they cannot be relied upon to
  exist everywhere by portable software.  Accretions tries to address
  that situation, providing solid implementations of these data
  structures and their related algorithms."
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system") 
  :depends-on ("accretions/misc" "accretions/spv" "accretions/all")
  :in-order-to ((test-op (test-op "accretions/test/all"))))

(asdf:defsystem "accretions/test"
  :perform (test-op (o s)
	     (uiop:symbol-call "accretions/test/all" 'test-all))
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system") 
  :depends-on ("fiveam" "accretions/all" "accretions/test/all"))
