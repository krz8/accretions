#-asdf3.1 (error "ACCRETIONS requires ASDF 3.1.2")
(asdf:defsystem :accretions
  :description "Accretions is a collection of performant data
structures and algorithms."
  :version "0.1.0"
  :license "MIT"
  :author "Bob Krzaczek"
  :mailto "RobertKrzaczek+cl@gmail.com"
  :homepage "http://github.com/krz8/accretions/"
  :long-description "Accretions provides a growing set of data
structures, such as ternary search tries, used in various
applications that aren't present in the Common Lisp standard.  Some
of these are provided by various CL implementations, certainly, but
they cannot be relied upon to exist everywhere by portable software
\(and performance is often not known\).  Accretions seeks to address
that situation, providing solid implementations of these data
structures and their related algorithms."

  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system) 
  :depends-on (:accretions/src/bag
	       :accretions/src/all)
  :in-order-to ((test-op (test-op :accretions/test/all))))
