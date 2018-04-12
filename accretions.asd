(defsystem "accretions"
  :description "a collection of fast performant data structures"
  :version "0.1.0"
  :license "MIT"
  :depends-on ("alexandria" "anaphora" "iterate")
  :in-order-to ((test-op (test-op "accretions-test")))

  :author "Bob Krzaczek"
  :mailto "RobertKrzaczek+cl@gmail.com"
  :homepage "http://github.com/krz8/accretions/"
  :long-description "Accretions provides a growing set of alternative
  data structures, such as ternary search tries, used in various
  applications that aren't present in the Common Lisp standard.  Some
  of these are, undoubtedly, provided by various CL implentations, but
  they are unmeasured in performance and cannot be relied upon to
  exist everywhere by portable software.  Accretions seeks to address
  that situation."

  :components ((:file "pkg")
	       (:file "misc" :depends-on ("pkg"))
	       (:file "generics" :depends-on ("pkg"))
	       (:file "counted" :depends-on ("misc"))
	       (:file "valued" :depends-on ("misc"))
	       (:file "bag" :depends-on ("counted"))
	       (:file "ternary" :depends-on ("counted" "valued"))))
