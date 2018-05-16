#-asdf3.1 (error "ACCRETIONS requires ASDF 3.1")
(defsystem "accretions"
  :description "a collection of fast performant data structures"
  :version "0.1.0"
  :license "MIT"
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

  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :in-order-to ((test-op (load-op "accretions/test/all")))
  :perform (test-op (o c) (symbol-call :accretions/test/all :test-suite))
  :depends-on ("accretions/src/all")

  ;; :components ((:file "pkg")
  ;; 	       (:file "misc" :depends-on ("pkg"))
  ;; 	       (:file "generics" :depends-on ("pkg"))
  ;; 	       (:file "conditions" :depends-on ("generics"))
  ;; 	       (:file "collections" :depends-on ("conditions"))
  ;; 	       (:file "counted" :depends-on ("conditions"))
  ;; 	       #+nil (:file "bag" :depends-on ("counted"))
  ;; 	       #+nil (:file "deque" :depends-on ("counted"))
  ;; 	       #+nil (:file "ternary" :depends-on ("counted" "valued")))

  )
