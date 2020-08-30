#-asdf3.1 (error "ACCRETIONS requires ASDF 3.1.2 or later")
(asdf:defsystem :accretions
  :description "Accretions is a collection of performant data
structures and algorithms."
  :version "0.1.0"
  :license "MIT"
  :author "Bob Krzaczek"
  :mailto "RobertKrzaczek+cl@gmail.com"
  :homepage "http://github.com/krz8/accretions/"
  :long-description "Accretions provides a growing set of data
structures \(e.g., ternary search tries, red-black trees, sparse
arrays and vectors\) used in various applications that aren't present
in the Common Lisp standard.  Some of these are provided by various CL
implementations, certainly, but they cannot be relied upon to exist
everywhere by portable software.  Accretions seeks to address that
situation, providing solid implementations of these data structures
and their related algorithms."

  ;; This might seem out of place; it's not.  :accretions/all mentions
  ;; the packages that will be exported, in one way or another, from
  ;; the Accretions package.  However, Accretions itself is really
  ;; just a set of small packages providing implementations and one
  ;; relatively simple CLOS implementation sitting on top. It's
  ;; important, however, that the symbols in those standalone
  ;; packages, like spv, are NOT exported from the Accretions package
  ;; itself.  These strict packaging rules ensure that end users may
  ;; choose to use the low-level functionality directly, or they may
  ;; use the high-level CLOS functionality, but not get stuck in
  ;; shadowed symbol hell.
  ;;
  ;; Therefore, Accretions itself is built up via all.lisp.  That
  ;; package, in turn, expects that others (e.g., spv.lisp) is already
  ;; available in the Lisp environment.  Thus, we list spv.lisp here
  ;; as a dependency, and leave all.lisp to assume bag and friends are
  ;; already in place when it assembles the main package.
  ;;
  ;; Plus, there's a little bit of :import-from over in all.lisp
  ;; that judiciously brings certain low-level symbols into the
  ;; high-level Accretions package.  But that, in fact, doesn't affect
  ;; the dependencies that ASDF is following.  It might seem like it
  ;; should, but a closer read of ASDF will show that it's a red
  ;; herring; don't get fooled!  :depends-on in a defsystem is
  ;; interpreted as a dependency; :import-from in a defpackage is not.
  
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system) 
  :depends-on (:accretions/spv :accretions/all)

  :in-order-to ((test-op (test-op :accretions/test/all))))

(asdf:defsystem :accretions/test
  :perform (test-op (o s)
	     (uiop:symbol-call :accretions/tests/all 'test-all))
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system) 
  :depends-on (:fiveam :accretions/all :accretions/test/all))

;; (asdf:defsystem :accretions/bag
;;   :description "Part of the Accretions system, a simple un-ordered
;;   collection of values."
;;   :version "0.1.0"
;;   :license "MIT"
;;   :author "Bob Krzaczek"
;;   :mailto "RobertKrzaczek+cl@gmail.com"
;;   :homepage "http://github.com/krz8/accretions/"
;;   :class :package-inferred-system
;;   :defsystem-depends-on (:asdf-package-system)
;;   :depends-on (:accretions/bag)
;;   :in-order-to ((test-op (test-op :accretions/test/bag))))
