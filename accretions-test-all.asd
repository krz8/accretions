#-asdf3.1 (error "ACCRETIONS requires ASDF 3.1")
(defsystem "accretions-test-all"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on ("accretions/src/all")
  )
