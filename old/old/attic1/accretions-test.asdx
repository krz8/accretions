#-asdf3.1 (error "ACCRETIONS requires ASDF 3.1")
(defsystem :accretions-test
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:accretions/test/all)
  :perform (test-op (o c) (symbol-call :accretions/test/all atests:all)))
