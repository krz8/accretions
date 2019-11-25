(defsystem "accretions-test"
  :description "accretions-test: sanity checking the accretions library"
  :version "0.1.0"
  :license "MIT"
  :depends-on ("accretions" "fiveam" "anaphora")

  :author "Bob Krzaczek"
  :mailto "RobertKrzaczek+cli@gmail.com"
  :homepage "http://github.com/krz8/accretions/"

  :components ((:file "accretions-test"))
  :perform (test-op (o s)
	     (uiop:symbol-call :fiveam '#:run!
                               (uiop:find-symbol* '#:all :accretions-test))))
