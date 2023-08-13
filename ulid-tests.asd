(in-package :asdf-user)
(defsystem "ulid-tests"
  :description "Test suite for the ulid system"
  :author "elderica <1130138+elderica@users.noreply.github.com>"
  :version "0.0.1"
  :depends-on (:ulid
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-ulid"))))

  ;; The following would not return the right exit code on error, but still 0.
  :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
