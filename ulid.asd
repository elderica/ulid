(in-package :asdf-user)

(defsystem "ulid"
  :author "elderica <1130138+elderica@users.noreply.github.com>"
  :version "0.0.1"
  :license "MIT"
  :description "Universally Unique Lexicographically Sortable Identifier"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")

  ;; Dependencies.
  :depends-on ()

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "ulid"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "ulid"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "ulid:main")
