
(load "ulid.asd")
(load "ulid-tests.asd")
(ql:quickload "ulid-tests")

#+sbcl (require 'sb-cover)
#+sbcl (declaim (optimize sb-cover:store-coverage-data))

(asdf:load-system "ulid" :force t)
(asdf:load-system "ulid-tests" :force t)

(in-package :ulid-tests)

(fiveam:run-all-tests)

#+sbcl (sb-cover:report "/tmp/report/")
#+sbcl (declaim (optimize (sb-cover:store-coverage-data 0)))
