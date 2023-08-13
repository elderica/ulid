
(load "ulid.asd")
(load "ulid-tests.asd")

(ql:quickload "ulid-tests")

(in-package :ulid-tests)

(uiop:quit (if (run-all-tests) 0 1))
