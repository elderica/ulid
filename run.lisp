(load "ulid.asd")
(ql:quickload "ulid")

(in-package :ulid)
(handler-case
    (main)
  (error (c)
    (format *error-output* "~&An error occured: ~a~&" c)
    (uiop:quit 1)))
