(in-package :ulid)

;; Define your project functionality here..
(defparameter *crockford-alphabet* "0123456789ABCDEFGHJKMNPQRSTVWXYZ")
(defparameter *crockford-bitmask* #x1F)
(defparameter *encoded-timestamp-length* 10)
(defparameter *encoded-random-length* 16)

(declaim (ftype (function ((vector (unsigned-byte 8) *))
			  (integer 0 *))
		bytes-to-integer))
(defun bytes-to-integer (bytes)
  (loop with total = 0
	for byte across bytes
	do (setf total (+ byte (ash total 8)))
	finally (return total)))

(declaim (ftype (function ((integer 0 *) (integer 0 *))
			  (vector (unsigned-byte 8) *))
		integer-to-bytes))
(defun integer-to-bytes (int len)
  (loop with vec = (make-array len :element-type '(unsigned-byte 8))
	for i from 0 upto (1- len)
	do (setf (aref vec i)
		 (logand (ash int (* -8 (- len 1 i)))
			 #xFF))
	   finally (return vec)))

(declaim (ftype (function ((integer 0 *))
			  (simple-array (unsigned-byte 8) (*)))
		random-bytes))
(defun random-bytes (len)
  (loop with random-bytes = (cl+ssl:random-bytes len)
	for i upto (1- len)
	do (setf (aref random-bytes i)
		 (logand (aref random-bytes i)
			 *crockford-bitmask*))
	finally (return random-bytes)))

(defun encode-random (len)
  (loop with random-bytes = (random-bytes len)
	with encoded-random = (make-string len)
	for i upto (1- len)
	do (setf (aref encoded-random i)
		 (aref *crockford-alphabet*
		       (aref random-bytes i)))
	     finally (return encoded-random)))

(defun get-current-unix-msec ()
  (let ((tm (local-time:now)))
    (+ (* 1000 (local-time:timestamp-to-unix tm))
       (local-time:timestamp-millisecond tm))))


(defun encode-time (now len)
  (loop with enct = (make-string len)
	for i from (1- len) downto 0
	do (setf (aref enct i)
		 (aref *crockford-alphabet*
		       (logand now *crockford-bitmask*)))
	   (setf now (ash now -5))
	finally (return enct)))


(defun ulid ()
  (let ((enct (encode-time (get-current-unix-msec)
			   *encoded-timestamp-length*))
	(encr (encode-random *encoded-random-length*)))
    (concatenate 'string enct encr)))


(defun greet (&optional (name "elderica"))
  (format t "Hello ~a from ~a!~&" name "ulid"))

(defun help ()
  (format t "~&Usage:

  ulid [name]~&"))

(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (greet  (or (first argv)
              "dear lisp user")))

(defun main ()
  "Entry point for the executable.
  Reads command line arguments."
  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (%main (uiop:command-line-arguments)))
