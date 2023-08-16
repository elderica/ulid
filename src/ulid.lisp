(in-package :ulid)

(defparameter *crockford-alphabet* "0123456789ABCDEFGHJKMNPQRSTVWXYZ")
(defparameter *crockford-bitmask* #x1F)
(defparameter *encoded-timestamp-length* 10)
(defparameter *encoded-randomness-length* 16)

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
		generate-randomness))
(defun generate-randomness (len)
  "Generate vector of 5-bits random number."
  (loop with randomness = (cl+ssl:random-bytes len)
	for i upto (1- len)
	do (setf (aref randomness i)
		 (logand (aref randomness i)
			 *crockford-bitmask*))
	finally (return randomness)))

(declaim (ftype (function ((integer 0 *))
			  (simple-array character (*)))
		encode-randomness))
(defun encode-randomness (len)
  "Return randomness encoded with Crockford's Base32."
  (loop with randomness = (generate-randomness len)
	with encoded-randomness = (make-string len)
	for i upto (1- len)
	do (setf (aref encoded-randomness i)
		 (aref *crockford-alphabet*
		       (aref randomness i)))
	finally (return encoded-randomness)))

(defun get-current-unix-msec ()
  "Get the current time in milliseconds."
  (let ((tm (local-time:now)))
    (+ (* 1000 (local-time:timestamp-to-unix tm))
       (local-time:timestamp-millisecond tm))))

(declaim (ftype (function ((integer 0 *) (integer 0 *))
			  (simple-string *))
		encode-base32))
(defun encode-base32 (int len)
  "Encode integer to string with Crockford's Base32."
  (loop with enct = (make-string len)
	for i from (1- len) downto 0
	do (setf (aref enct i)
		 (aref *crockford-alphabet*
		       (logand (ash int (* -5 (- len 1 i)))
				    *crockford-bitmask*)))
	finally (return enct)))


(declaim (ftype (function (&optional (integer 0 *))
			  (simple-string 26))
		ulid))
(defun ulid (&optional (unix-msec (get-current-unix-msec)))
  "Generate ULID from seed time or current time if no seed is given."
  (let ((enct (encode-base32 unix-msec
			     *encoded-timestamp-length*))
	(encr (encode-randomness *encoded-randomness-length*)))
    (concatenate 'string enct encr)))
