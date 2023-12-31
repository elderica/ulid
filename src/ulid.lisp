(in-package :ulid)

(defparameter *crockford-alphabet* "0123456789ABCDEFGHJKMNPQRSTVWXYZ")
(defconstant +crockford-bitmask+ #x1F)
(defparameter *decode-table*
  (alexandria:alist-hash-table
   (loop for i from 0
	 for c across *crockford-alphabet*
	 collect (cons c i))))

(deftype u128 ()
  "represents unsigned 128-bit integer."
  '(unsigned-byte 128))

(deftype base32 ()
  "represents Crockford's Base32 string."
  '(string 26))

(defstruct ulid
  "holds unsigned 128-bit integer that represents unique lexicographically sortable identifier.
The first 48 bits of 128 bits are a UNIX timestamp in milliseconds for lexicographically sorting.
The remaining 80 bits are randomness that ensure the identifier is unique."
  (timestamp-randomness #x00000000000000000000000000000000
 :type u128))

(defun ulid-timestamp (u)
  "returns timestamp part of ulid."
  (ash (ulid-timestamp-randomness u) -80))

(defun ulid-randomness (u)
  "returns randomness part of ulid."
  (logand (ulid-timestamp-randomness u)
	  #xFFFFFFFFFFFFFFFFFFFF))

(define-condition ulid-out-of-range (error)
  ((datum :initarg :datum
	  :reader datum))
  (:report (lambda (condition stream)
	     (format stream "out of range: ~a" (datum condition)))))

(define-condition ulid-overflow (error)
  ((integer :initarg :integer
	    :reader overflow-integer))
  (:report (lambda (condition stream)
	     (format stream "overflow when incrementing: ~a"
		     (overflow-integer condition)))))

(defun ulid->u128 (u)
  "converts unsigned 128-bit integer to ulid."
  (declare (type ulid u))
  (ulid-timestamp-randomness u))

(defun u128->base32 (hw)
  "encodes unsigned 128-bit integer with Crockford's Base32."
  (declare (type u128 hw))
  (loop with hw = hw
	with b32 = (make-string 26)
	for i from 25 downto 0
	do (setf (aref b32 i)
		 (aref *crockford-alphabet*
		       (logand (ash hw (* -5 (- 25 i))) #x1F)))
	finally (return b32)))

(defun ulid->base32 (u)
  "creates Crockford's Base32 encoded string that represents the ulid."
  (declare (type ulid u))
  (u128->base32 (ulid-timestamp-randomness u)))

(defun base32->u128 (b32)
  "decodes Crockford's Base32 string."
  (declare (type base32 b32))
  (unless (find (aref b32 0) "01234567")
    (error (make-condition 'ulid-out-of-range :datum b32)))
  (loop with n = 0
	with ub32 = (string-upcase b32)
	for c across ub32
	do (setf n (logior (ash n 5)
			   (gethash c *decode-table* 0)))
	   finally (return n)))

(defun u128->ulid (hw)
  "converts ulid to unsigned 128-bit integer."
  (declare (type u128 hw))
  (make-ulid :timestamp-randomness hw))

(defun base32->ulid (b32)
  "construct ulid from Crockford's Base32 string."
  (declare (type base32 b32))
  (make-ulid :timestamp-randomness (base32->u128 b32)))

(defun timestamp-to-unix-msec (tm)
  (declare (type local-time:timestamp tm))
  (+ (* 1000 (local-time:timestamp-to-unix tm))
     (local-time:timestamp-millisecond tm)))

(defun bytes-to-integer (bytes)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (loop with int = 0
	for b across bytes
	do (setf int
		 (logior (ash int 8) b))
	finally (return int)))

(defun generate-randomness-from-cprng ()
  "Generate 80-bits randomness from CPRNG"
  (bytes-to-integer (cl+ssl:random-bytes 10)))

(defun ulid-increment (u)
  (let ((rd (ulid-randomness u)))
    (if (= rd #xFFFFFFFFFFFFFFFFFFFF)
	(error 'ulid-overflow :integer (ulid->u128 u))
	(u128->ulid (1+ (ulid->u128 u))))))

(defun generate-from-timestamp-with-source (tm random)
  (let ((tm (timestamp-to-unix-msec tm))
	(rd (funcall random)))
    (make-ulid :timestamp-randomness (logior (ash tm 80) rd))))

(defun generate-with-source (random)
  (generate-from-timestamp-with-source (local-time:now)
				       random))

(defun generate-from-timestamp (tm)
  "Generate a `ulid` matching the given `local-time:timestamp`"
  (generate-from-timestamp-with-source tm #'generate-randomness-from-cprng))

(defun generate-now ()
  "Generate ULID from current timestamp."
  (generate-from-timestamp (local-time:now)))

;; TODO: monotonic ULID factory
