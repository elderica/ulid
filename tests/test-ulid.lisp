(in-package :ulid-tests)

(def-suite integer-and-bytes)
(in-suite integer-and-bytes)

(test test-bytes-to-integer-1
  (is (equal
       (ulid::bytes-to-integer
	(make-array 2
		    :element-type '(unsigned-byte 8)
		    :initial-contents '(#xAB #xBA)))
       #xABBA)))

(test test-bytes-to-integer-2
  (is (equal
       (ulid::bytes-to-integer
	(make-array 4
		    :element-type '(unsigned-byte 8)
		    :initial-contents '(#xDE #xAD #xBE #xEF)))
       #xDEADBEEF)))

(test test-integer-to-bytes-1
  (is (equalp (ulid::integer-to-bytes #xDEADBEEF 4)
	      (vector #xDE #xAD #xBE #xEF))))

(test test-integer-to-bytes-2
  (is (equalp (ulid::integer-to-bytes #xDEADBEEF 8)
	      (vector #x00 #x00 #x00 #x00 #xDE #xAD #xBE #xEF))))

(def-suite encode-base32)
(in-suite encode-base32)

(test should-return-expected-encoded-result
  (is (equalp (ulid::encode-base32 1469918176385 10)
	      "01ARYZ6S41")))

(test should-change-length-property
  (is (equalp (ulid::encode-base32 1470264322240 12)
	      "0001AS99AA60")))

(test should-truncate-time-if-not-enough-length
  (is (equalp (ulid::encode-base32 1470118279201 8)
	      "AS4Y1E11")))


(def-suite encode-randomness)
(in-suite encode-randomness)

(test should-return-correct-length
  (is (eql (length (ulid::encode-randomness 12))
	   12)))

(def-suite ulid)
(in-suite ulid)

(test should-return-correct-length
  (is (eql (length (ulid:ulid))
	   26)))

(test should-return-expected-encoded-timestamp-component-result
  (is (string-equal (ulid:ulid 1469918176385)
		    "01ARYZ6S41"
		    :end1 10)))

(def-suite testmain
    :description "test suite 1")

(in-suite testmain)

(test test1
  (is (= (+ 1 1)
         2)))
