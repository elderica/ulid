(in-package :ulid-tests)

(def-suite* encode-decode-base32)

(test should-return-expected-encoded-result
  (is (string-equal (ulid::u128->base32 1469918176385)
		    "01ARYZ6S41"
		    :start1 16)))

(def-suite* ulid)

(test should-return-correct-length
  (is (eql (length (ulid:ulid->base32 (ulid:generate-now)))
	   26)))

(def-suite* u128-ulid)

(test reversible-conversion
  (is (= (ulid->u128
	  (u128->ulid 1469918176385))
	 1469918176385)))

(def-suite* base32-ulid)

(test should-return-expected-base32-encoded-result
  (is (string-equal (ulid->base32
		     (u128->ulid #x41414141414141414141414141414141))
		    "21850M2GA1850M2GA1850M2GA1")))

(test should-return-decoded-integer
  (is (= (base32->u128 "21850M2GA1850M2GA1850M2GA1")
	 #x41414141414141414141414141414141)))

(test should-return-decoded-ulid
  (is (equalp (base32->ulid "21850M2GA1850M2GA1850M2GA1")
	      (u128->ulid #x41414141414141414141414141414141))))

(test should-throw-error-when-out-of-range-value
  (signals ulid-out-of-range
    (ulid:base32->u128 "80000000000000000000000000")))
