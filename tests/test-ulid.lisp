(in-package :ulid-tests)

(def-suite* encode-decode-base32)

(test should-return-expected-encoded-result
  (is (string-equal
       "01ARYZ6S41"
       (ulid::u128->base32 1469918176385)
       :start2 16)))

(def-suite* ulid)

(test should-return-correct-length
  (is (= 26
	 (length (ulid:ulid->base32 (ulid:generate-now))))))

(def-suite* u128-ulid)

(test reversible-conversion
  (is (= 1469918176385
	 (ulid->u128
	  (u128->ulid 1469918176385)))))

(def-suite* base32-ulid)

(test should-return-expected-base32-encoded-result
  (is (string-equal "21850M2GA1850M2GA1850M2GA1"
		    (ulid->base32
		     (u128->ulid #x41414141414141414141414141414141)))))

(test should-return-decoded-integer
  (is (= #x41414141414141414141414141414141)
      (base32->u128 "21850M2GA1850M2GA1850M2GA1")))

(test should-return-decoded-ulid
  (is (equalp (u128->ulid #x41414141414141414141414141414141)
	      (base32->ulid "21850M2GA1850M2GA1850M2GA1"))))

(test should-throw-error-when-out-of-range-value
  (signals ulid-out-of-range
    (ulid:base32->u128 "80000000000000000000000000")))

(def-suite* increment-ulid)

(test increment-ulid-1
  (let* ((u (base32->ulid "01BX5ZZKBKAZZZZZZZZZZZZZZZ"))
	 (v (ulid-increment u)))
    (is (string-equal
	 "01BX5ZZKBKB000000000000000"
	 (ulid->base32 v)))))

(test increment-ulid-2
  (let* ((u (base32->ulid "01BX5ZZKBKZZZZZZZZZZZZZZZX"))
	 (v (ulid-increment u)))
    (is (string-equal
	 "01BX5ZZKBKZZZZZZZZZZZZZZZY"
	 (ulid->base32 v)))))

(test signals-error-when-max-randomness
  (let ((u (base32->ulid "01BX5ZZKBKZZZZZZZZZZZZZZZZ")))
    (signals ulid-overflow
      (ulid-increment u))))
