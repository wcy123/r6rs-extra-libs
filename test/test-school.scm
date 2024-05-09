(library (test test-school)
  (export test-school)
  (import (rnrs (6))
          (skeme protobuf)
          (google protobuf test school)
          (skeme protobuf private display)
          (skeme protobuf private test))
  (define test-school (list google-protobuf-test-school))
  (define-test create-school
    ;; load the library
    (let ([school (make-message '(google protobuf test School))])
      (display-objects
       "test/test-school.scm:13:8: "
       " school=" (school ':debug-string)
       "\n"
       ))))
