(library (test test-pretty-print)
  (export test-pretty-print)
  (import (rnrs (6))
          (skeme loop)
          (skeme protobuf private pretty-print)
          (skeme protobuf private test)
          (skeme protobuf private display))
  (define test-pretty-print '())

  (define-test
    test-pretty-print-1
    (check equal? (pretty-print 1) "1\n")))
