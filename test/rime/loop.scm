#!r6rs
(library (test rime loop)
  (export main)
  (import (rnrs (6))
          (rime unit-test)
          (rime loop))

  (define-test test:for-in-list-void
    (CHECK equal? (loop  :trace-parser :trace-codegen
                         :for i :in '(a b c)) (if #f 0)))
  (define-test test:for-in-list-do
    (CHECK equal?
           (loop :for i :in '(a b c)
                 :do (map display (list ">>>>>> i=" i "\n"))
                 :do (map display (list ">>>>>> (symbol->string i) ;; => " (symbol->string i) "\n")))
           (if #f 0)))

  (define-test test:for-in-list-collect
    (CHECK equal? (loop :for i :in '(a b c)
                        :collect i)
           '(a b c))
    (CHECK equal? (loop :for i :in '(1 2 3)
                        :collect i)
           '(1 2 3)))

  (define-test test:for-ij-from-0-to-3
    (CHECK equal? (loop :for i :from 0 :to 3
                        :for j :from 0 :to 4
                        :collect (list i j))
           '((0 0) (1 1) (2 2) (3 3))))

  (define-test test:for-i-on-123
    (lambda ()
      (CHECK equal? (loop :for i :on '(1 2 3)
                          :collect i)
             '((1 2 3) (2 3) (3)))))

  (define-test
    test:for-i-in-vector-123
    (CHECK equal? (loop :for i :in-vector (vector 1 2 3)
                        :collect i)
           '(1 2 3))
    (CHECK equal? (loop :for i :across (vector 1 2 3)
                        :collect i)
           '(1 2 3)))


  (define-test
    for-i-in-hash-one-two-three
    (lambda ()
      (define ht (make-eq-hashtable))
      (hashtable-set! ht 1 "one")
      (hashtable-set! ht 2 "two")
      (hashtable-set! ht 3 "three")
      (CHECK equal?
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (loop :for (k v) :in-hashtable ht
                              :collect (cons k v)))
             '((1 . "one") (2 . "two") (3 . "three")))
      (CHECK equal?
             (list-sort < (loop :for k :in-hashtable ht
                                :collect k ))
             '(1 2 3))))
  (define-test
    repeat-3
    (CHECK equal? (loop :repeat 3
                        :collect 103)
           '(103 103 103)))
  (define-test
    test-for-upfrom
    (CHECK equal? (loop :for i :upfrom 1
                        :for sym :in '(a b c)
                        :collect (cons i sym))
           '((1 . a) (2 . b) (3 . c)))
    (CHECK equal? (loop :for i :upfrom 1 :by 2
                        :for sym :in '(a b c)
                        :collect (cons i sym))
           '((1 . a) (3 . b) (5 . c))))

  (define-test
    test-if-odd?
    (CHECK equal? (loop :for i :upfrom 1 :to 10
                        :if (odd? i)
                        :collect i)
           '(1 3 5 7 9))
    (CHECK equal? (loop :for i :upfrom 1 :to 10
                        :when (odd? i)
                        :collect i)
           '(1 3 5 7 9))
    (CHECK equal? (loop :for i :upfrom 1 :to 10
                        :unless (odd? i)
                        :collect i)
           '(2 4 6 8 10))
    )

  (define-test
    test-collect-append-flat-map
    (CHECK equal? (loop :for i :in '((a b c) (d e f))
                        :append i)
           '(a b c d e f)))

  (define-test
    test-basic-with
    (CHECK equal? (loop :for i :upfrom 0 :to 2
                        :with i+ := (+ 1 i)
                        :with i++ := (+ 1 i+)
                        :collect i++)
           '(2 3 4))
    )

  (define-test
    test-basic-finally
    (CHECK equal? (loop :for i :upfrom 0 :to 10
                        :if (odd? i)
                        :with i+ := (+ 1 i)
                        :with i++ := (+ 1 i+)
                        :collect i++ :into acc123
                        :finally (map (lambda (x) (+ 100 x)) acc123))
           '(103 105 107 109 111))
    )

  (define-test
    test-break-2
    (CHECK equal? (loop :for i :upfrom 0 :to 10
                        :break i :if (= i 2))
           2))
  (define-test
    test-break
    (CHECK equal? (loop :for i :upfrom 0 :to 10
                        :with i+ := (fx+ 1 i)
                        :with i++ := (fx+ 1 i+)
                        :break
                        :break
                        :break)
           (if #f 0))

    (CHECK equal? (loop :for i :upfrom 0 :to 10
                        :with i+ := (+ 1 i)
                        :with i++ := (+ 1 i+)
                        :collect i++
                        :break)
           '(2))
    (CHECK equal? (loop :for i :upfrom 0 :to 10
                        :with i+ := (+ 1 i)
                        :with i++ := (+ 1 i+)
                        :collect i++
                        :break i++)
           2)

    (CHECK equal? (loop :for i :upfrom 0 :to 10
                        :with i+ := (+ 1 i)
                        :with i++ := (+ 1 i+)
                        :collect i++
                        :break :if (> i 2)
                        :finally (map (lambda (test-for-break) (+ 100 test-for-break)) :return-value))
           '(102 103 104 105))

    (CHECK equal? (loop :for i :upfrom 0 :to 10
                        :with i+ := (+ 1 i)
                        :with i++ := (+ 1 i+)
                        :collect i++
                        :break :if (>= i 0)
                        :finally (map (lambda (test-for-break) (+ 100 test-for-break)) :return-value))
           '(102))
    )



  (define-test
    test-count
    (CHECK equal? (loop :for i :upfrom 0 :to 10
                        :count)
           11)
    (CHECK equal? (loop :for i :upfrom 0 :to 10
                        :count
                        :finally (+ :return-value 100))
           111)
    )
  (define-test
    test-in-string
    (CHECK equal? (loop :for ch :in-string "HELLO"
                        :collect ch)
           (string->list "HELLO"))
    (CHECK equal? (loop :for ch :in-string "HELLO" :offset 1
                        :collect ch)
           (string->list "ELLO"))
    )

  (define-test
    test-initially
    (let ( ;; [init-var1 '(A B C)]
          [init-var2 (list 'A 'B 'C)])
      (CHECK equal? (loop :initially := init-var2
                          :for ch :in '(d e f)
                          :collect ch
                          :do (map display (list "RET=" :return-value "\n")))
             '(A B C d e f))
      (CHECK equal? init-var2 '(A B C d e f))
      ;; warning, this is undefined behavior I guess, because collect deconstructly append values.
      ;; (CHECK equal? (loop :initially := init-var1
      ;;                     :for ch :in '(d e f)
      ;;                     :collect ch
      ;;                     :do (printf "RET=~s~n" :return-value))
      ;;        '(A B C d e f))
      ;; (printf "init-var1=~s ~s~n" init-var1 (equal? init-var1 (list 'A 'B 'C 'd 'e 'f)))
      ;; (for-each (lambda (x y)
      ;;             (printf "x=~s y=~s (eq? x y)=~s~n" x y (eq? x y)))
      ;;           init-var1 '(A B C d e f))
      ;; (CHECK eq? (equal? init-var1 '(A B C d e f)) #f)
      ))
  (define-test
    test-initially-more-vars-exprs
    (let ()
      (CHECK equal? (loop :initially
                          := (list "something")
                          characters := '(d e f)
                          numbers := '(1 2 3)
                          :for ch :in characters
                          :for number :in numbers
                          :collect (cons number ch))
             '("something" (1 . d) (2 . e) (3 . f)))
      ))
  (define-test
    test-nested-loop
    (let ()
      (CHECK equal? (loop :for i :in '(0 1 2)
                          :for j :in '(A B C)
                          (:loop :for x :in '("one" "two" "three")
                                 :for y :in '("Apple" "Boy" "Cat")
                                 :if (not (= i 0))
                                 :collect (list i j x y)))
             '((1 B "one" "Apple")
               (1 B "two" "Boy")
               (1 B "three" "Cat")
               (2 C "one" "Apple")
               (2 C "two" "Boy")
               (2 C "three" "Cat")
               ))
      ))
  (define (main)
    (run-all-tests))
  )
