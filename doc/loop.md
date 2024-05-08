Introduction
============


`(rime loop)` library mimic the common lisp loop facility with many
modifications, while the bacis idea is similar, also borrow some ideas
from Python list comprehension.

# Basic Iteration

## Arithmetic Iteration

```scheme
:for <var>
    [{:from :upfrom :downfrom} <expr1>}]
    [{:to :upto :downto :below :above} <expr2>}]
    [:by <expr3>]
```

for example

```scheme
(loop :for i :from 0 :to 2 :collect i) ; => (0 1 2)
```

## List Iteration

Iterate on each element.

```scheme
:for <var> {:in :on} <list-expr>
```

```scheme
(loop :for i :in '(1 2 3) :collect i) ; => (1 2 3)
```

Iterate on each sub-list

```scheme
(loop :for i :on '(1 2 3) :collect i) ; => ((1 2 3) (2 3) (3))
```

## Vector Iteration

```scheme
:for <var> :in-vector <vector-expr> {:offset <offset-expr>}
```

```scheme
(loop :for i :in-vector '#(1 2 3) :collect i) ; => (1 2 3)
```

`:offset` keyword is used to start from a different position other than 0.

## String Iteration


```scheme
:for <var> :in-string <string-expr> {:offset <offset-expr>}
```


```scheme
(loop :for i :in-string "HELLO" :collect i) ; => (#\H #\E #\L #\L #\O)
```

`:offset` keyword is used to start from a different position other than 0.

```scheme
(loop :for i :in-string "HELLO" :offset 2 :collect i) ; => (#\L #\L #\O)
```

## Hashtable Iteration

```scheme
:for (<var-key> <var-value>) :in-hashtable <hashtable-expr>
```

```scheme
(define ht (make-eq-hashtable))
(hashtable-set! ht 1 "one")
(hashtable-set! ht 2 "two")
(hashtable-set! ht 3 "three")
(sort (lambda (x y) (< (car x) (car y)))
      (loop :for (k v) :in-hashtable ht
            :collect (cons k v)))
;; => ((1 . "one") (2 . "two") (3 . "three"))
```

## `:if` `:when` `:unless` for fitering.

```scheme
{:if :when :unless} <cond>
```

Similiar to Python list comprehension, `:if` is used to fiter out
elements which does not match a certain condition. `:when` is an alias
of `:if`, and `:unless cond` is as same as `:if (not cond)`

```scheme
(loop :for i :from 0 :to 8
      :if (odd? i)
      :collect i) ; => (1 3 5 7)

(loop :for i :from 0 :to 8
      :unless (even? i)
      :collect i) ; => (1 3 5 7)
```

## Parallel Iteration

There are can be more than one `:for` clauses and if any of the
clauses is ended, the who loop is ended also.

```scheme
(loop :for i :from 0 :to 8
      :for c :in '(a b c)
      :collect (cons i c)) ; => ((0 . a) (1 . b) (2 . c))
```

so that above example can also be written as below.

```scheme
(loop :for i :from 0
      :for c :in '(a b c)
      :collect (cons i c)) ; => ((0 . a) (1 . b) (2 . c))
```


## Accumulate

```scheme
{:collect :append} <expr> [ :into <var> ]
```

As above examples have shown, a loop expression return a value, by default it is `(void)`.

```scheme
(loop :for i :from 0 :to 1) ;; => (void)
```

A `:collect` clause is used to for collect an expression into a list
and return the list as the return value of the whole loop expression.

`flat-map` is a common pattern in functional programming so that
`:append` keywords is used for this purpose, it quite similar to
`:collect`, except that it consumes an expression and assume the
expression is evaluated into a list and collect each elements into a
list.

```scheme
(loop :for index :from 0
      :for ch :in '(a b c)
      :append (list index ch)) ; => (0 a 1 b 2 c)
```

NOTE: `:collect` clause initialize the return value of the whole loop
expression with `()`, so that an empty iteration results in `()`
returned, for example,

```scheme
(loop :for index :in '()
      :collect index) ; => (), not #void
```

it implies that `:initially := '()`, see also `:initially clauses`


`:append <expr> :into <var>` and `:collect <expr> :into <var>`
accumulate elements into a new variable other than `:return-value`.

## Count

```
:count [:into <var>]
```

`count` clause set the `:return-value` to the number of iteration.

```scheme
(loop :for ch :in-string "Hello World"
      :if (char-upper-case? ch)
      :count) ; => 2
```

Similiar to `:append` and `:collect`, `:count <expr> :into <var>` set
the `<var>` instead of `:return-value`

## `:do` clause

```
:do <expr>
```

`:do` clause is used to evaluate an expression with side effects.

```scheme
(loop :for index :from 0
      :for ch :in '(a b c)
      :append (list index ch)
      :do (display (list index ch))) ; => (0 a 1 b 2 c)
```

## `:with` clause

```
:with <var> := <expr>
```

`:with` clause is used to create an intermediate variable not only
improve readability but als to avoid evaluate a common expression more
than twice.


```scheme
(loop :for index :from 0
      :for ch :in '(a b c)
      :with pair := (list index ch)
      :append pair
      :do (display (list index ch))) ; => (0 a 1 b 2 c)
```

`:with` clause can not only create a new variable but also change the value, and used by other clauses


```scheme
(loop :for index :from 0
      :for ch :in-string "this is HELLO."
      :with is_uppercase := (char-upper-case? ch)
      :if is_uppercase
      :with 1000_plus_pos := (+ 500 index)
      :with 1000_plus_pos := (+ 500 1000_plus_pos)
      :collect 1000_plus_pos) ; => (1008 1009 1010 1011 1012)

```

## `:initially` clause

```
:initially ([<var>] := <expr>) ...
```

`<var>` is assigned to `<expr>` before iteration started, and all
`<var>` are visiable in `:finally` clauses. There can be more than one
`:initially` clauses.

`<var>` is optional, by default it is `:return-value`.

There can be more than one variable bindings.

## `:finally` clause

```
:finally <expr>
```

`:finally` expression is evaluated after the loop is ended, and the
value of the expression is the return value of the whole loop
expression.

```scheme
(loop :for i :from 0 :to 2
      :collect i
      :finally (map (lambda (x) (+ 100 x)) :return-value)) ; => (100 101 102)
```

`:return-value` can be used to access the current return value of the
whole expression anywhere inside a loop expression.

There are can be more than one `:finally` clauses, each of them are evaluated in order.


```scheme
(loop :for i :from 0 :to 2
      :collect i
      :finally (map (lambda (x) (+ 100 x)) :return-value)
      :finally (map (lambda (y) (+ 1000 y)) :return-value)) ; => (1100 1101 1102)
```

## `:break` clause

```
:break [<expr>] [{:if :when :unless} <cond>]
```

`:break` expression is used to terminate the loop expression earlier.

```scheme
(loop :for i :from 0 :to 2
      :break
      :collect i)
;; => ()
```

`:break` can return a value as the whole loop expression. `:break`
without a return value does not change the current return value.

```scheme
(loop :for i :from 0 :to 2
      :break 100
      :collect i) ; => 100
```

`:break expr :if cond` is more common than a bear `:break`.

```scheme
(loop :for i :from 0 :to 200
      :break :if (> i 2)
      :collect i) ; => (0 1 2)
```

NOTE: `:if` is part of `:break` clause, it is not a standalone `:if` clause.

## Nested Loop, `:loop` clause

```
(:loop <all other valid loop clauses>)
```

As mentioned in above, multiple `:for` clauses mean parallele
iteration, if any of them is terminated, the whole loop is
terminated. `:loop` is used to support nested loop.

```scheme
(loop :for i :in '(0 1 2)
      (:loop :for j :in '(a b c)
             :collect (cons i j)))
;; =>
;; ((0 . a)
;;  (0 . b)
;;  (0 . c)
;;  (1 . a)
;;  (1 . b)
;;  (1 . c)
;;  (2 . a)
;;  (2 . b)
;;  (2 . c))
```

It is not as same as embeding another loop expression, for example

```scheme
(loop :for i :in '(0 1 2)
      :append (loop :for j :in '(a b c)
                       :collect (cons i j)))
;; =>
;; (((0 . a) (0 . b) (0 . c))
;;  ((1 . a) (1 . b) (1 . c))
;;  ((2 . a) (2 . b) (2 . c)))
```

To get same result, we need `:append` instead `:collect` in the outer loop expression.

```scheme
(loop :for i :in '(0 1 2)
      :append (loop :for j :in '(a b c)
                    :collect (cons i j)))
```

However, embeding another loop expression is slower, because a list
object is created for the inner loop expression. On the other hand,
`:loop` clauses share all variables created by `:initially` clause,
`with` clauses and `:return-value`.

all `:initially` clauses inside `:loop` are shared the same
enviornment bindings of the top level loop exrpession.

While `:finally` clauses of a `:loop` clauses are evaluated at the end
of its own iteration.

more examples,

```scheme
(loop :for i :in '(0 1 2)
      :collect i
      (:loop :for j :from i :to 2
             :collect (cons i j)))
;; =>
;; (0 (0 . 0) (0 . 1) (0 . 2)
;;  1 (1 . 1) (1 . 2)
;;  2 (2 . 2))
```



# Some Practical Examples

## `map`

```scheme
(define map
  (lambda (f lst)
     (loop :for i :in lst :collect (f i))))

(map odd? '(1 2 3)) => (#t #f #f)
```

## `filter`

```scheme
(define filter
  (lambda (f lst)
     (loop :for i :in lst :if (f i) :collect i)))

(filter odd? '(1 2 3)) => (1 3)
```

## `string-starts`

```scheme
(define string-start
    (lambda (prefix from str)
       (loop :for c1 :in-string prefix
             :for c2 :in-string str :offset from
             :break :unless (char=? c1 c2)
             :count
             :finally (fx=? (string-length prefix) :return-value))))

(string-start "loop-" 5 "this-loop-parser") ; => #t
(string-start "loop-" 0 "this-loop-parser") ; => #f
(string-start "loop-" 0 "x") ; => #f
```

## `intersection`

```scheme
(define intersection
    (lambda (list-a list-b)
       (loop :for c1 :in list-a
             (:loop :for c2 :in list-b
                    :if (eq? c1 c2)
                    :collect c2))))
(intersection '(a b c) '(b c d)) ;; => b c
```


## `get-column-line-number`

```scheme
(define (get-column-line-number file-name position)
  (call-with-port (open-file-input-port file-name
                                        (file-options)
                                        (buffer-mode block)
                                        (native-transcoder))
    (lambda (port)
      (loop :initially line-number := 1
            :initially column-number := 1
            :for pos :upfrom 0
            :with ch := (get-char port)
            :break #f :if (eof-object? ch)
            :count :into column-number
            :break :unless (fx<? pos position)
            :if (char=? ch #\newline)
            :count :into line-number
            :with column-number := 0
            :finally (cons line-number column-number)
            ))))
```

Given a file name `file-name` and character position `position`,
`get-column-line-number` returns a pair which contains line number and
column.
