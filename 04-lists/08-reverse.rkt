#lang racket
(require rackunit)
(require rackunit/text-ui)

; Търсим функция, която обръща даден списък
(define (reverse xs)
  (if (null? xs)
      '()
      (append (reverse (cdr xs)) (list (car xs))))
)

; И нейн итеративен вариант
(define (reverse-iter xs)
  (define (helper xs res)
    (if (null? xs)
        res
        (helper (cdr xs) (cons (car xs) res))))
  (helper xs '())
)

(define tests
  (test-suite "Reverse tests"
      (check-equal? (reverse-iter '(1 2 3)) (reverse '(1 2 3)))
      (check-equal? (reverse '()) '())
      (check-equal? (reverse '(1)) '(1))
      (check-equal? (reverse '(1 5)) '(5 1))
  )
)

(run-tests tests 'verbose)