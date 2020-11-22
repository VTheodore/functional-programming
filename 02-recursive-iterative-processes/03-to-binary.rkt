#lang racket
(require rackunit)
(require rackunit/text-ui)
(require trace)

; Обръщаме число в двоична бройна система

(define (to-binary number)
  (define (helper num res weight)
    (if (= num 0)
        res
        (helper (quotient num 2) (+ res (* (remainder num 2) (expt 10 weight))) (+ weight 1))))
  (helper number 0 0)
)

(define tests
  (test-suite "to-binary tests"
    (check-equal? (to-binary 10) 1010)
    (check-equal? (to-binary 0) 0)
    (check-equal? (to-binary 8) 1000)
  )
)

(run-tests tests 'verbose)