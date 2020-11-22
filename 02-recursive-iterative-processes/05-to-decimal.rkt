#lang racket
(require rackunit)
(require rackunit/text-ui)


; Обръщаме число от двоична в десетична бройна система
(define (to-decimal number)
  (define (helper num res weight)
    (if (= num 0)
        res
        (helper (quotient num 10) (+ res (* (remainder num 10) (expt 2 weight))) (+ weight 1))))
    (helper number 0 0)
)

(define tests
  (test-suite "to-decimal tests"
    (check-equal? (to-decimal 11001) 25)
    (check-equal? (to-decimal 1100011) 99)
  )
)

(run-tests tests 'verbose)