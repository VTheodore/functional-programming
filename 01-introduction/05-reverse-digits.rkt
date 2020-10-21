#lang racket
(require rackunit)
(require rackunit/text-ui)
; 1.6 - Съчинете процедура, която обръща цифрите на дадено число.
; Трябва да работи и за отрицателни числа.

(define (reverse-digits number)
  (define (helper number reversed)
    (if (= number 0)
        reversed
        (helper (quotient number 10) (+ (* reversed 10) (remainder number 10)))))
  (helper number 0)
)

(define tests
  (test-suite
  "Reverse digits tests"

    (test-case "Should reverse correctly"
      (check-equal? (reverse-digits 2134) 4312)
    )

    (test-case "Should work alright with digits"
      (let ((k (random 10)))
        (check-equal? (reverse-digits k) k))
    )

    (test-case "Should work with negative numbers"
      (check-equal? (reverse-digits -298245) -542892)
    )
  )
)

(run-tests tests)