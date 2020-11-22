#lang racket
(require rackunit)
(require rackunit/text-ui)

; Търсим процедура, която проверява дали дадено число завършва на дадено друго.

(define (ends-with? n t)
  (define (helper n t first?)
    (if (< n t)
        #f
        (if (= (remainder t 10) 0)
            (if first?
             (= (remainder n 10) 0)
             #t)
            (if (= (remainder n 10) (remainder t 10))
                (helper (quotient n 10) (quotient t 10) #f)
                #f))))
  (helper n t #t)
)

(define tests
  (test-suite "ends-with? tests"
    (check-true (ends-with? 8317 17))
    (check-true (ends-with? 82 82))
    (check-false (ends-with? 8213 31))
    (check-true (ends-with? 210 0))
    (check-false (ends-with? 2921 2))
    (check-false (ends-with? 213 0))
  )
)

(run-tests tests 'verbose)