#lang racket
(require rackunit)
(require rackunit/text-ui)

; Стъпвайки на дефиницията за бързо повдигане на степен,
; търсим такава, която генерира итеративен процес
(define (expt x n)
  (define (expt-iter x n rem)
    (cond
      ((= n 0) 1)
      ((= n 1) (* x rem))
      (else
       (if (= (remainder n 2) 0)
           (expt-iter (* x x) (/ n 2) rem)
           (expt-iter (* x x) (quotient n 2) (* rem x))))))
  (if (< n 0)
      (/ 1 (expt-iter x (abs n) 1))
      (expt-iter x n 1))
)

(define tests
  (test-suite "expt tests"
    (check-equal? (expt 4 4) 256)
    (check-equal? (expt 29139123 0) 1)
    (check-equal? (expt 3 4) 81)
    (check-equal? (expt 2 1) 2)
  )
)

(run-tests tests 'verbose)