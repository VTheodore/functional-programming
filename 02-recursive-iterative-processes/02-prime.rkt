#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да проверим дали число е просто.

(define (prime? n)
  (define (prime-iter? n i)
    (cond
      ((= i n) #t)
      ((= (remainder n i) 0) #f)
      (else (prime-iter? n (+ i 1)))))

  (if (< n 2) #f (prime-iter? n 2)))

(define tests
  (test-suite "prime? tests"
    (check-false (prime? 1))
    (check-true (prime? 5))
    (check-false (prime? 1729))
    (check-false (prime? 41041))
  )
)

(run-tests tests 'verbose)