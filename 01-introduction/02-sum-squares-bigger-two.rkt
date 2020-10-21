#lang racket
(require "helpers.rkt")
(require rackunit)
(require rackunit/text-ui)
;1.3 - Съчинете процедура, която по дадени три числа, намира сумата от квадратите на по-големите две от тях.
; За по-удобно, може да разбиете задачата на по-малки такива.

(define (squares-sum-bigger-two a b c)
  (define (max-two a b c)
    (if (>= a b)
        (if (>= b c)
            (list a b)
            (list a c))

        (if (>= a c)
            (list a b)
            (list b c))))

  (define (sum-of-squares a b)
    (+ (expt a 2) (expt b 2)))

  (apply sum-of-squares (max-two a b c))
)

(define tests
  (test-suite
    "Sum of squares tests"

    (let ((a (random 10))
          (b (random 10))
          (c (random 10)))
      
    (check-true (all-equal? (map (lambda (args) (apply squares-sum-bigger-two args))
                                (permute (list a b c)))))
)))

(run-tests tests 'verbose)