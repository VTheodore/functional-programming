
#lang racket
(require rackunit)
(require rackunit/text-ui)

; zip
; може да видим какво се очаква да прави в тестовете

(define (zip xs ys)
  (define (loop xs ys res)
    (if (or (null? xs) (null? ys))
        res
        (loop (cdr xs) (cdr ys) (append res (cons (list (car xs) (car ys)) '())))))
  (loop xs ys '())
)

(define tests
  (test-suite "Zip"
    (check-equal? (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))
    (check-equal? (zip '(28 9 12) '(1 3)) '((28 1) (9 3)))
  )
)

(run-tests tests 'verbose)