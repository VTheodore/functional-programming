#lang racket
(require rackunit)
(require rackunit/text-ui)

; chunk 
; разбива списъка xs на подсписъци с дължина n

(define (chunk n xs)
  (define (drop n xs)
    (if (or (null? xs) (= n 0))
        xs
        (drop (- n 1) (cdr xs))))
  
  (define (make-chunk xs i res)
    (if (or (null? xs) (= i n))
        res
        (make-chunk (cdr xs) (+ i 1) (append res (list (car xs))))))
  (define (loop xs res)
    (if (null? xs)
        res
        (loop (drop n xs) (append res (cons (make-chunk xs 0 '()) '())))))

  (loop xs '())
)

(define tests
  (test-suite "chunk"
    (check-equal? (chunk 2 '(1 2 3 4 5 6 7 8 9))  '((1 2) (3 4) (5 6) (7 8) (9)))
    (check-equal? (chunk 3 '(1 2 3 4 5 6 7 8 9))  '((1 2 3) (4 5 6) (7 8 9)))
  )
)

(run-tests tests 'verbose)