
#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да компресираме списък по следния начин:
; поредни повтарящи се елементи слагаме в двойка, чийто първи елемент
; е стойността на елемента от списъка, а втория - броя поредни срещания.
; например:
; (encode '(1 1 1 2 3 3 4 4 4 4 4 4 1 1 2)) -> '((1 . 3) (2 . 1) (3 . 2) (4 . 6) (1 . 2) (2 . 1))

(define (encode xs)
  (define (loop prev i xs res)
    (cond
      ((null? xs) (append res (list (cons prev i))))
      ((= (car xs) prev) (loop prev (+ i 1) (cdr xs) res))
      (else (loop (car xs) 1 (cdr xs) (append res (list (cons prev i))))))
  )

  (if (null? xs) '() (loop (car xs) 1 (cdr xs) '()))
)

(define tests
  (test-suite
   "encode"
   (test-case "empty list" (check-equal? (encode '()) '()))
   (test-case "example list" (check-equal? (encode '(1 1 1 2 3 3 4 4 4 4 4 4 1 1 2)) '((1 . 3) (2 . 1) (3 . 2) (4 . 6) (1 . 2) (2 . 1))))
 )
)

(run-tests tests 'verbose)
                        