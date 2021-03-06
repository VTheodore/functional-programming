#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме функция, която приема списък и две числа и връща
; списък, състоящ се от елементите на списъка, които се намират на индекси от първото число до второто.

(define (slice xs start end)
  (define (loop i xs res)
    (cond
      ((or (null? xs) (> i end)) res)
      ((< i start) (loop (+ i 1) (cdr xs) res))
      (else (loop (+ i 1) (cdr xs) (append res (list (car xs)))))))
  (loop 0 xs '()))

(define tests
 (test-suite "Slice tests"
     (check-equal? (slice '(1 9 8 2) 1 2) '(9 8))
     (check-equal? (slice '(1 9 2 8 3) 2 10) '(2 8 3))
     (check-equal? (slice '(9 7 2 3) 0 2) '(9 7 2)) 
  )
)

(run-tests tests 'verbose)