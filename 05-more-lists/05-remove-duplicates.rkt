  #lang racket
(require rackunit)
(require rackunit/text-ui)

; remove-duplicates 
; премахва всички повтарящи се елементи от списъка

(define (remove-duplicates xs)
  (define (contains? el xs)
    (cond
      ((null? xs) #f)
      ((= el (car xs)) #t)
      (else (contains? el (cdr xs)))))
  
  (define (loop xs res)
    (cond
      ((null? xs) res)
      ((contains? (car xs) res) (loop (cdr xs) res))
      (else (loop (cdr xs) (append res (list (car xs)))))))
  (loop xs '())
)

(define tests
  (test-suite "remove-duplicates"
    (check-equal? (remove-duplicates '(1 1 2 2 1 3 3 2 3))  '(1 2 3))
    (check-equal? (remove-duplicates '(1 2 3))  '(1 2 3))
  )
)

(run-tests tests 'verbose)