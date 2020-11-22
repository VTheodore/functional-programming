#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (min-el xs)
  (define (loop xs min)
    (cond
      ((null? xs) min)
      ((< (car xs) min) (loop (cdr xs) (car xs)))
      (else (loop (cdr xs) min))))
  (if (null? xs) '() (loop (cdr xs) (car xs))))

(define (remove-first xs el)
  (define (loop xs res)
    (cond
      ((null? xs) res)
      ((= el (car xs)) (append res (cdr xs)))
      (else (loop (cdr xs) (append res (list (car xs)))))))
  (loop xs '()))

(define (selection-sort xs)
  (let (
        (min-element (min-el xs))
        )
    (if (null? xs)
        '()
        (cons min-element (selection-sort (remove-first xs min-element))))
    ))


(define min-el-tests
 (test-suite "min-el tests"
     (check-equal? (min-el '(4 -2 12 0 4 -1 5 2)) -2)
     (check-equal? (min-el '(1 1 1 1 1 1 1)) 1)
     (check-equal? (min-el '(1)) 1)
     (check-true (null? (min-el '())))
  )
)

(define remove-first-tests
 (test-suite "remove-first tests"
     (check-equal? (remove-first '(4 -2 12 0 4 -1 5 2) -2) '(4 12 0 4 -1 5 2))
     (check-equal? (remove-first '(1 2 3 4 1 1 2) 1) '(2 3 4 1 1 2))
     (check-equal? (remove-first '(1 2 3 4 5 6 7) 7) '(1 2 3 4 5 6))
     (check-equal? (remove-first '(1 2 3 4) 5) '(1 2 3 4))
     (check-equal? (remove-first '(1) 1) '())
     (check-equal? (remove-first '() 1) '())
  )
)

(define selection-sort-tests
  (test-suite "selection-sort tests"
     (check-equal? (selection-sort '(5 4 3 2 1)) '(1 2 3 4 5))
     (check-equal? (selection-sort '(3 -1 2 -3 5 -11 10)) '(-11 -3 -1 2 3 5 10))
     (check-equal? (selection-sort '(2 1)) '(1 2))
     (check-equal? (selection-sort '(1)) '(1))
     (check-equal? (selection-sort '()) '())
  )
)

(run-tests min-el-tests 'verbose)
(run-tests remove-first-tests 'verbose)
(run-tests selection-sort-tests 'verbose)