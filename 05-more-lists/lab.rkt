#lang racket

(define (take-while p? xs)
  (if (or (null? xs) (not (p? (car xs))))
      '()
      (cons (car xs) (take-while p? (cdr xs)))))

(take-while even? '(2 4 6 8 7 123 2 6))

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(append '(1 2 3 4) '(5 6 7))

(define (min-element xs)
  (define (helper xs min)
      (if (null? xs)
          min
          (helper (cdr xs) (if (< (car xs) min) (car xs) min))))
  (if (null? xs) '() (helper (cdr xs) (car xs))))

(define (remove-first xs el)
  (define (helper xs res)
    (cond
      ((null? xs) res)
      ((= (car xs) el) (append res (cdr xs)))
      (else (helper (cdr xs) (append res (list (car xs)))))))
  (helper xs '()))

(define (selection-sort xs)
  (let (
        (min-el (min-element xs))
        )
    (if (null? xs)
        '()
        (cons min-el (selection-sort (remove-first xs min-el))))
    ))

(selection-sort '(10 9 0 4 -1 4 5))

(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))

(map (lambda (x) (* x 10)) '(1 2 3 4 5))
