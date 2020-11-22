#lang racket

(define c (cons 1 2)) ; construct, constructor
(car c) ;contents of address register
(cdr c) ;contents of decrements register

(cons 1 #t)
(define d (cons #t (lambda (x) (+ x 1)))) ; could be pair of anything

(define e (cons (cons 1 2) 3))
(cdr (car e)) ; expected: 2

(define f (cons 1 (cons (cons 3 4) 2)))
(car (car (cdr f))) ; expected: 3

'(1 2 3) ; list (linked)
'() ;  empty list

(cons 1 '()) ; list with 1 item
(cons 1 2) ; pair

(cons 1 (cons 2 '())) ; 1 -> 2 -> '() --linked list
(cons 1 '(2 3 4)) ; appends 1 to front

(define l (cons 1 (cons 2 '())))
(car l) ; expected: 1
(cdr l) ; expected: (2)
(cdr (cdr l)) ; expected '()

'(1 2 3)
; head/car 1
; tail/cdr (2 3)

(null? '()) ; expected: true
(null? '(1 2 3)) ; expected: false
(eq? '() '()) ; expected: true

; = (numbers only), eq? (symbols and numbers), eqv? (reference check), equal? (generic)

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(length '(1 2 3 4)) ; expected: 4

; 'l' is list of numbers 
(define (sum l)
  (define (sum-i l res)
    (if (null? l)
        res
        (sum-i (cdr l) (+ res (car l)))))
  (sum-i l 0))

(sum '(1 2 3 4 5)) ; expected: 15