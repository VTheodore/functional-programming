#lang racket
(require rackunit)
(require rackunit/text-ui)

; chunk 
; разбива списъка xs на подсписъци с дължина n

(define (chunk n xs)
  (define (safe-take n xs)
    (cond
      ((> n (length xs)) xs)
      (else (take xs n)))
   )

  (define (safe-drop n xs)
    (cond
      ((> n (length xs)) '())
      (else (drop xs n)))
  )
  
  (if (null? xs)
      '()
      (cons (safe-take n xs) (chunk n (safe-drop n xs))))
)

;(define tests
;  (test-suite "chunk"
;    (check-equal? (chunk 2 '(1 2 3 4 5 6 7 8 9))  '((1 2) (3 4) (5 6) (7 8) (9)))
;    (check-equal? (chunk 3 '(1 2 3 4 5 6 7 8 9))  '((1 2 3) (4 5 6) (7 8 9)))
;  )
;)
;
;(run-tests tests 'verbose)
;

(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map f (cdr xs)))))

(map (lambda (x) (remainder x 2)) '(2 3 4)) ; expected: '(0 1 0))
(map (lambda (x) (list x x)) '(2 3 4)) ; expected: '((2 2) (3 3) (4 4))

(define (filter p? ys)
  (cond
    ((null? ys) '())
    ((p? (car ys)) (cons (car ys) (filter p? (cdr ys))))
    (else (filter p? (cdr ys))))
)

(filter odd? '(1 2 3 4 5)) ; expected: '(1 3 5)

(define (sum-list xs)
  (if (null? xs)
      0
      (+ (car xs) (sum-list (cdr xs)))))

(define (mult-list xs)
  (if (null? xs)
      1
      (* (car xs) (mult-list (cdr xs)))))

(define (fold op nv xs)
  (if (null? xs)
      nv
      (op (car xs) (fold op nv (cdr xs)))))

(fold * 1 '(1 2 3 4)) ; expected: 24
(fold + 0 '(1 2 3 4)) ; expected 10

; Функция, която ни дава най-голямото число в списък
; Приемаме, че списъкът ни има поне един елемент
(define (biggest-in-list xs)
  (if (null? xs)
      (error "called with an empty list")
      (fold max (car xs) xs)) ; (fold (lambda (x y) (if (> x y) x y)) (car xs) xs)
)

(biggest-in-list '(1 2 123 1000 -1 2)) ; expected: 1000

(define (filter2 p? xs)
  (fold (lambda (curr acc)
          (if (p? curr)
              (cons curr acc)
              acc))
        '() xs)
)

(filter2 odd? '(1 2 3 4 5 6 7 8)) ; expected: '(1 3 5 7)

(define (map2 f xs)
  (fold (lambda (curr acc) (cons (f curr) acc)) '() xs)
)

(map2 (lambda (x) (* x 2)) '(1 2 3 4 5)) ; expected: '(2 4 6 8 10)

;(fold + 0 '(1 2 3 4))
;(+ 1 (+ 2 (+ 3 (+ 4 0)))) fold right 

(define (fold-left op nv xs)
  (if (null? xs)
      nv
      (op (fold-left op nv (cdr xs)) (car xs)))
)

(fold-left + 0 '(1 2 3 4))
;(+ (+ (+ (+ 0 4) 3) 2) 1)

(define (fold-left-iter op res xs)
  (if (null? xs)
      res
      (fold-left-iter op (op res (car xs)) (cdr xs)))
)

(fold-left-iter + 0 '(1 2 3 4))
;(+ 0 1)
;(+ 1 2)
;(+ 3 3)
;(+ 6 4)

(define (reverse xs)
  (fold (lambda (curr acc) (append acc (list curr))) '() xs)
)

(reverse '(1 2 3 4)) ; expected '(4 3 2 1)

(define (clone xs)
  (fold cons '() xs)
)

(clone '(1 2 3 4)) ; expected '(1 2 3 4)

(define lst '((1 2) (3 4) (5)))
(map (lambda (x) (fold + 0 x)) lst) ; expected: '(3 7 5)
