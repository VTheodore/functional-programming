#lang racket

(define-syntax my-delay
  (syntax-rules () ((my-delay expr) (lambda () expr))))

(define (my-force p) (p))

(define-syntax stream-cons
  (syntax-rules () ((stream-cons x xs) (cons x (my-delay xs)))))

; (stream-cons 1 '(2)) -> (cons 1 (my-delay '(2)))

(define (ones) (stream-cons 1 (ones)))

(define (head s) (car s))
(define (tail s) (my-force (cdr s)))
(define empty-stream '())
(define (stream-empty? s) (null? s))

; (head (ones)) -> 1
; (tail (ones)) -> (1 . #<procedure>)
; Задачи

;1. Искаме поток от всички естествени числа
(define (naturals)
  (define (naturals-from x)
    (stream-cons x (naturals-from (+ x 1)))
  )

  (naturals-from 0)
)

; 2. Искаме да сравниме 2 потока
(define (compare-streams s1 s2)
  (cond
    ((and (stream-empty? s1) (stream-empty? s2)) #t)
    ((or (and (stream-empty? s1) (not (stream-empty? s2)))
         (and (not (stream-empty? s1)) (stream-empty? s2))) #f)
    ((equal? (head s1) (head s2)) (compare-streams (tail s1) (tail s2)))
    (else #f))
)

; 3. Искаме да генерираме поток в даден range
(define (stream-range s e)
  (if (> s e)
      empty-stream
      (stream-cons s (stream-range (+ s 1) e)))
)

; 4. Искаме да вземем n-тия елемент на поток
(define (nth s n)
  (cond
    ((or (stream-empty? s) (< n 1)) empty-stream)
    ((= n 1) (head s))
    (else (nth (tail s) (- n 1))))
)

; (nth (stream-range 1 10) 11) ; expected: 3

; 5. Искаме да вземем първите n елемента на даден поток
(define (stream-take s n)
  (if (or (stream-empty? s) (= n 0))
    empty-stream
    (stream-cons (head s) (stream-take (tail s) (- n 1))))
)

; (define res (stream-take (stream-range 1 5) 3))
; (head res) ; expected: 1
; (head (tail res)) ; expected: 2
; (head (tail (tail res))) ; expected: 3
; (stream-empty? (tail (tail (tail res)))) ; expected: #t

; 6. Превръщаме поток в списък
(define (stream->list s)
  (if (stream-empty? s)
      '()
      (cons (head s) (stream->list (tail s))))
)

; (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))

; (stream->list (stream-range 1 5)) ; expected: '(1 2 3 4 5)

; 7. Филтрираме елементите на поток
(define (stream-filter p? s)
  (cond
    ((stream-empty? s) empty-stream)
    ((p? (head s)) (stream-cons (head s) (stream-filter p? (tail s))))
    (else (stream-filter p? (tail s))))
)

; (define res (stream-filter (lambda (x) (= (remainder x 3) 0)) (stream-range 1 10)))
; (stream->list res) ; expected: '(3 6 9)


; 8. Прилагаме функция върху всеки елемент на поток
(define (stream-map f s)
  (if (stream-empty? s)
      empty-stream
      (stream-cons (f (head s)) (stream-map f (tail s))))
)

; (define res (stream-map (lambda (x) (* x 2)) (stream-range 1 3)))
; (stream->list res) ; expected: '(2 4 6)

; 9. Конкатенираме два потока
(define (stream-append s t)
  (if (stream-empty? s)
      t
      (stream-cons (head s) (stream-append (tail s) t)))
)

; (define res (stream-append (stream-range 1 3) (stream-range 4 6)))
; (stream->list res) ; expected: '(1 2 3 4 5 6)

; 10. Пропускаме първите n елемента на даден поток
(define (stream-drop s n)
  (cond
    ((or (stream-empty? s) (< n 0)) empty-stream)
    ((= n 0) s)
    (else (stream-drop (tail s) (- n 1))))
)

; (define res (stream-drop (stream-range 1 20) 17))
; (stream->list res) ; expected: '(18, 19, 20)

; 11. Правим безкраен поток от вида (x f(x) f(f(x)) f(f(f(x))) ...)
(define (iterate f x)
  (stream-cons x (iterate f (f x)))
)

; (define res (iterate (lambda (x) (* x 2)) 1))
; (head res) ; expected: 1
; (tail res) ; expected: (2 . #<procedure>)
; (tail (tail res)) ; expected: (4 . #<procedure>)
; (tail (tail (tail res))) ; expected: (8 . #<procedure>)

; 12. Правим поток от вида (nv op(nv, a1) op(op(nv, a1), a2) ...)
(define (scanl op nv stream)
  (if (stream-empty? stream)
      (stream-cons nv empty-stream)
      (stream-cons nv (scanl op (op nv (head stream)) (tail stream))))
)

; (define res (scanl + 1 (stream-range 1 10)))
; (head res) ; expected 1 
; (tail res) ; expected (2 . #<procedure>)
; (tail (tail res)) ; expected (4 . #<procedure>)
; (tail (tail (tail res))) ; (expected 7 . #<procedure>)

; 13. Поток от всички прости числа
(define (primes)
  ; в списъка с числа от 2 до n - 1 няма такова, което дели нашето число
  (define (any? p? xs)
    (cond
      ((null? xs) #f)
      ((p? (car xs)) #t)
      (else (any? p? (cdr xs))))
  )
  
  (define (prime? n)
    (not (any? (lambda (x) (= (remainder n x) 0)) (range 2 (- n 1))))
  )
  
  (stream-filter prime? (iterate (lambda (x) (+ x 1) ) 2))
)


; (primes) ; expected: (2 . #<procedure>)
; (tail (primes)) ; expected: (3 . #<procedure>)
; (tail (tail (primes))) ; expected: (5 . #<procedure>)
; (tail (tail (tail (primes)))) ; expected: (7 . #<procedure>)
; (tail (tail (tail (tail (primes))))) ; expected: (11 . #<procedure>)