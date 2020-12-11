#lang racket
; Извежда елементите на списъка L
(define (print L)
  (define (loop L)
    (cond ((pair? L)
      (display ", ")
      (display (car L))
      (loop (cdr L)))))

  (cond
    ((null? L) "{empty}")
    (else
     (display #\{)
     (display (loop L))
     (display #\})))
)

; Намира дължината на L рекурсивно
(define (length-rec L)
  (if (null? L)
      0
      (+ 1 (length-rec (cdr L))))
)

; Намира дължината на L итеративно
(define (length-iter L)
  (define (loop L count)
    (if (null? L)
        count
        (loop (cdr L) (+ count 1))))

  (loop L 0)
)

; Генерира списък от всички цели числа в интервала [a, b]
(define (interval a b)
  (if (> a b)
      '()
      (cons a (interval (+ a 1) b)))
)

(define (accumulate op term init a next b)
  (define (loop i)
    (if (> i b)
        init
        (op (term i) (loop (next i)))))
  (loop a)
)

(define (interval-acc a b)
  (define (id x) x)
  (define (1+ x) (+ x 1))
  (accumulate cons id '() a 1+ b)
)

; Генерира списък от всички четни числа в интервала [a, b]
(define (collect-even a b)
  (cond
    ((> a b) '())
    ((even? a) (cons a (collect-even (+ a 1) b)))
    (else (collect-even (+ a 1) b)))
)

(define (collect-even-acc a b)
  (define (id x) x)
  (define (1+ x) (+ x 1))
  (define (even-cons curr res)
    (if (even? curr)
        (cons curr res)
        res)
  )
  
  (accumulate even-cons id '() a 1+ b)
)

; Конкатенира два списъка
(define (append L1 L2)
  (if (null? L1)
      L2
      (cons (car L1) (append (cdr L1) L2)))
)

; Обръща елементите на списъка итеративно
(define (reverse L)
  (define (loop L res)
    (if (null? L)
        res
        (loop (cdr L) (cons (car L) res))))
  (loop L '())
)

; Конкатенира два списъка итеративно
(define (append-iter L1 L2)
  (define (loop L1 res)
    (if (null? L1)
        res
        (loop (cdr L1) (cons (car L1) res))))

  (loop (reverse L1) L2)
)

; Охождане на няколко нива
(define (flatten L)
  (cond
    ((null? L) '())
    ((pair? (car L)) (append (flatten (car L)) (flatten (cdr L))))
    (else (cons (car L) (flatten (cdr L)))))
)

; Прилага op върху всеки от елементите на L
(define (u-map op L)
  (if (null? L)
      '()
      (cons (op (car L)) (map op (cdr L))))
)

; Връща списък от онези елементи на L, за които е верен pred?
(define (filter pred? L)
  (cond
    ((null? L) '())
    ((pred? (car L)) (cons (car L) (filter pred? (cdr L))))
    (else (filter pred? (cdr L))))
)

(define (foldr op init L)
  (if (null? L)
      init
      (op (car L) (foldr op init (cdr L))))
)

; (- 1 (-  2 (- 3 0)))

(define (foldl op init L)
  (define (loop L res)
    (if (null? L)
        res
        (loop (cdr L) (op (car L) res))))
  (loop L init)
)

(define matrix '((1 2 3 4) (5 6 7 8)))

(define (number-rows M)
  (length M)
)

(define (number-cols M)
  (if (null? M)
      0
      (length (car M)))
)

(define (first-row M)
  (car M)
)

(define (remove-first-row M)
  (cdr M)
)

(define (get-row M n)
  (list-ref M n)
)

(define (first-col M)
  (map car M)
)

(define (remove-first-col M)
  (map cdr M)
)

(define (get-col M m)
  (map (lambda (row) (list-ref row m)) M)
)

(define (get-element M n m)
  (list-ref (list-ref M n) m)
)

(define (remove-nth l n)
  (cond
    ((null? l) '())
    ((= n 0) (cdr l))
    (else (cons (car l) (remove-nth (cdr l) (- n 1)))))
)

(define (remove-row M n)
  (remove-nth M n)
)

(define (remove-col M m)
  (map (lambda (row) (remove-nth row m)) M)
)

(define (transpose M)
  (apply map list M)
)

