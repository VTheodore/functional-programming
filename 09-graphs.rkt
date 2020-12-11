#lang racket

; Граф за нас ще означава списък от двойки (ребра)
; например '((1 . 2) (2 . 3) (2 . 5) (2 . 4) (4 . 3) (5 . 4))

(define g '((1 . 2) (2 . 3) (2 . 5) (2 . 4) (4 . 3) (5 . 4)))

; търсим входна степен на даден връх - колко ребра влизат в него
(define (in-degree g v)
  (length (filter (lambda (x) (= (cdr x) v)) g))
)
; (in-degree g 3) ; expected: 2

; търсим изходна степен на даден връх - колко ребра излизат от него
(define (out-degree g v)
  (length (filter (lambda (x) (= (car x) v)) g))
)

; (out-degree g 2) ; expected: 3

; искаме списък с всички върхове на g
(define (nodes g)
  ; (remove-duplicates (foldl (lambda (curr res) (append res (list (car curr)) (list (cdr curr)))) '() g))
  (remove-duplicates (flatten g))
)

(define (neighbours curr) (map cdr (filter (lambda (x) (= (car x) curr)) g)))

; преобразуваме g към представяне със списък на съседство
(define (to-adjacency-list g)
  (foldl (lambda (curr res) (append res (list curr (neighbours curr)))) '() (nodes g))
)

; (to-adjacency-list g)

(define (contains? x xs)
    (cond
      ((null? xs) #f)
      ((equal? x (car xs)) #t)
      (else (contains? x (cdr xs)))))

; искаме да проверим дали списъкът от върхове nodes е път в графа g
(define (path? g nodes)
  (define (remove-first xs)
    (if (null? xs)
        '()
        (cdr xs)))
  (define (remove-last xs)
    (if (or (null? xs) (null? (cdr xs)))
        '()
        (cons (car xs) (remove-last (cdr xs)))))
  
  (define zipped (map cons (remove-last nodes) (remove-first nodes)))
  (empty? (filter (lambda (curr) (not (contains? curr g))) zipped))
)

; (path? g '(1 2 5 4 3))

; искаме всички (прости) пътища между два дадени върха
(define (simple-paths g from to)
  (define (helper path curr)
    (cond
      ((contains? curr path) '())
      ((= curr to) (list (reverse (cons to path))))
      (else (foldr (lambda (x y) (append (helper (cons curr path) x) y)) '() (neighbours curr)))))
  (helper '() from)
)

; искаме най-късият (прост) път между два дадени върха
(define (shortest-path g from to)
  (foldl (lambda (curr res) (if (or (< (length curr) (length res)) (null? res)) curr res)) '() (simple-paths g from to))
)