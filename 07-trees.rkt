#lang racket

; construction
(define empty-tree '())

(define (make-tree root left right) (list root left right))
; Valid trees
; '()
; (list 2 '() '())
; (list 5 (list 3 '() '()) '())

(define (make-leaf root) (make-tree root empty-tree empty-tree))
; (make-leaf 2) -> '(2 () ())

(define example (make-tree 1
                           (make-tree 3 (make-leaf 8) empty-tree)
                           (make-tree 7 empty-tree (make-tree 9
                                                              (make-leaf 10)
                                                              (make-leaf 11))))
)
; getters
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)

; validation
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (left-tree t))
           (tree? (right-tree t))))
)

(define empty-tree? null?)

(define example-bst (make-tree 8
                       (make-tree 3
                                  (make-leaf 1)
                                  (make-tree 6 (make-leaf 4) (make-leaf 7)))
                       (make-tree 10
                                  empty-tree
                                  (make-tree 14
                                             (make-leaf 13)
                                             empty-tree)))
)


; искаме да проверим дали нещо се среща в дървото
; във всяка от задачите започваме с проверка за празно дърво
; елемент се среща в дърво, тогава когато:
; - или е корена на дървото
; - или се среща в лявото, или в дясното поддърво
(define (number-tree? x tree)
  (cond
    ((empty-tree? tree) #f)
    ((equal? x (root-tree tree)) #t)
    (else (or (number-tree? x (left-tree tree)) (number-tree? x (right-tree tree)))))
)

; искаме да намерим сумата на всички елементи в дървото
(define (sum-tree tree)
  (if (empty-tree? tree)
      0
      (+ (root-tree tree) (sum-tree (left-tree tree)) (sum-tree (right-tree tree))))
)

; искаме да намерим всички елементи на дадено ниво в дървото
; тук имахме две дъна
; - отново гледаме за празно дърво
; - проверяваме дали сме поискали ниво 0 - тогава е окей да върнем корена на дървото (след като знаем, че не е празно)
; - иначе комбинираме (с append) резултатите за лявото и дясното поддърво, но с по-малко ниво
(define (tree-level n tree)
  (cond
    ((or (< n 0) (empty-tree? tree)) '())
    ((= n 0) (list (root-tree tree)))
    (else (append (tree-level (- n 1) (left-tree tree)) (tree-level (- n 1) (right-tree tree)))))
)

; искаме да приложим функцията f върху всички елементи на дървото (като истинската функция map, ама за дървета)
(define (tree-map f tree)
  (if (empty-tree? tree)
      '()
      (make-tree (f (root-tree tree))
                 (tree-map f (left-tree tree))
                 (tree-map f (right-tree tree))))
)

; за сравнение долу има map за списъци
; приличат си по това, че имаме проверка за празна структура (дърво - empty-tree? или списък - null?).
; ако последната е непразна, конструираме структура (дърво - make-tree или списък - cons), като приложим функцията f върху текущия елемент

; (define (map f xs)
;  (if (null? xs)
;      '()
;      (cons (f (car xs)) (map f (cdr xs)))
; )

; искаме да върнем списък от елементите на дърво - ляво, корен, дясно

(define (tree->list tree)
  (if (empty-tree? tree)
      '()
      (append (tree->list (left-tree tree)) (list (root-tree tree)) (tree->list (right-tree tree)))))

; искаме да проверим дали x се среща в двоичното наредено дърво tree
; тук правим итеративен процес (опашкова рекурсия)

(define (bst-member x tree)
  (cond
    ((empty-tree? tree) #f)
    ((equal? (root-tree tree) x) #t)
    ((< x (root-tree tree)) (bst-member x (left-tree tree)))
    (else (bst-member x (right-tree tree))))
)

; искаме да вкараме елемент в двоично дърво
(define (bst-insert x tree)
  (cond
    ((empty-tree? tree) (make-leaf x))
    ((< x (root-tree tree)) (make-tree (root-tree tree) (bst-insert x (left-tree tree)) (right-tree tree)))
    (else (make-tree (root-tree tree) (left-tree tree) (bst-insert x (right-tree tree)))))
)

; искаме да сортираме даден списък, използвайки tree->list и bst-insert
(define (sort xs)
  (tree->list (foldr bst-insert empty-tree xs))
)