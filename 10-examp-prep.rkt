#lang racket

(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
;getters
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
; validation
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))
(define empty-tree? null?)


(define example (make-tree 1
                           (make-tree 3 (make-leaf 8) empty-tree)
                           (make-tree 7 empty-tree (make-tree 9
                                                              (make-leaf 10)
                                                              (make-leaf 11)))))

; (invert tree)
;     4               4
;    / \   invert    / \
;   2   5 ========> 5   2
;  / \                 / \
; 1   3               3   1

(define (invert tree)
  (cond
    ((empty-tree? tree) '())
    ((and (empty-tree? (left-tree tree)) (empty-tree? (right-tree tree))) (make-leaf (root-tree tree)))
    (else (make-tree (root-tree tree) (invert (right-tree tree)) (invert (left-tree tree)))))
)

; прави пълно дърво с дадена височина и всичко стойности във върховете са дадената стойност
(define (full-tree level value)
  (if (= level 0)
      empty-tree
      (make-tree value (full-tree (- level 1) value) (full-tree (- level 1) value)))
)

; поток от всички пълни дървета с дадена стойност
(define (full-trees value)
  (stream value (full-trees value) (full-trees value))
)

; Да се напише функция extremum, която по даден списък от списъци от числа намира число, което е минимално или максимално във всеки от списъците, ако има такова, или 0 иначе
; (extremum '((1 2 3 2) (3 5) (3 3) (1 1 3 3))) → 3

(define (extremum xs)
  (define (min lst)
    (cond ((null? (cdr lst)) (car lst))
          ((< (car lst) (min (cdr lst))) (car lst))
          (else (min (cdr lst)))) )

  (define (max lst)
    (cond ((null? (cdr lst)) (car lst))
          ((> (car lst) (max (cdr lst))) (car lst))
          (else (max (cdr lst)))) )
  
  (define (contains? x pair)
    (if (pair? pair)
        (or (= (car pair) x) (= (cdr pair) x))
        (= x pair))
  )
  
  (foldl
   (lambda (curr res)
     (cond
       ((null? res) (cons (min curr) (max curr)))
       ((and (contains? (min curr) res) (contains? (max curr) res)) res)
       ((contains? (min curr) res) (min curr))
       ((contains? (max curr) res) (max curr))
       (else 0)
   ))
   '() xs)
)