#lang racket
(define (% a b)
  (remainder a b))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (>= a b)
  (or
   (> a b)
   (= a b)))

(define (<= a b)
  (not (> a b)))

(define (which-quadrant x y)
  (cond
    ((and (> x 0) (> y 0)) 1)
    ((and (< x 0) (> y 0)) 2)
    ((and (< x 0) (< y 0)) 3)
    ((and (> x 0) (< y 0)) 4)
    (else "It's on one of the axes")))

(define (fib n)
  (cond
    ((= n 0) 1)
    ((= n 1) 1)
    (else
     (+
      (fib (- n 1))
      (fib (- n 2))))))

(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

(define (pow a b)
  (if (= b 0)
      1
      (* a (pow a (- b 1)))))

(define (abs x)
  (if (< x 0)
      (- x)
      x))