#lang racket

(define (make-rational num denom)
  (cons num denom))

(define (numerator number)
  (car number))

(define (denominator number)
  (cdr number))

(define (sum-rationals first second)
  (/
   (+
      (* (numerator first) (denominator second))
      (* (numerator second) (denominator first)))
   (* (denominator first) (denominator second))))

(define (subtract-rationals first second)
  (/
   (-
      (* (numerator first) (denominator second))
      (* (numerator second) (denominator first)))
   (* (denominator first) (denominator second))))

(define (mult-rationals first second)
  (/
   (* (numerator first) (numerator second))
   (* (denominator first) (denominator second))))

(define (divide-rationals first second)
  (/
   (* (numerator first) (denominator second))
   (* (numerator first) (denominator second))))

(define rational-pi (make-rational 22 7))
(define random-number (make-rational 1 3))

(mult-rationals rational-pi random-number)