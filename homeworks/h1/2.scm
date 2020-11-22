(define (filter-accumulate p? op term init a next b)
  (define (loop i res)
    (cond
      ((> i b) res)
      ((p? (term i)) (loop (next i) (op res (term i))))
      (else (loop (next i) res))))
  (loop a init))

(define (accumulate op term init a next b) (filter-accumulate (lambda (x) #t) op term init a next b))
(define (1+ n) (+ n 1))

(define (set-contains? set elem) ; elem-times integer division leaves us with the (n + 1)th binary digit.
  (= 1 (remainder (accumulate quotient (lambda (ignored) 2) set 1 1+ elem) 2))) ; if it is a one => the set contains elem.
(set-contains? 35 0)

(define (set-add set elem)
  (if (not (set-contains? set elem))
      (+ set (expt 2 elem)) ; if the number is not in the set we append 1 to the left of its binary representation which simply means adding 2^elem to its decimal form.
      set)) ; if the number is in the set we do nothing

(define (set-remove set elem)
  (if (set-contains? set elem)
      (- set (expt 2 elem)) ; same idea as set-add
      set))

(define (set-empty? set)
  (= set 0))

(define (set-size set)
  (define (last-digit-one? n)
    (= (remainder n 2) 1))
  (define (prev i)
    (if (= 0 i) (+ set 1) (quotient i 2)))

  (define (incr res ignored) (+ 1 res))

  (filter-accumulate last-digit-one? incr (lambda (x) x) 0 set prev set))

(define (set-loop-b p? s1 s2 post-f)
  (define (loop s1 s2 res i)
    (cond
      ((= 0 s1) (post-f res s2 i))
      ((= 0 s2) (post-f res s1 i))
      ((p? s1 s2) (loop (quotient s1 2) (quotient s2 2) (+ res (expt 2 i)) (+ i 1)))
      (else (loop (quotient s1 2) (quotient s2 2) res (+ i 1)))))

  (loop s1 s2 0 0))

(define (set-intersect s1 s2)
  (define (p? x y)
    (and (= (remainder x 2) 1) (= (remainder y 2) 1)))
  (define (post-f res i1 i2) res)
  (set-loop-b p? s1 s2 post-f))

(define (set-union s1 s2)
  (define (p? x y)
    (or (= (remainder x 2) 1) (= (remainder y 2) 1)))
  (define (post-f res bigger i)
    (cond
      ((= bigger 0) res)
      ((= (remainder bigger 2) 1) (post-f (+ res (expt 2 i)) (quotient bigger 2) (+ 1 i)))
      (else (post-f res (quotient bigger 2) (+ 1 i)))))
  (set-loop-b p? s1 s2 post-f))

(define (set-difference s1 s2)
  (define (p? x y)
    (and (= (remainder x 2) 1) (= (remainder y 2) 0)))

 (define (count-digits number)
   (define (helper number count)
     (if (= number 0)
         (if (= count 0) 1 count)
         (helper (quotient number 10) (+ count 1))))
   (helper number 0))
  
  (define (post-f res bigger i)
    (cond
      ((> i (count-digits s1)) res)
      ((= (remainder bigger 2) 1) (post-f (+ res (expt 2 i)) (quotient bigger 2) (+ 1 i)))
      (else (post-f res (quotient 2) (+ 1 i)))))
  (set-loop-b p? s1 s2 post-f))