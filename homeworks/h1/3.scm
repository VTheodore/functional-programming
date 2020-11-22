(define (accumulate p? op term init a next b)
  (define (loop i res)
    (cond
      ((> i b) res)
      ((p? (term i)) (loop (next i) (op res (term i))))
      (else (loop (next i) res))))
  (loop a init))

(define (char-digit? c) (and (char>=? c #\0) (char<=? c #\9)))

(define (expr-valid? expr)
  (define (operator? ch)
    (or (equal? ch #\+) (equal? ch #\-) (equal? ch #\*) (equal? ch #\/) (equal? ch #\^)))
  
  (define (loop i first? last-dig? last-op? last-num?)
    (let (
          (curr (if (< i (string-length expr)) (string-ref expr i)))
          )
       (cond
         ((>= i (string-length expr)) (not last-op?))
         ((equal? curr #\space) (loop (+ 1 i) first? last-dig? last-op? last-dig?))
         ((char-digit? curr) (if last-num? #f (loop (+ 1 i) #f #t #f #f)))
         ((operator? curr) (if (or last-op? first?) #f (loop (+ 1 i) #f #f #t #f)))
         (else #f)))
      )

  (loop 0 #t #f #f #f))

(define (expr-rp expr)
  (define (precedence t)
    (cond
      ((or (equal? t #\+) (equal? t #\-)) 0)
      ((or (equal? t #\*) (equal? t #\/)) 1)
      ((equal? t #\^) 2)))

  (define (compare-precedence t1 t2) (- (precedence t1) (precedence t2)))
  
  (define (remove-whitespace expr)
    (define (not-whitespace? ch)
      (not (equal? ch " ")))
    (define (term i) (string (string-ref expr i)))
    (define (next i) (+ i 1))
  
    (accumulate not-whitespace? string-append term "" 0 next (- (string-length expr) 1)))

  (define (to-pop? token stack)
    (and (not (= (string-length stack) 0))
         (<= (compare-precedence token (string-ref stack 0)) 0)))

  (define (stack-operation res token stack)
    (if (to-pop? token stack)
      (stack-operation (string-append res (string (string-ref stack 0))) token (substring stack 1))
      (string-append res ",")))

  (define (stack-remove token stack)
    (if (to-pop? token stack)
      (stack-remove token (substring stack 1))
      stack))

  (define (append-remaining res stack)
    (if (not (= (string-length stack) 0))
      (append-remaining (string-append res (string (string-ref stack 0))) (substring stack 1))
      res))

  (define (loop i res stack expr)
    (let ((curr (if (< i (string-length expr)) (string-ref expr i))))
      (cond
        ((>= i (string-length expr)) (append-remaining res stack))
        ((char-digit? curr) (loop (+ 1 i) (string-append res (string curr)) stack expr))
        (else (loop (+ 1 i) (stack-operation res curr stack) (string-append (string curr) (stack-remove curr stack)) expr)))
    ))

  (if (expr-valid? expr) (loop 0 "" "" (remove-whitespace expr)) #f)
  )


(define (expr-eval expr)
  
  (define (top-number stack)
    (define (number-iter i stack res)
      (let ((curr (if (not (= (string-length stack) 0)) (string-ref stack 0))))
        (if (and (not (= (string-length stack) 0)) (not (equal? curr #\,)))
            (number-iter (+ i 1) (substring stack 1) (+ res (* (- (char->integer curr) 48) (expt 10 i))))
            res
        )))
    (number-iter 0 stack 0))

  (define (calculate ch n1 n2)
    (cond
      ((equal? ch #\+) (+ n2 n1))
      ((equal? ch #\-) (- n2 n1))
      ((equal? ch #\*) (* n2 n1))
      ((equal? ch #\/) (/ n2 n1))
      ((equal? ch #\^) (expt n2 n1))))

  (define (remove-top stack)
    (if (and (not (= (string-length stack) 0)) (not (equal? (string-ref stack 0) #\,)))
        (remove-top (substring stack 1))
        (if (= (string-length stack) 0) stack (substring stack 1))))

  (define (reverse str)
    (define (helper i res)
      (if (< i 0)
          res
          (helper (- i 1) (string-append res (string (string-ref str i))))))
    (helper (- (string-length str) 1) ""))
    
  (define (loop expr stack)
    (let* (
           (curr (if (not (= (string-length expr) 0)) (string-ref expr 0)))
           (res (lambda (op stack) (calculate op (top-number stack) (top-number (remove-top stack)))))
           (updated (lambda (curr stack)
                      (string-append (reverse (number->string (res curr stack))) "," (remove-top (remove-top stack))))))
      (cond
        ((= (string-length expr) 0) (top-number stack))
        ((or (char-digit? curr) (equal? curr #\,)) (loop (substring expr 1) (string-append (string curr) stack)))
        (else (loop (substring expr 1) (updated curr stack)))
      )))

  (if (expr-valid? expr) (loop (expr-rp expr) "") #f))