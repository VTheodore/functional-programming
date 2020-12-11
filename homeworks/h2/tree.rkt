#lang racket
(require racket/stream)

(provide (all-defined-out))

(define (tree? str)
  (define trimmed-str (string-trim str))
  (define str-length (string-length trimmed-str))
  (define (whitespace? ch) (or (char-whitespace? ch) (eq? ch #\tab)))

  (define (number i start? opening-brackets balance) ; i => current index, start? => indicating whether there have been digits before or they should start to appear, opening-brackets - number of '{', balance - (left-childs - right-childs); if != 0 at the end the tree is not valid
    (let (
          (curr-char (if (>= i str-length) #f (string-ref trimmed-str i)))
         )
      (cond
        ((>= i str-length) #f) ; end of string => not valid tree
        ((and (whitespace? curr-char) start?) (number (+ i 1) #t opening-brackets balance)) ; whitespace after { 
        ((char-numeric? curr-char) (number (+ i 1) #f opening-brackets balance)) ; digit
        (start? #f) ; no number right after { and arbitrarily number of whitespaces => not valid tree 
        ((eq? curr-char #\*) (right-child (+ i 1) opening-brackets (+ balance 1))) ; proceed to right child
        ((eq? curr-char #\{) (number (+ i 1) #t (+ opening-brackets 1) (+ balance 1))) ; nested child
        ((whitespace? curr-char) (whitespace-after-number (+ i 1) opening-brackets balance)) ; arbitrary number of whitespaces are allowed 
        (else #f) ; anything else is not valid
      )
    )
  )

  (define (whitespace-after-number i opening-brackets balance)
    (let (
          (curr-char (if (>= i str-length) #f (string-ref trimmed-str i)))
         )
      (cond
        ((>= i str-length) #f) ; end of string => not valid tree
        ((eq? curr-char #\*) (right-child (+ i 1) opening-brackets (+ balance 1))) ; proceed to right child
        ((eq? curr-char #\{) (number (+ i 1) #t (+ opening-brackets 1) (+ balance 1))) ; nested child
        ((whitespace? curr-char) (whitespace-after-number (+ i 1) opening-brackets balance))
        (else #f) ; anything else is not valid
      )
    )
  )

  (define (right-child i opening-brackets balance)
    (let (
          (curr-char (if (>= i str-length) #f (string-ref trimmed-str i)))
         )
      (cond
        ((>= i str-length) #f) ; end of string => not valid tree
        ((eq? curr-char #\*) (after-right-child (+ i 1) opening-brackets #t (- balance 1))) ; right child is empty => proceed to closing brackets
        ((eq? curr-char #\{) (number (+ i 1) #t (+ opening-brackets 1) (- balance 1))) ; nested right child
        ((whitespace? curr-char) (right-child (+ i 1) opening-brackets balance))
        (else #f) ; anything else is not valid
      )
    )
  )

  (define (after-right-child i opening-brackets first? balance)
    (let (
          (curr-char (if (>= i str-length) #f (string-ref trimmed-str i)))
         )
      (cond
        ((and (>= i str-length) (not (= opening-brackets 0))) #f) ; end of string but some brackets are not closed => not valid
        ((and (>= i str-length) (= opening-brackets 0) (= balance 0)) #t) ; end of string, all brackets are closed and the nodes are balanced => valid tree
        ((and (>= i str-length) (= opening-brackets 0) (not (= balance 0))) #f) ; end of string and nodes are not balanced => not valid
        ((<= opening-brackets 0) #f) ; reached end of tree but there are still elements => not valid 
        ((eq? curr-char #\}) (after-right-child (+ i 1) (- opening-brackets 1) #f balance)) ; loop again after-right-child. More '}' might appear :)
        ((eq? curr-char #\*) (after-right-child (+ i 1) opening-brackets #t (- balance 1)))
        ((whitespace? curr-char) (after-right-child (+ i 1) opening-brackets #f balance)) ; arbitrary number of whitespaces allowed
        (first? #f)
        ((eq? curr-char #\{) (number (+ i 1) #t (+ opening-brackets 1) (- balance 1))) ; proceeding to right child; cannot be anything else than a right child because of ((<= opening-brackets 0) #f)
        (else #f) ; anything else is not valid
      )
    )
  )
  
  (cond
     ((not (non-empty-string? trimmed-str)) #f) ; empty string passed as input => not valid
     ((equal? trimmed-str "*") #t) ; empty tree: "*", " *", "* ", " * " etc. (\r\n\t) => valid
     ((eq? (string-ref trimmed-str 0) #\{) (number 1 #t 1 0)) ; input starting with left curly bracket ({2 * *})
     (else #f) ; anything else is not valid
  )
)

; tree construction
(define (make-tree root left right) (list root left right))

(define (make-leaf root) (make-tree root '() '()))

; getters
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)


(define (string->tree str)
  (define trimmed-tree (string-replace str " " ""))
  
  (define (helper i); working only with strings with NO whitespaces, tabs, etc
     (make-tree (string->number (extract-root i)) (extract-left i) (extract-right i 0 #t))
  )

  (define (extract-root i) ; always in this pattern {num...}
    (let (
          (curr (string-ref trimmed-tree i))
          )
      (if (or (eq? (string-ref trimmed-tree i) #\{) (eq? (string-ref trimmed-tree i) #\*))
          ""
          (string-append (string (string-ref trimmed-tree i)) (extract-root (+ i 1)))
      )
    )
  )

  (define (extract-left i)
    (let (
          (curr (string-ref trimmed-tree i))
         )
      (cond
        ((char-numeric? curr) (extract-left (+ i 1)))
        ((eq? curr #\*) '())
        ((eq? curr #\{) (helper (+ i 1)))
      )
    )
  )

  (define (extract-right i opening-brackets flag?) ; flag? - indicating whether the left child is passed
    (define (right-helper i) ; we know the i-th element is the right child. This function chooses what to return
      (if (eq? (string-ref trimmed-tree i) #\*) 
          '()
          (helper (+ i 1)))) ; right child is a tree
  
    (let (
          (curr (string-ref trimmed-tree i))
         )
      (cond
        ((and flag? (eq? curr #\{)) (extract-right (+ i 1) (+ opening-brackets 1) #f)) ; left nested child
        ((eq? curr #\{) (extract-right (+ i 1) (+ opening-brackets 1) #f)) ; some deeply-nested grandson :D
        ((and (= opening-brackets 1) (eq? curr #\})) (right-helper (+ i 1))) ; end of left nested child. If the curr element is } and the count of the opening brackets are 1 => the next element is the right child
        ((and flag? (eq? curr #\*)) (right-helper (+ i 1))) ; first appearence of '*' => proceed to right child
        ((or (char-numeric? curr) (eq? curr #\*)) (extract-right (+ i 1) opening-brackets flag?))
        ((eq? curr #\}) (extract-right (+ i 1) (- opening-brackets 1) flag?)) ; end of the some deeply-nested grandson
      )
   )
  )

  (if (tree? str)
      (helper 1) ; working only with string with no spaces or tabs
      #f); if the input is not valid return false
)

(define (tree->string tree)
  (if (null? tree)
      "*"
      (string-append "{" (number->string (root-tree tree)) " " (tree->string (left-tree tree)) " " (tree->string (right-tree tree)) "}")
  )
)

(define (height tree)
   (define (max a b) (if (> a b) a b))
    
   (if (null? tree)
       0
       (+ 1 (max (height (left-tree tree)) (height (right-tree tree))))
    )
  )

(define (balanced? tree)
  (define (abs x)
    (if (< x 0) (* x -1) x)
  )

  (cond
    ((null? tree) #t)
    ((< 1 (abs (- (height (left-tree tree)) (height (right-tree tree))))) #f)
    (else (and (balanced? (left-tree tree)) (balanced? (right-tree tree))))
  )
)

(define (ordered? tree)
  (cond
    ((null? tree) #t)
    ((and (not (null? (left-tree tree))) (< (root-tree tree) (root-tree (left-tree tree)))) #f)
    ((and (not (null? (right-tree tree))) (> (root-tree tree) (root-tree (right-tree tree)))) #f)
    (else (and (ordered? (left-tree tree)) (ordered? (right-tree tree))))
  )
)

(define (compare-streams s1 s2)
  (cond
    ((and (stream-empty? s1) (stream-empty? s2)) #t)
    ((or (and (stream-empty? s1) (not (stream-empty? s2)))
         (and (not (stream-empty? s1)) (stream-empty? s2))) #f)
    ((equal? (stream-first s1) (stream-first s2)) (compare-streams (stream-rest s1) (stream-rest s2)))
    (else #f))
)


(define (tree->stream tree order)
  (if (null? tree)
    empty-stream
    (cond
      ((equal? order 'inorder) (stream-append (tree->stream (left-tree tree) order) (stream (root-tree tree)) (tree->stream (right-tree tree) order)))
      ((equal? order 'preorder) (stream-append (stream (root-tree tree)) (tree->stream (left-tree tree) order) (tree->stream (right-tree tree) order)))
      ((equal? order 'postorder) (stream-append (tree->stream (left-tree tree) order) (tree->stream (right-tree tree) order) (stream (root-tree tree))))
      (else empty-stream)
    )
  )
)
