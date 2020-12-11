#lang racket

(require rackunit rackunit/gui)

(require "tree.rkt")

(define (expect-valid str)
  (test-true
    (string-append "String '" str "' should be a valid binary tree")
    (tree? str)
  )
)

(define (expect-valid-extra-whitespaces str)
  (test-true
   (string-append "Arbitrary number of whitespaces, tabs and new lines do not affect validity of the string " (string-replace (string-trim str) " " ""))
   (tree? str)
  )
)

(define (expect-invalid str)
  (test-false
   (string-append "String " str " should not be a valid binary tree")
   (tree? str)
  )
)

(define (expect-balanced tree)
  (test-true
   (string-append "The tree " (tree->string tree) " should be a balanced binary tree")
   (balanced? tree)
  )
)

(define (expect-unbalanced tree)
  (test-false
   (string-append "The tree " (tree->string tree) " should NOT be a balanced binary tree")
   (balanced? tree)
  )
)

(define (expect-ordered tree)
  (test-true
   (string-append "The tree " (tree->string tree) " should be an ordered binary tree")
   (ordered? tree)
  )
)

(define (expect-unordered tree)
  (test-false
   (string-append "The tree " (tree->string tree) " should NOT be an ordered binary tree")
   (ordered? tree)
  )
)

(define (expect-equal-streams s1 s2)
  (test-true
   (string-append "Stream should be equal")
   (compare-streams s1 s2)
  )
)

(define (expect-unequal-streams s1 s2)
  (test-false
   (string-append "Stream should NOT be equal")
   (compare-streams s1 s2)
  )
)

(define empty-tree '())
(define no-leafs-tree '(1 () ()))
(define single-child-tree '(1 (2 () ()) ()))
(define balanced-tree '(1 (2 () (3 () ())) (4 () ())))
(define bst '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))

(test/gui

 (test-suite
  "tree?"
  (expect-valid "*")
  (expect-valid "{1**}")
  (expect-valid-extra-whitespaces " *  ")
  (expect-valid-extra-whitespaces " \t*  \t  \r\n")
  (expect-valid-extra-whitespaces "\t { 1  \t{2 *  *}       *   }\n")
  (expect-valid "{1 {2 {3 * *} {4 * {5 * *}}} {6 * *}}")
  (expect-invalid "")
  (expect-invalid "{}")
  (expect-invalid "{*}")
  (expect-invalid "{* * *}")
  (expect-invalid "{1 * *")
  (expect-invalid "1 * * ")
  (expect-invalid "{{2 * *} * *}")
  (expect-invalid "{1 {1 * *} * *}")
  (expect-invalid "{1 * * *}")
  (expect-invalid "{1 a b}")
  (expect-invalid "{1 * *} 12")
 )

 (test-suite
  "tree->string"
  (test-equal? "The empty tree '() should be displayed correctly" (tree->string empty-tree) "*")
  (test-equal? "The tree with no leafs '(1 () ()) should be displayed correctly" (tree->string no-leafs-tree) "{1 * *}")
  (test-equal? "The tree with nested node '(1 (2 () ()) ()) should be displayed correctly" (tree->string single-child-tree) "{1 {2 * *} *}")
  (test-equal? "The tree with deeply nested nodes '(1 (2 (3 () ()) ()) ()) should be displayed correctly" (tree->string '(1 (2 (3 () ()) ()) ())) "{1 {2 {3 * *} *} *}")
 )

 (test-suite
  "height works correctly"
  (test-eqv? "Empty tree should have height of 0" (height empty-tree) 0)
  (test-eqv? "Tree with no leafs should have a height of 1" (height no-leafs-tree) 1)
  (test-eqv? "Tree with a single child node should have a height of 2" (height single-child-tree) 2)
  (test-eqv? (string-append "The tree " (tree->string balanced-tree) " should have a height of 3") (height balanced-tree) 3)
 )

 (test-suite
  "tree helpers work correctly"
  (test-equal? "make-leaf constructs correctly the childless tree {1 * *}" (make-leaf 1) no-leafs-tree)
  (test-equal? "make-tree correctly builds the tree {1 {2 * *} *}" (make-tree 1 (make-leaf 2) empty-tree) single-child-tree)
 )

 (test-suite
  "balanced?"
  (expect-balanced empty-tree)
  (expect-balanced no-leafs-tree)
  (expect-balanced single-child-tree)
  (expect-balanced '(1 (2 () ()) (3 () ())))
  (expect-balanced balanced-tree)
  (expect-unbalanced '(1 (2 () ()) (3 (4 () (5 () ())) (4 () ()))))
  (expect-unbalanced '(1 (2 (3 () ()) ()) ()))
  (expect-unbalanced '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))
 )

 (test-suite
  "ordered?"
  (expect-ordered empty-tree)
  (expect-ordered no-leafs-tree)
  (expect-ordered '(1 (0 () ()) (2 () ())))
  (expect-ordered bst)
  (expect-unordered single-child-tree)
  (expect-unordered '(1 (2 () ()) (0 () ())))
  (expect-unordered balanced-tree)
 )

 (test-suite
  "compare-streams"
  (expect-equal-streams (stream 1 2 3 4) (stream 1 2 3 4))
  (expect-equal-streams empty-stream empty-stream)
  (expect-equal-streams (stream-cons 1 empty-stream) (stream-cons 1 empty-stream))
  (expect-unequal-streams (stream 1 2) (stream 1))
  (expect-unequal-streams empty-stream (stream 1 2 3 4))
 )

 (test-suite
  "tree->stream"
  (test-true "Invalid order results in a empty-stream" (compare-streams (tree->stream bst 'someorder) empty-stream))
  (test-true "All orders of the signle noded tree '(1 () ()) give the stream (1)" (and (compare-streams (tree->stream no-leafs-tree 'inorder) (stream 1)) (compare-streams (tree->stream no-leafs-tree 'preorder) (stream 1)) (compare-streams (tree->stream no-leafs-tree 'postorder) (stream 1))))
  (test-true (string-append "Inorder of the the tree " (tree->string bst) " results in a stream with values (1 3 4 6 7 8 10 13 14)") (compare-streams (tree->stream bst 'inorder) (stream 1 3 4 6 7 8 10 13 14)))
  (test-true (string-append "Preorder of the the tree " (tree->string bst) " results in a stream with values (8 3 1 6 4 7 10 14 13)") (compare-streams (tree->stream bst 'preorder) (stream 8 3 1 6 4 7 10 14 13)))
  (test-true (string-append "Postorder of the the tree " (tree->string bst) " results in a stream with values (1 4 7 6 3 13 14 10 8)") (compare-streams (tree->stream bst 'postorder) (stream 1 4 7 6 3 13 14 10 8)))
 )
)