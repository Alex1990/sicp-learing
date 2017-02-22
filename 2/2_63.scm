(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
        (tree->list-1
          (left-branch tree))
        (cons (entry tree)
              (tree->list-1
                (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
          (left-branch tree)
          (cons (entry tree)
                (copy-to-list
                  (right-branch tree)
                  result-list)))))
  (copy-to-list tree '()))

(define test-tree1 (make-tree 1 '() '()))
(define test-tree2
  (make-tree 1
             '()
             (make-tree 2 '() '())))

(newline)
(display (tree->list-1 test-tree1))
(newline)
(display (tree->list-2 test-tree1))
(newline)
(display (tree->list-1 test-tree2))
(newline)
(display (tree->list-2 test-tree2))

; Produce the same result for every tree? Yes.


(define tree1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define tree2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9
                                   '()
                                   (make-tree 11 '() '())))))

(define tree3
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))

; The lists produced for the trees in Figure 2.16

(newline)
(display (tree->list-1 tree1))
(newline)
(display (tree->list-1 tree2))
(newline)
(display (tree->list-1 tree3))

(newline)
(display (tree->list-2 tree1))
(newline)
(display (tree->list-2 tree2))
(newline)
(display (tree->list-2 tree3))

; tree->list-1: O(n^2)
; tree->list-2: O(n)
; tree->list-2 grows more slowly
