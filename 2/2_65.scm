(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else
          (cons (car set)
                (adjoin-set x (cdr set))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1)
                                 set2))
              ((> x1 x2)
               (intersection-set set1
                                 (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1
                         (union-set (cdr set1)
                                (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1)
                                   set2)))
                  ((> x1 x2)
                   (cons x2 (union-set set1
                                       (cdr set2)))))))))
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

(define (list->tree elements)
  (car (partial-tree
         elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
              (quotient (- n 1) 2)))
        (let ((left-result
                (partial-tree
                  elts left-size)))
          (let ((left-tree
                  (car left-result))
                (non-left-elts
                  (cdr left-result))
                (right-size
                  (- n (+ left-size 1))))
            (let ((this-entry
                    (car non-left-elts))
                  (right-result
                    (partial-tree
                      (cdr non-left-elts)
                      right-size)))
              (let ((right-tree
                      (car right-result))
                    (remaining-elts
                      (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define (union-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((result (union-set list1 list2)))
      (list->tree result))))

(define (intersection-set-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((result (intersection-set list1 list2)))
      (list->tree result))))

(define set1 (list->tree (list 1 3 4 5 7)))
(define set2 (list->tree (list 2 4 5 6 8)))

(newline)
(display (union-set-tree set1 set2))

(newline)
(display (intersection-set-tree set1 set2))
