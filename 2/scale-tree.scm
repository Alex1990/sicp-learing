(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (* tree factor))
        (else
          (cons (scale-tree (car tree) factor)
              (scale-tree (cdr tree) factor)))))

(define tree (list (list (list 1 2) 3) (list 4 5)))

(display tree)
(newline)

(display (scale-tree tree 2))
