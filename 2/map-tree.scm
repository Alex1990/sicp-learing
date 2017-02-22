(define (map-tree proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (proc tree))
        (else
          (cons (map-tree proc (car tree))
                (map-tree proc (cdr tree))))))

(define tree (list (list (list 1 3) 2) (list 4 5)))

(display tree)
(newline)

(display (map-tree (lambda (x) (* x x)) tree))
