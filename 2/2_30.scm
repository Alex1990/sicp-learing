(define (square x) (* x x))

; #1
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (square tree))
        (else
          (cons (square-tree (car tree))
                (square-tree (cdr tree))))))

(define tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(display (square-tree tree))
(newline)

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

; #2
(define (square-tree1 tree)
  (map (lambda (item)
         (if (pair? item)
             (square-tree1 item)
             (square item)))
       tree))

(display (square-tree1 tree))
(newline)
