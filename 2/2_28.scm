(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (for-each proc items)
  (cond ((null? items) true)
        (else
          (proc (car items))
          (for-each proc (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))
; Performance?
(define (fringe tree)
  (define (walk queue)
    (cond ((null? queue) '())
          ((not (pair? queue))
           (cons queue '()))
          (else
           (append (walk (car queue))
                 (walk (cdr queue))))))
  (walk tree))

(define x
  (list (list 1 2) (list 3 4)))

(display (fringe x))
(newline)

(display (fringe (list x x)))
