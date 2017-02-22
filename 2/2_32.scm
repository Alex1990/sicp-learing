(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (item)
                       (cons (car s) item))
                     rest)))))

(display (append (list 1 2) (list 3 4 5)))
(newline)

(display (map (lambda (x) (* x x)) (list 1 2 3)))
(newline)

(display (subsets (list 1 2 3)))
