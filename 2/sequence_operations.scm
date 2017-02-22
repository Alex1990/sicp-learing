(define (map proc sequence)
  (if (null? sequence)
      '()
      (cons (proc (car sequence))
            (map proc (cdr sequence)))))

(define (filter predicate sequence)
  (if (null? sequence)
      '()
      (if (predicate (car sequence))
          (cons (car sequence)
                (filter predicate (cdr sequence)))
          (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ low 1) high))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else
          (append
            (enumerate-tree (car tree))
            (enumerate-tree (cdr tree))))))

(define (square x) (* x x))

(define (sum-odds-tree tree)
  (accumulate
    +
    0
    (map square
         (filter odd?
                 (enumerate-tree tree)))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else
          (+ (fib (- n 1))
             (fib (- n 2))))))

(define (even-fibs n)
  (accumulate
    cons
    '()
    (filter even?
            (map fib
                 (enumerate-interval 0 n)))))

(display (sum-odds-tree (list 1 (list 2 (list 3 4)) 5)))
(newline)

(display (even-fibs 10))
(newline)

