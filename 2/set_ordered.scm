(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

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

(define set1 (list 1 3 4))
(define set2 (list 2 3 4 5))

(newline)
(display (element-of-set? 0 set1))

(newline)
(display (element-of-set? 1 set1))

(newline)
(display (adjoin-set 0 set1))

(newline)
(display (adjoin-set 1 set1))

(newline)
(display (intersection-set set1 set2))
