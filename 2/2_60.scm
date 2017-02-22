; Low efficiency
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; High efficiency
(define (adjoin-set x set)
  (cons x set))

; Low efficiency
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)

; High efficiency
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1)
                 (adjoin-set (car set1)
                  set2))))

(define set1 (list 1 3 4))
(define set2 (list 2 3 4 5))

(newline)
(display (union-set set1 set2))
