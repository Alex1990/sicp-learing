(define (equal? a b)
  (cond ((and (pair? a)
              (pair? b))
          (if (eq? (car a) (car b))
              (equal? (cdr a) (cdr b))
              #f))
        ((and (not (pair? a))
              (not (pair? b)))
         (eq? a b))
        (else #f)))
            
(newline)
(display (equal? 'a 'b))

(newline)
(display (equal? 'a 'a))

(newline)
(display (equal? '(this is a list) '(this is a list)))

(newline)
(display (equal? '(this is (a) list) '(this is a list)))

(newline)
(display (equal? #f '()))
