(define (fast_multiply a b)
  (cond ((= a 0) 0)
        ((= b 0) 0)
        ((= b 1) a)
        ((is_even b)
         (fast_multiply (double a) (halve b)))
        (else
          (+ a
             (fast_multiply a (- b 1))))))

(define (is_even a)
  (if (= (remainder a 2) 0)
      #t
      #f))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))
