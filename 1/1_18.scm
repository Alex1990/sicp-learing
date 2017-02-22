(define (fast_multiply a b)
  (cond ((= a 0) 0)
        (else (fast_multiply_iter a b 0))))

(define (fast_multiply_iter a b p)
  (if (= b 0)
      p
      (if (is_even b)
          (fast_multiply_iter (double a)
                              (- (halve b) 1)
                              (+ p (double a)))
          (fast_multiply_iter a
                              (- b 1)
                              (+ p a)))))

(define (is_even a)
  (if (= (remainder a 2) 0)
      #t
      #f))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))
