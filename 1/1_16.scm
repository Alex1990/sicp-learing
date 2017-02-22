(define (fast_expt b n)
  (fast_expt_iter b n 1))

(define (fast_expt_iter b n a)
  (if (= n 0)
      a
      (if (is_even n)
          (fast_expt_iter
            (square b)
            (- (/ n 2) 1)
            (* a (square b)))
          (fast_expt_iter
            b
            (- n 1)
            (* a b)))))

(define (is_even n)
  (if (= (remainder n 2) 0)
      #t
      #f))

(define (square x)
  (* x x))
