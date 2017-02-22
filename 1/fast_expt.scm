(define (fast_expt b n)
  (if (= n 0)
      1
      (if (is_even n)
          (square (fast_expt b (/ n 2)))
          (* b (fast_expt b (- n 1))))))

(define (is_even n)
  (if (= (remainder n 2) 0)
      #t
      #f))

(define (square x)
  (* x x))

