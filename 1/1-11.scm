(define (recur_f n)
  (if (< n 3)
      n
      (+ (recur_f (- n 1))
         (+ (* 2 (recur_f (- n 2)))
            (* 3 (recur_f (- n 3)))))))

(define (iter_f n)
  (iter_f-iter 2 1 0 n))

(define (iter_f-iter a b c n)
  (cond ((= n 0) c)
        ((= n 1) b)
        ((= n 2) a)
        (else (iter_f-iter (+ a (+ (* 2 b)
                             (* 3 c))) a b (- n 1)))))
