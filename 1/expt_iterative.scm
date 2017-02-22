(define (expt b n)
  (expt_iter b n 1))

(define (expt_iter b counter product)
  (if (= counter 0)
      product
      (expt_iter b (- counter 1) (* product b))))
