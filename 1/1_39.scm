(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (iter (+ i 1))))))
  (iter 1))

(define (cont-frac-iter n d k)
  (define (iter i v)
      (if (= i 0)
          v
          (iter (- i 1)
                (/ (n i)
                   (+ (d i)
                      v)))))
  (iter k 0))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- 0 (* x x))))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac n d k))
