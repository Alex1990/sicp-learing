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

(define (e k)
  (define (n i) 1.0)
  (define (d i)
    (if (= (remainder i 3) 2)
        (* 2.0 (+ 1 (floor (/ i 3))))
        1.0))
  (+ 2 (cont-frac n d k)))

