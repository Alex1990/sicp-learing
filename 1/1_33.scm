(define (filtered-accumulate combiner
                             null-value
                             term
                             a
                             next
                             b
                             filter)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner
                                     null-value
                                     term
                                     (next a)
                                     next
                                     b
                                     filter))))

(define (square x) (* x x))

(define (identity x) x)

(define (inc n) (+ n 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (divises? a b)
  (= (remainder a b) 0))

(define (prime? n)
  (define (find-prime k)
    (cond ((> (square k) n) #t)
          ((divises? n k) #f)
          (else (find-prime (+ k 1)))))
  (find-prime 2))

(define (sum-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-prime n)
  (define (filter k)
    (= (gcd k n) 1))
  (filtered-accumulate * 1 identity 1 inc n filter))
