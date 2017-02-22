(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (identity x) x)

(define (inc n) (+ n 1))

(define (add a b) (+ a b))

(define (multiply a b) (* a b))

(define (sum a b)
  (accumulate + 0 identity a inc b))

(define (product a b)
  (accumulate * 1 identity a inc b))

(define (sum-iter a b)
  (accumulate-iter + 0 identity a inc b))

(define (product-iter a b)
  (accumulate * 1 identity a inc b))

