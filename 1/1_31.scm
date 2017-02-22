(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (product-inner-iter term a next b 1))

(define (product-inner-iter term a next b result)
  (if (> a b)
      result
      (product-inner-iter term
                          (next a)
                          next
                          b
                          (* result
                             (term a)))))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial n)
  (product identity 1 inc n))

(define (factorial-iter n)
  (product-iter identity 1 inc n))

(define (pi n)
  (define (term a)
    (if (even? a)
        (/ (+ a 2)
           (+ a 1))
        (/ (+ a 1)
           (+ a 2))))
  (* 4.0 (product term 1 inc n)))

(define (pi-iter n)
  (define (term a)
    (if (even? a)
        (/ (+ a 2)
           (+ a 1))
        (/ (+ a 1)
           (+ a 2))))
  (* 4.0 (product-iter term 1 inc n)))

