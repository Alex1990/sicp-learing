(define pi 3.141592654)

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (integral f a b n)
  (define (factor k)
    (cond ((OR (= k 1)
               (= k n))
           1)
          ((even? k) 2)
          (else 4)))
  (define h (/ (- b a) n))
  (define (term k)
    (* (factor k)
       (f (+ a (* k h)))))
  (define (next x)
    (+ x 1))

  (* (/ h 3.0)
     (sum term 0 next n)))

(define (cube-integral a b n)
  (integral cube a b n))
