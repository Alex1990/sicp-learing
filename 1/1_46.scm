(define (iterative-improve close-enough? improve)
  (define (try guess)
    (let ((next (improve guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (lambda (first-guess)
    (try first-guess)))

(define tolerance 0.00001)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2))
     tolerance))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  ((iterative-improve close-enough?
                     (lambda (guess)
                       (average guess (/ x guess))))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (cube-root x)
  (fixed-point
    (average-damp
      (lambda (y)
        (/ x (square y))))
    1.0))
