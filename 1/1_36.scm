(define tolerance 0.00001)

(define (close-enough? a b)
  (< (abs (- a b)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (answer1 first-guess)
  (fixed-point
    (lambda (x) (/ (log 1000) (log x)))
    first-guess))

(define (average x y)
  (/ (+ x y) 2.0))

(define (answer2 first-guess)
  (fixed-point
    (lambda (x) (average x (/ (log 1000) (log x))))
    first-guess))
