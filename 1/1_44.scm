(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (repeated (compose f f) (- n 1))))

(define dx 0.1)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (smooth-n f n)
  ((repeated smooth n) f))

((smooth (lambda (x) (* x x))) 2)

((smooth-n (lambda (x) (* x x)) 3) 2)
