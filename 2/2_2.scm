(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment a b)
  (cons a b))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment l)
  (let ((a (start-segment l))
        (b (end-segment l)))
    (make-point (average (x-point a)
                         (x-point b))
                (average (y-point a)
                         (y-point b)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define p1 (make-point 2 3))
(define p2 (make-point 9 5))
(print-point p1)
(print-point p2)
(define line (make-segment p1 p2))
(print-point (midpoint-segment line))
