(define (gcd m n)
  (let ((a (abs m))
        (b (abs n)))
    (if (< a b)
        (gcd b a)
        (let ((k (remainder a b)))
          (if (= k 0)
              b 
              (gcd b k))))))

(define (make-rat n d)
  (if (< d 0)
    (make-rat (- 0 n) (- 0 d))
    (let ((g (gcd n d)))
          (cons (/ n g)
                (/ d g)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x)
               (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x)
               (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

