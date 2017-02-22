(define (accumulate proc init sequence)
  (if (null? sequence)
      init
      (proc (car sequence)
            (accumulate proc init (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ low 1)
                                high))))

(define (flatmap proc sequence)
  (accumulate append '() (map proc sequence)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (square x) (* x x))

(define (prime? n)
  (define (prime-iter i)
    (cond ((> (square i) n) #t)
          ((not (= (gcd n i) 1)) #f)
          (else
            (prime-iter (+ i 1)))))
  (prime-iter 1))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (list (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
         prime-sum?
         (unique-pairs n))))

(for-each (lambda (x)
            (newline)
            (display x))
          (prime-sum-pairs 10))

