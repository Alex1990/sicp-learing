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

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(newline)
(display (unique-triples 10))

(define (sum-of-triple triple)
  (+ (car triple)
     (cadr triple)
     (cadr (cdr triple))))

(define (sum-triples n s)
  (accumulate
    cons
    '()
    (filter
      (lambda (triple)
        (= (sum-of-triple triple) s))
      (unique-triples n))))

(newline)
(display (sum-triples 10 15))
