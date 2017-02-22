(define (reverse items)
  (define (run-iter a result)
    (if (null? a)
        result
        (run-iter (cdr a)
                  (cons (car a)
                        result))))
  (run-iter items '()))

(define (deep-reverse tree)
  (define (run-iter a result)
    (if (null? a)
        result
        (run-iter (cdr a)
                  (cons (reverse (car a))
                        result))))
  (run-iter tree '()))

(newline)
(display (deep-reverse (list (list 1 2) (list 3 4))))

