(define (reverse items)
  (define (run-iter a result)
    (if (null? a)
        result 
        (run-iter (cdr a)
                  (cons (car a)
                        result))))
  (run-iter items '()))

(define (same-parity . ints)
  (define (run-iter r items result)
    (if (null? items)
        result
        (let ((item (car items)))
          (if (= (remainder item 2)
                 r)
              (run-iter r
                         (cdr items) 
                         (cons item
                               result))
              (run-iter r
                        (cdr items)
                        result)))))
  (if (null? ints)
      '()
      (let ((k (remainder (car ints) 2)))
        (run-iter k (reverse ints) '()))))

(display (reverse (list 1 2 3 4 5)))
(newline)

(display (same-parity 1 2 3 4 5 6 7))
(newline)

(display (same-parity 2 4 5 9 10))
(newline)
