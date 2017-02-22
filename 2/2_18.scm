; Iterative process
(define (reverse items)
  (define (run-iter items new)
    (if (null? items)
        new
        (let ((item (car items)))
          (run-iter (cdr items)
                    (cons item new)))))
  (run-iter items '()))

; Recursive process?

(display (reverse (list 1 4 9 16 25)))
(newline)

