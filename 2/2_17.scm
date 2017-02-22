(define (last-pair items)
  (if (null? items)
      items
      (let ((next (cdr items)))
        (if (null? next)
            items
            (last-pair next)))))

(display (last-pair (list 23 72 149 34)))
(newline)

