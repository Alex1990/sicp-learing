(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
                     (- n 1))))

(define squares (list 1 4 9 16 25))

(display (list-ref squares 3))
(newline)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(display (length odds))
(newline)

(define (length1 items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a )
                     (+ 1 count))))
  (length-iter items 0))

(display (length1 odds))
(newline)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2))))
