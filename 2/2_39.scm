(define (reverse1 sequence)
  (fold-right
    (lambda (x y)
      (display x)
      (newline)
      (display y)
      (newline)
      (append y
              (list x)))
    '()
    sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; Understand?
(define (reverse2 sequence)
  (fold-left
    (lambda (x y)
      (display x)
      (newline)
      (display y)
      (newline)
      (append (list y) 
              x))
    '()
    sequence))

(define items (list 1 2 3 4 5))

(display (append items (list 6 7)))
(newline)

(display (reverse1 items))
(newline)

(display (reverse2 items))
(newline)
