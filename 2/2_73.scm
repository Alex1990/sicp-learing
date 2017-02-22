(define (variable x) (symbol? x))

(define (attach-tag tag x) (cons tag x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var)
              1
              0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


; 1.
; (1) `operator` get the type tag of the expression, such as "sum" or "product".
;     `operands` get the operands of the expression.
;
; (2) The `number?` and `variable?` are the 

; 2
(define (install-deriv-sum)
  ;; internal procedures
  (define (addend exp) (car exp))
  (define (augend exp) (cadr exp))
  (define (make-sum a b) (list a b))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'sum x))
  (put 'make-sum 'sum
       (lambda (a b)
         (tag (make-sum a b))))
  (put 'deriv 'sum
       (lambda (exp var)
         (tag (deriv-sum exp var))))
  'done)

(define (install-deriv-product)
  ;; internal procedures
  (define (multiplier exp) (car exp))
  (define (multiplicand exp) (cadr exp))
  (define (make-product a b) (list a b))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'product x))
  (put 'make-product 'product
       (lambda (a b)
         (tag (make-product a b))))
  (put 'deriv 'product
       (lambda (exp var)
         ((get 'make-sum 'sum)
          (tag (make-product
                 (multiplier exp)
                 (deriv (multiplicand exp) var)))
          (tag (make-product
                 (multiplicand exp)
                 (deriv (multiplier exp) var))))))
  'done)

; 3
(define (install-deriv-exponentation)
  ;; internal procedures
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))
  (define (make-exponentation a b) (list a b))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'exponentation x))
  (put 'deriv 'exponentation
       (lambda (exp var)
         ((get 'make-product 'product)
            (exponent exp)
            ((get 'make-product 'product)
              (tag (make-exponentation
                     (base exp)
                     (- (exponent exp) 1)))
              (deriv (base exp) var)))))
  'done)

; 4 Swap the <op> and <type> order in the put's arguments?
