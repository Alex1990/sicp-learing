(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product
             (multiplier exp)
             (deriv (multiplicand exp) var))
           (make-product
             (deriv (multiplier exp) var)
             (multiplicand exp))))
        (else (error "unknow expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum . args)
  (cons '+ args))

(define (addend s)
  (cadr s))

(define (augend s)
  (let ((a (cdddr s)))
    (if (pair? a)
        (apply make-sum (cddr s))
        (caddr s))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (make-product . args)
  (cons '* args))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (let ((m (cdddr p)))
    (if (pair? m)
        (apply make-product (cddr p))
        (caddr p))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(newline)
(display (deriv '(+ x 3) 'x))

(newline)
(display (deriv '(* x y) 'x))

(newline)
(display (deriv '(* (* x y) (+ x 3)) 'x))

(newline)
(display (deriv '(* x y (+ x 3)) 'x))

