(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

; + +, + +
; + +, - + 
; + +, - -
; - +, + +
; - +, - + (need more than two multiplications)
; - +, - -
; - -, + +
; - -, - +
; - -, - -
(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y))
    (cond ((> lx 0)
           (cond ((> ly 0) (make-interval (* lx ly) (* ux uy)))
                 ((< uy 0) (make-interval (* ux ly) (* lx uy)))
                 (else (make-interval (* ux ly) (* ux uy)))))
          ((< ux 0)
           (cond ((> ly 0) (make-interval (* lx ly) (* ux uy)))
                 ((< uy 0) (make-interval (* lx uy) (* lx ly)))
                 (else (make-interval (* ux uy) (* lx ly)))))
          (else
            (cond ((> ly 0) (make-interval (* lx uy) (* ux uy)))
                  ((< uy 0) (make-interval (* ux ly) (* lx ly)))
                  (else
                    (make-interval (min (* lx uy) (* ux ly))
                                   (max (* lx ly) (* ux uy))))))))))

;Hard to understand this
(define (div-interval x y)
  (let ((uy (upper-bound y))
        (ly (lower-bound y)))
    (if (> (* uy ly) 0)
        (mul-interval x
                      (make-interval
                        (/ 1.0 (upper-bound y))
                        (/ 1.0 (lower-bound y))))
        (error "The interval divied by must not span zero"))))

