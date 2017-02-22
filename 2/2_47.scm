(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame1 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-vect frame)
  (car frame))

(define (edge1-vect frame)
  (cadr frame))

(define (edge2-vect frame)
  (cadr (cdr frame)))

(define (origin-vect1 frame)
  (car origin))

(define (edge1-vect1 frame)
  (cadr frame))

(define (edge2-vect1 frame)
  (cdr (cdr frame)))
