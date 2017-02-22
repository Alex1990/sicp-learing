(define (accumulate proc init sequence)
  (if (null? sequence)
      init
      (proc (car sequence)
            (accumulate proc init (cdr sequence)))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-vect frame)
  (car frame))

(define (edge1-vect frame)
  (cadr frame))

(define (edge2-vect frame)
  (cadr (cdr frame)))

(define (make-vect x y)
  (list x y))

(define (xcord-vect vect)
  (car vect))

(define (ycord-vect vect)
  (cadr vect))

(define (add-vect . vects)
  (make-vect
    (accumulate +
                0
                (map (lambda (vect)
                       (xcord-vect vect))
                     vects))
    (accumulate +
                0
                (map (lambda (vect)
                       (ycord-vect vect))
                     vects))))

(define (sub-vect . vects)
  (make-vect
    (- (xcodr-vect (car vects))
       (accumulate +
                   0
                   (map (lambda (vect)
                          (xcord-vect vect))
                        (cdr vects))))
    (- (ycodr-vect (car vects))
       (accumulate +
                   0
                   (map (lambda (vect)
                          (ycord-vect vect))
                        (cdr vects))))))

(define (scale-vect vect s)
  (make-vect
    (* s (xcord-vect vect))
    (* s (ycord-vect vect))))

(define (frame-outline frame)
  (let ((origin (origin-vect frame))
        (edge1 (edge1-vect frame))
        (edge2 (edge2-vect frame)))
    (let ((vect (sub-vect (add-vect edge1 edge2)
                         origin)))
      (segments->painter
        (list
          (make-segment origin edge1)
          (make-segment origin edge2)
          (make-segment edge1 vect)
          (make-segment edge2 vect))))))

(define (cross frame)
  (let ((origin (origin-vect frame))
        (edge1 (edge1-vect frame))
        (edge2 (edge2-vect frame)))
    (let ((vect (sub-vect (add-vect edge1 edge2)
                         origin)))
      (segments->painter
        (list
          (make-segment origin vect)
          (make-segment edge1 edge2))))))

(define (diamond frame)
  (let ((origin (origin-vect frame))
        (edge1 (edge1-vect frame))
        (edge2 (edge2-vect frame)))
    (let ((vect (sub-vect (add-vect edge1 edge2)
                         origin)))
      (let ((vect1 (scale-vect (sub-vect edge1 origin) 0.5))
            (vect2 (scale-vect (sub-vect edge2 origin) 0.5)))
        (let ((vect3 (add-vect vect1 (sub-vect edge2 origin)))
              (vect4 (add-vect vect2 (sub-vect edge1 origin))))
          (segments-painter
            (list
              (make-segment vect1 vect2)
              (make-segment vect2 vect3)
              (make-segment vect3 vect4)
              (make-segment vect4 vect1))))))))

; wave painter? wtf.

