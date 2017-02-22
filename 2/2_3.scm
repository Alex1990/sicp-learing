(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-rect p1 p2)
  (let ((w (abs (- (x-point p1)
                   (x-point p2))))
        (h (abs (- (y-point p1)
                   (y-point p2)))))
    (cons (cons p1 p2)
          (cons w h))))

(define (rect-lefttop rect)
  (car (car rect)))

(define (rect-rightbottom rect)
  (cdr (car rect)))

(define (print-rect rect)
  (let ((p1 (rect-lefttop rect))
        (p2 (rect-rightbottom rect))
        (w (rect-width rect))
        (h (rect-height rect)))
    (display "p1: ")
    (print-point p1)
    (display ", p2: ")
    (print-point p2)
    (display ", w: ")
    (display w)
    (display ", h: ")
    (display h)
    (newline)))

(define (perimeter rect)
  (let ((p1 (rect-lefttop rect))
        (p2 (rect-rightbottom rect)))
    (* (+ (abs (- (x-point p1)
                  (x-point p2)))
          (abs (- (y-point p1)
                  (y-point p2))))
       2)))

(define (area rect)
  (let ((p1 (rect-lefttop rect))
        (p2 (rect-rightbottom rect)))
    (* (abs (- (x-point p1)
                (x-point p2)))
        (abs (- (y-point p1)
                (y-point p2))))))

(define (make-rect-alt x y w h)
  (cons (make-point x y)
        (cons w h)))

(define (rect-width rect)
  (car (cdr rect)))

(define (rect-height rect)
  (cdr (cdr rect)))

(define (perimeter-alt rect)
  (let ((w (rect-width rect))
        (h (rect-height rect)))
    (* (+ w h) 2)))

(define (area-alt rect)
  (let ((w (rect-width rect))
        (h (rect-height rect)))
    (* w h)))

(define (perimeter-generic rect)
  (let ((w (rect-width rect))
        (h (rect-height rect)))
    (* (+ w h) 2)))

(define (area-generic rect)
  (let ((w (rect-width rect))
        (h (rect-height rect)))
    (* w h)))

(define a (make-point 0 0))
(define b (make-point 3 2))
(define w 3)
(define h 2)
(define rect (make-rect a b))
(define rect-alt (make-rect-alt 0 0 w h))

(define (print x)
  (display x)
  (newline))

; Representation 1
(print-rect rect)
(print (perimeter rect))
(print (area rect))

; Representation 2
(print (perimeter-alt rect-alt))
(print (area-alt rect-alt))

; Generic perimeter and area
(print (perimeter-generic rect))
(print (perimeter-generic rect-alt))
(print (area-generic rect))
(print (area-generic rect-alt))

