(define (accumulate proc init sequence)
  (if (null? sequence)
      init
      (proc (car sequence)
            (accumulate proc init (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ low 1)
                                high))))

(define (flatmap proc sequence)
  (accumulate append '() (map proc sequence)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions)
            (safe? k positions))
          (flatmap
            (lambda (new-row)
              (map (lambda (rest-of-queens)
                     (adjoin-position
                       new-row k rest-of-queens))
                   (queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define empty-board '())

(define (safe? k positions)
  (if (= k 1)
      #t
      (let ((new-pos (car positions))
            (last-pos (cadr positions)))
        (accumulate
          (lambda (x y)
            (and x y))
          #t
          (map (lambda (pos)
                 (and
                   (not (= (car new-pos)
                           (car pos)))
                   (> (abs (- (car new-pos)
                              (car last-pos)))
                      1)))
               (cdr positions))))))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

;(for-each (lambda (positions)
;            (newline)
;            (display positions))
;          (queens 7))

(define start (runtime))

(queens 7)

(newline)
(display (- (runtime) start))
