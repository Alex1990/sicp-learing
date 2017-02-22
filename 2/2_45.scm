(define (split proc1 proc2)
  (define (smaller painter n)
    (if (= n 0)
        painter
        (let ((small (smaller painter
                              (- n 1))))
          (proc1 painter small))))
  smaller)

(define right-split (split beside below))

(define up-split (split below beside))
