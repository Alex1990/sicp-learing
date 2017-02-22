(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n k)
  (cond ((> (square k) n) n)
        ((divises? n k) k)
        (else (find-divisor n (+ k 1)))))

(define (odd? n)
  (= (remainder n 2) 1))

(define (square a)
  (* a a))

(define (divises? n k)
  (= (remainder n k) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime)
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a n)
  (if (odd? a)
      (find-primes a n)
      (find-primes (+ a 1) n)))

(define (prime-count n)
  (if (prime? n)
      1
      0))

(define (find-primes a n)
  (if (> n 0)
      (AND (timed-prime-test a)
        (find-primes (+ a 2) (- n (prime-count a))))))

