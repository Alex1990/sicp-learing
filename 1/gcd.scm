(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2


a0 = 206 
b0 = 40

(gcd 206 40)
(gcd a0 b0)

(gcd 40 (remainder 206 40))

a1 = b0 = 40
b1 = (remainder a0 b0)

(gcd a1 b1)

(if (= b1 0)
    a1 
    (gcd b1 (remainder a1 b1)))

(if (= 6 0)
    a1
    (gcd b1 (remainder a1 b1)))

(gcd a2 b2)

a2 = b1
b2 = (remainder a1 b1)

(if (= b2 0)
    a2
    (gcd b2 (remainder a2 b2)))

(if (= (remainder a1 b1) 0)
    a2
    (gcd b2 (remainder a2 b2)))
    
(if (= (remainder b0 (remainder a0 b0)) 0)
    a2
    (gcd b2 (remainder a2 b2)))
    

(if (= 4 0)
    a2
    (gcd b2 (remainder a2 b2)))
    
(gcd a3 b3)

a3 = b2;
b3 = (remainder a2 b2)

(if (= b3 0)
    a3
    (gcd b3 (remainder a3 b3)))

(if (= (remainder a2 b2) 0)
    a3
    (gcd b3 (remainder a3 b3)))

(if (= (remainder b1 (remainder a1 b1)) 0)
    a3
    (gcd b3 (remainder a3 b3)))

(if (= (remainder (remainder a0 b0) (remainder b0 (remainder a0 b0))) 0)
    a3
    (gcd b3 (remainder a3 b3)))

(if (= 2 0)
    a3
    (gcd b3 (remainder a3 b3)))

(gcd a4 b4)

a4 = b3
b4 = (remainder a3 b3)

(if (= b4 0)
    a4
    (gcd b4 (remainder a4 b4)))

(if (= (remainder (remainder b0
                             (remainder a0 b0)) 
                  (remainder (remainder a0 b0) 
                             (remainder b0
                                        (remainder a0 b0)))) 0)
    a4
    (gcd b4 (remainder a4 b4)))

a4

b3

(remainder a2 b2)

(remainder b1
           (remainder a1 b1))

(remainder (remainder a0 b0)
           (remainder b0
                      (remainder a0 b0)))

