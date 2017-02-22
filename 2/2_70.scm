(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch
                  (car bits)
                  current-branch)))
          (if (leaf? next-branch)
              (cons
                (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit:
                     CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
          (cons (car set)
                (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
          (make-leaf (car pair)
                     (cadr pair))
          (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append
        (encode-symbol (car message)
                       tree)
        (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '() 
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (cond ((exist? symbol (symbols left))
               (cons 0
                     (encode-symbol symbol left)))
              ((exist? symbol (symbols right))
               (cons 1
                     (encode-symbol symbol right)))
              (else (error "Symbol is not in the tree " symbol))))))

(define (exist? symbol symbol-set)
  (cond ((null? symbol-set) #f)
        ((eq? symbol (car symbol-set)) #t)
        (else (exist? symbol (cdr symbol-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge
    (make-leaf-set pairs)))

(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (successive-merge
        (adjoin-set
          (make-code-tree
            (car set)
            (cadr set))
          (cddr set)))))

(define rock-song-lyrics '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define rock-song-tree (generate-huffman-tree rock-song-lyrics))

(newline)
(display rock-song-tree)

(newline)
(display (encode '(Get a job) rock-song-tree))

(newline)
(display (encode '(Sha na na na na na na na na) rock-song-tree))

(newline)
(display (encode '(Get a job) rock-song-tree))

(newline)
(display (encode '(Sha na na na na na na na na) rock-song-tree))

(newline)
(display (encode '(Wah yip yip yip yip) rock-song-tree))

(newline)
(display (encode '(yip yip yip yip yip) rock-song-tree))

(newline)
(display (encode '(Sha boom) rock-song-tree))

; (* 36 3) bits
