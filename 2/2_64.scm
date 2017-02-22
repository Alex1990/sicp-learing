(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree
         elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
              (quotient (- n 1) 2)))
        (let ((left-result
                (partial-tree
                  elts left-size)))
          (let ((left-tree
                  (car left-result))
                (non-left-elts
                  (cdr left-result))
                (right-size
                  (- n (+ left-size 1))))
            (let ((this-entry
                    (car non-left-elts))
                  (right-result
                    (partial-tree
                      (cdr non-left-elts)
                      right-size)))
              (let ((right-tree
                      (car right-result))
                    (remaining-elts
                      (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; 接受一组元素 elts 和一个长度值 n 作为参数
; 如果 n 等于 0，则返回结果：(() . elts)
; 否则，设定：
; - left-size 为左分支元素数，其值为 (n - 1)/2，向下取整
; - left-result 为左分支 partial-tree 结果，通过递归获取
; - left-tree 为左分支
; - non-left-elts 为除了左分支的其他元素
; - right-size 为右分支元素数，其值为 (n - (left-size + 1))
; - this-entry 为除了左分支和右分支剩下的一个元素，其值大于所有左分支元素，小于所有右分支元素
; - right-result 为右分支的 partial-tree 结果，通过递归获取
; - right-tree 为右分支
; - remaining-elts 为 elts 当中非前 n 个元素集合
; 根据 this-entry，left-tree，right-tree 创建树 Tree
; 返回结果 (cons Tree remaining-elts)

(newline)
(display (list->tree (list 1 3 5 7 9 11)))
; Output: (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;
;           5
;         ------
;        /      \
;        1       9
;        -      ---
;         \    /   \
;          3  7     11

; O(n)，每个元素都只会执行一次 partial-tree
