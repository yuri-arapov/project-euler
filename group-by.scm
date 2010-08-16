; Group elements of input sequence s by n.                                                          
;; Example:
;;   (group-by 2 (iota 10)) -> ((0 1) (2 3) (4 5) (6 7) (8 9))
;;
(define (group-by n s)

  (define (update-res el res) (cons (reverse el) res))

  (let loop ((s s)
             (el '())
             (el-len 0)
             (res '()))
    (cond
      ((null? s)
       (reverse (update-res el res)))

      ((= n el-len)
       (loop s '() 0 (update-res el res)))

      (else
        (loop (cdr s) (cons (car s) el) (1+ el-len) res)))))


;; end of file
;; vim: ts=4 sw=4
