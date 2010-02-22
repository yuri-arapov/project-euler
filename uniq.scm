;; uniq.scm


;; Remove equal consecutive elements from the list.
;;
(define (uniq s)
  (if (null? s)
    s
    (let loop ((s (cdr s))
               (res (list (car s))))
      (cond ((null? s)
             (reverse res))
            ((= (car s) (car res))
             (loop (cdr s) res))
            (else
              (loop (cdr s) (cons (car s) res)))))))


;; Alias for uniq
;;
(define undup uniq)

;; end of file
