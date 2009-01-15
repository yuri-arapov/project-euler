;; generator.scm
;;
;;


(define (make-generator from to)
  (let ((n  from)
        (to to))
    (lambda ()
      (if (> n to)
        #f
        (let ((r n))
          (set! n (+ n 1))
          r)))))


;; end of file
