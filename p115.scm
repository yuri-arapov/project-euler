;; 2010-12-23
;;
;; Project Euler
;;
;; Problem 115
;; 24 February 2006
;;
;; NOTE: This is a more difficult version of problem 114.
;;
;; A row measuring n units in length has red blocks with a minimum length of m
;; units placed on it, such that any two red blocks (which are allowed to be
;; different lengths) are separated by at least one black square.
;;
;; Let the fill-count function, F(m, n), represent the number of ways that a
;; row can be filled.
;;
;; For example, F(3, 29) = 673135 and F(3, 30) = 1089155.
;;
;; That is, for m = 3, it can be seen that n = 30 is the smallest value for
;; which the fill-count function first exceeds one million.
;;
;; In the same way, for m = 10, it can be verified that F(10, 56) = 880711 and
;; F(10, 57) = 1148904, so n = 57 is the least value for which the fill-count
;; function first exceeds one million.
;;
;; For m = 50, find the least value of n for which the fill-count function
;; first exceeds one million.
;;
;; Answer: 168
;;  
;;

(define (fill-count m n)

  (let ((memo (make-vector (1+ n) #f)))

    (define (memo-ref n)   (vector-ref  memo n))
    (define (memo-set n x) (vector-set! memo n x) x)

    (define (helper n)
      (cond 
        ((< n m) 0)   ;; nothing
        ((= n m) 2)   ;; xxx and ooo
        (else
          (or (memo-ref n)
              (memo-set 
                n 
                (let loop ((left 0)
                           (len  m)
                           (acc  0))
                  (cond 
                    ((> (+ left len) n) (loop 0 (1+ len) acc))
                    ((= len n)          (+ 2 acc))
                    (else
                      (loop 
                        (1+ left)
                        len
                        (+ acc (max 1 (helper (- n (+ left len 1))))))))))))))
    (helper n)))


(define (upper-bound m count)
  (let loop ((n m))
    (if (>= (fill-count m n) count)
      n
      (loop (* 2 n)))))


(define (p115-int m count)
  (let loop ((iter 0)   ;; iter is to see number of iterations
             (a m)
             (b (upper-bound m count)))
    (if (= 1 (- b a))
      (values b iter)
      (let ((c (quotient (+ a b) 2)))
        (if (< (fill-count m c) count)
          (loop (1+ iter) c b)
          (loop (1+ iter) a c))))))


(define (p115)
  (p115-int 50 1000000))

;; end of file
;; vim: ts=4 sw=4 et
