;; 2011-02-02
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=122
;;
;; Problem 122
;; 02 June 2006
;;
;; The most naive way of computing n15 requires fourteen multiplications:
;;
;; n * n * ... * n = n15
;;
;; But using a "binary" method you can compute it in six multiplications:
;;
;; n    * n   = n^2
;; n^2  * n^2 = n^4
;; n^4  * n^4 = n^8
;; n^8  * n^4 = n^12
;; n^12 * n^2 = n^14
;; n^14 * n   = n^15
;;
;; However it is yet possible to compute it in only five multiplications:
;;
;; n    * n   = n^2
;; n^2  * n   = n^3
;; n^3  * n^3 = n^6
;; n^6  * n^6 = n^12
;; n^12 * n^3 = n^15
;;
;; We shall define m(k) to be the minimum number of multiplications to compute
;; n^k; for example m(15) = 5.
;;
;; For 1 <= k <= 200, find  m(k).
;;
;; Answer: 1582


(define (println s) (for-each (curry format #t "~a~%") s))


(define (map-and-filter f s)
  (fold-right (lambda (e res) 
                (let ((r (f e)))
                  (if r (cons r res) res)))
              '()
              s))


(define (compose . funcs)
  (lambda (x) (fold-right (lambda (f res) (f res)) x funcs)))


(define (p122-int size)

  (let ((data (make-vector (1+ size) '((0)))))

    (define (get-rows  n)       (vector-ref  data n))
    (define (set-rows! n rows)  (vector-set! data n rows))

    (define (make-rows n)

      (define (from-parent parent n)
        (let ((x (find (lambda (i) (= n (+ (car parent) i))) parent)))
          (and x (cons (+ (car parent) x) parent))))

      (define (set-res res new)
        (cond ((null? new)                                  res)
              ((null? res)                                  new)
              ((= (length (car res)) (length (car new)))    (append res new))
              ((< (length (car res)) (length (car new)))    res)
              (else                                         new)))

      (let next ((i 1) (res '()))
        (if (= i n)
          res
          (next (1+ i) (set-res res (map-and-filter (lambda (r) (from-parent r n)) 
                                                    (get-rows i)))))))

    (set-rows! 1 '((1)))

    (dotimes (n 2 size)
      (set-rows! n (make-rows n)))

    (vector->list data)))


(define (p122)
  (apply + (map (compose 1- length car) (p122-int 200))))


;; end of file
;; vim: sw=4 ts=4
