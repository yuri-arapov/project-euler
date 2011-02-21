;; 2011-02-17
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=127
;;
;; Problem 127
;; 01 September 2006
;; 
;; The radical of n, rad(n), is the product of distinct prime factors of n. For
;; example, 504 = 2^3 + 3^2 + 7, so rad(504) = 2x3x7 = 42.
;; 
;; We shall define the triplet of positive integers (a, b, c) to be an abc-hit if:
;; 
;; 1. GCD(a, b) = GCD(a, c) = GCD(b, c) = 1
;; 2. a < b
;; 3. a + b = c
;; 4. rad(abc) <  c
;; For example, (5, 27, 32) is an abc-hit, because:
;; 
;; 1. GCD(5, 27) = GCD(5, 32) = GCD(27, 32) = 1
;; 2. 5 < 27
;; 3. 5 + 27 = 32
;; 4. rad(4320) = 30 < 32
;;
;; It turns out that abc-hits are quite rare and there are only thirty-one
;; abc-hits for c<1000, with SUM(c) = 12523.
;; 
;; Find SUM(c) for c<120000.
;; 
;; Note: This problem has been changed recently, please check that you are
;; using the right parameters.
;; 
;; Answer: 18407904


(load "miller-rabin-primality-test.scm")
(load "pollard-rho.scm")
(load "uniq.scm")
(load "range.scm")


(define (fold-range from to init proc)
  (let loop ((n from) (res init))
    (if (> n to)
      res
      (loop (1+ n) (proc n res)))))


;; Factor number n.
(define (factor n)
  (pollard prime? brent n))


;; Compute radical of n.
(define (rad n)
  (apply * (uniqp (factor n) =)))


(define (p127-ex max-c)

  (let ((rad-vec (list->vector (map rad (range 0 (1- max-c))))))

    (define (radical n) (vector-ref rad-vec n))

    (fold-range 1 (1- max-c) '()
      (lambda (c res)
        (if (zero? (remainder c 100))
          (format #t "~a~%" c))
        (let ((cc (/ c (radical c))))
          (if (< cc 3)
            res
            (fold-range  (1+ (quotient c 2)) (1- c) res
              (lambda (b res) 
                (let ((a (- c b)))
                  (if (and (< (* (radical a) (radical b)) cc)
                           (= 1 (gcd (radical a) (radical b) (radical c))))
                    (begin
                      (format #t "*** ~a ~a ~a - ~a - (~a ~a ~a)~%" 
                              c b a
                              cc
                              (radical c)
                              (radical b)
                              (radical a))
                      (cons c res))
                    res))))))))))


(define (p127)
  (apply + (p127-ex 120000)))


;; end of file
;; vim: sw=4 ts=4
