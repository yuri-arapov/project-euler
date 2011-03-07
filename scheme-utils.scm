;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; Various Scheme utilities.



;; 2011-03-07
;; Turn number into list of digits of given base.
;; Order of the digits preserved:
;;   (number->digits 123 10) -> (1 2 3)
(define (number->digits n base)
  (unfold-right
    zero?                   ; stop test: seed is zero
    (rcurry remainder base) ; make element from seed
    (rcurry quotient base)  ; next seed
    n))                     ; initial seed



;; 2011-03-07
;; Compute g power e modulo c: (g^e) % c
;; http://www.cacr.math.uwaterloo.ca/hac/about/chap14.pdf
;; HAC Algorithm 14.79
;; (found in Python source code, file longobject.c, function long_pow().
;; NOTE: each multiplication is reduced by modulo c.
(define (mod-pow g e c)
  (define (mul x y) (modulo (* x y) c))
  (fold
    (lambda (e a)
      (if (= 1 e) 
        (mul (mul a a) g)
        (mul a a)))
    1
    (number->digits e 2)))



;; 2011-03-07
;; Make list of max-len length.
;; Elements are made by successful calls of (make-fn seed),
;; next seed is obtained by (next-fn seed) call.
;; Initial seed is init.
(define (construct-list-length max-len make-fn init next-fn)
  (let loop ((len 0) (seed init) (res '()))
    (if (= len max-len)
      (reverse res)
      (let ((e (make-fn seed)))
        (if e
          (loop (1+ len) (next-fn seed) (cons e res))
          (loop len (next-fn seed) res))))))



;; end of file
;; vim: sw=4 ts=4
