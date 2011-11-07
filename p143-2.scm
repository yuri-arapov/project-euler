;; 2011-10-07
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=143
;;
;; Problem 143
;;
;; 02 March 2007
;; 
;; Let ABC be a triangle with all interior angles being less than 120 degrees.
;; Let X be any point inside the triangle and let XA = p, XB = q, and XC = r.
;; 
;; Fermat challenged Torricelli to find the position of X such that p + q + r
;; was minimised.
;;
;; Torricelli was able to prove that if equilateral triangles AOB, BNC and AMC
;; are constructed on each side of triangle ABC, the circumscribed circles of
;; AOB, BNC, and AMC will intersect at a single point, T, inside the triangle.
;; Moreover he proved that T, called the Torricelli/Fermat point, minimises p +
;; q + r. Even more remarkable, it can be shown that when the sum is minimised,
;; AN = BM = CO = p + q + r and that AN, BM and CO also intersect at T.
;;
;;                             <nice figure here>
;;
;; If the sum is minimised and a, b, c, p, q and r are all positive integers we
;; shall call triangle ABC a Torricelli triangle. For example, a = 399, b =
;; 455, c = 511 is an example of a Torricelli triangle, with p + q + r = 784.
;;
;; Find the sum of all distinct values of p + q + r <= 120000 for Torricelli
;; triangles.
;;
;; Note: This problem has been changed recently, please check that you are
;; using the right parameters.
;;
;; Answer: 


(define (square? x) (integer? (sqrt x)))
(define (sqr x)     (* x x))


(define limit 60000)


(define *bv* (make-bitvector (1+ (sqr limit))))
(dotimes (n limit) (bitvector-set! *bv* (* n n) #t))
(define (square? n) (bitvector-ref *bv* n))


(define (divisor? d x) (zero? (remainder x d)))


(define (f1 p)
  (let ((p2 (* p 2)))
    (let loop ((r (1- (* 2 p)))
               (res '()))
      (let* ((p2-r (- p2 r))
             (p2+r (+ p2 r)))
        (cond
          ((or (= r p) (>= (* 3 p2-r p2+r) (* r r)))
           (if (divisor? 1000 p)
             (format #t "~a~%" p))
           ;;;(format #t "~a ~a~%" p res)
           (reverse res))

          ((and (not (divisor? 3 p2-r)) (not (divisor? 3 p2+r)))
           (loop (1- r) res))

          (else
            (let ((xx (/ (* p2-r p2+r) 3)))
              (if (not (square? xx))
                (loop (1- r) res)
                (let* ((x (sqrt xx))
                       (y (/ (- r x) 2)))
                  (if (integer? y)
                    (loop (1- r) (cons (cons x y) res))
                    (loop (1- r) res)))))))))))


;; end of file
;; vim: sw=4 ts=4
