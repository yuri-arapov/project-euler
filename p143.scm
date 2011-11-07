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
(define (s x y)     (+ (sqr x) (sqr y) (* x y)))


(define (p143-int limit)
  (let loop ((p   2)
             (res 0))
    (if (> p limit)
      res
      (loop (1+ p) (+ res
                      (let loop-q ((q 1))
                        (if (or (= p q)(> (+ p q) limit)) 0
                          (let ((bb (s p q)))
                            (if (not (square? bb)) (loop-q (1+ q))
                              (let ((x (let loop-r ((r 1))
                                         (if (or (> r q) (> (+ p q r) limit)) #f
                                           (let ((aa (s q r))
                                                 (cc (s p r)))
                                             (if (and (square? aa)
                                                      (square? cc))
                                               (begin
                                                 (format #t "~a ~a ~a ~a (~a)~%" p r q (map (compose inexact->exact sqrt) (list aa bb cc)) (gcd p r q))
                                                 (+ p r q))
                                               (loop-r (1+ r))))))))
                                (if x x
                                  (loop-q (1+ q)))))))))))))


(define (p143)
  (p143-int 120000))



;; end of file
;; vim: sw=4 ts=4
