;; 18 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=39
;;
;; Problem 39
;; 14 March 2003
;;
;; If p is the perimeter of a right angle triangle with integral length sides,
;; {a,b,c}, there are exactly three solutions for p = 120.
;;
;; {20,48,52}, {24,45,51}, {30,40,50}
;;
;; For which value of p < 1000, is the number of solutions maximised?
;;
;; Answer: 840


;;
;;
;; a^2 + b^2 = c^2          (1)
;; a + b + c = p            (2)
;;
;; from (2): c = p - (a+b)  (3)
;;
;; replace c in (1) with (3):
;;
;;   a^2 + b^2 = (p - (a+b))^2
;;
;; simplify:
;;
;;        p(p - 2a)
;;   b = -----------        (4)
;;         2(p - a)
;;
;; so dividend is p(p - 2a)
;; and divisor is 2(p - a)
;;
;; remainder of dividend/divisor must be 0
;;
;; c >= b >= a -- we can use it to limit
;; enumeration of the a
;;
;; note:
;;   the p is always even for right triangle
;;
;;   a is odd, b is odd -> a*a is odd, b*b is odd -> a*a+b*b is
;;   even, so c*c is even, and c is even as well, so a+b+c is even.
;;
;;   a is odd, b is even -> a*a is odd, b*b is even -> a*a+b*b is
;;   odd, so c*c is odd, and c is odd as well, so a+b+c is even.
;;
;;   a is even, b is even, so c is even as well, and the p must
;;   be even
;;
;;
(define (number-of-right-triangles p)
  (define (iter n a)
    (let* ((dividend (* p (- p (* 2 a))))
           (divisor  (* 2 (- p a)))
           (b        (/ dividend divisor)))
      (cond ((> a b)                              n) ;; the end
            ((zero? (remainder dividend divisor)) (iter (+ n 1) (+ a 1)))
            (else                                 (iter n (+ a 1))))))
  (iter 0 1))


(define (p39-impl limit)
  (define (iter max-p max-n p)
    (if (>= p limit)
      (list max-p max-n)
      (let ((n (number-of-right-triangles p)))
        (if (> n max-n)
          (iter p n (+ p 2))
          (iter max-p max-n (+ p 2))))))
  (iter 0 0 2))


(define (p39)
  (p39-impl 1000))


;; end of file
