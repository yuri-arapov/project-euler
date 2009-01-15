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
;; kind of brute force


(load "print.scm")


(define (right-triangle? a b c)
  (= (+ (* a a) (* b b)) (* c c)))


(define (enumerate-right-triangles p)
  (define (iter n m)
    (let* ((c (- p n))
           (b (- c m))
           (a (- p (+ b c))))
      (cond ((< b 1)
             '())
            ((or (> a b) (< a 1))
             (iter (+ n 1) 1))
            ((right-triangle? a b c)
             (cons (list a b c) (iter n (+ m 1))))
            (else
              (iter n (+ m 1))))))
  (iter 2 1))


(define (p39)
  (define (iter max-triangles-so-far max-p-so-far p)
    (if (zero? (remainder p 100))
      (println p " " max-p-so-far " " max-triangles-so-far))
    (if (>= p 1000)
      (list max-p-so-far max-triangles-so-far)
      (let ((triangles (length (enumerate-right-triangles p))))
        (if (> triangles max-triangles-so-far)
          (iter triangles p (+ p 10))
          (iter max-triangles-so-far max-p-so-far (+ p 10))))))
  (iter 0 0 10))


;; end of file
