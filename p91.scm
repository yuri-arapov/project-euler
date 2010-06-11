;; 2010-06-11
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=91
;;
;; Problem 91 18 March 2005
;;
;; See description by address above.
;;
;; Answer: 14234
;;
;; NOTE: I guess smarter algorithm would not generate duplicated
;;       segments.


(load "uniq.scm")


;; Simple print
(define (print s) (for-each (lambda (i) (format #t "~a\n" i)) s))


;; Point
(define (make-p x y) (cons x y))


;; Point accessors
(define (get-x p) (car p))
(define (get-y p) (cdr p))


;; Points comparison: true if p1 is less than p2
(define (less-p? p1 p2)
  (let ((x1 (get-x p1))
        (y1 (get-y p1))
        (x2 (get-x p2))
        (y2 (get-y p2)))
    (or (< x1 x2)
        (and (= x1 x2) (< y1 y2)))))


;; Points comparison: true if p1 is equal to p2
(define (eq-p? p1 p2)
  (equal? p1 p2))


;; Sum of 2 points
(define (add-p p1 p2) (make-p (+ (get-x p1) (get-x p2))
                              (+ (get-y p1) (get-y p2))))

;; Product: point by number
(define (mul-p p m) (make-p (* (get-x p) m) (* (get-y p) m)))


;; Normal to vector from (0,0) to given point
(define (point->norm p)
  (let* ((x (get-x p))
         (y (get-y p))
         (d (gcd x y)))
    (make-p (- (/ y d))
            (+ (/ x d)))))


;; Segment
(define (make-s beg end) (cons beg (list end)))

;; Segment accessors
(define (get-s-beg s) (car s))
(define (get-s-end s) (cadr s))


;; Segments comparison:: true if s1 is less than s2
(define (less-s? s1 s2)
  (let ((b1 (get-s-beg s1))
        (e1 (get-s-end s1))
        (b2 (get-s-beg s2))
        (e2 (get-s-end s2)))
    (or (less-p? b1 b2)
        (and (eq-p? b1 b2) (less-p? e1 e2)))))


;; Segments comparison:: true if s1 is equal to s2
(define (eq-s? s1 s2)
  (equal? s1 s2))


;; Generate list of points that represent square of size n;
;; exclude origin O(0,0)
(define (gen-square n)
  (let loop ((x n) (y n) (res '()))
    (cond ((negative? x )
           (cdr res))   ;; drop first point: origin (0,0)
          ((negative? y)
           (loop (1- x) n res))
          (else
            (loop x (1- y) (cons (make-p x y) res))))))


;; Generate list of points from given point p along given direction
;; dir so that all the points would belong to the square of size n
(define (extend p dir n)

  (define (out? v) (or (negative? v) (> v n)))

  (let loop ((k 1) (res '()))
    (let ((pp (add-p p (mul-p dir k))))
      (if (or (out? (get-x pp)) (out? (get-y pp)))
        res
        (loop (1+ k) (cons pp res))))))


;; Make list of segments by given point p.
;; Each segment (p1,p2) meets the following conditions:
;; 1. given p is one of p1 or p2.
;; 2. both ens of segments belong to square of size n.
;; 3. triangle (O,p1,p2) is a right-angle one.
(define (point->segments p n)
  (map (lambda (i) (make-s p i))
       (let ((x (get-x p))
             (y (get-y p)))
         (cond ((zero? y)
                ;; (x,0) -> (0,1) (0,2) ... (0,Y)
                ;;       -> (x,1) (x,2) ... (x,Y)
                (append
                  (map (lambda (y) (make-p x y)) (iota n 1))
                  (map (lambda (y) (make-p 0 y)) (iota n 1))))
               ((zero? x)
                ;; (0,y) -> (1,0) (2,0) ... (X,0)
                ;;       -> (1,y) (2,y) ... (X,y)
                (append
                  (map (lambda (x) (make-p x y)) (iota n 1))
                  (map (lambda (x) (make-p x 0)) (iota n 1))))
               (else
                 ;; (x,y) -> (x,0)
                 ;;       -> (0,y)
                 ;;       -> (x,y) + norm*k
                 ;;       -> (x,y) - norm*k
                 (append
                   (list (make-p x 0))
                   (list (make-p 0 y))
                   (extend p (mul-p (point->norm p) +1) n)
                   (extend p (mul-p (point->norm p) -1) n)))))))


;; Problem 91
(define (p91-int n)
  (let* (
         ;; make list of points for given square
         (points (gen-square n))

         ;; turn each point into segment
         (segments (map (lambda (p) (point->segments p n)) points))

         ;; flatten list of segments
         (s2 (apply append segments))

         ;; sort ends of segments
         (s3 (map (lambda (s) (sort s less-p?)) s2))

         ;; sort segments
         (s4 (sort s3 less-s?))

         ;; drop duplicates
         (s5 (uniqp s4 eq-s?)))

    ;; result
    (length s5)))

(define (p91) (p91-int 50))

;; end of file
;; vim: sw=4 ts=4
