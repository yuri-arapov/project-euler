;; 2010-02-22
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=75
;; 
;; Project Euler
;;
;; Problem 75
;; 30 July 2004
;;
;; It turns out that 12 cm is the smallest length of wire that can be bent to
;; form an integer sided right angle triangle in exactly one way, but there are
;; many more examples.
;;
;; 12 cm: (3,4,5)
;; 24 cm: (6,8,10)
;; 30 cm: (5,12,13)
;; 36 cm: (9,12,15)
;; 40 cm: (8,15,17)
;; 48 cm: (12,16,20)
;;
;; In contrast, some lengths of wire, like 20 cm, cannot be bent to form an
;; integer sided right angle triangle, and other lengths allow more than one
;; solution to be found; for example, using 120 cm it is possible to form
;; exactly three different integer sided right angle triangles.
;;
;; 120 cm: (30,40,50), (20,48,52), (24,45,51)
;;
;; Given that L is the length of the wire, for how many values of L <= 1,500,000
;; can exactly one integer sided right angle triangle be formed?
;;
;; Answer: 161667


(define (sqr x) (* x x))


;; Make Pythagorean triple.
;; See http://en.wikipedia.org/wiki/Pythagorean_triple
;;
(define (make-pt m n)
  (let ((a (- (sqr m) (sqr n)))
        (b (* 2 m n))
        (c (+ (sqr m) (sqr n))))
      (list a b c)))


;; True if abc is primitive Pythagorean triple.
;;
(define (ppt? abc)
  (let ((a (car   abc))
        (b (cadr  abc))
        (c (caddr abc)))
    (and (odd? (+ a b))     ;; exactly one of a, b is odd;
         (odd? c)           ;; c is odd

         (= 1 (gcd a b))    ;; any two sides of PT
         (= 1 (gcd b c))    ;; are relatively 
         (= 1 (gcd c a))))) ;; prime


;; Solve problem 75.
;;
(define (p75)
  (let ((acc-vec (make-vector 1500001 0)))

    (let loop ((m 2)
               (n 1)
               (count 0))           ;; number of triangles made for given n

      (let* ((abc (make-pt m n))    ;; a b and c sides
             (l   (apply + abc)))   ;; length of triangle

        (cond ((> l 1500000)
               (if (not (zero? count))
                 (loop (+ 2 n) (+ 1 n) 0))) ;; next n

              (else
                (if (ppt? abc)
                  (do ((ll l (+ ll l)))
                      ((> ll 1500000))
                    (vector-set! acc-vec ll (+ 1 (vector-ref acc-vec ll)))))

                (loop (+ 1 m) n (+ 1 count)))))) ;; next m

    (length (filter (lambda (x) (= 1 x)) (vector->list acc-vec)))))


;; end of file
;; vim: sw=4 ts=4
