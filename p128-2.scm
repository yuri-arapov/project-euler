;; 2011-02-27
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=128
;;
;; Problem 128
;; 29 September 2006
;;
;; A hexagonal tile with number 1 is surrounded by a ring of six hexagonal
;; tiles, starting at "12 o'clock" and numbering the tiles 2 to 7 in an
;; anti-clockwise direction.
;;
;; New rings are added in the same fashion, with the next rings being numbered
;; 8 to 19, 20 to 37, 38 to 61, and so on. The diagram below shows the first
;; three rings.
;;
;;
;;                     (picture here)
;;
;; By finding the difference between tile n and each its six neighbours we
;; shall define PD(n) to be the number of those differences which are prime.
;;
;; For example, working clockwise around tile 8 the differences are 12, 29, 11,
;; 6, 1, and 13. So PD(8) = 3.
;;
;; In the same way, the differences around tile 17 are 1, 17, 16, 1, 11, and
;; 10, hence PD(17) = 2.
;;
;; It can be shown that the maximum value of PD(n) is 3.
;;
;; If all of the tiles for which PD(n) = 3 are listed in ascending order to
;; form a sequence, the 10th tile would be 271.
;;
;; Find the 2000th tile in this sequence.
;;
;; Answer: 14516824220


(load "miller-rabin-primality-test.scm")


;; Compute 1+2+..l+n
(define (s n) (/ (* (+ n 1) n) 2))


;; Compute value of of the node specified as number of circle (n)
;; and node index on the circle (i).
(define (ni->val n i)
  (+ 2 i (* 6 (s (- n 1)))))


;;        A
;;          M
;;   B         L
;; C             K
;;
;; D             J
;;
;; E             I
;;    F       H
;;        G
;;
;; A C E G I K -- are corners.
;; B D F H J L -- nodes on segments.
;; M           -- node right before A.
;;
;; The only nodes that may contain three prime neighbour diffs are A and M.
;;
;; Compute list of diffs between node A and its neighbours
;; (non-prime diffs are omitted).
(define (A n)
  (list 
    (+ (* 6 n) 1)
    (- (* 6 n) 1)
    (+ (* 12 n) 5)))


;; Compute list of diffs between node M and its neighbours
;; (non-prime diffs are omitted).
(define (M n)
  (list
    (- (* 6 n) 1)
    (- (* 12 n) 7)
    (+ (* 6 n) 5)))


;; Sort and undup list of numbers.
(define (uniq-sort s)
  (fold (lambda (e res)
          (if (and (not (null? res)) (= e (car res)))
            res
            (cons e res)))
        '()
        (sort s <)))


;; Problem 128, limit provided.
(define (p128-ex limit)
  (call/cc 
    (lambda (return)
      (letrec ((test-return-continue
                 (lambda (res val)
                   (format #t "~a~%" val)
                   (if (= limit res)
                     (return val)
                     res)))
               (loop 
                 (lambda (n res)  ; n   -- number of the circle, 
                                  ; res -- number of hits so far
                   (loop 
                     (1+ n)
                     (fold
                       (lambda (x res)
                         (let ((f (car x))
                               (i (cdr x))) ; i -- node index on the circle
                           (if (= 3 (length (filter prime? (uniq-sort (f n)))))
                             (test-return-continue (1+ res) (ni->val n i))
                             res)))
                       res
                       (list
                         (cons A 0)
                         (cons M (- (* n 6) 1))))))))
        (loop 2 3))))) ; 2 -- start from 2nd circle
                       ; 3 -- one hit is central node, two hits on first circle


;; Problem 128
(define (p128)
  (p128-ex 2000))


;; end of file
;; vim: sw=4 ts=4
