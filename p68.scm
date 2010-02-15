;; 2010-02-14
;;
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=68
;;
;; Problem 68
;; 23 April 2004
;;
;; Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6,
;; and each line adding to nine.
;;
;;
;; Working clockwise, and starting from the group of three with the numerically
;; lowest external node (4,3,2 in this example), each solution can be described
;; uniquely. For example, the above solution can be described by the set:
;; 4,3,2; 6,2,1; 5,1,3.
;;
;; It is possible to complete the ring with four different totals: 9, 10, 11,
;; and 12. There are eight solutions in total.
;;
;; Total        Solution Set
;;   9       4,2,3; 5,3,1; 6,1,2
;;   9       4,3,2; 6,2,1; 5,1,3
;;   10      2,3,5; 4,5,1; 6,1,3
;;   10      2,5,3; 6,3,1; 4,1,5
;;   11      1,4,6; 3,6,2; 5,2,4
;;   11      1,6,4; 5,4,2; 3,2,6
;;   12      1,5,6; 2,6,4; 3,4,5
;;   12      1,6,5; 3,5,4; 2,4,6
;; By concatenating each group it is possible to form 9-digit strings; the
;; maximum string for a 3-gon ring is 432621513.
;;
;; Using the numbers 1 to 10, and depending on arrangements, it is possible to
;; form 16- and 17-digit strings. What is the maximum 16-digit string for a
;; "magic" 5-gon ring?
;;
;; Answer: 6531031914842725
;;
;; Note: 13 is the minimum sum for the chain containing 10:
;;       13 = 10 + 2 + 1.
;;       20 is the maximum sum for the chain containing 1:
;;       20 = 1 + 9 + 10.
;;       So sum of any valid chain must be in [13 20] range.
;;
;; Note: Enumerating from greater numbers to smaller will ensure
;;       that the first result is the maximum one.
;;


(define (x)

  ;; Try to compose next segment starting with given n.
  (define (x6 segments n)

    ;; True if n is used already by some segment.
    (define (already-used? n)
      (any (lambda (s) (member n s)) segments))

    (if (or (already-used? n)
            (< n (first (last segments))))
      #f
      (let* ((s (apply + (car segments)))
             (o (third (car segments)))
             (p (- s n o)))
        (cond ((or (< p 1) (> p 10))
               #f)
              ((= 4 (length segments))
               (if (= p (second (last segments)))
                 (reverse (cons (list n o p) segments)) ;; bingo!
                 #f))
              ((or (already-used? p) (= 10 p))
               #f)
              (else
                (x5 (cons (list n o p) segments)))))))

  (define (x5 segments)
    (let loop ((n (list 9 8 7 6 5 4 3 2 1 10)))
      (and (not (null? n))
           (or (x6 segments (car n))
               (loop (cdr n))))))

  (define (x4 a1 a2 a3)
    (cond ((or (= a1 a2) (= a2 a3) (= a3 a1))
           #f)
          ((not (<= 13 (+ a1 a2 a3) 20))
           #f)
          (else
            (x5 (list (list a1 a2 a3))))))

  (define (x3 a1 a2)
    (let loop ((a3 (list 9 8 7 6 5 4 3 2 1)))
      (and (not (null? a3))
           (or (x4 a1 a2 (car a3))
               (loop (cdr a3))))))

  (define (x2 a1)
    (let loop ((a2 (list 9 8 7 6 5 4 3 2 1)))
      (and (not (null? a2))
           (or (x3 a1 (car a2))
               (loop (cdr a2))))))

  (define (x1)
    (let loop ((a1 (list 6 5 4 3 2 1 10)))
      (and (not (null? a1)) 
           (or (x2 (car a1))
               (loop (cdr a1))))))

  (x1))


;; Concatenate two numbers:
;; a b -> ab
;;
(define (concat-numbers a b)
  (let loop ((m 10))
    (if (zero? (quotient b m))
      (+ (* a m) b)
      (loop (* m 10)))))


;; Concatenate numbers.
;;
(define (concat-n a . z)
  (if (null? z)
    a
    (apply concat-n (cons (concat-numbers a (car z)) (cdr z)))))


(define (p68)
  (let* ((f (x))
         (n (apply concat-n 
                   (map (lambda (i) (apply concat-n i)) 
                        f))))
    (format #t "~a -> ~a\n" f n)))

;; end of file
