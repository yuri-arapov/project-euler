;; Let S(A) represent the sum of elements in set A of size n. We shall call it a
;; special sum set if for any two non-empty disjoint subsets, B and C, the
;; following properties are true:
;; 
;; S(B) ≠ S(C); that is, sums of subsets cannot be equal.
;; If B contains more elements than C then S(B) > S(C).
;; For this problem we shall assume that a given set contains n strictly
;; increasing elements and it already satisfies the second rule.
;; 
;; Surprisingly, out of the 25 possible subset pairs that can be obtained from a
;; set for which n = 4, only 1 of these pairs need to be tested for equality
;; (first rule). Similarly, when n = 7, only 70 out of the 966 subset pairs need
;; to be tested.
;; 
;; For n = 12, how many of the 261625 subset pairs that can be obtained need to be
;; tested for equality?
;; 
;; NOTE: This problem is related to Problem 103 and Problem 105.
;; 
;; Answer:


(load "combinations.scm")


(define (indices->bits s)
  (fold (lambda (i res) (logior res (ash 1 i))) 0 s))


(define (disjoint? s1 s2)
  (zero? (logand (indices->bits s1) (indices->bits s2))))


(define (good? s1 s2)
  (let ((l1 (length s1))
        (l2 (length s2)))
    (cond ((= 1 l1 l2) #f)
          ((= 1 l1) #f);;(> (first s1) (last s2)))
          ((= 1 l2) #f);;(> (first s2) (last s1)))
          (else
            (let ((b1 (first s1))
                  (e1 (last  s1))
                  (b2 (first s2))
                  (e2 (last  s2)))
              (or (< b1 b2 e2 e1) (< b2 b1 e1 e2)))))))


(define (good2? s1 s2)
  (if (not (= (length s1) (length s2))) #f
    (let ((n (max (last s1) (last s2))))
      (let loop ((sum 0)
                 (i (min (first s1) (first s2))))
        (if (= i n) (zero? sum)
          (cond ((member i s1) (loop (if (positive? sum) sum (1+ sum)) (1+ i)))
                ((member i s2) (loop (if (negative? sum) sum (1- sum)) (1+ i)))
                (else (loop sum (1+ i)))))))))



(define (disjoint-sets n)
  (filter 
    (lambda (i) (disjoint? (car i) (cadr i))) 
    (combinations 
      (apply append 
             (map (lambda (c) (combinations (iota n) c)) 
                  (iota (1- n) 1))) 
      2)))


;; end of file
