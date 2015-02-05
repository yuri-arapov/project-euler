;; Let S(A) represent the sum of elements in set A of size n. We shall call it a
;; special sum set if for any two non-empty disjoint subsets, B and C, the
;; following properties are true:
;; 
;; S(B) ≠ S(C); that is, sums of subsets cannot be equal.
;; If B contains more elements than C then S(B) > S(C).
;; If S(A) is minimised for a given n, we shall call it an optimum special sum
;; set. The first five optimum special sum sets are given below.
;; 
;; n = 1: {1}
;; n = 2: {1, 2}
;; n = 3: {2, 3, 4}
;; n = 4: {3, 5, 6, 7}
;; n = 5: {6, 9, 11, 12, 13}
;; 
;; It seems that for a given optimum set, A = {a1, a2, ... , an}, the next optimum
;; set is of the form B = {b, a1+b, a2+b, ... ,an+b}, where b is the "middle"
;; element on the previous row.
;; 
;; By applying this "rule" we would expect the optimum set for n = 6 to be A =
;; {11, 17, 20, 22, 23, 24}, with S(A) = 117. However, this is not the optimum
;; set, as we have merely applied an algorithm to provide a near optimum set. The
;; optimum set for n = 6 is A = {11, 18, 19, 20, 22, 25}, with S(A) = 115 and
;; corresponding set string: 111819202225.
;; 
;; Given that A is an optimum special sum set for n = 7, find its set string.
;; 
;; NOTE: This problem is related to Problem 105 and Problem 106.
;; 
;; Answer: 20313839404245


(load "combinations.scm")


(define non-optimal-set-7 '(20 31 38 39 40 42 45))

(define (good-subsets? a subsets)

  (let* ((v (list->vector a))
         (l (vector-length v))
         (sum (lambda (i) 
                (let loop ((i i) (res 0))
                  (if (null? i) res
                    (if (>= (car i) l) #f
                      (loop (cdr i) (+ res (vector-ref v (car i))))))))))

    (let loop ((ss subsets))
      (if (null? ss) #t
        (let* ((i1 (car  (car ss)))
               (i2 (cadr (car ss)))
               (s1 (sum i1))
               (s2 (sum i2)))
          (if (and s1 s2) 
            (if (= s1 s2) #f
              (loop (cdr ss)))
            #t))))))


(define (indices->bits s)
  (fold (lambda (i res) (logior res (ash 1 i))) 0 s))


(define (disjoint? s1 s2)
  (zero? (logand (indices->bits s1) (indices->bits s2))))


(define *all-pairs*
  (filter (lambda (i) (disjoint? (car i) (cadr i)))
          (combinations (combinations '(0 1 2 3 4 5 6) 2) 2)))


(define *all-triples*
  (filter (lambda (i) (disjoint? (car i) (cadr i)))
          (combinations (combinations '(0 1 2 3 4 5 6) 3) 2)))


(define *all-subsets* (append *all-pairs* *all-triples*))


(define (p103)
  (define (good? a)
    (and (> (apply + (take a 4)) (apply + (take-right a 3)))
         (> (apply + (take a 3)) (apply + (take-right a 2)))
         (good-subsets? a *all-subsets*)))
  (let ((res   '())
        (res-s 500)
        (count 0))
    (letrec ((f1 (lambda (seed acc)
                   (if (<= (length acc) 7)
                     (begin
                       (begin
                         (set! count (1+ count))
                         (if (zero? (remainder count 1000))
                           (format #t "~a ~a                       \r" count (reverse acc))))
                       (if (and (= 7 (length acc)) 
                                (good? (reverse acc)) 
                                (< (apply + acc) res-s))
                         (begin
                           (set! res (reverse acc))
                           (set! res-s (apply + res))
                           (format #t "~%*** ~a ~a~%" res-s res)))
                       (let ((limit (if (>= (length acc) 2) (apply + (take-right acc 2)) res-s)))
                         (let loop ((c seed))
                           (if (and (< (apply + acc) res-s) (< c limit))
                             (begin
                               (f1 (1+ c) (cons c acc))
                               (loop (1+ c)))))))))))

      (f1 11 '())
      (format #t "\n\n")
      res)))


;; end of file
;; vim: ts=4 sw=4
