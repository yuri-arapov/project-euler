;;
;; replace ltters with digits from 1 2 3 4 5 6 7 8 9 set to make this true:
;; ab*c = def = g*hi


(load "permutations.scm")


(define (mk-number arr indices)
  (let loop ((res 0) (i indices))
    (if (null? i) res
      (loop (+ (* res 10) (vector-ref arr (car i))) (cdr i)))))

(define (left arr)
  (* (mk-number arr '(0 1)) (mk-number arr '(2))))

(define (middle arr)
  (mk-number arr '(3 4 5)))

(define (right arr)
  (* (mk-number arr '(6)) (mk-number arr '(7 8))))

(define (match? seq)
  (let ((x (list->vector seq)))
    (= (left x) (middle x) (right x))))


(define (solve)
  (filter match? (permutations '(1 2 3 4 5 6 7 8 9))))

;; end of file
