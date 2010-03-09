;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=76
;;
;; Problem 76
;; 13 August 2004
;;
;; It is possible to write five as a sum in exactly six different ways:
;;
;; 4 + 1
;; 3 + 2
;; 3 + 1 + 1
;; 2 + 2 + 1
;; 2 + 1 + 1 + 1
;; 1 + 1 + 1 + 1 + 1
;;
;; How many different ways can one hundred be written as a sum of at least two
;; positive integers?
;;
;; Answer: 190569291
;;


;;
;; Key method: memorization.
;;
;; IMPORTANT: (make-vector n (make-vector n)) DOES NOT WORK, because
;; IMPORTANT: it's not vector of DIFFERENT vectors, it's a vector
;; IMPORTANT: containing THE SAME instance of some vector.


;;
;; See http://en.wikipedia.org/wiki/Partition_(number_theory) 
;;   for details (Intermediate function)
;;


;;
;; Shadow system format.
;;
(define format (lambda (dst fmt . args) #t))


(define (p n)

  ;; memorization table
  (define pp-tab (list->vector (map (lambda (i) (make-vector (+ n 1) #f)) (make-list (+ n 1)))))
  ;;                                                                      ^^^^^^^^^^^^^^^^^^^
  ;;                                                                      Make a list on n+1 size,
  ;;
  ;;                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ;;                           then map list so each list item becomes a
  ;;                           vector of n+1 length initalized with #f,
  ;;
  ;;             ^^^^^^^^^^^^^
  ;;             then turn list into
  ;;             vector, so the result is
  ;;             vector of vectors, ie
  ;;             (n+1)x(n+1) table.

  (define (pp-get k n) (vector-ref (vector-ref pp-tab n) k))

  (define (pp-set! k n val) (vector-set! (vector-ref pp-tab n) k val))

  (define (pp k n)
    (cond ((> k n) 0)
          ((= k n) 1)
          (else
            (let ((pp-val (pp-get k n)))
              (if pp-val
                (begin
                  ;; already in the table
                  (format #t "~a ~a -> ~a *\n" k n pp-val)
                  pp-val)
                (let ((pp-val (+ (pp (+ k 1) n) (pp k (- n k)))))
                  ;; new value
                  (pp-set! k n pp-val)
                  (format #t "~a ~a -> ~a\n" k n pp-val)
                  pp-val))))))

  (let ((res (pp 1 n)))
    ;; print memorization table
    (for-each (lambda (v) (format #t "~a\n" v)) (vector->list pp-tab))
    (- res 1)))


;; end of file
;; vim: ts=4 sw=4
