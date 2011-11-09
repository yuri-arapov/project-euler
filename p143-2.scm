;; 2011-10-07
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=143
;;
;; Problem 143
;;
;; 02 March 2007
;; 
;; Let ABC be a triangle with all interior angles being less than 120 degrees.
;; Let X be any point inside the triangle and let XA = p, XB = q, and XC = r.
;; 
;; Fermat challenged Torricelli to find the position of X such that p + q + r
;; was minimised.
;;
;; Torricelli was able to prove that if equilateral triangles AOB, BNC and AMC
;; are constructed on each side of triangle ABC, the circumscribed circles of
;; AOB, BNC, and AMC will intersect at a single point, T, inside the triangle.
;; Moreover he proved that T, called the Torricelli/Fermat point, minimises p +
;; q + r. Even more remarkable, it can be shown that when the sum is minimised,
;; AN = BM = CO = p + q + r and that AN, BM and CO also intersect at T.
;;
;;                             <nice figure here>
;;
;; If the sum is minimised and a, b, c, p, q and r are all positive integers we
;; shall call triangle ABC a Torricelli triangle. For example, a = 399, b =
;; 455, c = 511 is an example of a Torricelli triangle, with p + q + r = 784.
;;
;; Find the sum of all distinct values of p + q + r <= 120000 for Torricelli
;; triangles.
;;
;; Note: This problem has been changed recently, please check that you are
;; using the right parameters.
;;
;; Answer: 30758397


(load "group-by.scm")


(load "uniq.scm")


;; For given integer value p find list of pairs (x y) so that
;; p*p = x*x + x*y + y*y, 
;; where x and y are integers, and x < y.
(define (f p)
  (define (divisor? d x) (zero? (remainder x d)))
  (let ((pp4 (* p p 4)))
    (let loop ((r (1- (* 2 p)))
               (res '()))
      (let* ((x (sqrt (/ (- pp4 (* r r)) 3.)))
             (y (/ (- r x) 2)))
        (cond
          ((or (= r p) (> x y))
           (if (divisor? 1000 p)
             (format #t "~a~%" p))
           ;;;(format #t "~a ~a~%" p res)
           (reverse res))

          (else
            (loop 
              (1- r)
              (if (integer? y) 
                (cons (cons x y) res)
                res))))))))


;; Read magic pairs from file, return list of lists containing
;; magic pairs:
;; (
;;  ((1 2) (3 4) ...)
;;  ((5 6) (7 8) ...)
;; )
;; The pairs where obtained by invocation of (f p) for each p in 1..120000
;; range.
(define (read-pairs fname)
  (define (blank-line? line) (string=? line ""))
  (read-file-with 
    fname
    (lambda (s)
      (group-by 2
         (map string->number
           (filter (compose not blank-line?) (string-split s #\space)))))))


(define (group-by-1st s)
  (define (same? x y) (= (car x) (car y)))
  (define (update-res x res) (cons (reverse x) res))

  (let loop ((x (car s))
             (tmp (list (car s)))
             (s (cdr s))
             (res '()))
    (cond
      ((null? s)
       (map
         (lambda (e) (cons (caar e) (map cadr e)))
         (reverse (update-res tmp res))))

      ((same? x (car s))
       (loop x (cons (car s) tmp) (cdr s) res))

      (else
        (loop (car s) (list (car s)) (cdr s) (update-res tmp res))))))


;; Solve problem 143 for given data.
(define (p143-int limit fname)
  (let* (
         
         ;; Read data from file: each line contains even number of numbers,
         ;; each pair (x y) meets this condition: x*x + x*y + y*y = z*z, where
         ;; x < y and z is integer.  The 'z' is the same for each line.
         (a (read-pairs fname))

         ;; for each pair (x y) add pair (y x) to each line:
         ;; ((x1 y1) (x2 y2) ...) -> ((x1 y1) (x2 y2) ... (y1 x1) (y2 x2) ...)
         (b (map (lambda (e) 
                   (append e (map (lambda (i) (list (cadr i) (car i))) e)))
                 a))

         ;; flatten list: all the pairs in one list.
         (c (fold (lambda (e res) (append e res)) '() b))

         ;; sort pairs by first pair's element.
         (d (sort c (with < car)))

         ;; group pairs starting with the same number together:
         ;; ((x1 y2) ...) -> ((x1 y1 ...) (x2 y2 ...) ...)
         (e (group-by-1st d))

         ;; store grouped data in vector: each xi element of the vector will
         ;; contain (yi ...) list.
         (f (let ((v (make-vector (1+ limit) '())))
              (for-each (lambda (x) (vector-set! v (car x) (cdr x))) e)
              v))

         ;; make list of triples (p q r) so that the following chain exists:
         ;; (p q) -> (q r) -> (r q)
         (g (let loop ((p limit) (res '()))
              (if (zero? p) res
                (loop 
                  (1- p)
                  (fold
                    (lambda (q res)
                      (fold
                        (lambda (r res)
                          (if (and (member p (vector-ref f r)) 
                                   (<= (+ p r q) limit))
                            (cons (sort (list p r q) <) res)
                            res))
                        res
                        (vector-ref f q)))
                    res
                    (vector-ref f p))))))

         ;; compute p+q+r, sort and undup.
         (h (undup (sort (map (lambda (e) (apply + e)) g) <))))

    (list (apply + h) h)))


(define (p143)
  (p143-int 120000 "143.data"))



;; end of file
;; vim: sw=4 ts=4
