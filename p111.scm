;; 2010-11-11
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=111
;;
;; Problem 111
;; 16 December 2005
;;
;; Considering 4-digit primes containing repeated digits it is clear that they
;; cannot all be the same: 1111 is divisible by 11, 2222 is divisible by 22,
;; and so on. But there are nine 4-digit primes containing three ones:
;;
;; 1117, 1151, 1171, 1181, 1511, 1811, 2111, 4111, 8111
;;
;; We shall say that M(n, d) represents the maximum number of repeated digits
;; for an n-digit prime where d is the repeated digit, N(n, d) represents the
;; number of such primes, and S(n, d) represents the sum of these primes.
;;
;; So M(4, 1) = 3 is the maximum number of repeated digits for a 4-digit prime
;; where one is the repeated digit, there are N(4, 1) = 9 such primes, and the
;; sum of these primes is S(4, 1) = 22275. It turns out that for d = 0, it is
;; only possible to have M(4, 0) = 2 repeated digits, but there are N(4, 0) =
;; 13 such cases.
;;
;; In the same way we obtain the following results for 4-digit primes.
;; Digit, d     M(4, d)         N(4, d)         S(4, d)
;; 0            2               13              67061
;; 1            3                9              22275
;; 2            3                1               2221
;; 3            3               12              46214
;; 4            3                2               8888
;; 5            3                1               5557
;; 6            3                1               6661
;; 7            3                9              57863
;; 8            3                1               8887
;; 9            3                7              48073
;;
;; For d = 0 to 9, the sum of all S(4, d) is 273700.
;;
;; Find the sum of all S(10, d).
;;
;; Answer: 612407567715
;;


(load "product.scm")
(load "combinations.scm")
(load "miller-rabin-primality-test.scm")


(define (flatten s) (apply append s))


;; Turn series of digits into number:
;;   (digits->number '(1 2 3)) -> 123
;;
(define (digits->number digits)
  (fold (lambda (d res) (+ d (* res 10))) 0 digits))



;; Make list of sequences of given length with single hole -- place a hole
;; in every possible position in the sequence ('#f' denotes a 'hole'):
;;   (mk-single-hole 4) -> 
;;   ((#f #t #t #t)
;;    (#t #f #t #t)
;;    (#t #t #f #t)
;;    (#t #t #t #f))
;;
(define (mk-single-hole len)
  (format #t "mk-single-hole~%")
  (let loop ((head  '(#f))
             (tail  (make-list (1- len) #t))
             (res   '()))
    (let ((r (append (list (append head tail)) res)))
      (if (null? tail)
        (reverse r)
        (loop (cons (car tail) head) (cdr tail) r)))))



;; Make list of sequences with given number of holes in each
;; ('#f' denotes a 'hole'):
;;   (mk-holes 4 2) ->
;;   ((#f #f #t #t) 
;;    (#f #t #f #t) 
;;    (#f #t #t #f) 
;;    (#t #f #f #t) 
;;    (#t #f #t #f) 
;;    (#t #t #f #f))
(define (mk-holes len num-holes)
  (cond ((> num-holes len)  '())
        ((zero? len)        '())
        ((= 1 num-holes)    (mk-single-hole len))
        ((= num-holes len)  (list (make-list len #f)))
        ((zero? num-holes)  (list (make-list len #t)))
        (else
          (let loop ((prefix    '(#f))
                     (len       (1- len))
                     (res       '()))
            (let ((x (mk-holes len (1- num-holes))))
              (if (null? x) 
                res
                (loop (cons #t prefix) 
                      (1- len) 
                      (append res
                              (map (lambda (i) (append prefix i)) x)))))))))



;; Fill the holes in the sequence: replace each #t with given digit and #f with
;; elements from fillers list:
;;   (fill-holes '(#f #f #t #t #t) 0 '(1 2)) ->
;;   (1 2 0 0 0)
;;
(define (fill-holes seq digit fillers)
  (format #t "fill-holes~%")
  (let loop ((s seq)
             (fillers fillers)
             (res '()))
    (if (null? s)
      (reverse res)
      (if (car s)
        (loop (cdr s)
              fillers
              (cons digit res))
        (loop (cdr s)
              (cdr fillers)
              (cons (car fillers) res))))))


;; Make a list of the primes that:
;;   * start with digit F;
;;   * end with digit L;
;;   * contains N digits total;
;;   * contains M repeated digits D
;; Return either list of primes (including empty one) or #f.
;;
;;   (mk-primes-list-ex 1 1 4 1 3) ->
;;   (1511 1811 1151 1171 1181)
;;
(define (mk-primes-list-ex
          F     ;; first digit of the prime
          L     ;; last digit of the prime
          N     ;; number of the digits of the prime
          D     ;; repeated digit
          M)    ;; number of D in the prime

  (define (make-number digits)
    (digits->number (append (list F) digits (list L))))

  (and-let* (
     (nn (- N 2))
     (mm (cond 
           ((and (= D F) (= D L))  (- M 2))
           ((or  (= D F) (= D L))  (- M 1))
           (else                   M)))
     ((>= nn mm))
     (num-holes (- nn mm)))
    (cond
      ((zero? num-holes)
       (filter prime? (list (make-number (make-list mm D))))) 
      (else
        (let ((holes (mk-holes nn num-holes))
              (holes-fillers (full-combinations 
                               (delete D '(0 1 2 3 4 5 6 7 8 9))
                               num-holes)))
          (filter prime?
                  (each-to-each
                    (lambda (seq fillers) 
                      (make-number (fill-holes seq D fillers)))
                    holes
                    holes-fillers)))))))


;; Make list of the primes that:
;;   * contain N digits;
;;   * contain M repeated digits D.
;;
;;   (mk-primes-list 4 1 3) ->
;;   (1511 1811 1151 1171 1181 1117 2111 4111 8111)
;;
(define (mk-primes-list N D M)
  (flatten
    (filter 
      (lambda (i) (and i (not (null? i))))
      (map
        (lambda (i)
          (mk-primes-list-ex (car i) (cadr i) N D M))
        (product 
          '(1 2 3 4 5 6 7 8 9)    ;; digits a prime may start with
          '(1 3 7 9))))))         ;; digits a prime may end with



;; Find list of primes that:
;;   * contain N digits;
;;   * contain maximum number of given repeated digit.
;;
;;   (find-primes 4 0) ->
;;   (1009 2003 3001 4001 4003 4007 5003 5009 6007 7001 8009 9001 9007)
;;
(define (find-primes N digit)
  (let loop ((M (1- N)))
    (let ((x (mk-primes-list N digit M)))
      (if (null? x)
        (loop (1- M))
        x))))



(define (p111)
  (apply 
    + 
    (flatten
      (map (lambda (d) (find-primes 10 d))
           (iota 10)))))


;; vim: ts=4 sw=4 et
