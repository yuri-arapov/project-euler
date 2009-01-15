

(module p50
   (main main))


(define true #t)


(define false #f)


(define primes-sieve-data false)


(define (prime? n)
  (and (> n 1)
       (< n (vector-length primes-sieve-data))
       (vector-ref primes-sieve-data n)))


(define (sieve-primes limit)

  (define (iter n)

    (define (strike-out x)
      (cond ((< x limit)
             (vector-set! primes-sieve-data x false)
             (strike-out (+ x n)))))

    (cond ((< n limit)
           (if (vector-ref primes-sieve-data n)
             (strike-out (+ n n)))
           (iter (+ n 1)))))

  (set! primes-sieve-data (make-vector limit true))
  (iter 2))


(define (print-list z) (for-each display z))


(define (print . z) (print-list z))


(define (println . z) (print-list z) (newline))



(define (range from to) (iota (+ 1 (- to from)) from))
;; get some SRFI-1 sugar
;;


;; NOTE: This version is way too slow comapring to 
;; NOTE:   (filter prime? (range 2 limit))
;; NOTE: How come?
;;
;;(define (make-primes-list limit)
;;  (let loop ((n 2)
;;             (res '()))
;;    (if (> n limit)
;;      res
;;      (loop (+ n 1) (if (prime? n)
;;                      (append res (list n))
;;                      res)))))


(define primes false)
(define psum   false)

(define (get-p n) (vector-ref primes n))
(define (get-s n) (vector-ref psum n))


(define (sum-of from to) (- (get-s from) (get-s (+ to 1))))


(define (accumulative-sum ls)
;; IMPORTANT: (map ...) works hundred times faster than
;; IMPORTANT: home-made (iter ... (append ...))
;;
  (let ((n 0))
    (map (lambda (x) (let ((t (+ x n))) (set! n t) t)) ls)))


(define (p50-run limit)

  (define (len from to) (+ (- to from) 1))

  (define (res-len res) (len (car res) (cadr res)))
  (define (res-prime res) (caddr res))

  (define (next-length res)
    (if (zero? (res-len res))
      2
      (+ (res-len res) 1)))


  (println "sieving primes...")
  (sieve-primes limit)

  (println "gathering primes...")
  (set! primes (filter prime? (range 2 limit)))

  (println "preparing diffs...")
  (set! psum (accumulative-sum primes))
  (set! primes (list->vector (reverse primes)))      ;; (1 2 3 4 ...) -> (... 4 3 2 1)
  (set! psum (list->vector (reverse (cons 0 psum)))) ;; (1 2 3 4 ...) -> (... 4 3 2 1 0)

  (println "searching for the most consecutive primes...")
  (let loop ((from 1)
             (to 2)
             (res (list 0 0 0))) ;; (from to prime)

    (cond ((= from (vector-length primes))
           (list (res-prime res) (res-len res))) 
           ;; all done, reformat result a little.

          ((>= to (vector-length primes))
           (loop (+ from 1) (+ from (next-length res)) res))
           ;; start another loop.

          (else
            (let ((p (sum-of from to)))
              (cond ((prime? p)
                     ;;(println p " " (len from to))
                     (loop from (+ to 1) (list from to p)))
                     ;; p is prime and [from, to] gives larger
                     ;; interval by definition, so this will be
                     ;; the result so far.  store it and keep looping.

                    ((> p limit)
                     (loop (+ from 1) (+ from (next-length res)) res))
                     ;; p is greater than our limit, and increasing the 'to'
                     ;; will give even bigger numvers.  so shift 'from' and 
                     ;; start another loop.

                    (else
                      (loop from (+ to 1) res))))))))
                      ;; shift 'to' and keep looping.


(define (p50) (p50-run 1000000))


(define (main argv)
  (let ((r (p50)))
    (display r)
    (newline)))

;; end of file
