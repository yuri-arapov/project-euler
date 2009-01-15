;; 31 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=50
;;
;; Problem 50
;; 15 August 2003
;;
;; The prime 41, can be written as the sum of six consecutive primes:
;; 41 = 2 + 3 + 5 + 7 + 11 + 13
;;
;; This is the longest sum of consecutive primes that adds to a prime below
;; one-hundred.
;;
;; The longest sum of consecutive primes below one-thousand that adds to a
;; prime, contains 21 terms, and is equal to 953.
;;
;; Which prime, below one-million, can be written as the sum of the most
;; consecutive primes?
;;
;; Answer: 997651


(load "sieve-primes.scm")
(load "print.scm")
(load "range.scm")


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


;; end of file
