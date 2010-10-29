;; 2010-10-25
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=110
;;
;; Problem 110
;; 02 December 2005
;;
;; In the following equation x, y, and n are positive integers.
;;   1   1   1
;;   - + - = -
;;   x   y   n
;;
;; It can be verified that when n = 1260 there are 113 distinct solutions and
;; this is the least value of n for which the total number of distinct
;; solutions exceeds one hundred.
;;
;; What is the least value of n for which the number of distinct solutions
;; exceeds four million?
;;
;; NOTE: This problem is a much more difficult version of problem 108 and as it
;; is well beyond the limitations of a brute force approach it requires a
;; clever implementation.
;;
;; Answer: 9350130049860600



;; Compute number of distinct divisors of the number made of 
;; primes in given powers.
;;
;; Example.
;; Let powers-list be (1 2 3), which means there is the following
;; factorisation:
;;
;;         1  2  3
;;   n =  2  3  5
;;
;; Each prime gives the following list of distinct divisors:
;;   2: 1 2         -- 2 divisors (power of 2 plus 1)
;;   3: 1 3 9       -- 3 divisors (power of 3 plus 1)
;;   5: 1 5 25 125  -- 4 divisors (power of 5 plus 1)
;;
;; The total number of distinct divisors is product of number of divisors 
;; of each prime:
;;   D = (1+1) * (2+1) * (3+1) = 2 * 3 * 4 = 24
;;
(define (number-of-divisors powers-list)
  (apply * (map 1+ powers-list)))



;; Compute number of distinct solutions (this is what 'ds' stands for)
;; of 
;;   1   1   1
;;   - + - = -  (A)
;;   x   y   n
;;
;; equation, where 'powers-list' is powers of factorisation of the 'n'.
;;
;; Initial equation (A) can be transformed to
;;
;;             2
;;            n
;;   y = n + ---,  where d = x - n
;;            d
;;
;; So d must be a divisor of n^2, and 1 <= d <= n
;;
;; So we're looking for number of distinct divisors of n^2.
;;
;;           p1    p2        pm
;; Let n = d1    d2   ...  dm    (factorisation of the n)
;;
;;     2     2p1    2p2        2pm
;; So n  = d1     d2    ...  dm
;;
;; n^2 has twice as more distinct divisors than n, and half of them
;; is <= than n, so we need just half of distinct divisors of the n^2
;; (note '1+' is to take into account the first divisor: 1).
;;
(define (number-of-ds powers-list)
  (1+ (quotient
        (number-of-divisors (map (lambda (n) (* n 2)) powers-list))
        2)))



;; There are at most 15 primes that provide factorisation we're looking
;; for:
;;           p1    p2        pm
;; Let n = d1    d2   ...  dm    (factorisation of the n)
;;
;;     2     2p1    2p2        2pm
;; So n  = d1     d2    ...  dm
;;
;; Number of distinct divisors:
;;
;;   (2p1 + 1) * (2p2 + 1) * ... * (2pm + 1)
;;   --------------------------------------- >= 4000000
;;                     2
;;
;; When each of p1,..,pm is equal to 1, then:
;;
;;   3 * 3 * ... * 3 >= 8000000, which is true when m=15
;;
(define first-15-primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47))



;; Compute number from list of powers (fold factorisation).
;;
(define (powers->number powers-list)
  (apply * 
         (map (lambda (d p) (integer-expt d p)) 
              first-15-primes 
              powers-list)))



;; Make list of descending powers.
;; Example:
;;   (make-descending-powers-list 3 2) ->
;;   ((2 2 2) (2 2 1) (2 2 0) (2 1 1) (2 1 0) (2 0 0) (1 1 1) (1 1 0) (1 0 0) (0 0 0))
;;
;; Result is a list of lists, each sublist is of given length and contains
;; elements that <= than given seed (up to 0).
;;
(define (make-descending-powers-list len seed)
  (if (= 0 seed)
    (list (make-list len 0))
    (if (= 1 len)
      (map list (reverse (iota (1+ seed))))
      (append
        (map (lambda (x) (cons seed x)) 
             (make-descending-powers-list (1- len) seed))
        (make-descending-powers-list len (1- seed))))))



;; Problem 110.
;;
;; Make a list of possible powers of factorisations made of 
;; first 15 primes (the highest power is 3 -- result of 'try and see'
;; approach) and check each factorisation against problem 110 rules.
;;
(define (p110)
  (fold
    (lambda (powers-list res)
      (or
        (and-let* ((ds (number-of-ds powers-list))
                   ((> ds 4000000))
                   (n (powers->number powers-list))
                   (res-n (car res))
                   ((or (zero? res-n) (< n res-n))))
                  (list n ds powers-list))
        res))
    (list 0 0 '())
    (make-descending-powers-list 15 3)))



;; end of file
;; vim: ts=4 sw=4
