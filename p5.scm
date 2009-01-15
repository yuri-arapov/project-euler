;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=5
;;
;; Problem 5
;; 14 December 2001
;; 
;; 2520 is the smallest number that can be divided by each of the
;; numbers from 1 to 10 without any remainder.
;; 
;; What is the smallest number that is evenly divisible by all of
;; the numbers from 1 to 20?
;;
;; Answer:
;;      232792560
;;      232792560
;;
;; Done


(define (range from to)
;; return list (from, from+1, ..., to)
    (if (> from to)
      '()
      (cons from (range (+ from 1) to))))


(define (split-number-into-primes n)
;; return list of the primes the number 'n' may be split into
;; (i.e. product of these primes give the 'n')
  (define (loop l p x)
    (if (= x 1)
      l
      (if (= (remainder x p) 0)
        (loop (append l (list p))     p      (/ x p))
        (loop         l            (+ p 1)      x   ))))

  (loop '()  ;; empty list
        2    ;; first prime
        n))  ;; the number we're splitting


(define (union l1 l2)
;; merge two sorted lists to make a union of l1 and l2
;; (i.e. each list item will be stored in resultant union just once)
  (define (iter res l1 l2)
    (cond ((null? l1) (append res l2))
          ((null? l2) (append res l1))
          (else
            (let ((i1 (car l1))
                  (i2 (car l2)))
                 (cond ((= i1 i2) (iter (append res (list i1)) (cdr l1) (cdr l2)))
                       ((< i1 i2) (iter (append res (list i1)) (cdr l1)      l2))
                       ((> i1 i2) (iter (append res (list i2))      l1  (cdr l2))))))))
  (iter '() l1 l2))


(define (p5-list from to)
;; return the shortes list of primes that
;; can produce every number in range [from, to]
  (define (iter ll from to)
    (if (> from to)
      ll
      (iter (union ll (split-number-into-primes from)) (+ from 1) to)))
  (iter '() from to))


(define (p5) (apply * (p5-list 1 20)))
;; return result of Euler Project problem N5


;; *************************************************************
;;
;; this one-liner does the job:
;;
;; (apply lcm (range 1 20))
;;
;; note: lcm is built-in letest-common-multiplier function
;;
;; *************************************************************


;; d = 1
;; 
;; 
;; for i in range(1, 21):
;;         y = d
;;         x = i
;;         p = 2 ;; prime
;; 
;;         ;; this loop splits x into primes
;;         while x != 1:
;;                 while x % p == 0:
;;                         ;; x is divisible by prime p evenly
;;                         x /= p
;;                         if y % p == 0:
;;                                 ;; y is divisible by p as well
;;                                 y /= p
;;                         else:
;;                                 d *= p
;;                 p += 1
;; 
;; print d

;; end of file
