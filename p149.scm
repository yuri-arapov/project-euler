;; Searching for a maximum-sum subsequence
;;
;; Problem 149
;;
;; Looking at the table below, it is easy to verify that the maximum possible sum
;; of adjacent numbers in any direction (horizontal, vertical, diagonal or
;; anti-diagonal) is 16 (= 8 + 7 + 1).
;;
;;  −2   5   3   2
;;   9  −6   5   1
;;   3   2   7   3
;;  −1   8  −4   8
;;
;; Now, let us repeat the search, but on a much larger scale:
;;
;; First, generate four million pseudo-random numbers using a specific form of
;; what is known as a "Lagged Fibonacci Generator":
;;
;; For 1 ≤ k ≤ 55, sk = [100003 − 200003k + 300007k^3] (modulo 1000000) − 500000.
;; For 56 ≤ k ≤ 4000000, sk = [sk−24 + sk−55 + 1000000] (modulo 1000000) − 500000.
;;
;; Thus, s10 = −393027 and s100 = 86613.
;;
;; The terms of s are then arranged in a 2000×2000 table, using the first 2000
;; numbers to fill the first row (sequentially), the next 2000 numbers to fill the
;; second row, and so on.
;;
;; Finally, find the greatest sum of (any number of) adjacent entries in any
;; direction (horizontal, vertical, diagonal or anti-diagonal).
;;
;;
;; Answer: 52852124
;;
;;
;; NOTE: the diagonals are not scanned for max sum of adjacent elements:
;;       I at first tried rows and columns, and the answer appeared to be
;;       correct.
;;



;; I guess I did solve this kind of problems during Algo 1 Coursera class,
;; will need to check it out.
(define (max-sub-seq s)
  (define (pass1 s)
    (let loop ((s s) (res '(0)))
      (if (null? s) res
        (loop (cdr s) (cons (+ (car res) (car s)) res)))))
  (define (pass2 r)
    (let loop ((r r) (top (car r)) (bot (car r)))
      (if (null? r) (- top bot)
        (cond ((and (not (null? (cdr r))) (> (car r) top)) (loop (cdr r) (car r) (car r)))
              ((< (car r) bot) (loop (cdr r) top     (car r)))
              (else            (loop (cdr r) top     bot))))))
  (pass2 (pass1 s)))



(define *size* 2000)

(define *lfg* (make-vector (1+ (* *size* *size*)) #f))
;; note: '1+' is because indices start from '1'.

(define (lfg k)
  (define (get k)   (vector-ref  *lfg* k))
  (define (set k x) (vector-set! *lfg* k x) x)
  (or
    (get k)
    (set k
         (if (<= k 55)
           (- (remainder (+ 100003 (* -200003 k) (* 300007 k k k)) 1000000) 500000)
           (- (remainder (+ (lfg (- k 24)) (lfg (- k 55)) 1000000) 1000000) 500000)))))

(define (lfg-tab-ref row col)
  (vector-ref *lfg* (+ col (* *size* (1- row)))))



;; iterate through 'from' to 'to' (inclusive) applying function '(fn i res)'
;; where 'i' is current counter and initial value of 'res' is 'init'.
(define (iterate fn from to init)
  (if (> from to) init
    (iterate fn (1+ from) to (fn from init))))

(define (iterate-size fn init) (iterate fn 1 *size* init))



(define (fill-data)
  (iterate
    (lambda (n res) (lfg n) res) 1 (* *size* *size*) #f))


(define (get-row r)
  (iterate-size (lambda (i res) (cons (lfg-tab-ref r i) res)) '()))


(define (get-column c)
  (iterate-size (lambda (i res) (cons (lfg-tab-ref i c) res)) '()))


(define (scan-rows)
  (iterate-size (lambda (row res) (max res (max-sub-seq (get-row row)))) 0))


(define (scan-columns)
  (iterate-size (lambda (col res) (max res (max-sub-seq (get-column col)))) 0))



(define (p149)
  (format #t "generating data table\n")
  (time (fill-data))
  (max (scan-rows) (scan-columns)))



;; end of file
