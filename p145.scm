;; 2011-11-12
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=145
;;
;; Problem 145
;; 16 March 2007
;;
;; Some positive integers n have the property that the sum [ n + reverse(n) ]
;; consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and
;; 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and
;; 904 are reversible. Leading zeroes are not allowed in either n or
;; reverse(n).
;;
;; There are 120 reversible numbers below one-thousand.
;;
;; How many reversible numbers are there below one-billion (109)?
;;
;;
;; Answer: 608720
;;
;; Solution (pen and paper!).
;;
;; Let digits of the number n be x...y (x and y first and last digits of the
;; number).  So sum of n and reverse(n) is
;;
;;    x...y
;;  + y...x
;;
;; Here's list of xy so that x+y is odd, x+y<10, and x<y:
;;    12 14 16 18
;;    23 25 27
;;    34 36             (A)
;;    45
;; So there are 20 pairs total -- xy and yx -- of the digits that may be
;; last/first digits of the number, and so that n+rev(n) has same number of
;; digits as n (i.e. no carry).
;; And therefore there are 20 2-digit reversible numbers.
;;
;; Let's consider second and but-last digits of the n:
;;
;;    xu...vy
;;  + yv...ux
;;
;; Here's the list of uv so that u+v is odd, u+v<10, and u<v:
;;    01 03 05 07 09
;;    12 14 16 18
;;    23 25 27          (B)
;;    34 36
;;    45
;; So there are 30 pairs total -- uv and vu -- of the second and but-last 
;; digits (without carry).
;;
;; Let's now consider pairs of digits with the carry of their sum.
;; First/last digits xy: x+y is odd, x+y>10, x<y
;;    29
;;    38
;;    47 49
;;    56 58             (C)
;;    67 69
;;    78
;;    89
;; 20 pairs total (xy and yx).
;;
;; Second and but-last digits:
;;    xu...vy
;;    yv...ux
;; Since x+y>10, then u+v must be even, so the carry digit (1) from x+y added to
;; u+v will make it to be odd.  Here's the list of uv:
;;    00     02 04 06 08
;;    11     13 15 17
;;    22     24 26      (D)
;;    33     35
;;    44
;; 25 pairs total (5+10x2).
;;
;; Let's count 3-digit reversible numbers:
;;    abc
;;  + cba
;; The b+b is always even, so we need to take first/last pairs that provide 
;; carry of their sum, i.e. 20 paris from (C).
;; The b+b must be <10, so b is one of 0,1,2,3,4 -- five digits.
;; So there are 100 (20x5) 3-digit reversible numbers.
;;
;; 4-digit reversible numbers:
;;    abcd
;;  + bcda
;; Non-carry paris of ad (A) and bc (B) give 600 (20x30) reversible numbers,
;; no pairs from (C) and (D) work here.
;;
;; 5-digit reversible numbers:
;;    abcde
;;  + edcba
;; The c+c is always even, so bd pair must provide carry when summing.  So ae 
;; pair must not have a carry when summing.  So when
;;    ..cde
;;  + ..cba
;;  meet task condition, then
;;    abc..
;;  + edc..
;;  does not meet (and vise versa).
;;  So there are no reversible 5-digit numbers.
;;
;; 6-digit reversible numbers:
;;    abcdef
;;  + fedcba
;; It's even number of digits, so only non-carry pairs work:
;; 20 of af pairs, 30 of be pairs, and 30 of de pairs: 18000 total.
;;
;; 7-digit reversible numbers:
;;    abcdefg
;;  + gfedcba
;; d+d is even, so ce pair must have carry when summing, so ce pairs are from
;; (C).  Therefore bf pair must be from (D) to "swallow" the carry, and 
;; Therefore ab pair must provide carry when summing (ie must be from (C)):
;; 20 ab pairs, 25 bf pairs 20 ce pairs, and 5 d digits -- 50000 total.
;;
;; 8-digit reversible numbers:
;; even number of digits, so first/last pairs from (A), all internal pairs from
;; (B).  20x30x30x30=540000 reversible numbers.
;;
;; 9-digit reversible numbers:
;; same as for 5-digit numbers -- no reversible numbers.
;;
;; Total summing: sum number of 2-,3-,4-,6-,7-,8-digit reversible numbers =
;;   608720
;;
;;
;; Note: all the code was used to test pen-and-paper math.


(define (number->digits n)
  (unfold-right
    (lambda (x) (zero? x))
    (lambda (x) (remainder x 10))
    (lambda (x) (quotient x 10))
    n))


(define (digits->number digits)
  (fold
    (lambda (d res) (+ d (* res 10)))
    0
    digits))


(define (range from to)
  (let loop ((n to) (res '()))
    (if (< n from) res
      (loop (1- n) (cons n res)))))



(define (test maxn)
  (filter
    (lambda (x)
      (every odd? (number->digits (cdr x))))
    (map 
      (lambda (x)
        (cons x (+ x (digits->number (reverse (number->digits x))))))
      (filter 
        (lambda (x) (not (zero? (remainder x 10))))
        (range 11 maxn)))))


(define (test2 maxn)
  (let loop ((n 11) (count 0))
    (if (> n maxn) count
      (loop (1+ n)
            (if (and (not (zero? (remainder n 10))) 
                     (every odd? 
                            (number->digits 
                              (+ n (digits->number 
                                     (reverse (number->digits n)))))))
              (1+ count)
              count)))))


;; end of file
;; vim: sw=4 ts=4
