;; 2009-09-30
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=206
;;
;; Problem 206
;; 06 September 2008
;;
;; Find the unique positive integer whose square has the form
;; 1_2_3_4_5_6_7_8_9_0,
;; where each “_” is a single digit.
;;
;; Answer: 1389019170
;;
;; FIXME: bruteforce, 72 seconds on AMD Athlon(tm) 64 X2 Dual Core 3800+


;;
;; Check if n is an '1.2.3.4.5.6.7.8.9' -like number,
;; where '.' is any digit.
;;
(define (is1-9? n)
  (let loop ((n n)
             (i 9))
    (cond ((zero? i)
           #t)
          ((not (= i (remainder n 10)))
           #f)
          (else
            (loop (quotient n 100) (- i 1))))))


;;
;; Problem 206
;;
(define (p206)
  (let ((nmin (inexact->exact (ceiling (expt 10203040506070809 0.5))))
         ;; lower bound

        (nmax (inexact->exact (floor   (expt 19293949596979899 0.5)))))
         ;; upper bound

    (let loop ((n (+ 3 (- nmin (remainder nmin 10))))) 
              ;; start with '3' as a last digit of the n

      (if (> n nmax)
        #f
        (if (is1-9? (* n n))
          (* n 10)
          (loop (+ n (if (= 3 (remainder n 10)) 4 6))))))))
                     ;; add 4 if last digit is '3' and
                     ;; 6 if last digit is '7',
                     ;; so it makes the following transformation:
                     ;;    ...3 -> ...7, and
                     ;;    ...7 -> ...3
                     ;;
                     ;; (because only square of 3 and 7 ends with 9)

;; end of file
