;; 03 Apr. 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=113
;;
;; Problem 113
;; 10 February 2006
;;
;; Working from left-to-right if no digit is exceeded by the digit to its left
;; it is called an increasing number; for example, 134468.
;;
;; Similarly if no digit is exceeded by the digit to its right it is called a
;; decreasing number; for example, 66420.
;;
;; We shall call a positive integer that is neither increasing nor decreasing a
;; "bouncy" number; for example, 155349.
;;
;; As n increases, the proportion of bouncy numbers below n increases such that
;; there are only 12951 numbers below one-million that are not bouncy and only
;; 277032 non-bouncy numbers below 10^10.
;;
;; How many numbers below a googol (10^100) are not bouncy?
;;
;; Answer: 51161058134250
;;
;;
;; FIXME: see LISP code here: http://projecteuler.net/index.php?section=forum&id=113&page=2
;; FIXME: people use nCr (number of combinations) to find the solution
;; FIXME: quickly.
;;
;; 
;; Let's take 2-digit numbers.
;;
;; Increasing:          
;;   00, 01, ... 09
;;   11, 12, ... 19      
;;   22, 23, ... 29      
;;   ...                 
;;   99                  
;;
;; Number of 2-digit increasing numbers starting from digit x: 10-x
;;
;; Decreasing:
;;   99, 98, 97, ... 90
;;   88, 87, 86, ... 80
;;   ...           
;;   11, 10
;;   00
;;
;; Number of 2-digit decreasing numbers starting from digit x: x+1
;;
;;
;; Now consider 3-digit number x?? (?? means 2 low digits are not defined).
;;
;; Number of increasing 3-digit numbers starting with x is
;;
;;   sum NIN(2, n), n=x,9
;;
;;   where NIN(2, n) is number of 2-digit increasing numbers starting from n.
;;
;;
;; Number of decreasing 3-digit numbers starting from x is
;;  
;;   sum NDN(2, n), n=0,x
;;
;;
;; Generalization (D is number of ditigs in the number, x is first digit of the number):
;;
;;   NIN(D, x) = sum NIN(D-1, n), n=0,x
;;
;;   NIN(2, x) = 10-x
;;
;;   NDN(D, x) = sum NDN(D-1, n), n=x,9
;;
;;   NDN(2, x) = x+1
;;
;; 
;; The sequence of numbers we have to check is:
;;  1, 2, ... 10^100-1 (i.e one-hundred-digit number, last one is 999...999)
;;
;; NOTE: 1, 2, ... 99 -- are all non-bouncy by definition.
;;


(define (range from to) (iota (+ 1 (- to from)) from))


(define NI (make-array 0 '(3 100) '(0 9))) ;; 100x10 matrix
(define ND (make-array 0 '(3 100) '(0 9))) ;; 100x10 matrix


(define (NIN D x)
  (if (= D 2)
    (- 10 x)
    (let ((r (array-ref NI D x)))
      (if (> r 0)
        r
        (let ((r (apply + (map (lambda (n) (NIN (- D 1) n)) 
                               (range x 9)))))
          (array-set! NI r D x)
          r)))))


(define (NDN D x)
  (if (= D 2)
    (+ x 1)
    (let ((r (array-ref ND D x)))
      (if (> r 0)
        r
        (let ((r (apply + (map (lambda (n) (NDN (- D 1) n))
                               (range 0 x)))))
          (array-set! ND r D x)
          r)))))


(define (nin-all digits)
  (apply + (map (lambda (x) (NIN digits x))
                (range 1 9))))

(define (ndn-all digits)
  (apply + (map (lambda (x) (NDN digits x))
                (range 1 9))))

(define (p113 max-digits)

  (let ((nin (apply + (map nin-all (range 3 max-digits))))
        (ndn (apply + (map ndn-all (range 3 max-digits)))))
    (+ nin ndn 99 (- (* (- max-digits 2) 9)))))
;;             ^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^
;;        first|  |number of numbers counted twice
;;   99 numbers|  |as increasing and decreasing,
;;      are not|  |e.g. 9 3-digit numbers (111, 222, ... 999)
;;    bouncy by|  |are counted as increading and as decreasing,
;;   definition|  |9 4-digit numbers (1111, 2222, ... 9999) 
;;                |counted the same way, etc.



(define (number->digits n)
  (if (< n 10)
    (list n)
    (append (number->digits (quotient n 10)) (list (remainder n 10)))))


(define (mono-number? n)
  (let ((nn (number->digits n)))
    (let loop ((nn nn))
      (cond ((null? nn) #t)
            ((null? (cdr nn)) #t)
            ((not (= (car nn) (cadr nn))) #f)
            (else (loop (cdr nn)))))))
        

(define (number->diff n)
  (let loop ((res '())
             (nn   (number->digits n)))
    (cond ((null? nn) res)
          ((null? (cdr nn)) res)
          (else (loop (append res (list (- (cadr nn) (car nn))))
                      (cdr nn))))))


(define (increasing-number? n)
  (let loop ((diff (number->diff n)))
    (cond ((null? diff) #t)
          ((< (car diff) 0) #f)
          (else (loop (cdr diff))))))


(define (decreasing-number? n)
  (let loop ((diff (number->diff n)))
    (cond ((null? diff) #t)
          ((> (car diff) 0) #f)
          (else (loop (cdr diff))))))


;; end of file
