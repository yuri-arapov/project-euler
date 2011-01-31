;; 2010-12-28
;;
;; Project Euler
;;
;; Problem 118
;; 24 March 2006
;;
;; Using all of the digits 1 through 9 and concatenating them freely to form
;; decimal integers, different sets can be formed. Interestingly with the set
;; {2,5,47,89,631}, all of the elements belonging to it are prime.
;;
;; How many distinct sets containing each of the digits one through nine
;; exactly once contain only prime elements?
;;
;; Answer: 44680


(load "product.scm")
(load "permutations.scm")
(load "miller-rabin-primality-test.scm")


(define (print s) (for-each (curry format #t "~a~%") s))


(define (fill-variants n len)

  (define (helper n len)
    (cond 
      ((> n len) '())
      ((= n len) (list n))
      (else      (product (list n) (fill-variants n (- len n))))))

  (define (app res s) ;; smarter version of append
    (cond ((null? s)        res)
          ((list? (car s))  (append res s))
          (else             (append res (list s)))))

  (let loop ((n n) (res '()))
    (if (> n len)
      res
      (loop (1+ n) (app res (helper n len))))))


(define (make-set . digits)     (apply logior (map (curry expt 2) digits)))
(define (set-contains s1 s2)    (= (logand s1 s2) s2))  ;; true if s1 contains s2
(define (set-union s1 s2)       (logior s1 s2))         ;; union of s1 and s2
(define (set-exclude s1 s2)     (logand s1 (lognot s2)));; exclude s2 from s1


;; Turn list of digits into number.
;; Lower digits go first:
;;  (digits->number '(1 2 3)) -> 321
(define (digits->number digits)
  (fold-right (lambda (d res) (+ d (* res 10))) 0 digits))


(define (make-prime prime prime-digits)     (cons prime (apply make-set prime-digits)))
(define (prime-value p)                     (car p))
(define (prime-digits p)                    (cdr p))


;; 2- to 9-digit primes.
(define (digits->prime digits)
  (and-let* (((member (car digits) '(1 3 7 9)))
             (n (digits->number digits))
             ((prime? n)))
     (make-prime n digits)))


(define (make-n-digit-primes n)
  (sort
    (filter identity (map digits->prime (permutations-n '(1 2 3 4 5 6 7 8 9) n)))
    (lambda (x y) (< (prime-value x) (prime-value y)))))


(define (make-primes)
  (append
    (list (map (lambda (p) (make-prime p (list p))) '(2 3 5 7))) ;; 1-digit primes
    (map make-n-digit-primes '(2 3 4 5 6 7 8))           ;; 2- to 8-digit primes
    (list '())))                                                 ;; 9-digit primes (none)


(define (find-prime primes set prev num-digits)
  (let loop ((s (list-ref primes (1- num-digits))))
    (cond 
      ((null? s) #f)
      ((and (> (prime-value (car s)) prev)
            (set-contains set (prime-digits (car s))))
       (car s))
      (else
        (loop (cdr s))))))


(define (count-distinct-sets-rec primes template)
  (define (helper prev set template xx)
    (if (null? template)
      (if (null? xx)
        0
        (begin
          (format #t "~a~%" xx)
          1))
      (let* ((num-digits (car template))
             (p (find-prime primes set prev num-digits)))
        (if (not p)
          0
          (+ (helper (prime-value p)
                     (set-exclude set (prime-digits p))
                     (cdr template) 
                     (append xx (list (prime-value p))))
             (helper (prime-value p)
                     set
                     template
                     xx))))))
  (helper 0 (make-set 1 2 3 4 5 6 7 8 9) template '()))


(define (count-distinct-sets-iter primes template)

  (define (push stack prev set template xx)
    (cons (list prev set template xx) stack))

  (let loop ((accum 0)
             (stack (push '() 0 (make-set 1 2 3 4 5 6 7 8 9) template '())))
    (if (null? stack)
      accum
      (let-values (((prev set template xx) (apply values (car stack))))
        (let ((stack (cdr stack)))
          (if (null? template)
            (if (null? xx)
              (loop accum stack)
              (begin
                (format #t "~a~%" xx)
                (loop (1+ accum) stack)))
            (let* ((num-digits (car template))
                   (p (find-prime primes set prev num-digits)))
              (if (not p)
                (loop accum stack)
                (loop accum (push (push stack 
                                        (prime-value p)
                                        set
                                        template
                                        xx)
                                  (prime-value p)
                                  (set-exclude set (prime-digits p))
                                  (cdr template)
                                  (append xx (list (prime-value p)))))))))))))


(define (p118)
  (let ((res (map (curry count-distinct-sets-iter (make-primes))
                  (fill-variants 1 9))))
    (values res (apply + res))))


;; end of file
;; vim: ts=4 sw=4 et
