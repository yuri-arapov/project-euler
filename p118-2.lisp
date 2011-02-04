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


(load "product")
(load "permutations")
(load "miller-rabin-primality-test")


(load "lisp-utils")


(defun println (s) (for-each (curry #'format t "~a~%") s))


(defun fill-variants (n len)
  (labels ((helper (n len)
             (cond 
               ((> n len) '())
               ((= n len) (list n))
               (t         (product (list n) (fill-variants n (- len n))))))
           (app (res s)
             (cond ((null s)         res)
                   ((listp (car s))  (append res s))
                   (t                (append res (list s)))))
           (iter (n res)
             (if (> n len)
               res
               (iter (1+ n) (app res (helper n len))))))
    (iter n '())))


(defun make-set (&rest digits) (apply #'logior (mapcar (curry #'expt 2) digits)))
(defun set-contains (s1 s2)    (= (logand s1 s2) s2))  ;; true if s1 contains s2
(defun set-union (s1 s2)       (logior s1 s2))         ;; union of s1 and s2
(defun set-exclude (s1 s2)     (logand s1 (lognot s2)));; exclude s2 from s1


;; Turn list of digits into number.
;; Lower digits go first:
;;  (digits->number '(1 2 3)) -> 321
(defun digits->number (digits)
  (fold-right #'(lambda (d res) (+ d (* res 10))) 0 digits))


(defun make-prime (prime prime-digits)     (cons prime (apply #'make-set prime-digits)))
(defun prime-value (p)                     (car p))
(defun prime-digits (p)                    (cdr p))


;; 2- to 9-digit primes.
(defun digits->prime (digits)
  (and (member (car digits) '(1 3 7 9))
       (let ((n (digits->number digits)))
         (and (primep n)
              (make-prime n digits)))))


(defun make-n-digit-primes (n)
  (sort
    (filter #'identity (mapcar #'digits->prime (permutations-n '(1 2 3 4 5 6 7 8 9) n)))
    #'(lambda (x y) (< (prime-value x) (prime-value y)))))


(defun make-primes()
  (append
    (list (mapcar #'(lambda (p) (make-prime p (list p))) '(2 3 5 7))) ;; 1-digit primes
    (mapcar #'make-n-digit-primes '(2 3 4 5 6 7 8))                   ;; 2- to 8-digit primes
    (list '())))                                                      ;; 9-digit primes (none)


(defun find-prime (primes set prev num-digits)
  (loop for p in (elt primes (1- num-digits))
        when (and (> (prime-value p) prev)
                  (set-contains set (prime-digits p)))
        return p))


;;(defun count-distinct-sets-rec (primes template)
;;  (defun helper (prev set template xx)
;;    (if (null? template)
;;      (if (null? xx)
;;        0
;;        (begin
;;          (format #t "~a~%" xx)
;;          1))
;;      (let* ((num-digits (car template))
;;             (p (find-prime primes set prev num-digits)))
;;        (if (not p)
;;          0
;;          (+ (helper (prime-value p)
;;                     (set-exclude set (prime-digits p))
;;                     (cdr template) 
;;                     (append xx (list (prime-value p))))
;;             (helper (prime-value p)
;;                     set
;;                     template
;;                     xx))))))
;;  (helper 0 (make-set 1 2 3 4 5 6 7 8 9) template '()))


(defun count-distinct-sets-iter (primes template)

  (defun stack-push (stack prev set template xx)
    (cons (list prev set template xx) stack))

  (labels ((iter (accum stack)
                 (if (null stack)
                   accum
                   (destructuring-bind (prev set template xx) (car stack)
                     (let ((stack (cdr stack)))
                       (if (null template)
                         (if (null xx)
                           (iter accum stack)
                           (progn 
;;;                             (format t "~a~%" xx)
                             (iter (1+ accum) stack)))
                         (let* ((num-digits (car template))
                                (p (find-prime primes set prev num-digits)))
                           (if (not p)
                             (iter accum stack)
                             (iter accum (stack-push (stack-push stack
                                                                 (prime-value p)
                                                                 set
                                                                 template
                                                                 xx)
                                                     (prime-value p)
                                                     (set-exclude set (prime-digits p))
                                                     (cdr template)
                                                     (append xx (list (prime-value p)))))))))))))
    (iter 0 (stack-push '() 0 (make-set 1 2 3 4 5 6 7 8 9) template '()))))


(defun p118 ()
  (let ((res (mapcar (curry #'count-distinct-sets-iter (make-primes))
                  (fill-variants 1 9))))
    (values res (apply #'+ res))))


;; end of file
;; vim: ts=4 sw=4 et
