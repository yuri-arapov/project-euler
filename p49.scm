;; 30 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; Problem 49
;;
;; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
;; increases by 3330, is unusual in two ways: (i) each of the three terms are
;; prime, and, (ii) each of the 4-digit numbers are permutations of one
;; another.
;;
;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
;; primes, exhibiting this property, but there is one other 4-digit increasing
;; sequence.
;;
;; What 12-digit number do you form by concatenating the three terms in this
;; sequence?
;;
;; Answer: 296962999629


(load "permutations.scm")
(load "prime.scm")
(load "print.scm")
(load "range.scm")


(define (digits->number dd) (reduce (lambda (x res) (+ (* res 10) x)) 0 dd))


(define (number->digits n)
  (if (< n 10)
    (list n)
    (append (number->digits (quotient n 10)) (list (remainder n 10)))))


(define (sort-numbers ls) (sort-list ls (lambda (x y) (< x y))))


(define sort-digits sort-numbers)


(define (pipeline arg . procedures)
  (define (iter arg proc)
    (if (null? proc)
      arg
      (iter ((car proc) arg) (cdr proc))))
  (iter arg procedures)) 


(define (get-num x) (car x))


(define (get-tag x) (cadr x))


(define (split-by-tag ls)

  (define (iter tag subls ls res)
    (if (null? ls)
      (append res (list subls))
      (if (= tag (get-tag (car ls)))
        (iter tag
              (append subls (list (get-num (car ls))))
              (cdr ls)
              res)
        (iter (get-tag (car ls))
              '()
              ls
              (append res (list subls))))))

  (if (null? ls)
    '()
    (iter (get-tag (car ls))
          '()
          ls
          '())))


(define (the-three ls)

  (define (iter a b c)

    (define (next-b)
      (let ((b (cdr b)))
        (if (null? b)
          (next-a)
          (iter a b (cdr b)))))

    (define (next-a)
      (let ((a (cdr a)))
        (if (< (length a) 3)
          false
          (iter a (cdr a) (cddr a)))))

    (cond ((null? c) (next-b))
          ((null? b) (next-a))
          ((null? a) false)
          (else
            (if (= (- (car b) (car a)) (- (car c) (car b)))
              (list (car a) (car b) (car c))
              (iter a b (cdr c))))))
  (if (< (length ls) 3)
    false
    (iter ls (cdr ls) (cddr ls))))


(define (p49)
  (let* ((a (filter prime? (range 1000 9999)))
         ;; all 4-digit primes

         (b (map (lambda (x) (list x (sort-digits (number->digits x)))) a))
         ;; (4 8 1 7) -> (4817 (1 4 7 8))
         ;;                     ^^^^^^^
         ;;                     sorted list of digits

         (c (map (lambda (x) (list (car x) (digits->number (cadr x)))) b))
         ;; (4817 (1 4 7 8)) -> (4817 1478)
         ;; i.e. number and tag pair

         (d (sort-list c (lambda (x y) (< (get-tag x) (get-tag y)))))
         ;; sort list by tags

         (e (split-by-tag d))
         ;; gather together save-tag numbers

         (f (map (lambda (x) (sort-list x (lambda (u v) (< u v)))) e))
         ;; sort numbers in each sublist
         
         (g (sort-list f (lambda (x y) (< (car x) (car y))))))
         ;; sort by first number in each sublist (just to be able
         ;; to check some results easily)

    (filter (lambda (x) x) (map the-three g))))


;; end of file
