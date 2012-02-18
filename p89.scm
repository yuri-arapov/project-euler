;; 2010-05-01
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=89
;;
;; Problem 89
;; 18 February 2005
;;
;; The rules for writing Roman numerals allow for many ways of writing each
;; number (see FAQ: Roman Numerals). However, there is always a "best" way of
;; writing a particular number.
;;
;; For example, the following represent all of the legitimate ways of writing
;; the number sixteen:
;;
;; IIIIIIIIIIIIIIII
;; VIIIIIIIIIII
;; VVIIIIII
;; XIIIIII
;; VVVI
;; XVI
;;
;; The last example being considered the most efficient, as it uses the least
;; number of numerals.
;;
;; The 11K text file, roman.txt (right click and 'Save Link/Target As...'),
;; contains one thousand numbers written in valid, but not necessarily minimal,
;; Roman numerals; that is, they are arranged in descending units and obey the
;; subtractive pair rule (see FAQ for the definitive rules for this problem).
;;
;; Find the number of characters saved by writing each of these in their
;; minimal form.
;;
;; Note: You can assume that all the Roman numerals in the file contain no more
;; than four consecutive identical units.
;;
;; Answer: 743


;; Check if sequence pref is a prefix of sequence s.
;; Return subsequence of s that follows the perefix or #f if
;; pref is not a prefix.
;; Example:
;;   (prefix? '(1 2 3) '(1 2 3 4)) -> (4)
;;   (prefix? '(1 2 3) '(4 5 6))   -> #f
;;   
(define (prefix? pref s)
  (cond
    ((null? pref) s)
    ((or (null? s) (not (eq? (car s) (car pref)))) #f)
    (else (prefix? (cdr pref) (cdr s)))))


;; Apply pred function on elements of sequence s and return
;; first successful result.
;; Return #f in case of no success.
;;
(define (first-match pred s)
  (if (null? s) #f
    (let ((res (pred (car s))))
      (if res res
        (first-match pred (cdr s))))))


;; Roman numbers.
;; IMPORTANT: Order does matter, the greater romans must go first,
;; IMPORTANT: so the roman->number function would work correctly.
;;
(define *romans*
  '((1000 .  "M")
    ( 900 . "CM")
    ( 500 .  "D")
    ( 400 . "CD")
    ( 100 .  "C")
    (  90 . "XC")
    (  50 .  "L")
    (  40 . "XL")
    (  10 .  "X")
    (   9 . "IX")
    (   5 .  "V")
    (   4 . "IV")
    (   1 .  "I")))


;; Convert roman number (string) into number.
;; NOTE: Descending order of the roman number components is
;; NOTE: not verified.
;;
(define (roman->number s)
  (let ((r (map (lambda (i) 
                  (cons (car i) 
                        (string->list (cdr i))))
                *romans*)))
    (let loop ((s (string->list (string-upcase s)))
               (n 0))
      (if (null? s)
        n
        (let ((next (first-match 
                      (lambda (x)
                        (let ((rn (car x))
                              (rs (cdr x)))
                          (let ((p (prefix? rs s)))
                            (if p
                              (cons p rn)
                              #f))))
                      r)))
          (if next
            (loop (car next) (+ n (cdr next)))
            (error "roman->number error:" (list->string s))))))))


;; Convert number into roman (string).
;;
(define (number->roman n)
  (let loop ((n n)
             (r *romans*)
             (res ""))
    (if (zero? n)
      res
      (let ((rn (car (car r)))
            (rs (cdr (car r))))
        (if (>= n rn)
          (loop (- n rn) r (string-append res rs))
          (loop n (cdr r) res))))))


;; Solve problem 89.
;;
(define (p89)
  (let* ((a (iterate-input-file cons '() "roman.txt"))
         (b (map (lambda (i)
                   (number->roman (roman->number i)))
                 a))
         (c (map (lambda (x y)
                   (- (string-length x)
                      (string-length y)))
                 a b)))
    (apply + c)))


;; end of file
;; vim: sw=4 ts=4
