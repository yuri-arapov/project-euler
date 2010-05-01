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


(define (prefix? pref s)
  (cond
    ((null? pref) s)
    ((null? s) #f)
    ((not (eq? (car s) (car pref))) #f)
    (else
      (prefix? (cdr pref) (cdr s)))))


(define (first-match pred s data)
  (if (null? s)
    #f
    (let ((res (pred (car s) data)))
      (if res
        res
        (first-match pred (cdr s) data)))))


(define romans
  (list (cons 1000  "M")
        (cons  900 "CM")
        (cons  500  "D")
        (cons  400 "CD")
        (cons  100  "C")
        (cons   90 "XC")
        (cons   50  "L")
        (cons   40 "XL")
        (cons   10  "X")
        (cons    9 "IX")
        (cons    5  "V")
        (cons    4 "IV")
        (cons    1  "I")))


(define (roman->number s)
  (let ((r (map (lambda (i) 
                  (cons (car i) 
                        (string->list (cdr i))))
                romans)))
    (let loop ((s (map char-upcase (string->list s)))
               (n 0))
      (if (null? s)
        n
        (let ((next (first-match 
                      (lambda (x s)
                        (let ((rn (car x))
                              (rs (cdr x)))
                          (let ((p (prefix? rs s)))
                            (if p
                              (cons p rn)
                              #f))))
                      r 
                      s)))
          (if next
            (loop (car next) (+ n (cdr next)))
            (error "roman->number error:" (list->string s))))))))


(define (number->roman n)
  (let loop ((n n)
             (r romans)
             (res ""))
    (if (zero? n)
      res
      (let ((rn (car (car r)))
            (rs (cdr (car r))))
        (if (>= n rn)
          (loop (- n rn) r (string-append res rs))
          (loop n (cdr r) res))))))


(define (read-romans file)
  (define (read-lines port)
    (let loop ((res '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
          (reverse res)
          (loop (cons line res))))))
  (call-with-input-file file read-lines))


(define (p89)
  (let* ((a (read-romans "roman.txt"))
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
