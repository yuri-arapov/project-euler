;; 20 March 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;;
;; http://projecteuler.net/index.php?section=problems&id=42
;;
;; Problem 42
;; 25 April 2003
;;
;; The nth term of the sequence of triangle numbers is given by, 
;;
;;        n(n+1)
;;   tn = ------
;;          2
;;
;; so the first ten triangle numbers are:
;;
;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;;
;; By converting each letter in a word to a number corresponding to its
;; alphabetical position and adding these values we form a word value. For
;; example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word
;; value is a triangle number then we shall call the word a triangle word.
;;
;; Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
;; containing nearly two-thousand common English words, how many are triangle
;; words?
;;
;; Answer: 162


;; test for being triangular number:
;;
;; http://en.wikipedia.org/wiki/Triangle_number
;;
;;       --------
;;      V 8x + 1  - 1
;;  n = -------------
;;            2
;;
;; if n is an integer, then x is n-th triangular number
;;


(load "print.scm")


(define (triangular-number? x)
  (integer? (/ (- (sqrt (+ (* 8 x) 1)) 1) 2)))


(define (word->number w)
  (define base (- (char->ascii #\A) 1))
  (apply + (map (lambda (c) (- (char->ascii c) base)) 
                (string->list w))))


(define (triangular-word? w)
  (triangular-number? (word->number w)))


(define (p42)

  (define (read-words f)
    (let ((w (read-line f)))
      (if (eof-object? w)
        '()
        (cons w (read-words f)))))

  (length (filter triangular-word? (call-with-input-file "words" read-words))))


(define (p42-2)

  (define (read-triangular-words f)
    (let ((w (read-line f)))
      (cond ((eof-object? w) '())
            ((triangular-word? w) (cons w (read-triangular-words f)))
            (else (read-triangular-words f)))))

  (length (call-with-input-file "words" read-triangular-words)))


;; end of file
