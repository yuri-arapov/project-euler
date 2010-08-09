;; 2010-08-05
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=98
;;
;; Problem 98
;; 17 June 2005
;;
;; By replacing each of the letters in the word CARE with 1, 2, 9, and 6
;; respectively, we form a square number: 1296 = 36^(2). What is remarkable is
;; that, by using the same digital substitutions, the anagram, RACE, also forms
;; a square number: 9216 = 96^(2). We shall call CARE (and RACE) a square
;; anagram word pair and specify further that leading zeroes are not permitted,
;; neither may a different letter have the same digital value as another
;; letter.
;;
;; Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
;; containing nearly two-thousand common English words, find all the square
;; anagram word pairs (a palindromic word is NOT considered to be an anagram of
;; itself).
;;
;; What is the largest square number formed by any member of such a pair?
;;
;; NOTE: All anagrams formed must be contained in the given text file.
;;
;; Answer: 18769



(load "integer-square-root.scm")
(load "read-file.scm")
(load "number-digits.scm")


;; Make a key out of a word.
;;
(define (word->key word)
  (list->string (sort (string->list word) char<?)))



;; Take a list of (key.word) pairs sorted by key
;; and turn it into list of anagrams:
;;   (pack-anagrams ((key1.word1)...) -> ((word1 word2...)...)
;; where word1 and word2 are anagrams.
;;
(define (pack-anagrams s)

  (define (get-key s) (car (car s)))
  (define (get-value s) (cdr (car s)))

  (define (store vals res)
    (cons vals res))

  (let loop ((key       (get-key s))
             (vals      (list (get-value s)))
             (s         (cdr s))
             (res       '()))
    (cond 
      ((null? s)
       (reverse (store vals res)))
      ((string= key (get-key s))
       (loop key (cons (get-value s) vals) (cdr s) res))
      (else
        (loop (get-key s) (list (get-value s)) (cdr s) (store vals res))))))



;; Return list of n-digit square numbers.
;;
(define (n-digit-squares n)
  (let ((from   (expt 10 (1- n)))
        (to     (1- (expt 10 n))))

    (let loop ((x   (integer-square-root from)) ;; x^2 <= from < (x+1)^2
               (res '()))
      (let ((xx (* x x)))
        (cond
          ((> xx to)        (reverse res))
          ((<= from xx to)  (loop (1+ x) (cons xx res)))
          (else             (loop (1+ x) res)))))))



;; Determine number of digits of number n.
;;
(define (number-of-digits n)
  (inexact->exact (ceiling (log10 (1+ n)))))



;; Return list of digits ordered same way as sorted list of chars:
;;  (reorder-digits 
;;    (string->list "hello") 
;;    (number->digits 12345)) -> (2 1 3 4 5)
;;                             ;; e h l l 0
;;
(define (reorder-digits chars digits)
  (map first
       (sort
         (zip digits chars)
         (lambda (a b) (char<? (second a) (second b))))))



;; Take anagram -- words a1, and a2 -- and square number s1,
;; put digits of s1 at the same positions as characters of a2 and
;; so make another number:
;;   (anagram-pair->2nd-square "CARE" "RACE" 1296) -> 9216
;;
(define (anagram-pair->2nd-square a1 a2 s1)
  (let ((a1-digits (reorder-digits 
                     (string->list a1)
                     (number->digits s1)))
        (a2-indices (reorder-digits
                      (string->list a2)
                      (iota (string-length a2)))))
    (digits->number
      (map first
           (sort
             (zip a1-digits a2-indices)
             (lambda (a b) (< (second a) (second b))))))))
                  

;; Test of anagram-pair->2nd-square.
;;
(define (f1) (anagram-pair->2nd-square "CARE" "RACE" 1296))



;; Return true if every character of str corresponds to
;; its unique digit of number:
;;   (same-char-same-digit? "CARE" 1296) -> #t
;;   (same-char-same-digit? "REDUCTION" 400400100) -> #f
;;
(define (same-char-same-digit? str number)
  (let-values (((d s)
               (unzip2
                 (sort
                   (zip
                     (number->digits number)
                     (string->list str))
                   (lambda (x y) (< (car x) (car y)))))))
;;    (format #t "~a\n" d)
;;    (format #t "~a\n" s)
    (let loop ((s s) (d d))
      (cond
        ((or (null? d) (null? (cdr d)))
         #t)
        ((and (= (car d) (cadr d)) (not (char=? (car s) (cadr s))))
         #f)
        (else
          (loop (cdr s) (cdr d)))))))



;; Solve problem 98.
;;
(define (p98)
  (let* (
         ;; words from file
         (x (read-file "words"))

         ;; words -> (key . word) pairs
         (y (map (lambda (s) (cons (word->key s) s))
                 x))

         ;; sort by keys
         (z (sort y (lambda (i j) (string< (car i) (car j)))))

         ;; pack anagrams together
         (f (pack-anagrams z))

         ;; filter out non-anagrams
         (i (filter (lambda (x) (> (length x) 1)) f))

         ;; sort anagrams to bring the longest ones at the beginning
         (k (sort i (lambda (a b) (> (string-length (car a))
                                     (string-length (car b)))))))

    (let loop ((res-anagram     #f)
               (res-square      0)
               (anagrams        k))
      (cond
        ((null? anagrams)
         (values res-anagram 
                 res-square 
                 (integer-square-root res-square)))

        (else
          (let* ((anagram (car anagrams))   ;; current anagram
                 (x (fold (lambda (s1 res)  ;; and its max square number
                            (or 
                              (and-let* (
                                ((same-char-same-digit? (first anagram) s1))
                                (s2 (anagram-pair->2nd-square (first anagram)
                                                              (second anagram)
                                                              s1))
                                ((= (number-of-digits s1) (number-of-digits s2)))
                                ((square? s2)))
                                   (max res s1 s2))
                              res))
                          0
                          (n-digit-squares (string-length (first anagram))))))
            (cond
              ((> x res-square)
               (format #t "~a -> ~a\n" anagram x)
               (loop anagram x (cdr anagrams)))

              (else
                (loop res-anagram res-square (cdr anagrams))))))))))


;; end of file
;; vim: ts=4 sw=4
