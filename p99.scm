;; 05 June 2008
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=99
;;
;; Problem 99
;; 01 July 2005
;;
;; Comparing two numbers written in index form like 2^11 and 3^7 is not
;; difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.
;;
;; However, confirming that 632382^518061 > 519432^525806 would be much more
;; difficult, as both numbers contain over three million digits.
;;
;; Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text
;; file containing one thousand lines with a base/exponent pair on each line,
;; determine which line number has the greatest numerical value.
;;
;; NOTE: The first two lines in the file represent the numbers in the example
;; given above.
;;
;; Answer: 709
;;
;; NOTE:
;;   the key is to use logarithm:
;;
;;       b
;;      a   ->  b * log(a)


(define (read-base-exp-file fname)
  (call-with-input-file fname 
                        (lambda (f)
                          (let loop ((lineno 1)
                                     (line (read-line f))
                                     (res '()))
                            (if (eof-object? line)
                              (reverse res)
                              (let* ((ll   (map string->number (string-split line #\,)))
                                     (base (first ll))
                                     (pwr  (second ll)))
                                (loop (+ lineno 1)
                                      (read-line f)
                                      (cons (list lineno 
                                                  (* pwr (log10 base)) 
                                                  base 
                                                  pwr) 
                                            res))))))))


(define (p99)
  (let* ((x (read-base-exp-file "base_exp.txt"))                     ;; read
         (y (sort-list x (lambda (a b) (> (second a) (second b)))))) ;; sort
    (car y)))                                                        ;; show head



;; end of file
