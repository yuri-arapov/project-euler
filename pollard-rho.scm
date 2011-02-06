;; Pollard rho factoring.
;;
;; http://programmingpraxis.com/2011/01/21/pollard-rho-revisited/2/
;;
;; NOTE: unlike original (pollard factor n) the prime? predicate
;; NOTE: passed as argument.


(define (wheel-factors n limit)
  (define (wheel . xs) (set-cdr! (last-pair xs) xs) xs)
  (let loop ((n n) (f 2) (fs '()) (ws (cons* 1 2 2 (wheel 4 2 4 2 4 6 2 6))))
    (cond ((< limit f) (values n (reverse fs)))
          ((or (= n 1) (< n (* f f))) (values 1 (reverse (cons n fs))))
          ((zero? (modulo n f)) (loop (/ n f) f (cons f fs) ws))
          (else (loop n (+ f (car ws)) fs (cdr ws))))))


(define (pollard prime? factor n)
  (let-values (((n fs) (wheel-factors n 1000)))
    (sort (let fact ((n n) (fs fs))
            (cond ((= n 1) fs)
                  ((prime? n) (cons n fs))
                  (else (let ((f (factor n 1 100000000)))
                          (append fs (fact f '()) (fact (/ n f) '()))))))
          <)))


(define (floyd n c limit)
  (define (f x) (modulo (+ (* x x) c) n))
  (define (g p t h) (modulo (* p (abs (- t h))) n))
  (let loop1 ((j 1) (t 2) (h (f 2)) (x 2) (y (f 2)) (p 1))
    (if (= j limit) (error 'floyd "timeout")
      (if (= t h) (floyd n (+ c 1) (- limit j))
        (if (positive? (modulo j 100)) (loop1 (+ j 1) (f t) (f (f h)) x y (g p t h))
          (let ((d (gcd p n)))
            (if (= d 1) (loop1 (+ j 1) (f t) (f (f h)) t h 1)
              (if (< 1 d n) d
                (let loop2 ((k 1) (x x) (y y) (d (gcd (- x y) n)))
                  (if (= k 100) (floyd n (+ c 1) (- limit j))
                    (if (= d 1) (loop2 (+ k 1) (f x) (f (f y)) (gcd (- x y) n))
                      (if (= d n) (floyd n (+ c 1) (- limit j))
                        d))))))))))))


(define (brent n c limit)
  (define (f y) (modulo (+ (* y y) c) n))
  (define (g p x y) (modulo (* p (abs (- x y))) n))
  (let loop1 ((x 2) (y (+ 4 c)) (z (+ 4 c)) (j 1) (q 2) (p 1))
    (if (= j limit) (error 'brent "timeout")
      (if (= x y) (brent n (+ c 1) (- limit j)) ; cycle
        (if (= j q) (let ((t (f y))) (loop1 y (f y) z (+ j 1) (* q 2) (g p y t)))
          (if (positive? (modulo j 100)) (loop1 x (f y) z (+ j 1) q (g p x y))
            (let ((d (gcd p n)))
              (if (= d 1) (loop1 x (f y) y (+ j 1) q (g p x y))
                (if (< 1 d n) d ; factor
                  (let loop2 ((k 1) (z (f z)))
                    (if (= k 100) (brent n (+ c 1) (- limit j))
                      (let ((d (gcd (- z x) n)))
                        (if (= d 1) (loop2 (+ k 1) (f z))
                          (if (= d n) (brent n (+ c 1) (- limit j))
                            d))))))))))))))


;; end of file
;; vim: ts=4 sw=4 et
