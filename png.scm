;; png.scm
;;
;; prime numbers generator
;;


(define (make-primes-generator)
  ;; make generator of prime numbers.
  ;;
  ;; operators:
  ;;   'first    -- return first prime number and reset generator.
  ;;   'next     -- return next prime number (relatively to previous 
  ;;                call of 'first or 'next).
  ;;   'last     -- return last prime number determined so far.
  ;;   'prime?   -- test if given number is prime.
  ;;   'show     -- return list of prime number determined so far.
  ;;
  ;; example:
  ;;   (define x (make-primes-generator))
  ;;   (x 'first)
  ;;   (x 'next)
  ;;   (x 'prime? 34)
  ;;   (x 'prime" 34 35 36 37) ;; will return list of booleans


  (define (sqr x) (* x x))
  ;; return square of the argument


  (let* ((primes (list 2 3))                  ;; [initial] list of primes.
                                              ;; 3 is added to simplify adding
                                              ;; new primes: 3+2, 5+2, etc., since
                                              ;; only odd number are candidates for primes.

         (last-prime-ptr (last-pair primes))  ;; pointer to the last prime in the list.
                                              ;; purpose is to make adding of new primes
                                              ;; fast.

         (ptr '()))                           ;; pointer to current prime, used for 'first and 'next
                                              ;; operators.


    (define (expand)
      ;; append one more prime to the primes list.
      (let loop ((candidate (+ (last) 2))
                 (divizors primes))
        (cond ((or (null? divizors)
                   (> (sqr (car divizors)) candidate))
               (let ((new-pair (cons candidate '())))
                 (set-cdr! last-prime-ptr new-pair)
                 (set! last-prime-ptr new-pair)))

              ((zero? (remainder candidate (car divizors)))
               (loop (+ candidate 2) primes))

              (else
                (loop candidate (cdr divizors))))))


    (define (first)
      ;; return first prime and reset generator.
      (set! ptr primes)
      (car ptr))


    (define (last)
      ;; return last prime determined so far.
      (car last-prime-ptr))


    (define (next)
      ;; return next prime (relatively to previous calls of 'first or 'next operators).
      ;; list of primes is expanded if needed.
      (cond ((null? ptr)
             (first))
            (else
              (if (null? (cdr ptr))
                (expand))
              (set! ptr (cdr ptr))
              (car ptr))))


    (define (prime? n)
      ;; test if given number is a prime.
      ;; list of primes is expanded if needed.
      ;;
      ;; FIXME: 'prime? and 'expand share the same code.
      ;; FIXME: there must be a way to have this code
      ;; FIXME: written just once.

      (while (< (last) n) ;; NOTE: 'while' is non-standard
        (expand))

      (let test-loop ((divizors primes))
        (cond ((or (null? divizors)
                   (> (sqr (car divizors)) n))
               #t)

              ((zero? (remainder n (car divizors)))
               #f)

              (else
                (test-loop (cdr divizors))))))


    (lambda (operator . args)
      ;; main dispatcher
      (cond ((equal? operator 'first)
             (first))

            ((equal? operator 'last)
             (last))

            ((equal? operator 'next)
             (next))

            ((equal? operator 'prime?)
             (cond ((null? args)
                    ;; error: no arguments
                    (error "argument(s) expected for prime? operator"))

                   ((null? (cdr args))
                    ;; just one argument
                    (prime? (car args)))

                   (else
                     ;; list of arguments
                     (map prime? args))))

            ((equal? operator 'show)
             primes)

            (else
              (error "bad operator"))))))


;; end of file
