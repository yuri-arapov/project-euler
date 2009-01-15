;; permutations.scm
;;
;; (permutations '())      -> ()
;;
;; (permutations '(1))     -> ((1))
;;
;; (permutations '(1 2 3)) -> ((1 2 3) (1 3 2) (2 3 1) (2 1 3) (3 1 2) (3 2 1))
;;


(define (rotate-right-to-left l)
  (append (cdr l) (list (car l))))


(define (permutations l)

  (define (helper res rotations-left l)
    (if (zero? rotations-left)
      res
      (helper (append res (map (lambda (x) (cons (car l) x)) (permutations (cdr l))))
              (- rotations-left 1)
              (rotate-right-to-left l))))

  (cond ((null? l)
         '())                ;; return empty list as a permutation of empty list:
                             ;;   () -> ()

        ((null? (cdr l))     
         (list l))           ;; return list containing initial list for one-item list:
                             ;;   (a) -> ((a))

        (else
          (helper '() (length l) l)))) ;; run helper function

 
(define (permutations-n l n)
;; M by N permutations (where M is length of initial list l).
;;  
;; make list of n-size permutations of initial list.
;;
;; example:
;;   (permutations-n '(1 2 3) 4) -> ()
;;
;;   (permutations-n '(1 2 3) 3) -> ((1 2 3) (1 3 2) (2 3 1) (2 1 3) (3 1 2) (3 2 1))
;;
;;   (permutations-n '(1 2 3) 2) -> ((1 2) (1 3) (2 3) (2 1) (3 1) (3 2))
;;
;;   (permutations-n '(1 2 3) 1) -> ((1) (2) (3))
;;
;;   (permutations-n '(1 2 3) 0) -> (())
;;

  (define (helper res rotations-left l)
    (if (zero? rotations-left)
      res
      (helper (append res (map (lambda (x) (cons (car l) x)) (permutations-n (cdr l) (- n 1))))
              (- rotations-left 1)
              (rotate-right-to-left l))))

  (cond ((null? l)
         '())                ;; return empty list as a permutation of empty list:
                             ;;   () -> ()

        ((> n (length l))
         '())

        ((zero? n)
         (list '()))

        ((null? (cdr l))     
         (list l))           ;; return list containing initial list for one-item list:
                             ;;   (a) -> ((a))

        (else
          (helper '() (length l) l)))) ;; run helper function


;; end of file
