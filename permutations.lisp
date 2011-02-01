;; permutations.scm
;;
;; (permutations '())      -> ()
;;
;; (permutations '(1))     -> ((1))
;;
;; (permutations '(1 2 3)) -> ((1 2 3) (1 3 2) (2 3 1) (2 1 3) (3 1 2) (3 2 1))
;;


(defun rotate-right-to-left (l)
  (append (cdr l) (list (car l))))


(defun permutations (l)

  (labels ((helper (res rotations-left l)
             (if (zerop rotations-left)
               res
               (helper (append res (mapcar #'(lambda (x) (cons (car l) x)) (permutations (cdr l))))
                       (- rotations-left 1)
                       (rotate-right-to-left l)))))

    (cond ((null l)
           '())                ;; return empty list as a permutation of empty list:
          ;;   () -> ()

          ((null (cdr l))     
           (list l))           ;; return list containing initial list for one-item list:
          ;;   (a) -> ((a))

          (t
            (helper '() (length l) l))))) ;; run helper function

 
(defun permutations-n (l n)
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

  (labels ((helper (res rotations-left l)
             (if (zerop rotations-left)
               res
               (helper (append res (mapcar #'(lambda (x) (cons (car l) x)) (permutations-n (cdr l) (1- n))))
                       (1- rotations-left)
                       (rotate-right-to-left l)))))

    (cond ((null l)
           '())                ;; return empty list as a permutation of empty list:
          ;;   () -> ()

          ((> n (length l))
           '())

          ((zerop n)
           (list '()))

          ((null (cdr l))     
           (list l))           ;; return list containing initial list for one-item list:
          ;;   (a) -> ((a))

          (t
            (helper '() (length l) l))))) ;; run helper function


;; end of file
