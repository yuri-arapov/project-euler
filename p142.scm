;; 2011-10-05
;;
;; Yuri Arapov <yuridichesky@gmail.com>
;;
;; http://projecteuler.net/index.php?section=problems&id=142
;;
;; Problem 142
;;
;; 24 February 2007
;; 
;; Find the smallest x + y + z with integers x>y>z>0 such that 
;; x+y, x-y, x+z, x-z, y+z, y-z are all perfect squares.
;;
;; Answer: 1006193


;; (dorange (var from to) body ...) macro.
;; The range could be either way: from=to, from<to, from>to.
(define-syntax dorange
  (syntax-rules ()
    ((dorange (var from to) body ...)
     (let ((_to to))
       (let loop ((var from))
         (if (= var _to)
           (if #f #f)
           (begin
             (begin body ...)
             (loop (if (< var _to) (1+ var) (1- var))))))))))


;; Test if n is a perfect square.
(define (square? n) (and (positive? n) (integer? (sqrt n))))


;; Square the n.
(define (sqr n) (* n n))



;; Problem 142.
;; Solution is based on formulae stolen here:
;; http://freelancersunite.net/project_euler/project-euler-problem-142/
(define (p142)
  (with-return (ret)
    (let loop ((a 3))   ;; infinite loop
      (let ((aa (sqr a)))
        (dorange (c (- a 1) 1)
          (let ((cc (sqr c)))
            (let ((ff (- aa cc)))
              (if (square? ff)
                 (dorange (d (- c 1) 1)
                   (let* ((dd (sqr d))
                          (ee (- aa dd))
                          (bb (- cc ee))
                          (x  (/ (+ aa bb) 2))
                          (y  (/ (+ ee ff) 2))
                          (z  (/ (- cc dd) 2)))
                     (if (and (square? ee)
                              (square? bb)
                              (integer? x)
                              (integer? y)
                              (integer? z))
                       (ret (list (+ x y z) x y z))))))))))
      (loop (1+ a)))))


;; end of file
;; vim: sw=4 ts=4
