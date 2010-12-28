;; Memoization
;;

(use-modules (srfi srfi-69))    ;; Guile's hash tables

(define (make-memoized-proc equal-proc proc)
  (let ((proc proc)
        (ht (make-hash-table equal-proc)))
    (lambda (key)
      (if (hash-table-exists? ht key)
        (hash-table-ref ht key)
        (let ((v (proc key)))
          (hash-table-set! ht key v)
          v)))))


;; end of file
;; vim: ts=4 sw=4 et
