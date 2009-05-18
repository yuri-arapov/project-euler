;; ll.scm
;;
;; Simple listing facility.
;;

(define (ll l)
  (for-each (lambda (i) (format #t "~a\n" i)) l))

;; end of file
;; vim: ts=4 sw=4
