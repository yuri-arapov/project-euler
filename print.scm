;; print.scm
;;
;; some printing facilities
;;


(define (print-list z) (for-each display z))


(define (print . z) (print-list z))


(define (println . z) (print-list z) (newline))


;; end of file
