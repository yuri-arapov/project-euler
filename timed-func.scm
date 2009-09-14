;; timed-func.scm
;;

(define (make-timed-func name f)
  (define (get-time) (current-time time-process))
  (let ((total-time (get-time))                                                                       
        (count 0))
    (set-time-second! total-time 0)
    (set-time-nanosecond! total-time 0)
    (lambda args
      (if (and (not (null? args))
               (null? (cdr args))
               (equal? (car args) 'report-time))
        (format #t "~20a ~8d ~12d ~12d~%" 
                name 
                count 
                (time-nanosecond total-time) 
                (time-second total-time))
        (let* ((start (get-time))
               (res (apply f args)))
          (add-duration! total-time (time-difference (get-time) start))
          (set! count (+ 1 count))
          res)))))

;; end of file
