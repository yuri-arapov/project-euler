;; Read file line by line and return list of strings.
;;


(define (read-file file)
  (define (read-lines port)
    (let loop ((line (read-line port))
               (res '()))
      (if (eof-object? line)
        (reverse res)
        (loop (read-line port) (cons line res)))))
  (call-with-input-file file read-lines))


;; end of file
;; vim: ts=4 sw=4
