;; Read file line by line and return list of strings.
;;


(define (read-file-with file proc)
  (define (read-lines port)
    (let loop ((line (read-line port))
               (res '()))
      (if (eof-object? line)
        (reverse res)
        (loop (read-line port) (cons (proc line) res)))))
  (call-with-input-file file read-lines))



(define (read-file file)
  (read-file-with file (lambda (line) line)))



;; end of file
;; vim: ts=4 sw=4
