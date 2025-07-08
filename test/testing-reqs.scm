(set! %load-path (cons (string-append (getenv "PWD") "/src") %load-path))

(use-modules (ice-9 format)
             (god token)
             (god parse)
             (god conv))

(define (do-test name file test-data)
  (let ((file-data (god-doc->scm (god-parse-file file))))
    (if (equal? file-data test-data)
      (begin
        (test-print-result name #t))
      (begin
        (test-print-result name #f)))))

(define (test-print-result name status)
  (let* ((fmt-red "\x1B[1;31m")
         (fmt-green "\x1B[1;32m")
         (reset "\x1B[0m")
         (fail-msg (format #f "[~aFAIL~a]" fmt-red reset))
         (pass-msg (format #f "[~aPASS~a]" fmt-green reset)))
    (if status
      (begin (display (string-append pass-msg " " name "\n")))
      (begin (display (string-append fail-msg " " name "\n"))))))
