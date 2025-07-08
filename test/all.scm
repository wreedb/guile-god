(use-modules (ice-9 ftw))

(define (test? file)
  (if (string-suffix? ".test.scm" file) #t #f))

(define test-files
  (map (lambda (n) (string-append "test/" n))
    (filter test? (scandir "test"))))

(map load test-files)