(load "testing-reqs.scm")

(define test-data
  (list (cons "string" "words")
        (cons "number" 1234)
        (cons "yes" #t)
        (cons "no" #f)
        (cons "none" '())))

(do-test "types" "test/samples/types.god" test-data)