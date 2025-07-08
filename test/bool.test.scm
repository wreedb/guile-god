(load "testing-reqs.scm")

(define test-data
  (list (cons "nothing" '())
        (cons "perhaps" #t)
        (cons "not" #f)
        (cons "list"
          (list '() #f #t '() '() #f #t #t '() #f #f))))

(do-test "bool" "test/samples/bool.god" test-data)