(load "testing-reqs.scm")

(define test-data
  (list (cons "name" "Will")
        (cons "age" 26)))

(do-test "simple" "test/samples/simple.god" test-data)