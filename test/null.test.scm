(load "testing-reqs.scm")

(define test-data
  (list
    (cons "nothing"
      (list (cons "nope"
              (list '() '() '()))
            (cons "something-maybe" '())
            (cons "a-list"
              (list
                (list (cons "something" '() ))))))))



(do-test "null" "test/samples/null.god" test-data)