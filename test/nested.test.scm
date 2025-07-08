(load "testing-reqs.scm")

(define test-data
  (list (cons "movie"
          (list (cons "name" "Interstellar")
                (cons "release-year" 2014)
                (cons "director" "Christopher Nolan")
                (cons "lead-actors"
                  (list
                    (list
                      (cons "character" "Murphy Cooper")
                      (cons "actor"
                        (list
                          (list (cons "name" "Jessica Chastain")
                                (cons "role" "adult"))
                          (list (cons "name" "Ellen Burstyn")
                                (cons "role" "elderly"))
                          (list (cons "name" "Mackenzie Foy")
                                (cons "role" "child")))))
                    (list
                      (cons "actor" "Matthew McConaughey")
                      (cons "character" "Joseph Cooper"))
                    (list
                      (cons "actor" "Anne Hathaway")
                      (cons "character" "Amelia Brand"))))))))

(do-test "nested" "test/samples/nested.god" test-data)