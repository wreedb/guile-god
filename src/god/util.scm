(define-module (god util)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (incr
            decr
            add
            minus
            file->string
            last-index
            list-rm-index
            hash-path))

(define (file->string path)
  (let* ((handle (open-input-file path))
         (cont (read-string handle)))
    (close-input-port handle)
    (begin cont)))

(define (incr x) (begin (+ 1 x)))
(define (decr x) (begin (- 1 x)))
(define (add x y) (begin (+ x y)))
(define (minus x y) (begin (- x y)))

(define (last-index lst)
  (if (list? lst)
    (begin (minus (length lst) 1))
    (begin (error "input must be a list"))))

(define (list-rm-index index lst)
  (if (and (number? index) (list? lst))
    (begin (append (take lst index)
                   (drop lst (incr index))))
    (begin (error "expected number,list arguments"))))

(define (hash-path ht . keys)
  "search through hash table HT using sequence KEYS"
  (let loop ((table ht) (remaining-keys keys))
    (if (null? remaining-keys)
        table
        (loop (hash-ref table (car remaining-keys))
              (cdr remaining-keys)))))