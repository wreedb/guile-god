(define-module (god token)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-god-token
            god-token?
            god-token-type
            god-token-value
            god-token-line
            god-token-column
            incr
            decr
            add
            minus
            file->string
            tokeneyes
            single-chars
            re-spacing
            re-newline
            re-comment
            re-identifier
            re-bool
            re-null
            re-number
            validate-token
            str-unquote
            str-escape
            last
            list-rmi))

(define-record-type god-token
  (make-god-token type value line column)
  god-token?
  (type god-token-type)
  (value god-token-value)
  (line god-token-line)
  (column god-token-column))

(define (tkn type value line column)
  (make-god-token type value line column))

(define single-chars
  '((#\{ . brace-left)
    (#\} . brace-right)
    (#\[ . bracket-left)
    (#\] . bracket-right)
    (#\; . terminate)
    (#\= . equals)))

(define re-spacing (make-regexp "^[ \t]"))
(define re-newline (make-regexp "^\n"))
(define re-comment (make-regexp "^#[^\n]*"))
(define re-identifier (make-regexp "^[a-zA-Z_][a-zA-Z0-9_.-]*"))
(define re-bool (make-regexp "^(true|false)"))
(define re-null (make-regexp "^null"))
(define re-string-normal (make-regexp "^\"([^\"\\\\]|\\\\.)*\""))
(define re-number (make-regexp "^-?[0-9]+\\.?[0-9]*([eE][+-]?[0-9]+)?"))

(define (file->string path)
  (let* ((handle (open-input-file path))
         (cont (read-string handle)))
    (close-port handle)
    (begin cont)))

(define (str-escape raw-content)
  (let loop ((chars (string->list raw-content))
             (result '()))
    (cond
      ((null? chars) (list->string (reverse result)))
      ((and (eq? (car chars) #\\) 
            (not (null? (cdr chars))))
       (case (cadr chars)
         ((#\") (loop (cddr chars) (cons #\" result)))
         ((#\\) (loop (cddr chars) (cons #\\ result)))
         ((#\n) (loop (cddr chars) (cons #\newline result)))
         ((#\t) (loop (cddr chars) (cons #\tab result)))
         ((#\r) (loop (cddr chars) (cons #\return result)))
         (else 
          (loop (cddr chars) (cons (cadr chars) (cons #\\ result))))))
      (else
       (loop (cdr chars) (cons (car chars) result))))))

(define (incr x) (begin (+ 1 x)))
(define (decr x) (begin (- 1 x)))
(define (add x y) (begin (+ x y)))
(define (minus x y) (begin (- x y)))

(define (last lst)
  (if (list? lst)
    (begin (minus (length lst) 1))
    (begin (error "input must be a list"))))

(define (list-rmi index lst)
  (if (and (number? index) (list? lst))
    (begin (append (take lst index)
                   (drop lst (incr index))))
    (begin (error "expected number,list arguments"))))

(define (str-unquote str)
  (unless (string? str) (error "expected string argument"))
  (let* ((lst (string->list str))
         (lastx (last lst)))
    (begin
      (list->string (list-rmi 0 (list-rmi lastx lst))))))

(define (tokeneyes input)
  (let ((len (string-length input)))

    (let loop ((pos 0) (line 1) (col 1) (tokens '()))

      (if (>= pos len)
        ;; done: return the tokens
        (begin (reverse tokens))
        ;; main parse
        (begin
          (let ((char (string-ref input pos))
                (rem (substring input pos)))
            (cond

              ;; whitespace
              ((regexp-exec re-spacing rem)
               => (lambda (x)
                    (let ((m (match:substring x)))
                      (loop (add pos (string-length m))
                            line
                            (add col (string-length m))
                            tokens))))

            ;; newlines
            ((regexp-exec re-newline rem)
             => (lambda (x)
                  (loop (incr pos) (incr line) 1
                        ;(cons (validate-token 'newline #\newline line col) tokens))))
                        (cons (tkn 'newline #\newline line col) tokens))))

            ;; comments
            ((regexp-exec re-comment rem)
             => (lambda (x)
                  (let ((m (match:substring x)))
                    (loop (add pos (string-length m))
                          line
                          (add col (string-length m))
                          tokens))))

            ((regexp-exec re-number rem)
             => (lambda (x)
                  (let ((m (match:substring x)))
                    (loop (add pos (string-length m))
                          line
                          (add col (string-length m))
                          ;(cons (validate-token 'number m line col) tokens)))))
                          (let ((num (or (string->number m) (error "failed to parse number"))))
                          (cons (tkn 'number num line col) tokens))))))

            ((regexp-exec re-string-normal rem)
             => (lambda (x)
                  (let ((m (match:substring x)))
                    (loop (add pos (string-length m))
                          line
                          (add col (string-length m))
                          ;(cons (validate-token 'string m line col) tokens)))))
                          (cons (tkn 'string (str-escape (str-unquote m)) line col) tokens)))))

            ((regexp-exec re-bool rem)
             => (lambda (x)
                  (let ((m (match:substring x)))
                    (loop (add pos (string-length m))
                          line
                          (add col (string-length m))
                          ;(cons (validate-token 'bool m line col) tokens)))))
                          ;; convert "true/false" into #t/#f
                          (let ((scmrep (if (string=? "true" m)
                                          (begin #t)
                                          (begin #f))))
                          (cons (tkn 'bool scmrep line col) tokens))))))

            ((regexp-exec re-null rem)
             => (lambda (x)
                  (let ((m (match:substring x)))
                    (loop (add pos (string-length m))
                          line
                          (add col (string-length m))
                          ;(cons (validate-token 'null '() line col) tokens)))))
                          (cons (tkn 'null '() line col) tokens)))))

            ((regexp-exec re-identifier rem)
             => (lambda (x)
                  (let ((m (match:substring x)))
                    (loop (add pos (string-length m))
                          line
                          (add col (string-length m))
                          (cons (tkn 'identifier m line col) tokens)))))

            ((assq char single-chars)
             => (lambda (pair)
                  (loop (incr pos)
                        line
                        (incr col)
                        (cons (tkn (cdr pair) (string char) line col) tokens))))

            (else
              (error (format #f "unknown token at ~a:~a" line col))))))))))