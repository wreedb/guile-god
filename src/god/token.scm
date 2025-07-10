(define-module (god token)
  #:use-module (god util)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-9)
  #:export (make-god-token
            god-token?
            god-token-type
            god-token-value
            god-token-line
            god-token-column
            tokeneyes))

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

(define (str-unquote str)
  (unless (string? str) (error "expected string argument"))
  (let* ((lst (string->list str))
         (lastx (last-index lst)))
    (begin
      (list->string (list-rm-index 0 (list-rm-index lastx lst))))))

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