(define-module (god token)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-god-token))

(define-record-type god-token
  (make-god-token type value line column)
  god-token?
  (type god-token-type)
  (value god-token-value)
  (line god-token-line)
  (column god-token-column))

(define (toke type value line column)
  (make-god-token type value line column))

(define re-whitespace (make-regexp "^[ \t]+"))
(define re-newline (make-regexp "^\\(\n|\r\n\\)"))
(define re-comment (make-regexp "^//[^\n]*"))
(define re-identifier (make-regexp "^[a-zA-Z_][a-zA-Z0-9_-]*"))
;(define re-number 
