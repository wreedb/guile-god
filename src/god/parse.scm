(define-module (god parse)
  #:use-module (god token)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:export (make-god-document
            make-god-list
            make-god-element
            make-god-map
            make-god-field
            god-document?
            god-list?
            god-element?
            god-map?
            god-field?
            god-document-fields
            god-list-elements
            god-map-fields
            god-element-value
            god-field-ident
            god-field-type
            god-field-value
            god-parse
            peek-token
            consume-token!
            advance-token!
            parse-document
            parse-field
            parse-value
            parse-map
            parse-list
            god-parse-raw
            god-parse-file
            skip-newlines!
            make-parser-state
            parser-state?
            parser-state-tokens
            parser-state-position
            set-parser-state-position!))

(define-record-type god-document
  (make-god-document fields)
  god-document?
  (fields god-document-fields))

(define-record-type god-list
  (make-god-list elements)
  god-list?
  (elements god-list-elements))

(define-record-type god-map
  (make-god-map fields)
  god-map?
  (fields god-map-fields))

(define-record-type god-element
  (make-god-element value)
  god-element?
  (value god-element-value))

(define-record-type god-field
  (make-god-field ident type value)
  god-field?
  (ident god-field-ident)
  (type god-field-type)
  (value god-field-value))

(define-record-type parser-state
  (make-parser-state tokens position)
  parser-state?
  (tokens parser-state-tokens)
  (position parser-state-position set-parser-state-position!))

(define (peek-token state)
  (let ((tokens (parser-state-tokens state))
        (pos (parser-state-position state)))
    (if (< pos (length tokens))
        (list-ref tokens pos)
        #f)))

(define (advance-token! state)
  (set-parser-state-position! state (+ (parser-state-position state) 1)))

(define (consume-token! state exp-type)
  (let ((token (peek-token state)))
    (if (and token (eq? (god-token-type token) exp-type))
        (begin (advance-token! state) token)
        (error (format #f "expected ~a, found: ~a (line ~a)"
                       exp-type
                       (if token (god-token-type token) 'EOF)
                       (if token (god-token-line token) "unknown"))))))

(define (skip-newlines! state)
  (let loop ()
    (let ((token (peek-token state)))
      (when (and token (eq? (god-token-type token) 'newline))
        (advance-token! state)
        (loop)))))

(define (parse-value state)
  (skip-newlines! state)
  (let ((token (peek-token state)))
    (if (not token)
        (error "unexpected end of input")
        (case (god-token-type token)
          ((string number bool null)
           (advance-token! state)
           (make-god-element (god-token-value token)))
          ((bracket-left)
           (parse-list state))
          ((brace-left)
           (parse-map state))
          (else
           (error (format #f "unexpected token ~a [~a:~a]"
                          (god-token-type token)
                          (god-token-line token)
                          (god-token-column token))))))))

(define (parse-list state)
  (consume-token! state 'bracket-left)
  (skip-newlines! state)
  (let loop ((elements '()))
    (let ((token (peek-token state)))
      (cond
       ((not token) (error "unterminated list"))
       ((eq? (god-token-type token) 'bracket-right)
        (advance-token! state)
        (make-god-list (reverse elements)))
       (else
        (let ((element (parse-value state)))
          (skip-newlines! state)
          (loop (cons element elements))))))))

(define (parse-map state)
  (consume-token! state 'brace-left)
  (skip-newlines! state)
  (let loop ((fields '()))
    (let ((token (peek-token state)))
      (cond
       ((not token) (error "error: unterminated map"))
       ((eq? (god-token-type token) 'brace-right)
        (advance-token! state)
        (make-god-map (reverse fields)))
       (else
        (let ((field (parse-field state)))
          (skip-newlines! state)
          (loop (cons field fields))))))))

(define (parse-field state)
  (skip-newlines! state)
  
  (let ((ident-token (consume-token! state 'identifier)))
    (when (not ident-token)
      (error "expected field identifier"))
    
    (let ((ident-name (god-token-value ident-token)))
      (consume-token! state 'equals)
      (skip-newlines! state)
      
      (let* ((value (parse-value state))
             (field-type
               (cond ((god-element? value) 'simple)
                     ((god-list? value) 'list)
                     ((god-map? value) 'map)
                     (else 'unknown))))
        (consume-token! state 'terminate)
        (make-god-field ident-name field-type value)))))

(define (parse-document state)
  (skip-newlines! state)
  (consume-token! state 'brace-left)
  (skip-newlines! state)
  
  (let loop ((fields '()))
    (let ((token (peek-token state)))
      (cond
       ((not token) (error "document is missing closing brace"))
       ((eq? (god-token-type token) 'brace-right)
        (advance-token! state)
        (make-god-document (reverse fields)))
       ((eq? (god-token-type token) 'identifier)
        (let ((field (parse-field state)))
          (skip-newlines! state)
          (loop (cons field fields))))
       (else (error
              (format #f "unexpected token ~a [~a:~a]"
                      (god-token-type token)
                      (god-token-line token)
                      (god-token-column token))))))))

(define (god-parse tokens)
  (let ((state (make-parser-state tokens 0)))
    (parse-document state)))

(define (god-parse-raw text)
  (let* ((tokens (tokeneyes text))
         (state (make-parser-state tokens 0)))
    (parse-document state)))

(define (god-parse-file file)
  (if (file-exists? file)
    (let* ((fh (open-input-file file))
           (contents (read-string fh))
           (tokens (tokeneyes contents))
           (state (make-parser-state tokens 0)))
      (close-port fh)
      (parse-document state))
    (error (format #f "file ~a was not found" file))))