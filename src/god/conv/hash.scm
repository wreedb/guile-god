(define-module (god conv hash)
  #:use-module (god parse)
  #:use-module (god conv)
  #:export (god-list->hash!
            god-field->hash!
            god-map->hash!
            god-list->hash
            god-map->hash
            god-doc->hash
            god-any->hash!
            hash-path))

(define (god-list->hash! lst ht key)
  ;; Populate the hash table at 'key' with list contents
  (let ((list-ht (make-hash-table)))
    (let loop ((elements (god-list-elements lst))
               (index 0))
      (cond
        ((null? elements) 
         (hash-set! ht key list-ht))
        (else
         (let ((elem (car elements)))
           (cond
             ((god-element? elem) 
              (hash-set! list-ht index (god-element-value elem)))
             ((god-map? elem) 
              (god-map->hash! elem list-ht index))
             ((god-list? elem) 
              (god-list->hash! elem list-ht index))
             (else 
              (hash-set! list-ht index elem))) ;; fallback for raw values
           (loop (cdr elements) (+ index 1))))))))

(define (god-field->hash! field ht)
  ;; Populate hash table with this field
  (let ((id (god-field-ident field))
        (typ (god-field-type field))
        (val (god-field-value field)))
    (cond
      ((eq? typ 'simple)
       (hash-set! ht id
                  (if (god-element? val)
                      (god-element-value val)
                      val)))
      ((eq? typ 'map)
       (god-map->hash! val ht id))
      ((eq? typ 'list)
       (god-list->hash! val ht id))
      (else
       (error (format #f "unknown field: [type: ~a|identifier: ~a|value: ~a]" id typ val))))))

(define (god-map->hash! mmap ht key)
  ;; Populate hash table at 'key' with map contents
  (let ((map-ht (make-hash-table)))
    (for-each (lambda (field)
                (god-field->hash! field map-ht))
              (god-map-fields mmap))
    (hash-set! ht key map-ht)))

;; Top-level entry points that create the initial hash table
(define (god-list->hash lst)
  (let ((ht (make-hash-table)))
    (god-list->hash! lst ht 'root)
    (hash-ref ht 'root)))

(define (god-map->hash mmap)
  (let ((ht (make-hash-table)))
    (for-each (lambda (field)
                (god-field->hash! field ht))
              (god-map-fields mmap))
    ht))

(define (god-doc->hash doc)
  (let ((ht (make-hash-table)))
    (for-each (lambda (field)
                (god-field->hash! field ht))
              (god-document-fields doc))
    ht))

(define (hash-path ht . keys)
  "search through hash table HT using sequence KEYS"
  (let loop ((table ht) (remaining-keys keys))
    (if (null? remaining-keys)
        table
        (loop (hash-ref table (car remaining-keys))
              (cdr remaining-keys)))))

;; Alternative: Even more stateful - single hash table for everything
(define (god-any->hash! obj ht key)
  "Generic converter that handles any god object type"
  (cond
    ((god-element? obj)
     (hash-set! ht key (god-element-value obj)))
    ((god-list? obj)
     (god-list->hash! obj ht key))
    ((god-map? obj)
     (god-map->hash! obj ht key))
    (else
     (hash-set! ht key obj))))