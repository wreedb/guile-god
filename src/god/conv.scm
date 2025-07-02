(define-module (god conv)
  #:use-module (god token)
  #:use-module (god parse)
  #:export (god-list->scm
            god-map->scm
            god-element->scm
            god-field->scm
            god-doc->scm))

(define (god-list->scm lst)
  (map (lambda (elem)
         (cond
           ((god-element? elem) (god-element-value elem))
           ((god-map? elem) (god-map->scm elem))
           ((god-list? elem) (god-list->scm elem))
           (else elem)))
       (god-list-elements lst)))

(define (god-element->scm elem)
  (god-element-value elem))

(define (god-field->scm field)
  (let ((id (god-field-ident field))
        (typ (god-field-type field))
        (val (god-field-value field)))
    (cons id
          (cond
            ((eq? typ 'simple)
             (if (god-element? val)
                 (god-element-value val)
                 val))
            ((eq? typ 'map)
             (god-map->scm val))
            ((eq? typ 'list)
             (god-list->scm val))
            (else
             (error "could not parse field"))))))

(define (god-map->scm mmap)
  (map god-field->scm (god-map-fields mmap)))

(define (god-doc->scm doc)
  (map god-field->scm (god-document-fields doc)))
