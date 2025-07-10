(define-module (god)
  #:use-module (god token)
  #:use-module (god parse)
  #:use-module (god util)
  #:use-module (god conv)
  #:use-module (god conv hash))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (god token)
                   (god parse)
                   (god util)
                   (god conv)
                   (god conv hash))