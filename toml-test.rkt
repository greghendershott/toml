#!/usr/bin/env racket
#lang rackjure

(require json
         racket/date
         "main.rkt")

(module+ main
  (~> (current-input-port)
      port->string
      parse-toml
      type-jsexpr
      jsexpr->string
      displayln))

(define (type-jsexpr v)
  (match v
    [#t (hasheq 'type "bool" 'value "true")]
    [#f (hasheq 'type "bool" 'value "false")]
    [(? string? v) (hasheq 'type "string" 'value v)]
    [(? exact-integer? v) (hasheq 'type "integer" 'value (~a v))]
    [(? real? v) (hasheq 'type "float" 'value (~a v))]
    [(? date? d) (hasheq 'type "datetime"
                         'value
                         (parameterize ([date-display-format 'iso-8601])
                           (string-append (date->string d #t) "Z")))]
    [(? list? xs) (cond [(and (not (empty? xs)) ;an array of tables?
                              (hash? (first xs))) (for/list ([x xs])
                                                    (type-jsexpr x))]
                        [else (hasheq 'type "array" ;a table value
                                      'value (for/list ([x xs])
                                               (type-jsexpr x)))])]
    [(? hash? ht) (for/hasheq ([(k v) (in-hash ht)])
                    (values k (type-jsexpr v)))]))
