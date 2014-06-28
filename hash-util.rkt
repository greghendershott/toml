#lang racket/base

(require racket/contract
         racket/match
         racket/string)

(provide hash-sets
         hash-refs
         hasheq-merge)

(define/contract (hash-sets ht keys val)
  (-> (and/c immutable? hash?) (listof symbol?) any/c
      (and/c immutable? hash?))
  (match keys
    [(list key)       (hash-set ht key val)]
    [(list* key more) (hash-set ht key (hash-sets ht more val))]))

(module+ test
  (require rackunit)
  (check-equal? (hash-sets (make-immutable-hasheq) '(a) 0)
                #hasheq([a . 0]))
  (check-equal? (hash-sets (make-immutable-hasheq) '(a b) 0)
                #hasheq([a . #hasheq([b . 0])]))
  (check-equal? (hash-sets (make-immutable-hasheq) '(a b c) 0)
                #hasheq([a . #hasheq([b . #hasheq([c . 0])])])))

(define/contract (hash-refs ht keys)
  (-> (and/c immutable? hash?) (listof symbol?)
      any/c)
  (for/fold ([ht ht])
            ([key (in-list keys)])
    (hash-ref ht key)))

(module+ test
  (require rackunit)
  (check-equal? (hash-refs #hasheq([a . 0]) '())
                #hasheq([a . 0]))
  (check-equal? (hash-refs #hasheq([a . 0]) '(a))
                0)
  (check-equal? (hash-refs #hasheq([a . #hasheq([b . 0])]) '(a b))
                0)
  (check-equal? (hash-refs #hasheq([a . #hasheq([b . #hasheq([c . 0])])]) '(a b c))
                0))

;; Merge two hasheq's h0 and h1. When both have values for a key that
;; are both hasheqs, do a recursive hasheq-merge. When both have
;; values for a key that are boht lists, append the lists. Otherwise
;; raise an error.
(define/contract (hasheq-merge h0 h1 [keys '()])
  (->* ((and/c immutable? hash?) (and/c immutable? hash?))
       ((listof symbol?))
       (and/c immutable? hash?))
  (define (err ks v0 v1)
    (error 'toml
           "conflicting values for key~a `~a'\n~a\n~a"
           (if (= 1 (length ks)) "" "s")
           (string-join (map symbol->string (reverse ks)) ".")
           v0 v1))
  (for/fold ([h0 h0])
            ([(k v1) (in-hash h1)])
    (hash-set h0 k
              (cond [(list? v1)
                     (define v0 (hash-ref h0 k (list)))
                     (unless (list? v0)
                       (err (cons k keys) v0 v1))
                     (append v0 v1)]
                    [(hash? v1)
                     (define v0 (hash-ref h0 k (hasheq)))
                     (unless (hash? v0)
                       (err (cons k keys) v0 v1))
                     (hasheq-merge v1 v0 (cons k keys))]
                    [(hash-has-key? h0 k)
                     (err (cons k keys) (hash-ref h0 k) v1)]
                    [else v1]))))

(module+ test
  (check-equal?
   (hasheq-merge (hasheq 'foo "bar"
                         'bar "baz"
                         'baz (hasheq 'a "a"))
                 (hasheq 'a "a"
                         'baz (hasheq 'b "b")))
   (hasheq 'foo "bar"
           'bar "baz"
           'a "a"
           'baz (hasheq 'a "a"
                        'b "b")))
  (check-exn #rx"conflicting values for keys `a.b.c'\n0\n1"
             (λ ()
               (hasheq-merge
                (hasheq 'a (hasheq 'b (hasheq 'c 0)))
                (hasheq 'a (hasheq 'b (hasheq 'c 1)))))))
