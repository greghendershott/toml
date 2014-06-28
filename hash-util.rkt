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

(define (hash-refs ht keys)
  (match keys
    [(list)         ht]
    [(list* k more) (hash-refs (hash-ref ht k) more)]))

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

;; Merge two hasheq's h0 and h1.
;;
;; When a key exists in only one, use its value.
;;
;; When a key exists in both, when the values are
;;  - both hasheqs? do a recursive hasheq-merge
;;  - both lists? append the lists
;   - otherwise raise an error.
(define/contract (hasheq-merge h0 h1 [keys '()])
  (->* ((and/c immutable? hash?) (and/c immutable? hash?))
       ((listof symbol?))
       (and/c immutable? hash?))
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

(define (err ks v0 v1)
  (local-require json)
  (error 'toml
         "conflicting values for `~a'\n~a\n~a"
         (string-join (map symbol->string (reverse ks)) ".")
         (jsexpr->string v0)
         (jsexpr->string v1)))

(module+ test
  (check-equal?
   (hasheq-merge (hasheq 'foo "bar"
                         'bar "baz"
                         'baz (hasheq 'a "a")
                         'xs (list (hasheq 'x0 10 'x1 11)))
                 (hasheq 'a "a"
                         'baz (hasheq 'b "b")
                         'xs (list (hasheq 'x0 20 'x1 21))))
   (hasheq 'foo "bar"
           'bar "baz"
           'a "a"
           'baz (hasheq 'a "a"
                        'b "b")
           'xs (list (hasheq 'x0 10 'x1 11)
                     (hasheq 'x0 20 'x1 21))))
  (check-exn #rx"conflicting values for keys `a.b.c'\n0\n1"
             (λ ()
               (hasheq-merge
                (hasheq 'a (hasheq 'b (hasheq 'c 0)))
                (hasheq 'a (hasheq 'b (hasheq 'c 1)))))))
