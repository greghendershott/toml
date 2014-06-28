#lang racket/base

(require racket/contract
         racket/match)

(provide hash-sets
         hash-refs)

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

