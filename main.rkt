#lang at-exp racket/base

(require "parsack.rkt"
         "hash-util.rkt"
         racket/string
         racket/list
         racket/date
         racket/function
         racket/contract
         racket/match)

(provide parse-toml)

(module+ test (require rackunit))

;;; stx

;; Parsac automatically provides error messages with positions for
;; _syntax_ errors. To do so also for _semantic_ errors -- e.g. hash
;; conflicts -- we need to tag the source datums with pos info. Much
;; like Racket syntax objects. Unlike Racket syntax, it may be
;; sufficient for us to tag only _some_ of the input, such as
;; $key/val, for adequate error messages.

(struct stx (e pos) #:transparent)

;; Strip all stx structs rescursively. Analogous to Racket's
;; `syntax->datum`.
(define (stx->dat v)
  (match v
    [(? hash? ht) (for/hasheq ([(k v) (in-hash ht)]) (values k (stx->dat v)))]
    [(? list? xs) (for/list ([x (in-list xs)]) (stx->dat x))]
    [(stx e _)    e]
    [v            v]))

;; Depth-first search for the first value that's a stx? and return its
;; pos converted to a line:col:ofs string, or #f if none found.
(define (find-pos v)
  (match v
    [(? hash? ht) (for/or ([(k v) (in-hash ht)]) (find-pos v))]
    [(? list? xs) (for/or ([x (in-list xs)]) (find-pos x))]
    [(stx _ (Pos line col ofs)) (format "~a:~a:~a:" line col ofs)]
    [v #f]))

;;; Whitespace and comments

(define $space-char
  (<?> (oneOf " \t") "space or tab"))

(define $sp
  (<?> (many $space-char)
       "zero or more spaces or tabs"))

(define $spnl
  (<?> (pdo $sp (optional (char #\return)) (optional $newline) $sp
            (return null))
       "zero or more spaces, and optional newline plus zero or more spaces"))

(define $blank-line
  (try (pdo $sp $newline (return (void)))))

(define $comment
  (try (pdo (char #\#) (manyUntil $anyChar $newline)
            (return null))))

(define $blank-or-comment-line
  (<or> $blank-line $comment))

;;; Literal values

(define $string-char
  (<or> (pdo
         (char #\\)
         (<or> (>> (char #\b) (return #\backspace))
               (>> (char #\n) (return #\newline))
               (>> (char #\f) (return #\page))
               (>> (char #\r) (return #\return))
               (>> (char #\t) (return #\tab))
               (>> (char #\\) (return #\\))
               (>> (char #\") (return #\"))
               (>> (char #\/) (return #\/))
               (pdo (oneOf "uU")
                    (cs <- (many $hexDigit))
                    (return (integer->char (string->number (list->string cs)
                                                           16))))
               ))
        (noneOf "\"\\")))

(define $string-lit
  (<?> (try (pdo (char #\")
                 (cs <- (manyUntil $string-char (char #\")))
                 (return (list->string cs))))
       "double-quoted string value"))

(define $optional-sign
  (<or> (>> (char #\-) (return '(#\-)))
        (return '())))

(define $integer-lit
  (<?> (try (pdo (ss <- $optional-sign)
                 (xs <- (many1 $hexDigit))
                 (return (string->number (list->string (append ss xs))))))
       "numeric value"))

(define $float-lit
  (<?> (try (pdo (ss <- $optional-sign)
                 (xs <- (many1 $digit))
                 (char #\.)
                 (ys <- (many1 $hexDigit))
                 (return (string->number (list->string (append ss xs '(#\.) ys))))))
       "numeric value"))

(define $true-lit  (pdo (string "true")  (return #t)))
(define $false-lit (pdo (string "false") (return #f)))

(define ->num (compose string->number list->string list))
(define $4d (pdo-seq $digit $digit $digit $digit #:combine-with ->num))
(define $2d (pdo-seq $digit $digit #:combine-with ->num))

(define $datetime-lit
  ;; 1979-05-27T07:32:00Z
  (try (pdo (yr <- $4d) (char #\-) (mo <- $2d) (char #\-) (dy <- $2d)
            (char #\T)
            (hr <- $2d) (char #\:) (mn <- $2d) (char #\:) (sc <- $2d)
            (char #\Z)
            (return (date sc mn hr dy mo yr 0 0 #f 0)))))

(define ($array state) ($_array state)) ;; "forward decl"

(define $val
  (<or> $true-lit
        $false-lit ;before $numeric-lit. "fa" in "false" could be hex
        $datetime-lit ;before $numeric-lit. dates start with number
        $float-lit
        $integer-lit
        $string-lit
        $array))

;; TOML arrays require items to have same type. To handle this with
;; parsing (vs. semantically), we insist that same literal parser be
;; used for all items.
(define (array-of $value-parser)
  (try (pdo $sp
            (char #\[)
            $sp (optional $comment)
            (vs <- (many (pdo $spnl
                              (v <- $value-parser)
                              (optional (char #\,))
                              $sp (optional $comment)
                              (many $blank-or-comment-line)
                              $spnl
                              (return v))))
            (char #\])
            (return vs))))
(define $_array
  (<or> (array-of (<or> $true-lit $false-lit))
        (array-of $datetime-lit)
        (array-of $float-lit)
        (array-of $integer-lit)
        (array-of $string-lit)
        (array-of $array)))

;;; Keys for key = val pairs and for tables and arrays of tables

;; Valid chars for both normal keys and table keys
(define $common-key-char
  (<or> $alphaNum (oneOf "~!@#$^&*()_+-`\\|/?><,;:'")))

(define $table-key-char
  (<or> $common-key-char (oneOf " ")))

(define $key-char
  (<or> $common-key-char (oneOf "[].")))

(define $table-key ;; >> symbol?
  (<?> (pdo (cs <- (many1 $table-key-char))
            (return (string->symbol (list->string cs))))
       "table key"))

(define $key ;; >> symbol?
  (<?> (pdo (cs <- (many1 $key-char))
            (return (string->symbol (list->string cs))))
       "key"))

(define $key/val ;; >> (list/c symbol? stx?)
  (try (pdo $sp (key <- $key) $sp
            (char #\=)
            $sp
            (pos <- (getPosition))
            (val <- $val)
            $sp
            (<or> $comment $newline)
            (many $blank-or-comment-line)
            $sp
            (return (list key (stx val pos))))))

;;; Table keys, handled as #\. separated

(define (keys->string ks)
  (string-join (map symbol->string ks) "."))

(define $table-keys ;; >> (listof symbol?)
  (sepBy1 $table-key (char #\.)))

(define (table-keys-under parent-keys)
  (pdo (if (empty? parent-keys)
           (return null)
           (pdo (string (keys->string parent-keys))
                (char #\.)))
       (keys <- $table-keys)
       (return (append parent-keys keys))))

;;; Tables

(define (table-under parent-keys)
  (<?> (try (pdo $sp
                 (keys <- (between (char #\[) (char #\])
                                   (table-keys-under parent-keys)))
                 $sp (<or> $comment $newline)
                 (many $blank-or-comment-line)
                 (kvs <- (many $key/val))
                 (many $blank-or-comment-line)
                 $sp
                 (return (kvs->hasheq keys kvs))))
       "table"))

(define $table (table-under '()))

;;; Arrays of tables

(define (array-of-tables-under parent-keys)
  (<?> (try (pdo $sp
                 (keys <- (between (string "[[") (string "]]")
                                   (table-keys-under parent-keys)))
                 $sp (<or> $comment $newline)
                 (many $blank-or-comment-line)
                 (kvs <- (many $key/val))
                 (tbs  <- (many (<or> (table-under keys)
                                      (array-of-tables-under keys))))
                 (aots <- (many (array-of-tables-same keys)))
                 (many $blank-or-comment-line)
                 $sp
                 (return
                  (let* ([tbs (map (curryr hash-refs keys) tbs)] ;hoist up
                         [aot0 (merge (cons (kvs->hasheq '() kvs) tbs)
                                      keys)]
                         [aots (cons aot0 aots)])
                    (match-define (list all-but-k ... k) keys)
                    (kvs->hasheq all-but-k
                                 (list (list k aots)))))))
       "array-of-tables"))
            
(define (array-of-tables-same keys)
  (<?> (try (pdo $sp
                 (between (string "[[") (string "]]")
                          (string (keys->string keys)))
                 $sp (<or> $comment $newline)
                 (many $blank-or-comment-line)
                 (kvs <- (many $key/val))
                 (tbs  <- (many (<or> (table-under keys)
                                      (array-of-tables-under keys))))
                 (many $blank-or-comment-line)
                 $sp
                 (return
                  (let ([tbs (map (curryr hash-refs keys) tbs)]) ;hoist up
                    (merge (cons (kvs->hasheq '() kvs) tbs)
                           keys)))))
       "array-of-tables"))

(define $array-of-tables (array-of-tables-under '()))

;;; A complete TOML document

(define $toml-document
  (pdo (many $blank-or-comment-line)
       (kvs <- (many $key/val))
       (tbs <- (many (<or> $table $array-of-tables)))
       $eof
       (return (merge (cons (kvs->hasheq '() kvs) tbs)
                      '()))))

;; Main, public function. Returns a `hasheq` using the same
;; conventions as the Racket `json` library. e.g. You should be able
;; to give the result to `jsexpr->string`.
(define (parse-toml s)
  (stx->dat (parse-result $toml-document (string-append s "\n\n"))))


;;; hasheq-merge

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
            ([(k v1) h1])
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
         "conflicting values for `~a'\n~a ~a\n~a ~a"
         (string-join (map symbol->string (reverse ks)) ".")
         (and (find-pos v0) "") (jsexpr->string (stx->dat v0))
         (and (find-pos v1) "") (jsexpr->string (stx->dat v1))))

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
  (check-exn #rx"conflicting values for `a.b.c'"
             (λ ()
               (hasheq-merge
                (hasheq 'a (hasheq 'b (hasheq 'c 0)))
                (hasheq 'a (hasheq 'b (hasheq 'c 1)))))))

;;; misc utils

(define (merge hts keys) ;; (listof hasheq?) (listof symbol?) -> hasheq?
  (foldl (curryr hasheq-merge keys) (hasheq) hts))

(define (kvs->hasheq keys pairs)
  ;; (listof symbol?) (listof (list/c symbol? any/c)) -> hasheq?
  (match keys
    [(list) (apply hasheq (append* pairs))]
    [(list* this more) (hasheq this (kvs->hasheq more pairs))]))

(module+ test
  (check-equal? (kvs->hasheq '() '([x 0][y 1]))
                (hasheq 'x 0 'y 1))
  (check-equal? (kvs->hasheq '(a) '([x 0][y 1]))
                (hasheq 'a (hasheq 'x 0 'y 1)))
  (check-equal? (kvs->hasheq '(a b) '([x 0][y 1]))
                (hasheq 'a (hasheq 'b (hasheq 'x 0 'y 1))))
  (check-equal? (kvs->hasheq '(a) '())
                (hasheq 'a (hasheq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require racket/format)
  (check-equal?
   (parse-toml @~a{[a]})
   '#hasheq((a . #hasheq())))
  (check-equal?
   (parse-toml @~a{[a.b]})
   '#hasheq((a . #hasheq((b . #hasheq())))))
  (check-equal?
   (parse-toml @~a{today = 2014-06-26T12:34:56Z
                   })
   `#hasheq((today . ,(date 56 34 12 26 6 2014 0 0 #f 0))))
  (check-equal?
   (parse-toml @~a{[[aot.sub]] #comment
                   aot0 = 10
                   aot1 = 11

                   [[aot.sub]] #comment
                   aot0 = 20
                   aot1 = 21

                   [[aot.sub]] #comment
                   aot0 = 30
                   aot1 = 31
                   })
   '#hasheq((aot
             .
             #hasheq((sub
                      .
                      (#hasheq((aot0 . 10) (aot1 . 11))
                       #hasheq((aot0 . 20) (aot1 . 21))
                       #hasheq((aot0 . 30) (aot1 . 31))))))))
  (check-equal?
   (parse-toml @~a{# Comment blah blah
                   # Comment blah blah

                   foo = "bar" #comment
                   ten = 10
                   true = true
                   false = false
                   array0 = [1,2,3] #comment
                   array1 = [ 1, 2, 3, ]
                   array2 = [ #comment
                              1, #comment
                              2,
                              3,
                              ] #comment
                   nested-array = [[1 2 3][4 5 6]] #comment

                   [key0.key1] #comment
                   x = 1
                   y = 1
                   [key0.key2]
                   x = 1
                   y = 1

                   [[aot.sub]] #comment
                   aot0 = 10
                   aot1 = 11

                   [[aot.sub]] #comment
                   aot0 = 20
                   aot1 = 21

                   [[aot.sub]] #comment
                   aot0 = 30
                   aot1 = 31
                   })
   '#hasheq((foo . "bar")
            (false . #f)
            (true . #t)
            (aot
             .
             #hasheq((sub
                      .
                      (#hasheq((aot0 . 10) (aot1 . 11))
                       #hasheq((aot0 . 20) (aot1 . 21))
                       #hasheq((aot0 . 30) (aot1 . 31))))))
            (ten . 10)
            (array0 . (1 2 3))
            (array1 . (1 2 3))
            (array2 . (1 2 3))
            (nested-array . ((1 2 3) (4 5 6)))
            (key0
             .
             #hasheq((key1 . #hasheq((x . 1) (y . 1)))
                     (key2 . #hasheq((x . 1) (y . 1)))))))
  (check-equal?
   (parse-toml @~a{[[fruit]]
                   name = "apple"

                   [fruit.physical]
                   color = "red"
                   shape = "round"

                   [[fruit]]
                   name = "banana"
                   })
   '#hasheq((fruit
             .
             (#hasheq((name . "apple")
                      (physical
                       .
                       #hasheq((color . "red") (shape . "round"))))
              #hasheq((name . "banana"))))))
  ;; From TOML README
  (check-equal?
   (parse-toml @~a{[[fruit]]
                   name = "apple"

                   [fruit.physical]
                   color = "red"
                   shape = "round"

                   [[fruit.variety]]
                   name = "red delicious"

                   [[fruit.variety]]
                   name = "granny smith"

                   [[fruit]]
                   name = "banana"

                   [[fruit.variety]]
                   name = "plantain"
                   })
   '#hasheq((fruit
             .
             (#hasheq((name . "apple")
                      (physical
                       .
                       #hasheq((color . "red") (shape . "round")))
                      (variety
                       .
                       (#hasheq((name . "red delicious"))
                        #hasheq((name . "granny smith")))))
              #hasheq((name . "banana")
                      (variety
                       .
                       (#hasheq((name . "plantain")))))))))
  ;; https://github.com/toml-lang/toml/issues/214
  (check-equal?
   (parse-toml @~a{[[foo.bar]]})
   (parse-toml @~a{[foo]
                   [[foo.bar]]}))
  ;; example from TOML README
  (check-exn
   #rx"conflicting values for `fruit.variety'"
   (λ () (parse-toml @~a{# INVALID TOML DOC
                         [[fruit]]
                         name = "apple"

                         [[fruit.variety]]
                         name = "red delicious"

                         # This table conflicts with the previous table
                         [fruit.variety]
                         name = "granny smith"})))
  ;; https://github.com/toml-lang/toml/pull/199#issuecomment-47300021
  ;; The tables and arrays of tables may come in ANY order. A plain table
  ;; may come "in the middle" of a nested table definition.
  (check-equal?
   (parse-toml @~a{[table]
                   key = 5

                   [[table.array]]
                   a = 1
                   b = 2

                   [another table]
                   key = 10

                   [[table.array]]
                   a = 2
                   b = 4})
   #hasheq((|another table| . #hasheq((key . 10)))
           (table . #hasheq((key . 5)
                            (array . (#hasheq((a . 1) (b . 2))
                                      #hasheq((a . 2) (b . 4)))))))))


(require racket/format
         racket/pretty)
#;
(pretty-print
 (parse-toml @~a{# INVALID TOML DOC
                 [[fruit]]
                 name = "apple"

                 [[fruit.variety]]
                 name = "red delicious"

                 # This table conflicts with the previous table
                 [fruit.variety]
                 name = "granny smith"}))
