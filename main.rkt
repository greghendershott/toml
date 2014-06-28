#lang at-exp racket/base

(require "parsack.rkt"
         "hash-util.rkt"
         racket/string
         racket/list
         racket/date
         racket/function
         racket/contract
         racket/match)

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

(define $string-lit
  (<?> (try (pdo (char #\")
                 (cs <- (manyUntil $anyChar (char #\")))
                 (return (list->string cs))))
       "double-quoted string value"))

(define $numeric-lit
  (<?> (try (pdo (ds <- (many1 (<or> $hexDigit (char #\.))))
                 (return (string->number (list->string ds)))))
       "numeric value"))

(define $true-lit  (pdo (string "true")  (return #t)))
(define $false-lit (pdo (string "false") (return #f)))

(define ->num (compose string->number list->string list))
(define $4d (pdo-seq $digit $digit $digit $digit #:combine-with ->num))
(define $2d (pdo-seq $digit $digit #:combine-with ->num))

(define $datetime-lit
  ;; 1979-05-27T07:32:00Z
  (try (pdo (yr <- $4d)
            (char #\-)
            (mo <- $2d)
            (char #\-)
            (dy <- $2d)
            (char #\T)
            (hr <- $2d)
            (char #\:)
            (mn <- $2d)
            (char #\:)
            (sc <- $2d)
            (char #\Z)
            (return (date->seconds (date sc mn hr dy mo yr 0 0 #f 0))))))

(define ($array state) ($_array state)) ;; "forward decl"

(define $val
  (<or> $true-lit
        $false-lit ;try before $numeric-lit. "fa" in "false" could be hex
        $datetime-lit ;try before $numeric-list
        $numeric-lit
        $string-lit
        $array))

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

(define $key/val ;; >> (cons/c symbol? $val)
  (try (pdo $sp (key <- $key) $sp
            (char #\=)
            $sp (val <- $val) $sp
            (<or> $comment $newline)
            (many $blank-or-comment-line)
            $sp
            (return (cons key val)))))

(define (keys->string ks)
  (string-join (map symbol->string ks) "."))

(define (err ks v0 v1)
  (eprintf "conflicting values for key~a `~a'\n~a\n~a\n"
           (if (= 1 (length ks)) "" "s")
           (keys->string ks)
           v0 v1))

(define $_array
  ;; FIXME: TOML array item types are not allowed to be mixed. To
  ;; handle this with parsing (vs. semantically), we could insist on
  ;; same parser used for first item, for the remaining items.
  (try (pdo $sp
            (char #\[)
            $sp (optional $comment)
            (vs <- (many (pdo $spnl
                              (v <- $val)
                              (optional (char #\,))
                              $sp (optional $comment)
                              (many $blank-or-comment-line)
                              $spnl
                              (return v))))
            (char #\])
            (return vs))))
;; (define $_array
;;   (<or> (array-of (<or> $true-lit $false-lit))
;;         (array-of $datetime-lit)
;;         (array-of $numeric-lit)
;;         (array-of $string-lit)
;;         (array-of $_array)))
;; (define (array-of $value-parser)
;;   (try (pdo $sp
;;             (char #\[)
;;             $sp (optional $comment)
;;             (vs <- (many (pdo $spnl
;;                               (v <- $value-parser)
;;                               (optional (char #\,))
;;                               $sp (optional $comment)
;;                               (many $blank-or-comment-line)
;;                               $spnl
;;                               (return v))))
;;             (char #\])
;;             (return vs))))

(define $table-keys ;; >> (listof symbol?)
  (sepBy1 $table-key (char #\.)))

(define (table-keys-under parent-keys)
  (pdo (if (empty? parent-keys)
           (return null)
           (pdo (string (keys->string parent-keys))
                (char #\.)))
       (keys <- $table-keys)
       (return (append parent-keys keys))))

(define (table-under parent-keys)
  (<?> (try (pdo $sp
                 (keys <- (between (char #\[) (char #\])
                                   (table-keys-under parent-keys)))
                 $sp (<or> $comment $newline)
                 (many $blank-or-comment-line)
                 (kvs <- (many $key/val))
                 (many $blank-or-comment-line)
                 $sp
                 (return (merge (pairs->hasheqs keys kvs)))))
       "table"))

(define $table (table-under '()))

(define (array-of-tables-under parent-keys)
  (<?> (try (pdo $sp
                 (keys <- (between (string "[[") (string "]]")
                                   (table-keys-under parent-keys)))
                 $sp (<or> $comment $newline)
                 (many $blank-or-comment-line)
                 (kvs <- (many $key/val))
                 (ts  <- (many (<or> (table-under keys)
                                     (array-of-tables-under keys))))
                 (aots <- (many (array-of-tables-same keys)))
                 (many $blank-or-comment-line)
                 $sp
                 (return
                  (let* ([ts (map (curryr hash-refs keys) ts)] ;hoist up
                         [aot0 (merge (append (pairs->hasheqs '() kvs) ts))]
                         [aots (cons aot0 aots)])
                    (match-define (list all-but-k ... k) keys)
                    ;; (local-require racket/pretty)
                    ;; (displayln "===array-of-tables-under===")
                    ;; (pretty-print all-but-k)
                    ;; (pretty-print k)
                    ;; (pretty-print ts)
                    ;; (pretty-print aots)
                    (pair->hasheq all-but-k
                                  (cons k aots))))))
       "array-of-tables"))
            
(define (array-of-tables-same keys)
  (<?> (try (pdo $sp
                 (between (string "[[") (string "]]")
                          (string (keys->string keys)))
                 $sp (<or> $comment $newline)
                 (many $blank-or-comment-line)
                 (kvs <- (many $key/val))
                 (ts  <- (many (<or> (table-under keys)
                                     (array-of-tables-under keys))))
                 (many $blank-or-comment-line)
                 $sp
                 (return (merge (append (pairs->hasheqs '() kvs)
                                        (map (curryr hash-refs keys) ts))))))
       "array-of-tables"))

(define $array-of-tables (array-of-tables-under '()))

(define $toml-document
  (pdo (many $blank-or-comment-line)
       (kvs <- (many $key/val))
       (ts  <- (many (<or> $table $array-of-tables)))
       $eof
       (return (merge (append (pairs->hasheqs '() kvs)
                              ts)))))

;; Returns a `hasheq` using the same conventions as the Racket `json`
;; library. e.g. You should be able to give the result to
;; `jsexpr->string`.
(define (parse-toml s)
  (parse-result $toml-document (string-append s "\n\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc utils

(define (merge hts)
  (foldl hasheq-merge (hasheq) hts))

(define (pair->hasheq keys pair)
  (match keys
    [(list) (hasheq (car pair) (cdr pair))]
    [(list* this more) (hasheq this (pair->hasheq more pair))]))

(module+ test
  (require rackunit)
  (check-equal? (pair->hasheq '() '(x . 0))
                (hasheq 'x 0))
  (check-equal? (pair->hasheq '(a) '(x . 0))
                (hasheq 'a (hasheq 'x 0)))
  (check-equal? (pair->hasheq '(a b) '(x . 0))
                (hasheq 'a (hasheq 'b (hasheq 'x 0)))))

(define (pairs->hasheqs keys pairs)
  (map (curry pair->hasheq keys) pairs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require racket/format
;;          racket/pretty)
;; (pretty-print
;;  (parse-toml @~a{a = 1
;;                  b = 2
;;                  [x]
;;                  a = 1
;;                  [[aot]]
;;                  x=0
;;                  [[aot]]
;;                  x=1
;;                  }))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require rackunit
           racket/format)
  (check-equal?
   (parse-toml @~a{today = 2014-06-26T12:34:56Z
                   })
   '#hasheq((today . 1403800496)))
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
   #rx"conflicting values for key"
   (λ () (parse-toml @~a{# INVALID TOML DOC
                         [[fruit]]
                         name = "apple"

                         [[fruit.variety]]
                         name = "red delicious"

                         # This table conflicts with the previous table
                         [fruit.variety]
                         name = "granny smith"})))
  ;; https://github.com/toml-lang/toml/pull/199#issuecomment-47300021
  ;; Note: My original parser model FAILS this. The tables and arrays
  ;; of tables may come in ANY order!
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
   #hasheq((|another table| . #hasheq((key . 5)))
           (table . #hasheq((key . 5)
                            (array . (#hasheq((a . 1) (b . 2))
                                      #hasheq((a . 2) (b . 4)))))))))
