#lang at-exp racket/base

(require "parsack.rkt"
         "hash-util.rkt"
         racket/string
         racket/list
         racket/pretty
         racket/format)

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

(define ($array state) ($_array state)) ;; "forward decl"

(define $val
  (<or> $true-lit
        $false-lit ;try before $numeric-lit. "fa" in "false" could be hex
        $numeric-lit
        $string-lit
        ;; $datetime-lit  ;;TO-DO
        $array))

(define $key-char ;; valid for a key
  (<or> $alphaNum (oneOf "-!@#$%^&*|/")))

(define $key ;; >> symbol?
  (<?> (pdo (cs <- (many1 $key-char))
            (return (string->symbol (list->string cs))))
       "key"))

(define $key/val ;; >> (cons/c symbol? $val)
  (pdo $sp (key <- $key) $sp
       (char #\=)
       $sp (val <- $val) $sp
       (<or> $comment $newline)
       (many $blank-or-comment-line)
       $sp
       (return (hasheq key val))))

(define $_array
  ;; Note: TOML array item types are not allowed to be mixed. However
  ;; I think that's a semantic issue not a parsing issue (?).
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
            (return (list->vector vs)))))

(define $table-keys ;; >> (listof symbol?)
  (sepBy1 $key (char #\.)))

(define $table
  (try (pdo $sp
            (keys <- (between (char #\[) (char #\]) $table-keys))
            $sp (<or> $comment $newline)
            (many $blank-or-comment-line)
            (kvs <- (many $key/val))
            (many $blank-or-comment-line)
            $sp
            (return (hash-sets (make-immutable-hasheq)
                               keys
                               (foldl hasheq-merge
                                      (make-immutable-hash)
                                      kvs))))))

(define (group keys)
  (pdo (many $blank-or-comment-line)
       (kvs <- (many $key/val))
       (tabs <- (many $table))
       (aots <- (many (array-of-tables/under keys)))
       ;; "Hoist up" the hasheq's to same level as `keys`
       (tabs <- (return (for/list ([tab tabs]) (hash-refs tab keys))))
       (aots <- (return (for/list ([aot aots]) (hash-refs aot keys))))
       (return (foldl hasheq-merge
                      (make-immutable-hasheq)
                      (append kvs tabs aots)))))

(define (array-of-table-item keys)
  (try (pdo (between (string "[[") (string "]]") 
                     (string (string-join (map symbol->string keys) ".")))
            $sp (<or> $comment $newline)
            (many $blank-or-comment-line)
            (x <- (group keys))
            (return x))))

(define $array-of-tables
  (<?> (try (pdo $sp
                 (keys <- (lookAhead (between (string "[[") (string "]]")
                                              $table-keys)))
                 (xs <- (many1 (array-of-table-item keys)))
                 (return (hash-sets (make-immutable-hasheq)
                                    keys
                                    (list->vector xs)))))
       "array of tables"))

(define (array-of-tables/under parent-keys)
  (<?> (try (pdo $sp
                 (keys <-
                       (lookAhead
                        (between (string "[[") (string "]]")
                                 (pdo (string (string-join (map symbol->string parent-keys) "."))
                                      (char #\.)
                                      (keys <- $table-keys)
                                      (return (append parent-keys keys))))))
                 (xs <- (many1 (array-of-table-item keys)))
                 (return (hash-sets (make-immutable-hasheq)
                                    keys
                                    (list->vector xs)))))
       "array of tables"))

(define $toml-document
  (pdo (many $blank-or-comment-line)
       (xs <- (many $key/val))
       (ys <- (many (<or> $table $array-of-tables)))
       $eof
       (return (foldl hasheq-merge
                      (make-immutable-hasheq)
                      (append xs ys)))))

(define (parse-toml s)
  (parse-result $toml-document (string-append s "\n\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
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
                      #(#hasheq((aot0 . 10) (aot1 . 11))
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
                      #(#hasheq((aot0 . 10) (aot1 . 11))
                        #hasheq((aot0 . 20) (aot1 . 21))
                        #hasheq((aot0 . 30) (aot1 . 31))))))
            (ten . 10)
            (array0 . #(1 2 3))
            (array1 . #(1 2 3))
            (array2 . #(1 2 3))
            (nested-array . #(#(1 2 3) #(4 5 6)))
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
             #(#hasheq((name . "apple")
                       (physical
                        .
                        #hasheq((color . "red") (shape . "round"))))
               #hasheq((name . "banana"))))))
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
             #(#hasheq((name . "apple")
                       (physical
                        .
                        #hasheq((color . "red") (shape . "round")))
                       (variety
                        .
                        #(#hasheq((name . "red delicious"))
                          #hasheq((name . "granny smith")))))
               #hasheq((name . "banana")
                       (variety
                        .
                        #(#hasheq((name . "plantain"))))))))))
