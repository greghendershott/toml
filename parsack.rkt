#lang racket/base

(require (only-in parsack
                  [parser-compose pdo] ;; More concise, less indent
                  [parser-one pdo-one] ;; "
                  [parser-seq pdo-seq] ;; "
                  >>= >>
                  try <or> <?> choice $err
                  satisfy char string stringAnyCase
                  many many1
                  manyTill many1Till
                  manyUntil many1Until
                  sepBy sepBy1
                  oneOf noneOf oneOfStrings
                  option optional
                  return
                  between
                  lookAhead
                  notFollowedBy
                  $space $newline $anyChar $letter $digit $hexDigit
                  $alphaNum $eof
                  getState setState withState
                  Consumed Empty Ok Error
                  parse parse-result parsack-error parse-source
                  exn:fail:parsack?))

(provide pdo
         pdo-one
         pdo-seq
         >>= >>
         try <or> <?> choice $err
         satisfy char string stringAnyCase
         many many1
         manyTill many1Till
         manyUntil many1Until
         sepBy sepBy1
         oneOf noneOf oneOfStrings
         option optional
         return
         between
         lookAhead
         notFollowedBy
         $space $newline $anyChar $letter $digit $hexDigit
         $alphaNum $eof
         getState setState withState
         Consumed Empty Ok Error
         parse parse-result parsack-error parse-source
         exn:fail:parsack?)

(define (getPosition)
  (λ (in)
    (define-values (r c pos) (port-next-location in))
    (Empty (Ok (list r c pos)))))
(provide getPosition)
