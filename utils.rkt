#lang racket/base

(provide (all-defined-out))

(require db
         racket/list
         racket/match
         racket/string
         xml/plist)

; Process a phone number or email address into a common format
(define (normalize-contact value)
  (cond
    [(sql-null? value)
     #f]
    ; Standard phone numbers
    ; TODO: Figure out international numbers
    [(regexp-match #px"^\\+?1? ?[\\(\\.]?(\\d\\d\\d)[\\)\\.-]? ?(\\d\\d\\d)[ \\.-]?(\\d\\d\\d\\d)$" value)
     => (λ (match) (string-join (rest match) "."))]
    ; Email addresses
    [(regexp-match #px"^[^@]+@[^@]+$" value)
     value]
    ; Short phone numbers
    [(regexp-match #px"^\\d{,6}$" value)
     value]
    ; No idea...
    [else #f]))

; Convert a plist into a JSON expression
(define (plist->jsexpr data)
  (match data
    [(? string?) data]
    [`(true) #t]
    [`(false) #f]
    [`(integer ,v) v]
    [`(real ,v) v]
    [`(data ,v) v] ; Should we special case these?
    [`(date ,v) v] ; Ditto 
    [`(array . ,v*)
     (map plist->jsexpr v*)]
    [`(dict . ,kv*)
     (for/hash ([kv (in-list kv*)])
       (values (string->symbol (second kv)) (plist->jsexpr (third kv))))]))

; Read a plist file as a JSON expression from a file
(define (read-plist/jsexpr [in (current-input-port)])
  (plist->jsexpr (read-plist in)))