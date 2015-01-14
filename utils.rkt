#lang racket/base

(provide (all-defined-out))

(require db
         file/sha1
         racket/list
         racket/format
         racket/match
         racket/string
         racket/port
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

; Hash attachments so that we can find the local path
(define (get-attachment-hash path [domain "MediaDomain"])
  (for*/first ([prefix (in-list (list "/var/mobile/"
                                      "~/"))]
               #:when (and (> (string-length path) (string-length prefix))
                           (equal? (substring path 0 (string-length prefix)) prefix)))
    (define path-w/o-prefix (substring path (string-length prefix)))
    (call-with-input-string (~a domain "-" path-w/o-prefix) sha1)))