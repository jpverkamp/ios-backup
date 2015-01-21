#lang racket/base

(provide (all-defined-out))

(require db
         file/sha1
         racket/list
         racket/file
         racket/format
         racket/match
         racket/string
         racket/system
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

; Read a plist file as a JSON expression from a binary plist file
(define (read-plist/jsexpr/binary [in (current-input-port)])
  ; Copy the file to a temporary path
  (define temp-filename (~a (gensym) ".plist"))
  (call-with-output-file temp-filename
    (λ (out) (copy-port in out)))
    
  ; Run Apple's plutil to convert it
  ; Redirect err to suppress from missing programs
  (parameterize ([current-error-port (open-output-nowhere)])
    (for* ([path (in-list '("plutil"
                            "plutil.exe"
                            "\"C:\\Program Files (x86)\\Common Files\\Apple\\Apple Application Support\\plutil.exe\""))]
           [return (in-value (system (~a path " -convert xml1 " temp-filename)))]
           #:break return)
      #t))
  
  ; Patch over xml/plist's handling of empty string element
  (define plist-fixed-content 
    (regexp-replace* #px"<string></string>"
                     (file->string temp-filename)
                     "<string> </string>"))
  (delete-file temp-filename)
  
  ; Read the plist into memory and remove the temporary file
  (call-with-input-string plist-fixed-content read-plist/jsexpr))

; Hash attachments so that we can find the local path
(define (get-attachment-hash path [domain "MediaDomain"])
  (for*/first ([prefix (in-list (list "/var/mobile/"
                                      "~/"
                                      ""))]
               #:when (and (> (string-length path) (string-length prefix))
                           (equal? (substring path 0 (string-length prefix)) prefix)))
    (define path-w/o-prefix (substring path (string-length prefix)))
    (call-with-input-string (~a domain "-" path-w/o-prefix) sha1)))