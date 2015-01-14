#lang racket/base

(provide (struct-out contact)
         list-contacts
         find-contact)

(require db
         memoize
         racket/format
         racket/string
         "backup.rkt"
         "utils.rkt")

(define CONTACTS-DB "31bb7ba8914766d4ba40d6dfb6113c8b614be442")

; Name is a human readable name for a contact, identifiers are phone numbers / emails / etc
(struct contact (name identifiers) #:prefab)

; Store a seperate list of contacts for each backup (potentially)
(define contacts-by-backup (make-hash))
(hash-set! contacts-by-backup #f '())

; Load all contacts stored in a specific backup
(define (current-contacts)
  (hash-ref! 
   contacts-by-backup
   (current-backup)
   (λ ()
     (define contacts-db (sqlite3-connect #:database (build-path (backup-path (current-backup)) CONTACTS-DB)))
     
     (for/list ([(user-id first-name middle-name last-name organization)
                 (in-query contacts-db "SELECT ROWID, First, Middle, Last, Organization FROM ABPerson")])
       
       (define (fix str) (if (sql-null? str) "" str))
       
       (define name
         (let* ([name (~a (fix first-name) " " (fix middle-name) " " (fix last-name) " (" (fix organization) ")")]
                [name (regexp-replace* #px"\\(\\)" name "")]
                [name (regexp-replace* #px"\\s+" name " ")]
                [name (string-trim name)]
                [name (regexp-replace* #px"^\\((.*)\\)$" name "\\1")])
           name))
       
       (define identifiers
         (for*/list ([raw-value (in-list (query-list contacts-db "SELECT value FROM ABMultiValue WHERE record_id = $1" user-id))]
                     [value (in-value (normalize-contact raw-value))]
                     #:when value)
           value))
       
       (contact name identifiers)))))

; Return a list of current contacts
(define (list-contacts)
  (current-contacts))

; Load a user by name or value
(define/memo (find-contact key)
  (for/first ([contact (in-list (current-contacts))]
              #:when (or (equal? key (contact-name contact))
                         (member key (contact-identifiers contact))))
    contact))

