#lang racket/base

(provide (struct-out contact)
         list-contacts
         find-contact)

(require db
         racket/format
         racket/string
         "backup.rkt"
         "utils.rkt")

(define CONTACTS-DB (hash-filename "Library/AddressBook/AddressBook.sqlitedb" "HomeDomain"))

; Name is a human readable name for a contact, identifiers are phone numbers / emails / etc
(struct contact (name identifiers) #:prefab)

; Store a separate list of contacts for each backup (potentially)
(define contacts-by-backup (make-hash))
(hash-set! contacts-by-backup #f '())

; Load all contacts stored in a specific backup
(define (list-contacts)
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

; Load a user by name or value
(define (find-contact key)
  (for/first ([contact (in-list (list-contacts))]
              #:when (or (equal? key (contact-name contact))
                         (member key (contact-identifiers contact))))
    contact))

