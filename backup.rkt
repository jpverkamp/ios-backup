#lang racket/base

(provide (struct-out backup)
         current-backup
         list-backups
         read-backup
         with-backup)

(require json
         racket/dict
         xml/plist
         "utils.rkt")

; Represents an iPhone backup on disk
(struct backup (name hash date phone-number path) #:transparent)

; Store the most recently used backup for other modules
(define current-backup (make-parameter #f))

; Load all backups on disk into a list
(define (list-backups)
  ; Automatically find the backup path
  (define backup-root
    (for*/first ([path-parts (in-list '(("AppData" "Roaming" "Apple Computer" "MobileSync" "Backup")
                                        ("Library" "Application Support" "MobileSync" "Backup")))]
                 [path (in-value (apply build-path (cons (find-system-path 'home-dir) path-parts)))]
                 #:when (directory-exists? path))
      path))
  
  ; Determine which backup we are parsing
  (for/list ([dir (in-list (directory-list backup-root))])
    (define info-file (call-with-input-file (build-path backup-root dir "Info.plist") read-plist/jsexpr))
    (backup (dict-ref info-file '|Device Name|)
            (path->string dir)
            (dict-ref info-file '|Last Backup Date|)
            (normalize-contact (dict-ref info-file '|Phone Number|))
            (build-path backup-root dir))))

; Load a specific backup by either name or hash 
(define (read-backup #:by-date [by-date #f]
                     #:by-hash [by-hash #f]
                     #:by-name [by-name #f] 
                     #:by-phone-number [by-phone-number #f])
  
  (when (not (= 1 (length (filter (λ (x) x) (list by-date by-hash by-name by-phone-number)))))
    (error 'read-backup "must specify exactly one identifier"))
  
  (for/first ([backup (in-list (list-backups))]
              #:when (or (and by-date (equal? by-date (backup-date backup)))
                         (and by-name (equal? by-name (backup-name backup)))
                         (and by-hash (equal? by-hash (backup-hash backup)))
                         (and by-phone-number (equal? by-phone-number (backup-phone-number backup)))))
    backup))

; Parameterize code with a current backup
(define (with-backup thunk #:by-name [name #f] #:by-hash [hash #f])
  (parameterize ([current-backup (read-backup #:by-name name #:by-hash hash)])
    (thunk)))

