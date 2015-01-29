#lang racket/base

(provide (struct-out backup)
         current-backup
         list-backups
         read-backup
         with-backup)

(require racket/dict
         "utils.rkt")

; Represents an iPhone backup on disk
(struct backup (name hash date phone-number path) #:prefab)

; Store the most recently used backup for other modules
(define current-backup (make-parameter #f))

; Load all backups on disk into a list
(define list-backups
  (let* (; OS agnostic (I hope) way of finding the backup root
         [backup-root
          (for*/first ([path-parts (in-list '(("AppData" "Roaming" "Apple Computer" "MobileSync" "Backup")
                                              ("Library" "Application Support" "MobileSync" "Backup")))]
                       [path (in-value (apply build-path (cons (find-system-path 'home-dir) path-parts)))]
                       #:when (directory-exists? path))
            path)]
         
         ; List all backups in that directory, along with some metadata
         [backups
          (for/list ([dir (in-list (directory-list backup-root))]
                     #:when (file-exists? (build-path backup-root dir "Info.plist"))) 
            (define info-file (call-with-input-file (build-path backup-root dir "Info.plist") read-plist/jsexpr))
            (backup (dict-ref info-file '|Device Name|)
                    (path->string dir)
                    (dict-ref info-file '|Last Backup Date|)
                    (normalize-contact (dict-ref info-file '|Phone Number|))
                    (build-path backup-root dir)))])
    
    (λ () backups)))

; Load a specific backup, try to guess what the identifier is
(define (read-backup identifier)
  (for/first ([backup (in-list (list-backups))]
              #:when (or (equal? identifier (backup-date backup))
                         (equal? identifier (backup-name backup))
                         (equal? identifier (backup-hash backup))
                         (equal? identifier (backup-phone-number backup))))
    backup))

; Parameterize code with a current backup
(define-syntax-rule (with-backup identifier body ...)
  (parameterize ([current-backup (read-backup identifier)])
    body ...))

