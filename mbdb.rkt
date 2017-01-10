#lang racket/base

#|
(provide (struct-out record)
         (struct-out property)
         read-mbdb)

(require db
         "backup.rkt")

(struct record (domain path hash) #:prefab)

; Read mdbd records from the iOS 10 sqlite mbdb file
(define (read-mbdb path)
  (define manifest-db (sqlite3-connect #:database (build-path (backup-path (current-backup)) "Manifest.db")))

  ; FILES: fileID TEXT PRIMARY KEY, domain TEXT, relativePath TEXT, flags INTEGER, file BLOB
  ; CREATE TABLE Properties (key TEXT PRIMARY KEY, value BLOB)

  ; Flags = 1, 2, 4 for file, folder, and symlink(?) respectively; we only care about files for now
  
  (for/list ([(domain path type plist)
              (in-query "SELECT domain, relativePath, flags, file from Files")]
             #:when (= type 1))

    (define hash (hash-filename path domain))
    (record domain path hash)))
|#