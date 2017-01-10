#lang racket/base

(provide (struct-out app)
         (struct-out file)
         list-apps
         find-app)

(require db
         racket/format
         "backup.rkt"
         "utils.rkt")

(struct app (name plist files) #:prefab #:mutable)
(struct file (name path) #:prefab)

(define apps-by-backup (make-hash))
(hash-set! apps-by-backup #f '())

(define mdbd-records-by-backup (make-hash))
(hash-set! mdbd-records-by-backup #f '())

; List all installed applications
(define (list-apps)
  (hash-ref!
   apps-by-backup
   (current-backup)
   (λ ()
     (for/list ([name (in-list
                       (hash-ref
                        (call-with-input-file (build-path (backup-path (current-backup)) "Info.plist")
                          read-plist/jsexpr)
                        '|Installed Applications|))])
       (app name #f #f)))))

; Load an app's plist file
(define (load-plist! app)
  (set-app-plist!
   app
   (let* ([hash (hash-filename
                 (~a "Library/Preferences/" (app-name app) ".plist")
                 (~a "AppDomain-" (app-name app)))]
          [plist-path
           (build-path
            (backup-path (current-backup))
            (substring hash 0 2)
            hash)])

     ; Try to load in text mode first, if that fails fall back to binary
     (with-handlers ([exn? (λ (exn)
                             (call-with-input-file plist-path read-plist/jsexpr/binary))])
       (call-with-input-file plist-path read-plist/jsexpr)))))

; Load the list of files associated with an app
(define (load-files! app)
  (define app-domain (~a "AppDomain-" (app-name app)))
  
  (define manifest-db (sqlite3-connect #:database (build-path (backup-path (current-backup)) "Manifest.db")))
  (define manifest-files-sql "SELECT domain, relativePath, flags, file FROM Files WHERE domain = $1")

  (set-app-files!
   app
   (for/list ([(domain path type plist)
               (in-query manifest-db manifest-files-sql app-domain)]
             #:when (= type 1))
     (define hash (hash-filename path domain))
     (file path (build-path (backup-path (current-backup)) (substring hash 0 2) hash)))))

; Find an app by name (actually a case insensative regex)
; plists and files in domain are loaded when this is called and cached
(define (find-app name)
  (define app
    (for/first ([app (in-list (list-apps))]
                #:when (regexp-match (~a "(?i:" name ")") (app-name app)))
      app))

  ; If this app is missing it's plist / files, load them
  (when (and app (not (app-plist app))) (load-plist! app))
  (when (and app (not (app-files app))) (load-files! app))

  app)
