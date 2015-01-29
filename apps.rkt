#lang racket/base

(provide (struct-out app)
         (struct-out file)
         list-apps
         find-app)

(require db
         racket/format
         "backup.rkt"
         "utils.rkt"
         "mbdb.rkt")

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

; Get all MDBD records
(define (list-mdbd-records)
  (hash-ref!
   mdbd-records-by-backup
   (current-backup)
   (λ ()
     (with-input-from-file (build-path (backup-path (current-backup)) "Manifest.mbdb") read-mbdb))))

; Load an app's plist file
(define (load-plist! app)
  (set-app-plist!
   app 
   (let ([plist-path
          (build-path 
           (backup-path (current-backup))
           (hash-filename
            (~a "Library/Preferences/" (app-name app) ".plist")
            (~a "AppDomain-" (app-name app))))])
     
     ; Try to load in text mode first, if that fails fall back to binary
     (with-handlers ([exn? (λ (exn)
                             (call-with-input-file plist-path read-plist/jsexpr/binary))])
       (call-with-input-file plist-path read-plist/jsexpr)))))

; Load the list of files associated with an app
(define (load-files! app)
  (define app-domain (~a "AppDomain-" (app-name app)))
  
  (set-app-files!
   app
   (for/list ([record (in-list (list-mdbd-records))]
              #:when (and (equal? (record-domain record) app-domain)
                          (eq? 'file (record-mode record))))
     (file (record-path record)
           (build-path (backup-path (current-backup)) (record-hash record))))))

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