#lang racket

(provide (struct-out app)
         list-apps
         find-app)

(require db
         "backup.rkt"
         "utils.rkt")

(struct app (name plist) #:prefab #:mutable)

(define apps-by-backup (make-hash))
(hash-set! apps-by-backup #f '())

(define info.plist (build-path (backup-path (current-backup)) "Info.plist"))

; List all installed applications
(define (list-apps)
  (hash-ref! 
   apps-by-backup
   (current-backup)
   (for/list ([name (in-list
                     (hash-ref
                      (call-with-input-file info.plist
                        read-plist/jsexpr)
                      '|Installed Applications|))])
     (app name #f))))

; Find an app by name (actually a case insensative regex)
; plists are loaded when this is called and cached
(define (find-app name)
  (define app
    (for/first ([app (in-list (list-apps))]
                #:when (regexp-match (~a "(?i:" name ")") (app-name app)))
      app))
  
  ; If this app is missing it's plist, load it
  ; Try to load as an xml plist firs,t fall back to binary
  (when (and app (not (app-plist app)))
    (set-app-plist!
     app 
     (let ([plist-path
            (build-path 
             (backup-path (current-backup))
             (get-attachment-hash
              (~a "Library/Preferences/" (app-name app) ".plist")
              (~a "AppDomain-" (app-name app))))])
  
       (with-handlers ([exn? (λ (exn)
                               (call-with-input-file plist-path read-plist/jsexpr/binary))])
         (call-with-input-file plist-path read-plist/jsexpr)))))
  
  app)