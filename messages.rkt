#lang racket/base

(provide (struct-out chat)
         (struct-out message)
         (struct-out attachment)
         list-chats
         find-chats-by-contact)

(require db
         memoize
         racket/date
         racket/format
         racket/list
         racket/string
         "backup.rkt"
         "contacts.rkt"
         "utils.rkt")

(define MESSAGES-DB "3d0d7e5fb2ce288813306e4d4636395e047a3d28")

(struct chat (contacts messages) #:prefab)
(struct message (date service sender subject text attachments) #:prefab)
(struct attachment (name path) #:prefab)

(define chats-by-backup (make-hash))
(hash-set! chats-by-backup #f '())

; Load all chats from a backup directory
(define (list-chats)
  (hash-ref!
   chats-by-backup
   (current-backup)
   (λ ()
     (parameterize ([date-display-format 'iso-8601])
       ; Connect to the correct DB
       (define sms-db (sqlite3-connect #:database (build-path (backup-path (current-backup)) MESSAGES-DB)))
       
       ; Loop over the individual chat ids
       (for/list ([(chat-id) (in-query sms-db "SELECT ROWID FROM chat")])
         ; Determine which contacts were involved in the conversation by contact
         ; Use models/contacts.rkt to figure out who belongs to contact information
         (define user-query "SELECT id FROM chat_handle_join, handle WHERE chat_id = $1 AND handle_id = ROWID ORDER BY handle_id ASC")
         (define contacts
           (for/list ([(contact) (in-query sms-db user-query chat-id)])
             (find-contact (normalize-contact contact))))
         
         ; Load the individual messages
         (define msg-query "
SELECT
  message.ROWID as message_id,
  date,
  message.service,
  is_from_me,
  handle.id as them,
  (CASE WHEN subject IS NULL THEN '' ELSE subject END),
  (CASE WHEN text IS NULL THEN '' ELSE text END),
  (SELECT group_concat(filename) FROM message_attachment_join, attachment WHERE message_id = message.ROWID AND attachment_id = attachment.ROWID)
FROM
  chat_message_join,
  message,
  handle
WHERE
  chat_id = $1
  AND message_id = message.ROWID
  AND handle_id = handle.ROWID
ORDER BY date ASC")
         (define messages
           (for/list ([(message-id raw-date service from-me? other-party subject text raw-attachments)
                       (in-query sms-db msg-query chat-id)])
             ; Correct dates from Apple time to unix time
             ; TODO: Account for timezones?
             (define date (seconds->date (+ raw-date 978336000 (- (* 16 60 60))) #f))
             
             (define sender
               (if (= 1 from-me?)
                   (backup-phone-number (current-backup))
                   (normalize-contact other-party)))
             
             ; Load attachments,
             (define attachments
               (if (sql-null? raw-attachments)
                   '()
                   (for/list ([path (in-list (string-split raw-attachments ","))])
                     (attachment
                      (path->string (last (explode-path path)))
                      (build-path (backup-path (current-backup)) (get-attachment-hash path))))))
             
             (message date service sender subject text attachments)))
         
         ; Create the chat object
         (chat contacts messages))))))

; Get all chats involving a specific chat
(define (find-chats-by-contact contact #:direct? [direct? #f])
  ; Allow the user to specify the contact by name / phone number / email / etc
  (when (not (contact? contact))
    (set! contact (find-contact contact)))
  
  ; Filter the list of chats
  (for/list ([chat (in-list (list-chats))]
             #:when (if direct? 
                        (equal? (list contact) (chat-contacts chat))
                        (member contact (chat-contacts chat))))
    chat))