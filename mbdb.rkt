#lang racket/base

(provide (struct-out record)
         (struct-out property)
         read-mbdb)

(require "utils.rkt")

; File format documentation
; https://code.google.com/p/iphonebackupbrowser/wiki/MbdbMbdxFormat

(struct record (domain path hash mode size properties) #:prefab)
(struct property (name value) #:prefab)

; Read size bytes as a big-endian unsigned integer
(define (read-uint size [in (current-input-port)])
  (define b* (read-bytes size in))
  (for/sum ([i (in-naturals)]
            [b (in-bytes b*)])
    (* (expt 256 (- (bytes-length b*) i 1)) b)))

; Read a length + string, if length if #xFFFF the string is empty
; Note: Sometimes 'strings' are actually binary data, return those as bytes
(define (read-string-data [in (current-input-port)])
  (define size (read-uint 2 in))
  (cond
    [(equal? size #xFFFF) ""]
    [else 
     (define raw (read-bytes size in))
     (with-handlers ([exn? (λ (_) raw)])
       (bytes->string/utf-8 raw))]))

; Read an mbdb record
(define (read-record [in (current-input-port)])
  
  (define domain (read-string-data in))
  (define path (read-string-data in))
  (define link-target (read-string-data in))
  (define data-hash (read-string-data in))
  (define encryption-key (read-string-data in))
  
  (define raw-mode (read-uint 2 in))
  (define mode
    (case (arithmetic-shift raw-mode -12)
      [(#xA) (list 'symlink link-target)]
      [(#x8) 'file]
      [(#x4) 'directory]))
  
  (define inode (read-uint 8 in))
  (define user-id (read-uint 4 in))
  (define group-id (read-uint 4 in))

  (define last-modified (read-uint 4 in))
  (define last-accessed (read-uint 4 in))
  (define created-at (read-uint 4 in))
  (define file-size (read-uint 8 in))
  
  (define data-protection-class (read-uint 1 in)) ; 0x1 to 0xB
  (define property-count (read-uint 1 in))
  
  (define properties
    (for/list ([i (in-range property-count)])
      (define name (read-string-data in))
      (define value (read-string-data in))
      (property name value)))
  
  (define hash (hash-filename path domain)) 
  
  (record domain path hash mode file-size properties))

; Read mdbd records until eof
(define (read-mbdb [in (current-input-port)])
  (read-bytes 6 in)
  (let loop ()
    (cond
      [(eof-object? (peek-char in)) '()]
      [else 
       (cons (read-record in) (loop))])))

(require "backup.rkt" racket/list)
(with-backup "86b18eea28a991f4dd569d1f59737a842e24aa36"
  (take (call-with-input-file (build-path (backup-path (current-backup)) "Manifest.mbdb")
          read-mbdb)
        10))