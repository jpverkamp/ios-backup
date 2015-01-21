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
(define (read-uint size)
  (define b* (read-bytes size))
  (for/sum ([i (in-naturals)]
            [b (in-bytes b*)])
    (* (expt 256 (- (bytes-length b*) i 1)) b)))

; Read a length + string, if length if #xFFFF the string is empty
; Note: Sometimes 'strings' are actually binary data, return those as bytes
(define (read-string-data)
  (define size (read-uint 2))
  (cond
    [(equal? size #xFFFF) ""]
    [else 
     (define raw (read-bytes size))
     (with-handlers ([exn? (λ (_) raw)])
       (bytes->string/utf-8 raw))]))

; Read an mbdb record
(define (read-record)
  
  (define domain (read-string-data))
  (define path (read-string-data))
  (define link-target (read-string-data))
  (define data-hash (read-string-data))
  (define encryption-key (read-string-data))
  
  (define raw-mode (read-uint 2))
  (define mode
    (case (arithmetic-shift raw-mode -12)
      [(#xA) (list 'symlink link-target)]
      [(#x8) 'file]
      [(#x4) 'directory]))
  
  (define inode (read-uint 8))
  (define user-id (read-uint 4))
  (define group-id (read-uint 4))

  (define last-modified (read-uint 4))
  (define last-accessed (read-uint 4))
  (define created-at (read-uint 4))
  (define file-size (read-uint 8))
  
  (define data-protection-class (read-uint 1)) ; 0x1 to 0xB
  (define property-count (read-uint 1))
  
  (define properties
    (for/list ([i (in-range property-count)])
      (define name (read-string-data))
      (define value (read-string-data))
      (property name value)))
  
  (define hash (get-attachment-hash path domain)) 
  
  (record domain path hash mode file-size properties))

; Read mdbd records until eof
(define (read-mbdb)
  (read-bytes 6)
  (let loop ()
    (cond
      [(eof-object? (peek-char)) '()]
      [else (cons (read-record) (loop))])))