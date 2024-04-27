#lang racket/base

(require racket/list)

(provide (all-defined-out))

(define (string-split-at str c)
  (let ((index (index-of (string->list str) c)))
    (if index
        (let ((index (add1 index)))
          (cons (substring str 0 index)
                (substring str index)))
        #f)))

(define (char-between? low high c)
  (char<=? low c high))

(define (char-between-any? ranges c)
  (ormap (λ (range) (char-between? (car range) (cdr range) c))
          ranges))

(define range-ascii-upper '(#\A . #\Z))
(define range-ascii-lower '(#\a . #\z))
(define range-ascii-digit '(#\0 . #\9))

(define (string-form? start middle* end? str)
  (let* ((slen (string-length str))
         (stchar (string-first str))
         (enchar (if (> slen 1) (string-last str) #f))
         (midchars (if (> slen 2) (substring str 1 (- slen 2)) #f)))
    (and (start stchar)
         (if midchars (string-andmap middle* midchars) #t)
         (if enchar (end? enchar) #t))))

(define (string-first str)
  (string-ref str 0))

(define (string-last str)
  (string-ref str (sub1 (string-length str))))

(define (string-rest str)
  (substring str 1))

(define (string-char=? str k c)
  (char=? (string-ref str k) c))

(define (string-map proc str)
  (map proc (string->list str)))

(define (string-andmap proc str)
  (andmap proc (string->list str)))

(define (string-ormap proc str)
  (ormap proc (string->list str)))

(define (enumerate lst)
  (map cons (range (length lst)) lst))

(define (string-enumerate str)
  (enumerate (string->list str)))

(define (string-empty? str)
  (and (string? str) (= (string-length str) 0)))
