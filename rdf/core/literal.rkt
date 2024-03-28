#lang racket/base

(require racket/contract
         ;; --------------------------------------
         net/url-structs)

(provide (struct-out language-string)
         ;; --------------------------------------
         (struct-out typed-string)
         ;; --------------------------------------
         literal?)

(struct language-string (text language)
  #:sealed
  #:constructor-name make-language-string
  #:guard (struct-guard/c string? string?))

(struct typed-string (text datatype)
  #:sealed
  #:constructor-name make-typed-string
  #:guard (struct-guard/c string? url?))

(define literal? (or/c boolean? number? string? language-string? typed-string? url?))

