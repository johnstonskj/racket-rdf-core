#lang racket/base

(require racket/bool
         racket/contract
         racket/list
         racket/string
         ;; --------------------------------------
         net/url-string
         net/url-structs)

(provide ncname?
         qname?
         string->ncname
         symbol->ncname
         ;; --------------------------------------
         url-absolute?
         ;; --------------------------------------
         (except-out (struct-out namespace)
                     internal-make-namespace)
         make-namespace
         namespace-make-url
         namespace-make-qname
         namespace-make-default-qname
         ;; --------------------------------------
         (struct-out name)
         make-name
         name->url
         name->qname)

;; -------------------------------------------------------------------------------------------------
;; `ncname` contracts
;; -------------------------------------------------------------------------------------------------

(define (ncname? val)
  (cond
    ((symbol? val)
     (ncname? (symbol->string val)))
    ((non-empty-string? val)
     (let ((char-list (string->list val)))
       (and (name-start-char/c (car char-list))
            (andmap name-char/c (cdr char-list)))))
    (else #f)))

(define (string->ncname str)
  (if (ncname?) str #f))

(define (symbol->ncname sym)
  (string->ncname (symbol->string sym)))

(define name-start-char/c
  (or/c
   (λ (val) (char=? val #\_))
   (char-in #\A #\Z)
   (char-in #\a #\z)
   (char-in #\u00C0 #\u00D6)
   (char-in #\u00D8 #\u00F6)
   (char-in #\u00F8 #\u02FF)
   (char-in #\u0370 #\u037D)
   (char-in #\u037F #\u1FFF)
   (char-in #\u200C #\u200D)
   (char-in #\u2070 #\u218F)
   (char-in #\u2C00 #\u2FEF)
   (char-in #\u3001 #\uD7FF)
   (char-in #\uF900 #\uFDCF)
   (char-in #\uFDF0 #\uFFFD)
   ;; this does not work.
   ;;(char-in #\u10000 #\uEFFF9)
   ))

(define name-char/c
  (or/c
   name-start-char/c
   (λ (val) (or (char=? val #\-) (char=? val #\.) (char=? val #\u00B7)))
   (char-in #\0 #\9)
   (char-in #\u0300 #\u036F)
   (char-in #\u203F #\u2040)))

(define (qname? val)
  (let ((split (string-split val ":")))
    (or (and (= (length split) 1)
            (or (string-prefix? val ":") (string-suffix? val ":"))
            (ncname? (car split)))
        (and (= (length split) 2)
             (andmap ncname? split)))))

;; -------------------------------------------------------------------------------------------------
;; Additional URL predicate
;; -------------------------------------------------------------------------------------------------

(define (url-absolute? url)
  (and (url? url)
       (non-empty-string? (url-scheme url))
       (non-empty-string? (url-host url))
       (let ((host (url-host url)))
         (if (non-empty-string? host)
             (let ((host-parts (string-split host ".")))
               (and (>= (length host-parts) 2)
                    (for/and ((part host-parts))
                      (non-empty-string? part))))
             #f))
       (or (empty? (url-path url))
           (url-path-absolute? url))))

;; -------------------------------------------------------------------------------------------------
;; `namespace` struct type
;; -------------------------------------------------------------------------------------------------

(struct namespace (url prefix)
  #:sealed
  #:constructor-name internal-make-namespace
  #:guard (struct-guard/c url-absolute? ncname?))

(define (make-namespace url prefix)
  (let ((url (cond
               ((url? url) url)
               ((string? url) (string->url url)))))
    (internal-make-namespace url prefix)))

(define (namespace-make-url ns name)
  (when (and (namespace? ns) (ncname? name))
    (combine-url/relative (namespace-url ns) name)))

(define (namespace-make-qname ns name)
  (when (and (namespace? ns) (ncname? name))
    (string-append (namespace-prefix ns) ":" name)))

(define (namespace-make-default-qname ns)
  (when (namespace? ns)
      (string-append (namespace-prefix ns) ":")))

;; -------------------------------------------------------------------------------------------------
;; `namespace` constants
;; -------------------------------------------------------------------------------------------------

(define *namespace* (make-namespace "http://www.w3.org/2000/xmlns/" "xmlns"))

;; -------------------------------------------------------------------------------------------------
;; `name` struct type
;; -------------------------------------------------------------------------------------------------

(struct name (namespace name)
  #:sealed
  #:constructor-name make-name
  #:guard (struct-guard/c namespace? ncname?))

(define (name->url obj)
  (when (name? obj)
    (namespace-make-url (name-namespace obj) (name-name obj))))

(define (name->qname obj)
  (when (name? obj)
    (namespace-make-qname (name-namespace obj) (name-name obj))))

;; -------------------------------------------------------------------------------------------------
;; `name` constants
;; -------------------------------------------------------------------------------------------------

(define *base* (make-name *namespace* "base"))
