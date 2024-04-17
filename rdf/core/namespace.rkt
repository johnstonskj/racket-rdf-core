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
         url-namespace-safe?
         ensure-namespace-url-safety
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
         url->namespace+name
         name->url
         name->qname)

;; -------------------------------------------------------------------------------------------------
;; `ncname` contracts
;; -------------------------------------------------------------------------------------------------

(define/contract (ncname? val)
  (-> any/c boolean?)
  (cond
    ((symbol? val)
     (ncname? (symbol->string val)))
    ((non-empty-string? val)
     (let ((char-list (string->list val)))
       (and (name-start-char/c (car char-list))
            (andmap name-char/c (cdr char-list)))))
    (else #f)))

(define/contract (string->ncname str)
  (-> string? (or/c string? #f))
  (if (ncname?) str #f))

(define/contract (symbol->ncname sym)
  (-> symbol? (or/c string? #f))
  (string->ncname (symbol->string sym)))

(define/contract (qname? val)
  (-> any/c boolean?)
  (cond
    ((symbol? val) (qname? (symbol->string val)))
    ((string? val)
     (let ((split (string-split val *qname-separator*)))
       (or (and (= (length split) 1)
                (or (string-prefix? val *qname-separator*) (string-suffix? val *qname-separator*))
                (ncname? (car split)))
           (and (= (length split) 2)
                (andmap ncname? split)))))
    (else #f)))

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

(define *qname-separator* ":")

;; from https://www.w3.org/TR/2014/REC-turtle-20140225/Overview.html#sec-iri:
;;
;; NOTE
;; Prefixed names are a superset of XML QNames. They differ in that the local part of prefixed names may include:
;;
;; leading digits, e.g. leg:3032571 or isbn13:9780136019701
;; non leading colons, e.g. og:video:height
;; reserved character escape sequences, e.g. wgs:lat\-long

;; see https://www.w3.org/TR/2014/REC-turtle-20140225/Overview.html#sec-grammar-grammar
;; see https://www.w3.org/TR/sparql11-query/#sparqlGrammar
;; see https://www.w3.org/TR/2014/REC-rdf-syntax-grammar-20140225/Overview.html#section-Serialising

;; -------------------------------------------------------------------------------------------------
;; Additional URL predicate
;; -------------------------------------------------------------------------------------------------

(define/contract (url-absolute? url)
  (-> any/c boolean?)
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

(define/contract (url-namespace-safe? url)
  (-> any/c boolean?)
  (and (url-absolute? url)
       (or (let ((fragment (url-fragment url)))
             (and (string? fragment) (= (string-length fragment) 0)))
           (let ((path (url-path url)))
             (and (list? path) (not (empty? path)) (string=? (path/param-path (last path)) ""))))))

(define (boolean-guard v) (if (boolean? v) v (error)))

(define ensure-namespace-url-safety
  (make-parameter #t boolean-guard 'ensure-namespace-url-safety))

;; -------------------------------------------------------------------------------------------------
;; `namespace` struct type
;; -------------------------------------------------------------------------------------------------

(struct namespace (url prefix)
  #:sealed
  #:transparent
  #:constructor-name internal-make-namespace
  #:guard (struct-guard/c url-absolute? ncname?))

(define/contract (make-namespace url prefix)
  (-> (or/c url-absolute? string?) ncname? namespace?)
  (let* ((url (if (string? url) (string->url url) url)))
    (when (and (ensure-namespace-url-safety)
               (not (url-namespace-safe? url)))
      (raise-argument-error 'make-namespace "url-namespace-safe?" (url->string url)))
    (internal-make-namespace url prefix)))

(define/contract (namespace-make-url ns name)
  (-> namespace? ncname? url-absolute?)
  (when (and (namespace? ns) (ncname? name))
    (string->url (string-append (url->string (namespace-url ns)) name))))

(define/contract (namespace-make-qname ns name)
  (-> namespace? ncname? qname?)
  (when (and (namespace? ns) (ncname? name))
    (string-append (namespace-prefix ns) *qname-separator* name)))

(define/contract (namespace-make-default-qname ns)
  (-> namespace? qname?)
  (when (namespace? ns)
      (string-append (namespace-prefix ns) *qname-separator*)))

;; -------------------------------------------------------------------------------------------------
;; TODO `define-namespace` macro
;; -------------------------------------------------------------------------------------------------

;;(define-syntax (define-namespace stx)
;;  (define (make-qname name)
;;      (with-syntax ([qname (format-id #'name "~a:" (syntax-e #'name))])
;;        #'qname))
;;  (syntax-case stx ()
;;    ((_ url prefix)
;;     (with-syntax ((prefix-name (format-id #'prefix "~a:" (syntax-e #'prefix))))
;;       #'(define prefix-name (make-namespace url (quote prefix)))))
;;    ((_ url prefix name ...)
;;     (with-syntax ((prefix-name (format-id #'prefix "~a:" (syntax-e #'prefix)))
;;                   ((qname ...) (map make-qname (list #'(name ...)))))
;;       #'(begin
;;           (provide prefix-name qname ...)
;;           (define prefix-name (make-namespace url (quote prefix)))
;;           (define qname (make-name prefix-name name)) ...)))
;;    ))

;; -------------------------------------------------------------------------------------------------
;; `name` struct type
;; -------------------------------------------------------------------------------------------------

(struct name (namespace name)
  #:sealed
  #:transparent
  #:constructor-name make-name
  #:guard (struct-guard/c namespace? ncname?))

(define/contract (url->namespace+name url)
  (-> url-absolute? (or/c (cons/c url-absolute? string?) #f))
  (let* ((fragment (url-fragment url))
         (path (map (λ (segment) (path/param-path segment))
                    (url-path url)))
         (last-segment (if (empty? path) #f (last path))))
    (cond
      ((non-empty-string? fragment) (cons (make-url (url-scheme url)
                                                    (url-user url)
                                                    (url-host url)
                                                    (url-port url)
                                                    (url-path-absolute? url)
                                                    (url-path url)
                                                    (url-query url)
                                                    #f)
                                          fragment))
      ((and (non-empty-string? last-segment)
            (false? fragment)) (cons (make-url (url-scheme url)
                                               (url-user url)
                                               (url-host url)
                                               (url-port url)
                                               (url-path-absolute? url)
                                               (take (url-path url) (- (length path) 1))
                                               (url-query url)
                                               (url-fragment url))
                                     last-segment))
      (else #f))))

(define/contract (name->url obj)
  (-> name? url?)
  (namespace-make-url (name-namespace obj) (name-name obj)))

(define/contract (name->qname obj)
  (-> name? qname?)
  (namespace-make-qname (name-namespace obj) (name-name obj)))
