#lang racket/base

(require racket/bool
         racket/contract
         racket/list
         racket/string
         ;; --------------------------------------
         net/url-string
         net/url-structs
         ;; --------------------------------------
         "./name.rkt")

(provide (contract-out
          (url-absolute? (-> any/c boolean?))
          (namespace-url? (-> any/c boolean?))
          ;; --------------------------------------
          (namespace? (-> any/c boolean?))
          (url->namespace (-> namespace-url? namespace?))
          (string->namespace (-> string? namespace?))
          (namespace->url (-> namespace? url?))
          (namespace->string (-> namespace? string?))
          (namespace+name->url (-> namespace?
                                   (or/c local-name-string? local-name?)
                                   url-absolute?))
          ;; --------------------------------------
          (struct nsname ((namespace namespace?)
                          (name local-name?)))
          (make-nsname
           (-> (or/c string? namespace-url? namespace?)
               (or/c local-name-string? local-name?)
               nsname?))
          (nsname->url (-> nsname? url-absolute?))
          (nsname->string (-> nsname? string?))
          (nsname-make-nsname (-> nsname? (or/c local-name-string? local-name?) nsname?))
          (url->namespace+name (-> url-absolute? (or/c (cons/c url-absolute? string?) #f)))
          (url->nsname (-> url-absolute? nsname?))))

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

(define (namespace-url? url)
  (and (url-absolute? url)
       (or (let ((fragment (url-fragment url)))
             (and (string? fragment) (= (string-length fragment) 0)))
           (let ((path (url-path url)))
             (and (list? path)
                  (not (empty? path))
                  (string=? (path/param-path (last path)) ""))))))

;; -------------------------------------------------------------------------------------------------
;; `namespace` struct type
;; -------------------------------------------------------------------------------------------------

(struct namespace (url)
  #:transparent
  #:constructor-name url->namespace
  #:guard (struct-guard/c namespace-url?))

(define (string->namespace v)
  (url->namespace (string->url v)))

(define (namespace->url ns)
  (namespace-url ns))

(define (namespace->string ns)
  (url->string (namespace->url ns)))

(define (namespace+name->url ns name)
  (string->url (string-append (url->string (namespace-url ns))
                              (if (local-name? name) (local-name->string name) name))))

;; -------------------------------------------------------------------------------------------------
;; Namespaced-names
;; -------------------------------------------------------------------------------------------------

(struct nsname (namespace name)
  #:transparent
  #:guard (struct-guard/c namespace? local-name?))

(define (make-nsname ns name)
  (nsname (cond
            ((string? ns) (string->namespace ns))
            ((url? ns) (url->namespace ns))
            (else ns))
          (if (local-name? name) name (string->local-name name))))

(define (nsname->url ns)
  (namespace+name->url (nsname-namespace ns) (nsname-name ns)))

(define (nsname->string ns)
  (url->string (nsname->url ns)))

(define (nsname-make-nsname from name)
  (nsname (nsname-namespace from)
          (if (local-name? name) name (string->local-name name))))

(define (url->namespace+name url)
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
                                                    "")
                                          fragment))
      ((and (non-empty-string? last-segment)
            (false? fragment)) (cons (make-url (url-scheme url)
                                               (url-user url)
                                               (url-host url)
                                               (url-port url)
                                               (url-path-absolute? url)
                                               (append
                                                (take (url-path url) (- (length path) 1))
                                                (list (path/param "" '())))
                                               (url-query url)
                                               (url-fragment url))
                                     last-segment))
      (else #f))))

(define (url->nsname url)
  (let ((namespace+name (url->namespace+name url)))
   (make-nsname (car namespace+name) (cdr namespace+name))))
