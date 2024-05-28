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

(provide (contract-out (rename make-url
                               make-resource
                               (-> (or/c #f string?)
                                   (or/c #f string?)
                                   (or/c #f string?)
                                   (or/c #f exact-nonnegative-integer?)
                                   boolean?
                                   (listof path/param?)
                                   (listof (cons/c symbol? (or/c #f string?)))
                                   (or/c #f string?)
                                   resource?))
                       (rename url? resource?
                               (-> any/c boolean?))
                       (rename url-scheme resource-scheme
                               (-> resource? (or/c #f string?)))
                       (rename url-user resource-user
                               (-> resource? (or/c #f string?)))
                       (rename url-host resource-host
                               (-> resource? (or/c #f string?)))
                       (rename url-path-absolute? resource-path-absolute?
                               (-> resource? boolean?))
                       (rename url-path resource-path
                               (-> resource? (listof path/param?)))
                       (rename url-query resource-query
                               (-> resource? (listof (cons/c symbol? (or/c #f string?)))))
                       (rename url-fragment resource-fragment
                               (-> resource? (or/c #f string?)))
                       (rename string->url string->resource
                               (-> string? resource?))
                       (rename url->string resource->string
                               (-> resource? string?))
                       (rename combine-url/relative combine-resource/relative
                               (-> resource? string? resource?))
                       ;; --------------------------------------
                       (resource-absolute? (-> any/c boolean?))
                       (resource-maybe-namespace? (-> any/c boolean?))
                       (resource-maybe-nsname? (-> any/c boolean?))
                       (resource-name-only? (-> any/c boolean?))
                       (resource-empty? (-> any/c boolean?))
                       ;; --------------------------------------
                       (resource->namespace+name
                        (-> url? (values (or/c #f resource?) (or/c #f local-name-string?))))
                       (resource-namespace (-> resource? resource?))
                       (resource-name (-> resource? (or/c #f local-name-string?)))
                       ;; --------------------------------------
                       (resource-append-name
                        (-> resource-absolute? (or/c local-name? local-name-string?) resource?))))

;; -------------------------------------------------------------------------------------------------
;; Additional Predicates
;; -------------------------------------------------------------------------------------------------

(define resource? url?)

(define (resource-absolute? url)
  (and (url? url)
       (let* ((scheme (url-scheme url))
              (host (url-host url))
              (path (url-path url)))
         (and (non-empty-string? scheme)
              (if (non-empty-string? host)
                  (let ((host-parts (string-split host ".")))
                    (and (>= (length host-parts) 2)
                         (for/and ((part host-parts))
                           (non-empty-string? part))))
                  #f)
              (or (empty? path) (url-path-absolute? url))))))

(define (resource-maybe-namespace? url)
  (and (url? url)
       (let* ((path (url-path url))
              (absolute? (resource-absolute? url))
              (fragment (url-fragment url)))
         (and absolute?
              (or (and (string? fragment) (= (string-length fragment) 0))
                  (and (list? path)
                       (not (empty? path))
                       (string=? (path/param-path (last path)) "")))))))

(define (resource-maybe-nsname? url)
  (and (url? url)
       (let* ((path (url-path url))
              (path-parts (map (λ (segment) (path/param-path segment))
                               path))
              (last-segment (if (empty? path-parts) #f (last path-parts)))
              (fragment (url-fragment url)))
         (or (local-name-string? fragment)
             (and (local-name-string? last-segment)
                  (false? fragment))))))

(define (resource-name-only? url)
  (and (url? url)
       (let* ((scheme (url-scheme url))
              (user (url-user url))
              (host (url-host url))
              (port (url-port url))
              (query (url-query url))
              (path (url-path url))
              (fragment (url-fragment url)))
         (or (and (empty? path)
                  (empty? query)
                  (local-name-string? fragment)
                  (andmap not (list scheme user host port)))
             (and (empty? query)
                  (= (length path) 1)
                  (local-name-string? (path/param-path (first path)))
                  (andmap not (list scheme user host port fragment)))))))

(define (resource-empty? url)
  (and (url? url)
       (andmap empty?
               (list (url-path url)
                     (url-query url)))
       (andmap not
               (list (url-scheme url)
                     (url-user url)
                     (url-host url)
                     (url-port url)
                     (url-fragment url)))))

;; -------------------------------------------------------------------------------------------------
;; Additional Operations
;; -------------------------------------------------------------------------------------------------

(define (resource->namespace+name val)
  (cond
    ((resource-empty? val)
     (values #f #f))
    ((resource-maybe-namespace? val)
     (values val #f))
    ((resource-maybe-nsname? val)
     (let* ((fragment (url-fragment val))
            (path (map (λ (segment) (path/param-path segment))
                       (url-path val)))
            (last-segment (if (empty? path) #f (last path))))
       (cond
         ((non-empty-string? fragment)
          (values (struct-copy
                   url val
                   (fragment ""))
                  fragment))
         ((and (non-empty-string? last-segment)
               (false? fragment))
          (values (struct-copy
                   url val
                   (path (let ((short-path (take (url-path val) (- (length path) 1))))
                           (if (empty? short-path)
                               (list)
                               (append short-path
                                       (list (path/param "" '())))))))
                  last-segment))
         (else
          (values val #f)))))
    (else
     (values val #f))))

(define (resource-namespace val)
  (let-values (((namespace name) (resource->namespace+name val)))
    namespace))

(define (resource-name val)
  (let-values (((namespace name) (resource->namespace+name val)))
    name))

(define (resource-append-name base name)
  (string->url
   (string-append (url->string base)
                  (if (local-name? name)
                      (local-name->string name)
                      name))))
