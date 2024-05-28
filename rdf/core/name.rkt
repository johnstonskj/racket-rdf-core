#lang racket/base

(require racket/contract
         ;; --------------------------------------
         (only-in "./private/sparql-names.rkt"
                  local-name-string?))

(provide (contract-out
          (local-name-string? (-> any/c boolean?))
          (local-name? (-> any/c boolean?))
          (string->local-name (-> local-name-string? local-name?))
          (rename local-name-str local-name->string
                  (-> local-name? local-name-string?))))

(struct local-name (str)
  #:transparent
  #:constructor-name string->local-name
  #:guard (struct-guard/c local-name-string?))
