#lang racket/base

(require racket/bool
         racket/contract
         racket/list
         "./namespace.rkt"
         "./statement.rkt"
         "./graph.rkt")

(provide (except-out (struct-out match-op)
                     internal-make-match-op)
         ignore
         match-value
         return-variable
         ;; --------------------------------------
         (struct-out statement-pattern)
         make-statement-pattern
         statement-pattern-match
         ;; --------------------------------------
         graph-query)

(struct match-op (kind value pred)
  #:sealed
  #:constructor-name internal-make-match-op)

(define (ignore)
  (internal-make-match-op 'ignore #f #f))

(define (match-value value (pred equal?))
  (when (procedure? pred)
    (internal-make-match-op 'value value pred)))

(define (return-variable name)
  (when (ncname? name)
    (internal-make-match-op 'variable name #f)))

(define (match-op-match pattern value)
  (cond
    ((symbol=? (match-op-kind pattern) 'ignore)
     #t)
    ((symbol=? (match-op-kind pattern) 'value)
     (equal? (match-op-value pattern) value))
    ((symbol=? (match-op-kind pattern) 'variable)
     (cons (match-op-value pattern) value))))

(struct statement-pattern (subject predicate object)
  #:sealed
  #:constructor-name make-statement-pattern
  #:guard (struct-guard/c match-op? match-op? match-op?))

(define (statement-pattern-match pattern statement)
  (let ((subject (match-op-match (statement-pattern-subject pattern)
                                 (statement-subject statement)))
        (predicate (match-op-match (statement-pattern-predicate pattern)
                                   (statement-predicate statement)))
        (object (match-op-match (statement-pattern-object pattern)
                                (statement-object statement))))
    (if (and subject predicate object)
        (filter cons? (list subject predicate object))
        #false)))

(define (graph-query graph pattern)
  (filter-map
   (λ (stmt) (statement-pattern-match pattern stmt))
   (graph-statements graph)))
