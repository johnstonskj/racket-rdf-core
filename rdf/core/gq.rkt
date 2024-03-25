#lang racket/base

(require racket/bool
         racket/contract
         racket/list
         "./namespace.rkt"
         "./statement.rkt"
         "./graph.rkt")

(provide pattern-component?
         ignore
         ignore?
         comparitor
         comparitor?
         variable
         variable?
         ;; --------------------------------------
         statement-pattern?
         statement-pattern-match
         ;; --------------------------------------
         result-variable-value?
         result-statement?
         results?
         graph-query)

;; -------------------------------------------------------------------------------------------------
;; Component-level Patterns
;; -------------------------------------------------------------------------------------------------

(define pattern-component?
  (or/c
   false?
   (cons/c object? procedure?)
   ncname?))

(define (ignore)
  #f)

(define (ignore? val)
  (false? val))

(define (comparitor value (operator equal?))
  (when (and (object? value) (procedure? operator))
    (cons value operator)))

(define (comparitor? val)
  (pair? val))

(define (variable name)
  (when (ncname? name) name))

(define (variable? val)
  (ncname? val))

(define (component-match pattern value)
  (cond
    ((ignore? pattern)
     #t)
    ((comparitor? pattern)
     (apply (cdr pattern) (list value (car pattern))))
    ((variable? pattern)
     (cons pattern value))))

;; -------------------------------------------------------------------------------------------------
;; Statement-level Patterns
;; -------------------------------------------------------------------------------------------------

(define statement-pattern? (list/c pattern-component? pattern-component? pattern-component?))

(define (statement-pattern-match pattern statement)
  (let ((subject (component-match (first pattern) (statement-subject statement)))
        (predicate (component-match (second pattern) (statement-predicate statement)))
        (object (component-match (third pattern) (statement-object statement))))
    (if (and subject predicate object)
        (filter cons? (list subject predicate object))
        #false)))

;; -------------------------------------------------------------------------------------------------
;; Query and Results
;; -------------------------------------------------------------------------------------------------

(define result-variable-value? (cons/c string? object?))
(define result-statement? (listof result-variable-value?))
(define results? (listof result-statement?))

(define (graph-query graph pattern)
  (filter-map
   (λ (stmt) (statement-pattern-match pattern stmt))
   (graph-statements graph)))
