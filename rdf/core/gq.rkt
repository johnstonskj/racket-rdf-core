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

(define/contract pattern-component?
  (-> any/c boolean?)
  (or/c
   false?
   (cons/c object? procedure?)
   ncname?))

(define/contract (ignore)
  (-> pattern-component?)
  #f)

(define/contract (ignore? val)
  (-> pattern-component? boolean?)
  (false? val))

(define (comparitor value (operator equal?))
  (when (and (object? value) (procedure? operator))
    (cons value operator)))

(define/contract (comparitor? val)
  (-> pattern-component? boolean?)
  (pair? val))

(define/contract (variable name)
  (-> ncname? pattern-component?)
  name)

(define/contract (variable? val)
  (-> pattern-component? boolean?)
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

(define/contract (make-statement-pattern subject predicate object)
  (-> pattern-component? pattern-component? pattern-component? statement-pattern?)
  (list subject predicate object))

;; -------------------------------------------------------------------------------------------------
;; Query and Results
;; -------------------------------------------------------------------------------------------------

(define result-variable-value? (cons/c string? object?))
(define result-statement? (listof result-variable-value?))
(define results? (listof result-statement?))

(define (statement-pattern-match pattern statement)
  (let ((subject (component-match (first pattern) (get-subject statement)))
        (predicate (component-match (second pattern) (get-predicate statement)))
        (object (component-match (third pattern) (get-object statement))))
    (if (and subject predicate object)
        (filter cons? (list subject predicate object))
        #false)))

(define (graph-query graph pattern)
  (filter-map
   (λ (stmt) (statement-pattern-match pattern stmt))
   (graph-statements graph)))
