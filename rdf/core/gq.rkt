#lang racket/base

(require racket/bool
         racket/contract
         racket/list
         "./statement.rkt"
         "./graph.rkt"
         "./private/sparql-names.rkt")

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
   variable-name-string?))

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
  (-> variable-name-string? pattern-component?)
  name)

(define (char-range start end)
  (map integer->char (range (char->integer start) (char->integer end))))

(define (char-inclusive-range start end)
  (map integer->char (inclusive-range (char->integer start) (char->integer end))))

(define alnum-chars (list->vector
                      (append (char-inclusive-range #\a #\z)
                              (char-inclusive-range #\A #\Z)
                              (char-inclusive-range #\0 #\1)
                              (list #\_))))
(define alnum-char-count (vector-length alnum-chars))

(define/contract (random-variable (length 4))
  (-> (integer-in 1 #f))
  (append (list (vector-ref alnum-chars (random 0 52)))
          (map (lambda (_) (vector-ref alnum-chars (random 0 alnum-char-count))) (range length))))

(define/contract (variable? val)
  (-> pattern-component? boolean?)
  (variable-name-string? val))

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
;; Graph Patterns
;; -------------------------------------------------------------------------------------------------

(define graph-pattern? (listof statement-pattern?))

(define (make-common-subject-pattern predicate-object-patterns)
  (-> (listof (list/c pattern-component? pattern-component?)) graph-pattern?)
  (let ((subject (random-variable)))
    (map (lambda (ptns) (make-statement-pattern subject (car ptns) (cadr ptns)))
         predicate-object-patterns)))

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
