#lang racket/base

(require racket/bool
         racket/contract
         racket/function
         racket/list
         racket/set
         racket/stream
         ;; --------------------------------------
         "./statement.rkt"
         "./graph.rkt"
         ;; --------------------------------------
         (only-in
          "./private/sparql-names.rkt"
          variable-string?
          variable-name-string?))

(provide (contract-out (result-cell? contract?)
                       (result-row? contract?)
                       (results? contract?)
                       ;; --------------------------------------
                       (struct comparitor ((fn (-> any/c any/c boolean?))
                                           (value object?)))
                       (comparitor-match? (-> comparitor? object? boolean?))
                       ;; --------------------------------------
                       (component-pattern-kind/c flat-contract?)
                       (struct component-pattern ((kind component-pattern-kind/c)
                                                  (inner (or/c false?
                                                               comparitor?
                                                               variable-name-string?)))
                         #:omit-constructor)
                       (ignore (-> component-pattern?))
                       (compare/equal? (-> object? component-pattern?))
                       (variable-string? (-> any/c boolean?))
                       (variable-name-string? (-> any/c boolean?))
                       (variable (-> variable-name-string? component-pattern?))
                       (component-pattern-match
                        (-> component-pattern? object? (or/c boolean? result-cell?)))
                       ;; --------------------------------------
                       (struct statement-pattern ((subject component-pattern?)
                                                  (predicate component-pattern?)
                                                  (object component-pattern?)))
                       (statement-pattern-match (-> statement-pattern?
                                                    statement?
                                                    (or/c result-row? #f)))
                       ;; --------------------------------------
                       (struct graph-pattern ((patterns (set/c statement-pattern?))))
                       (make-common-subject-pattern
                        (-> (set/c (list/c component-pattern? component-pattern?))
                            graph-pattern?))
                       (graph-pattern-query (-> graph-pattern? graph? results?))))

;; -------------------------------------------------------------------------------------------------
;; Result Types
;; -------------------------------------------------------------------------------------------------

(define result-cell? (cons/c variable-name-string? object?))

(define result-row? (listof result-cell?))

(define results? (stream/c result-row?))

;; -------------------------------------------------------------------------------------------------
;; Component-level Patterns
;; -------------------------------------------------------------------------------------------------

(struct comparitor (fn value)
  #:transparent
  #:guard (struct-guard/c (-> any/c any/c boolean?) object?))

(define (comparitor-match? comp object)
  (apply (comparitor-fn comp) (list object (comparitor-value comp))))

(define component-pattern-kind/c (or/c 'ignore 'compare 'variable))

(struct component-pattern (kind inner)
  #:transparent
  #:constructor-name make-cpattern
  #:guard (struct-guard/c
           component-pattern-kind/c
           (or/c false?
                 comparitor?
                 variable-name-string?)))

(define (ignore)
  (make-cpattern 'ignore #f))

(define (compare comp)
  (make-cpattern 'compare comp))

(define (compare/equal? value)
  (make-cpattern 'compare (comparitor equal? value)))

(define (variable name) (make-cpattern 'variable name))

(define (component-pattern-match pattern value)
  (let ((kind (component-pattern-kind pattern))
        (actual (component-pattern-inner pattern)))
    (cond
      ((eq? kind 'ignore) #t)
      ((eq? kind 'compare) (comparitor-match? actual value))
      ((eq? kind 'variable) (cons actual value)))))

;; -------------------------------------------------------------------------------------------------
;; Statement-level Patterns
;; -------------------------------------------------------------------------------------------------

(struct statement-pattern (subject predicate object)
  #:transparent
  #:guard (struct-guard/c component-pattern? component-pattern? component-pattern?))

(define (statement-pattern-match pattern statement)
  (let ((subject (component-pattern-match (statement-pattern-subject pattern)
                                          (get-subject statement)))
        (predicate (component-pattern-match (statement-pattern-predicate pattern)
                                            (get-predicate statement)))
        (object (component-pattern-match (statement-pattern-object pattern)
                                         (get-object statement))))
    (if (and subject predicate object)
        (filter cons? (list subject predicate object))
        #false)))

;; -------------------------------------------------------------------------------------------------
;; Graph Patterns
;; -------------------------------------------------------------------------------------------------

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

(define (random-variable (length 4))
  (append (list (vector-ref alnum-chars (random 0 52)))
          (map (lambda (_) (vector-ref alnum-chars (random 0 alnum-char-count))) (range length))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(struct graph-pattern (patterns)
  #:transparent
  #:guard (struct-guard/c (set/c statement-pattern?)))

(define (make-common-subject-pattern predicate-object-patterns)
  (let ((subject (random-variable)))
    (map (lambda (ptns) (statement-pattern subject (car ptns) (cadr ptns)))
         predicate-object-patterns)))

;; -------------------------------------------------------------------------------------------------
;; Query
;; -------------------------------------------------------------------------------------------------

(define (graph-pattern-query pattern graph)
  ;; TODO: this is wrong!!
  (let ((a-pattern (car (set->list (graph-pattern-patterns pattern)))))
    (stream-filter identity
                   (stream-map (λ (stmt) (statement-pattern-match a-pattern stmt))
                               (set->stream (graph-statements graph))))))
