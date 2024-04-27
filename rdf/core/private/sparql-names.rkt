#lang racket/base

(require racket/bool
         racket/contract
         racket/function
         racket/list
         racket/string
         ;; --------------------------------------
         "./strings.rkt")

(provide prefixed-name-separator
         ;; --------------------------------------
         (contract-out
          (prefix-string? (-> any/c boolean?))
          (prefix-name-string? (-> any/c boolean?))
          (local-name-string? (-> any/c boolean?))
          (variable-string? (-> any/c boolean?))
          (variable-name-string? (-> any/c boolean?))
          (prefixed-name-string? (-> any/c boolean?))
          (prefixed-name-split
           (-> prefixed-name-string?
               (or/c (cons/c prefix-string? (or/c local-name-string? #f)) #f)))))

;; -------------------------------------------------------------------------------------------------

(define prefixed-name-separator #\:)

(define (prefix-string? v)
  (and (non-empty-string? v) (pname-ns? v)))

(define (prefix-name-string? v)
  (and (non-empty-string? v) (pn-prefix? v)))

(define (local-name-string? v)
  (and (non-empty-string? v) (pn-local? v)))

(define (variable-string? v)
  (and (non-empty-string? v) (var? v)))

(define (variable-name-string? v)
  (and (non-empty-string? v) (varname? v)))

(define (prefixed-name-string? v)
  (and (non-empty-string? v) (prefixed-name? v)))

;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------

(define pn-chars-base?
  ;; [164]  PN_CHARS_BASE   ::= [A-Z] | [a-z] | [#x00C0-#x00D6]
  ;;                          | [#x00D8-#x00F6] | [#x00F8-#x02FF]
  ;;                          | [#x0370-#x037D] | [#x037F-#x1FFF]
  ;;                          | [#x200C-#x200D] | [#x2070-#x218F]
  ;;                          | [#x2C00-#x2FEF] | [#x3001-#xD7FF]
  ;;                          | [#xF900-#xFDCF] | [#xFDF0-#xFFFD]
  ;;                          | [#x10000-#xEFFFF]
  (curry
   char-between-any?
   `(,range-ascii-upper
     ,range-ascii-lower
     (#\U00C0 . #\U00D6)
     (#\U00D8 . #\U00F6)
     (#\U00F8 . #\U02FF)
     (#\U0370 . #\U037D)
     (#\U037F . #\U1FFF)
     (#\U200C . #\U200D)
     (#\U2070 . #\U218F)
     (#\U2C00 . #\U2FEF)
     (#\U3001 . #\UD7FF)
     (#\UF900 . #\UFDCF)
     (#\UFDF0 . #\UFFFD)
     (#\U10000 . #\UEFFFF))))

(define (pn-chars-u? c)
  ;; [165]  PN_CHARS_U      ::= PN_CHARS_BASE | '_'
  (or (char=? c #\_) (pn-chars-base? c)))

(define (pn-chars? c)
  ;; [167]  PN_CHARS        ::= PN_CHARS_U | '-' | [0-9] | #x00B7
  ;;                          | [#x0300-#x036F] | [#x203F-#x2040]
  (or (char=? c #\-)
      (char=? c #\U00B7)
      (pn-chars-base? c)
      (char-between-any?
       `(,range-ascii-digit
         (#\U0300 . #\U036F)
         (#\U203F . #\U2040))
       c)))

(define hex?
  ;; [172]  HEX             ::= [0-9] | [A-F] | [a-f]
  (curry char-between-any?
         `(,range-ascii-digit
           (#\A . #\F)
           (#\a . #\f))))

(define (percent? v)
  ;; [171]  PERCENT         ::= '%' HEX HEX
  (and (= (string-length v) 3)
       (string-char=? v 0 #\%)
       (string-andmap hex? (string-rest v))))

(define pn-local-escaped
  '(#\_ #\~ #\. #\- #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\= #\/ #\? #\# #\@ #\%))

(define (pn-local-esc? v)
  ;; [173]  PN_LOCAL_ESC    ::= '\' ( '_' | '~' | '.' | '-' | '!' | '$'
  ;;                                | '&' | "'" | '(' | ')' | '*' | '+'
  ;;                                | ',' | ';' | '=' | '/' | '?' | '#'
  ;;                                | '@' | '%' )
  (and (= (string-length v) 2)
       (string-char=? v 0 #\\)
       (not (false? (member (string-ref v 1) pn-local-escaped)))))

(define (plx? v)
  ;; [170]  PLX             ::= PERCENT | PN_LOCAL_ESC
  (or (percent? v)
      (pn-local-esc? v)))

(define pn-prefix?
  ;; [168]  PN_PREFIX       ::= PN_CHARS_BASE
  ;;                            ( ( PN_CHARS | '.' )* PN_CHARS )?
  (curry string-form?
         pn-chars-base?
         (λ (c) (or (char=? c #\.) (pn-chars? c)))
         pn-chars?))

(define (pn-local/one? c v pred?)
  (cond
    ((char=? c #\\) (if (pn-local-esc? (substring v 0 2)) 2 #f))
    ((char=? c #\%) (if (percent? (substring v 0 3)) 3 #f))
    (else (if (pred? c) 1 #f))))

;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------

(define var1-prefix "?")
;; [143]  	VAR1	  ::=  	'?' VARNAME

(define var2-prefix "$")
;; [144]  	VAR2	  ::=  	'$' VARNAME

(define (varname? v)
  ;; [166]  	VARNAME	  ::=  	( PN_CHARS_U | [0-9] )
  ;;                            ( PN_CHARS_U | [0-9]
  ;;                            | #x00B7 | [#x0300-#x036F]
  ;;                            | [#x203F-#x2040] )*
  (and (string? v)
       (> (string-length v) 0)
       (and (let ((first (string-first v)))
                (or (pn-chars-u? first) (char-between? #\0 #\9 first)))
            (andmap (λ (c) (or (pn-chars-u? c)
                               (char-between? #\0 #\9 c)
                               (char=? #\U00B7 c)
                               (char-between-any? '((#\U0300 . #\U036F) (#\U203F . #\U2040)) c)))
                    (string->list (string-rest v))))))

(define (var? v)
  ;; [108]  	Var	  ::=  	VAR1 | VAR2
  (and (or (string-prefix? v var1-prefix)
           (string-prefix? v var2-prefix))
       (varname? (substring v 1))))

;; -------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------

(define (pn-local? v)
  ;; [169]  PN_LOCAL        ::= ( PN_CHARS_U | ':' | [0-9] | PLX )
  ;;                            ( ( PN_CHARS | '.' | ':' | PLX )*
  ;;                              ( PN_CHARS | ':' | PLX ) )?
  (let* ((first? (λ (c) (or (char=? c #\:)
                            (char-between? #\0 #\9 c)
                            (pn-chars-u? c))))
         (middle? (λ (c) (or (char=? c #\.)
                             (char=? c #\:)
                             (pn-chars? c))))
         (last? (λ (c)  (or (char=? c #\:)
                            (pn-chars-u? c))))
         (vlast (sub1 (string-length v))))
    (let loop ((i 0))
      (let ((step (pn-local/one? (string-ref v i)
                                 (substring v i)
                                 (cond
                                   ((= i 0) first?)
                                   ((= i vlast) last?)
                                   (else middle?)))))
        (if (and step (not (= i vlast)))
            (loop (+ i step))
            (= i vlast))))))

(define (pname-ns? v)
  ;; [140]  PNAME_NS        ::= PN_PREFIX? ':'
  (let ((vlen (string-length v)))
    (and (>= vlen 1)
         (char=? (string-last v) prefixed-name-separator)
         (or (= vlen 1)
             (pn-prefix? (substring v 0 (sub1 vlen)))))))

(define (prefixed-name-split pname)
  (let ((prefix+name (string-split-at pname prefixed-name-separator)))
    (if (and prefix+name (not (empty? prefix+name)))
        (let ((prefix (car prefix+name))
              (name (cdr prefix+name)))
          (cons
           prefix
           (if (non-empty-string? name) name #f)))
        #f)))

(define (prefixed-name? pname)
  ;; [137]  PrefixedName    ::= PNAME_LN | PNAME_NS
  ;;
  ;;                        ==> PNAME_NS PN_LOCAL?
  (let ((prefix+name (prefixed-name-split pname)))
    (and (pair? prefix+name)
         (let ((prefix (car prefix+name))
               (name (cdr prefix+name)))
           (and (pname-ns? prefix)
                (or (false? name) (pn-local? name)))))))

;; -------------------------------------------------------------------------------------------------
;; In-Module Tests
;; -------------------------------------------------------------------------------------------------

(module+ test

  (require rackunit
           rackunit/text-ui)

  ;; -----------------------------------------------------------------------------------------------
  ;; Test Suite(s)
  ;; -----------------------------------------------------------------------------------------------

  (define prefixed-name-test-suite
    (test-suite
     "Test prefixed-name"

     (test-case
         "function pname-ns?"
       (check-equal? (pname-ns? "") #f)
       (check-equal? (pname-ns? "abc") #f)
       (check-equal? (pname-ns? ":abc") #f)
       (check-equal? (pname-ns? "a:bc") #f)
       (check-equal? (pname-ns? ":") #t)
       (check-equal? (pname-ns? "abc:") #t))

     (test-case
         "function prefixed-name-split"
       (check-equal? (prefixed-name-split "") #f)
       (check-equal? (prefixed-name-split ":") '(":" . #f))
       (check-equal? (prefixed-name-split "rdf:") '("rdf:" . #f))
       (check-equal? (prefixed-name-split ":Resource") '(":" . "Resource"))
       (check-equal? (prefixed-name-split "rdf:Resource") '("rdf:" . "Resource")))

     (test-case
         "function prefixed-name?"
       (check-equal? (prefixed-name-string? "") #f)
       (check-equal? (prefixed-name-string? ":") #t)
       (check-equal? (prefixed-name-string? "rdf:") #t)
       (check-equal? (prefixed-name-string? ":Resource") #t)
       (check-equal? (prefixed-name-string? "rdf:Resource") #t))

     ))

  ;; -----------------------------------------------------------------------------------------------
  ;; Test Runner
  ;; -----------------------------------------------------------------------------------------------

  (run-tests prefixed-name-test-suite))
