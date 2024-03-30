#lang racket/base

(require racket/bool
         racket/contract
         racket/string)

(provide language-tag?)

(define rx/has-named-groups (make-parameter #f boolean? 'has-named-groups))

(define group-repeat? (or/c 'one 'optional 'zero-or-more 'one-or-more))

(define (rx/or exprs)
  (string-join exprs "|"))

(define (rx/and exprs)
  (string-join exprs ""))

(define (rx/optional expr)
  (string-append expr "?"))

(define (rx/zero-or-more expr)
  (string-append expr "*"))

(define (rx/one-or-more expr)
  (string-append expr "+"))

(define (rx/group expr #:named (named #f) #:repeat (repeat 'one))
  (let ((name-str (if (and (rx/has-named-groups) named) (format "?<~a>" named) ""))
        (repeat-str (cond
                      ((symbol=? repeat 'optional) "?")
                      ((symbol=? repeat 'zero-or-more) "*")
                      ((symbol=? repeat 'one-or-more) "+")
                      (else ""))))
    (string-append "(" name-str expr ")" repeat-str)))

(define (rx/or-group exprs #:named (named #f) #:repeat (repeat 'one))
  (rx/group (rx/or exprs) #:named named #:repeat repeat))

(define (rx/and-group exprs #:named (named #f) #:repeat (repeat 'one))
  (rx/group (rx/and exprs) #:named named #:repeat repeat))

(define (rx/string-prefix expr)
  (string-append "^" expr))

(define (rx/string-suffix expr)
  (string-append expr "$"))

(define (rx/string-exactly expr)
  (rx/string-suffix (rx/string-prefix expr)))

(define bcp47-grandfathered-group-1
  (rx/or-group '("en-GB-oed" "i-ami" "i-bnn" "i-default" "i-enochian" "i-hak" "i-klingon" "i-lux" "i-mingo" "i-navajo" "i-pwn" "i-tao" "i-tay" "i-tsu" "sgn-BE-FR" "sgn-BE-NL" "sgn-CH-DE")))

(define bcp47-grandfathered-group-2
  (rx/or-group '("art-lojban" "cel-gaulish" "no-bok" "no-nyn" "zh-guoyu" "zh-hakka" "zh-min" "zh-min-nan" "zh-xiang")))

(define bcp47-language
  (rx/or-group ;;      v-- extlang
   '("([A-Za-z]{2,3}(-([A-Za-z]{3}(-[A-Za-z]{3}){0,2}))?)"
     "[A-Za-z]{4}"
     "[A-Za-z]{5,8}")
   #:named "language"))

(define bcp47-script
  (rx/group "-([A-Za-z]{4})" #:named "script" #:repeat 'optional))

(define bcp47-region
  (rx/group "-([A-Za-z]{2}|[0-9]{3})" #:named "region" #:repeat 'optional))

(define bcp47-variant
  (rx/group "-([A-Za-z0-9]{5,8}|[0-9][A-Za-z0-9]{3})" #:named "variant" #:repeat 'zero-or-more))

(define bcp47-extension
  (rx/group "-([0-9A-WY-Za-wy-z](-[A-Za-z0-9]{2,8})+)" #:named "extension" #:repeat 'zero-or-more))

(define bcp47-private-use
  (rx/group "-(x(-[A-Za-z0-9]{1,8})+)" #:named "privateUse" #:repeat 'optional))

(define bcp47-private-use-1
  (rx/group "x(-[A-Za-z0-9]{1,8})+"  #:named "privateUse1"))

(define bcp47-pattern
  (pregexp
   (rx/string-exactly
    (rx/or-group
     (list
      (rx/or-group
       (list
        bcp47-grandfathered-group-1
        bcp47-grandfathered-group-2)
       #:named "grandfathered")
      (rx/and-group
       (list
        bcp47-language
        bcp47-script
        bcp47-region
        bcp47-variant
        bcp47-extension
        bcp47-private-use))
      bcp47-private-use-1)))))

(define/contract (language-tag? val)
  (-> string? boolean?)
  (cond
    ((symbol? val) (language-tag? (symbol->string val)))
    ((string? val) (not (false? (regexp-match bcp47-pattern val))))
    (else #f)))
