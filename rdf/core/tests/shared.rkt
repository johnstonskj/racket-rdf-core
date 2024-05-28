#lang racket/base

(provide test-bnode-label-maker)

(define (test-bnode-label-maker)
  (let ((value 1))
    (λ () (let ((result value))
            (set! value (+ value 1))
            (format "test-~a" result)))))
