#lang racket

(provide values-list applize)

(define-syntax-rule (values-list form)
  (call-with-values (λ () form) list))

(define ((applize func) . args)
  (apply func (apply list* args)))
