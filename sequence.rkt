#lang racket

(require "stream.rkt")

(provide in-do sequence-take in-consecutive in-consecutive* in-values)

(define-sequence-syntax in-do
  (lambda (stx)
    (syntax-case stx ()
      [(_ d init-expr step-expr)
       #'(in-do d init-expr step-expr #f)]
      [(_ d init-expr step-expr stop-expr)
       #'(make-do-sequence
          (λ ()
            (values
             (λ (pos) pos)
             (λ (d) step-expr)
             init-expr
             (λ (d) (not stop-expr))
             #f
             #f)))]))
  (lambda (stx)
    (syntax-case stx ()
      [[(d) (_ init-expr step-expr)]
       #'[(d) (in-do init-expr step-expr #f)]]
      [[(d) (_ init-expr step-expr stop-expr)]
       #'[(d) (:do-in
               ()
               #t
               ([lv init-expr])
               #t
               ([(d) lv])
               (not stop-expr)
               #t
               (step-expr))]])))

(define (sequence-take cnt s)
  (if (stream? s)
      (stream-take cnt s)
      (make-do-sequence
       (lambda ()
         (values (λ (v+n+i) (apply values (car v+n+i)))
                 (λ (v+n+i)
                   (let-values ([(vals next i) (apply values v+n+i)])
                     (if (>= i cnt)
                         (list #f next (add1 i))
                         (let-values ([(vals next) (next)])
                           (list vals next (add1 i))))))
                 (if (= cnt 0)
                     (list #f #f 1)
                     (let-values ([(vals next) (sequence-generate* s)])
                       (list vals next 1)))
                 car
                 #f
                 #f)))))

(define (in-consecutive seq)
  (define-values (more? get) (sequence-generate (in-values-sequence seq)))
  (define v #f)
  (make-do-sequence
   (λ ()
     (values
      (λ (_)
        (let ([nv (get)])
          (begin0
              (apply values (append v nv))
            (set! v nv))))
      values
      (when (more?) (set! v (get)))
      (λ (_) (more?))
      #f
      #f))))

(define (in-consecutive* n m seq)
  (define-values (more? get) (sequence-generate (in-values-sequence seq)))
  (define (next k)
    (for/list ([i (in-range k)] #:when (more?))
      (get)))
  (if (= m 1)
      (make-do-sequence
       (λ ()
         (define vs null)
         (values
          (λ (_)
            (let* ([nv (get)]
                   [avs (append vs (list nv))])
              (set! vs (cdr avs))
              (apply values (apply append avs))))
          values
          (set! vs (next (sub1 n)))
          (λ (_) (more?))
          #f
          #f)))
      (make-do-sequence
       (λ ()
         (values
          (λ (vals)
            (apply values (apply append vals)))
          (λ (vals)
            (append (drop vals m) (next m)))
          (next n)
          (λ (vals) (= (length vals) n))
          #f
          #f)))))

(define (in-values seq)
  (define-values (more? get) (sequence-generate seq))
  (make-do-sequence
   (λ ()
     (values
      (λ (_)
        (apply values (get)))
      values
      #f
      (λ (_) (more?))
      #f
      #f))))
