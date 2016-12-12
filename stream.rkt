#lang racket

(provide stream-take stream-producer
         ->dedup-filter ->dedup-filter1)

(define (stream-take cnt s)
  (if (or (= cnt 0)
          (stream-empty? s))
      empty-stream
      (stream-cons (stream-first s)
                   (stream-take (sub1 cnt) (stream-rest s)))))

(define (-stream-producer -producer)
  (stream-cons (-producer)
               (-stream-producer -producer)))

(define (-stream-producer-stopf producer stop)
  (define (recursive)
    (call-with-values producer
                      (λ vals
                        (if (apply stop vals)
                            empty-stream
                            (stream-cons (values vals)
                                         (recursive))))))
  (recursive))

(define (-stream-producer-stopv producer stop)
  (define (recursive)
    (let ([val (producer)])
      (if (eq? stop val)
          empty-stream
          (stream-cons val
                       (recursive)))))
  (recursive))

(define stream-producer
  (case-lambda
    [(producer)
     (-stream-producer producer)]
    [(producer stop . args)
     (let ([func (if (empty? args)
                     producer
                     (λ () (apply producer args)))])
       ((if (procedure? stop)
            -stream-producer-stopf
            -stream-producer-stopv)
        func
        stop))]))

(define (->dedup-filter)
  (let ([s (mutable-set)])
    (λ args
      (if (set-member? s args)
          #f
          (begin
            (set-add! s args)
            #t)))))

(define (->dedup-filter1 [eq-mode 'eqv])
  (let* ([make-set
          (case eq-mode
            [(equal) mutable-set]
            [(eqv) mutable-seteqv]
            [(eq) mutable-seteq]
            [else (error '->dedup-filter1
                         "error eq-mode: ~a, must be 'eq, 'eqv, or 'equal"
                         eq-mode)])]
         [s (make-set)])
    (λ (v)
      (if (set-member? s v)
          #f
          (begin
            (set-add! s v)
            #t)))))
