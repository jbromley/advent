#lang racket
(require racket/generic)
(require racket/port)

(define-generics gate
  (name gate)
  (outs gate)
  (handle-signal gate signal))

(struct wire [name state outputs] #:mutable #:transparent
  #:methods gen:gate
  [(define (name gate) (wire-name gate))
   (define (outs gate) (wire-outputs gate))
   (define (handle-signal gate signal)
     )])




