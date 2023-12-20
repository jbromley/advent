#lang racket/base

;;; Network and node structures

(struct flip-flop [state outs] #:transparent)
(struct conjunction [state ins outs] #:transparent)
(struct broadcaster [outs] #:transparent)
(struct network [conns nodes] #:transparent)

(define (make-flip-flop [outputs '()])
  (flip-flop #f outputs))

(define (make-conjunction [ins '()] [outs '()])
  (conjunction (build-list (length ins) #f)
               (for/hash ([in ins]) (values in #f))
               outs))

(define (make-broadcaster [outs '()])
  (broadcaster outs))

(define (make-network)
  (network (hash "button" "broadcaster") (hash "broadcaster" (make-broadcaster))))

;;; Read and parse the input file
  
(define (read-input input-file)
  (call-with-input-file input-file
    (lambda (port)
      (for/list ([line (in-lines port)])
        (string->list line)))))