#lang racket/base
(require racket/generic)
(require racket/match)
(require racket/port)
(require racket/string)

;;; Network and node structures

(define-generics node
  (name node)
  (outs node)
  (handle-pulse node pulse net))

(struct pulse [src level])

(struct flip-flop [name state outs] #:transparent
  #:methods gen:node
  [(define (name node) (flip-flop-name node))
   (define (outs node) (flip-flop-outs node))
   (define (handle-pulse node pulse net) (list node pulse))])

(struct conjunction [name state outs] #:transparent
  #:methods gen:node
  [(define (name node) (conjunction-name node))
   (define (outs node) (conjunction-outs node))
   (define (handle-pulse node pulse net) (list node pulse))])

(struct broadcaster [name outs] #:transparent
  #:methods gen:node
  [(define (name node) (broadcaster-name node))
   (define (outs node) (broadcaster-outs node))
   (define (handle-pulse node pulse net) (list node pulse))])

(define (make-flip-flop name [outs '()])
  (flip-flop name #f outs))

(define (make-conjunction name [outs '()])
  (conjunction name (hash) outs))

(define (conjunction-add-input conj in)
  (let ([ins (conjunction-state conj)])
    (struct-copy conjunction conj [state (hash-set ins in #f)])))

(define (make-broadcaster [outs '()])
  (broadcaster "broadcaster" outs))

(define (make-node type name outs)
  (match type
    [#\b (make-broadcaster outs)]
    [#\% (make-flip-flop name outs)]
    [#\& (make-conjunction name outs)]))

;;; Read and parse the input file

(define (read-input input-file)
  (let* ([lines (port->lines (open-input-file input-file) #:line-mode 'linefeed)]
         [network (create-nodes lines)])
    (add-inputs network)))

(define (create-nodes lines)
  (for/hash ([line lines])
    (match-let* ([(list node out-str) (string-split line " -> " #:trim? #t)]
                 [outs (string-split out-str ", " #:trim? #t)]
                 [node-type (string-ref node 0)]
                 [node-name (if (eq? node-type #\b) node (substring node 1))])
      (values node-name (make-node node-type node-name outs)))))

(define (add-inputs network)
  (for/fold ([top-net network])
            ([node (hash-values network)])
    (for/fold ([node-net top-net]) 
              ([out-name (outs node)])
      (let ([out-node (hash-ref node-net out-name)])
        (if (conjunction? out-node)
          (hash-set node-net out-name (conjunction-add-input out-node (name node)))
          node-net)))))
