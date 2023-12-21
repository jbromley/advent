#lang racket/base
(require racket/generic)
(require racket/match)
(require racket/port)
(require racket/set)
(require racket/string)
(require data/queue)

;;; Pulse struct and functions

(struct pulse [src dst level] #:transparent)

(define (low? pulse)
  (eq? (pulse-level pulse) 'low))

(define (high? pulse)
  (eq? (pulse-level pulse) 'high))

(define (toggle state)
  (if (eq? state 'low) 'high 'low))

;;; Network and node structures

(define-generics node
  (name node)
  (outs node)
  (handle-pulse node pulse))

(struct flip-flop [name state outs] #:mutable #:transparent
  #:methods gen:node
  [(define (name node) (flip-flop-name node))
   (define (outs node) (flip-flop-outs node))
   (define (handle-pulse node p)
     (when (low? p)
       (let ([new-state (toggle (flip-flop-state node))]
             [node-name (flip-flop-name node)])
         (set-flip-flop-state! node new-state)
         (for/list ([out (flip-flop-outs node)])
           (pulse node-name out new-state)))))])

(struct conjunction [name state outs] #:mutable #:transparent
  #:methods gen:node
  [(define (name node) (conjunction-name node))
   (define (outs node) (conjunction-outs node))
   (define (handle-pulse node p)
     (let* ([src (pulse-src p)]
            [new-mem (conjunction-update-memory node src p)]
            [out-pulse (if (andmap (λ (x) x) (hash-values new-mem)) 'low 'high)])
      (for/list ([out (outs node)]) (pulse (name node) out out-pulse))))])

(define (conjunction-update-memory conj src p)
  (let* ([mem (conjunction-state conj)]
         [new-mem (hash-set mem src (pulse-level p))])
    (set-conjunction-state! conj (hash-set mem src (pulse-level p)))
    new-mem))

(struct broadcaster [name outs] #:transparent
  #:methods gen:node
  [(define (name node) (broadcaster-name node))
   (define (outs node) (broadcaster-outs node))
   (define (handle-pulse node pulse) (list node pulse))])

(struct sink [name] #:transparent
  #:methods gen:node
  [(define (name node) (sink-name node))
   (define (outs node) (list))
   (define (handle-pulse node pulse) (list node pulse))])

(define (make-flip-flop name [outs '()])
  (flip-flop name 'low outs))

(define (make-conjunction name [outs '()])
  (conjunction name (hash) outs))

(define (conjunction-add-input conj in)
  (let ([ins (conjunction-state conj)])
    (struct-copy conjunction conj [state (hash-set ins in 'low)])))

(define (make-broadcaster [outs '()])
  (broadcaster "broadcaster" outs))

(define (make-sink name)
  (sink name))

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
  (for/fold ([sources (set)]
             [sinks (set)]
             [nodes (hash)]
             #:result (ensure-nodes nodes sources sinks))
            ([line lines])
    (match-let* ([(list node out-str) (string-split line " -> " #:trim? #t)]
                 [outs (string-split out-str ", " #:trim? #t)]
                 [node-type (string-ref node 0)]
                 [node-name (if (eq? node-type #\b) node (substring node 1))])
      (values (set-add sources node-name)
              (set-union sinks (list->set outs))
              (hash-set nodes node-name (make-node node-type node-name outs))))))

(define (add-inputs network)
  (for/fold ([top-net network])
            ([node (hash-values network)])
    (for/fold ([node-net top-net]) 
              ([out-name (outs node)])
      (let ([out-node (hash-ref node-net out-name)])
        (if (conjunction? out-node)
          (hash-set node-net out-name (conjunction-add-input out-node (name node)))
          node-net)))))

(define (ensure-nodes nodes sources sinks)
  (let ([uncreated-nodes (set-subtract sinks sources)])
    (for/fold ([aug-nodes nodes])
              ([sink-node-name (in-set uncreated-nodes)])
      (hash-set aug-nodes sink-node-name (sink sink-node-name))))) 

;;; Part 1

(define (press-button network)
  (let ([pulse-queue (make-queue)])
    (enqueue! pulse-queue (pulse "button" "broadcaster" 'low))
    (propagate-pulses pulse-queue
                      network
                      (cons 0 0))))

(define (propagate-pulses pulse-q network counts) #f)
;;   (cond
;;     [(queue-empty? pulse-q) counts]
;;     [else
;;      (let* ([pulse (dequeue! pulse-queue)]
;;             [dst (hash-ref network
     

