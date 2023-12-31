#lang racket/base
(require racket/list)
(require racket/match)
(require racket/set)
(require racket/string)

;;; Graph functions


(define (make-graph)
  (make-hash))

(define (has-edge? g v1 v2)
  (and (hash-has-key? g v1)
       (hash-has-key? g v2)
       (set-member? (hash-ref g v1) v2)))

(define (add-edge! g v1 v2)
  (when (not (has-edge? g v1 v2))
    (hash-update! g v1 (lambda (edges) (cons v2 edges)) (list))
    (hash-update! g v2 (lambda (edges) (cons v1 edges)) (list))))

#|
(define (add-vertex! g v)
  (when (not (has-vertex? g v))
    (hash-set! g v (set))))

(define (has-vertex? g v)
  (hash-has-key? g v))

(define (vertices g)
  (sort (hash-keys g) string<=?))

(define (vertex-edges g v)
  (hash-ref g v))

(define (edges g)
  (set-map (for*/set ([v (vertices g)]
                      [next-v (vertex-edges g v)])
             (set v next-v))
           set->list))

(define (graphviz g out-file)
  (let ([es (edges g)])
    (call-with-output-file out-file
      (lambda (port)
        (fprintf port "graph G {~n")
        (for ([e es])
          (let ([v1 (first (set->list e))]
                [v2 (second (set->list e))])
            (fprintf port "  ~a -- ~a;~n" v1 v2)))
        (fprintf port "}~n"))
      #:exists 'replace)))
|#
(define (random-choice lst)
  (let ([n (random (length lst))])
    (first (drop lst n))))

;;; Read the input and build a graph

(define (read-graph input-file)
  (let ([g (make-graph)])
    (call-with-input-file input-file
      (lambda (port)
        (for ([line (in-lines port)])
          (let* ([strs (filter non-empty-string? (string-split line #rx"[: ]" #:trim? #t))]
                 [v (first strs)]
                 [vs (rest strs)])
            (for ([next-v vs])
              (add-edge! g v next-v))))))
    g))

;;; Part 1 - min-cut

(define (karger g)
  (let ([s-nodes (for/hash ([node (hash-keys g)]) (values node (list node)))])
    (do-karger g s-nodes)))

(define (do-karger g s-nodes)
  (cond
    [(= (hash-count g) 2)
     (let* ([n (first (hash-keys g))]
            [cuts (length (hash-ref g n))]
            [supernodes (hash-values s-nodes)])
       (list (first supernodes) (second supernodes) cuts))]
    [else
     (match-let* ([(list-rest u vs) (random-choice (hash->list g))]
                  [v (random-choice vs)])
       (hash-update! g u (lambda (edges)
                           (filter (lambda (node) (not (member node (list u v))))
                                   (append edges (hash-ref g v)))))
       (for ([node (hash-ref g v)])
         (when (not (member node (list u v)))
           (hash-update! g node (lambda (nodes) (cons u (remove v nodes))))))
       (hash-remove! g v)
       (do-karger g (update-supernodes s-nodes u v)))]))

(define (update-supernodes s-nodes u v)
  (hash-remove (hash-update s-nodes u (lambda (nodes) (append nodes (hash-ref s-nodes v))))
               v))

(define (find-min-cut g)
  (match-let ([(list g1 g2 cuts) (karger (hash-copy g))])
    (if (= cuts 3)
        (* (length g1) (length g2))
        (find-min-cut g))))

(module+ main
  (printf "AoC 2023 Day 25 - Snowverload~n")
  (let ([g (read-graph "input.txt")])
    (printf "Part 1: ~a~n" (find-min-cut g))))
