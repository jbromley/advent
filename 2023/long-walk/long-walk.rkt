#lang racket/base
(require racket/list)
(require racket/match)
(require racket/set)
(require racket/vector)
(require data/queue)

(struct pos (x y) #:transparent)

(define (pos+ p1 p2)
  (pos (+ (pos-x p1) (pos-x p2))
       (+ (pos-y p1) (pos-y p2))))

(define neighbors (list (pos 0 -1) (pos 1 0) (pos 0 1) (pos -1 0)))

(define slides (for/hash ([slide (list #\^ #\> #\v #\<)]
                          [dir neighbors])
                 (values slide dir)))

(define (matrix-ref mat x y)
  (vector-ref (vector-ref mat y) x))

;;; Graph functions

(define (add-vertex! g v)
  (when (not (has-vertex? g v))
    (hash-set! g v (list))))

(define (has-vertex? g v)
  (hash-has-key? g v))

(define (add-edge! g v1 v2 w)
  (when (not (has-edge? g v1 v2))
    (when (not (has-vertex? g v1)) (hash-set! g v1 (list)))
    (when (not (has-vertex? g v2)) (hash-set! g v2 (list)))
    (hash-update! g v1 (lambda (edges) (cons (cons v2 w) edges)) (list))))

(define (has-edge? g v1 v2)
  (and (hash-has-key? g v1)
       (hash-has-key? g v2)
       (not (equal? #f (member v2 (map car (hash-ref g v1)))))))

(define (vertices g)
  (sort (hash-keys g)
        (lambda (v1 v2) (cond
                          [(= (pos-y v1) (pos-y v2)) (< (pos-x v1) (pos-x v2))]
                          [else (< (pos-y v1) (pos-y v2))]))))

;;; Memoization


(define-syntax-rule (memoize-fn! fn)
  (set! fn (memoize-fn fn)))

(define (memoize-fn fn)
  (let ([cache (make-hash)]
        [orig-fn fn])
    (λ args
      (when (not (hash-has-key? cache args))
        (hash-set! cache args (apply orig-fn args)))
      (hash-ref cache args))))

;;; Maze and graph set-up

(define (valid-pos? pos maze)
  (and (< -1 (pos-x pos) (vector-length (vector-ref maze 0)))
       (< -1 (pos-y pos) (vector-length maze))
       (not (eq? #\# (matrix-ref maze (pos-x pos) (pos-y pos))))))

(define (read-maze input-file)
  (call-with-input-file input-file
    (lambda (port)
      (for/vector ([line (in-lines port)])
        (list->vector (string->list line))))))

(define (find-start-end maze)
  (let ([max-y (sub1 (vector-length maze))])
    (list (pos (vector-member #\. (vector-ref maze 0)) 0)
          (pos (vector-member #\. (vector-ref maze max-y)) max-y))))

(define (find-vertices maze start-v end-v)
  (let ([rows (vector-length maze)]
        [cols (vector-length (vector-ref maze 0))]
        [g (make-hash (list (cons start-v (list)) (cons end-v (list))))])
    (for ([y rows])
      (for ([x cols])
        (when (and (equal? (matrix-ref maze x y) #\.)
                   (> (length (next-moves maze (pos x y) #f)) 2))
          (add-vertex! g (pos x y)))))
    g))

(define (find-edges maze g one-ways)
  (for ([v (vertices g)])
    (letrec ([q (make-queue)]
             [edge-bfs (lambda (seen)
                         (cond
                           [(queue-empty? q) (void)]
                           [else
                            (match-let* ([(list this-v dist one-way-path) (dequeue! q)]
                                         [now-one-way-path (or one-way-path
                                                               (not (equal? #f (member (matrix-ref maze (pos-x this-v) (pos-y this-v))
                                                                                       (hash-keys slides)))))])
                              (cond
                                [(and (not (equal? v this-v)) (has-vertex? g this-v))
                                 (add-edge! g v this-v dist)
                                 (unless (and one-ways now-one-way-path)
                                   (add-edge! g this-v v dist))
                                 (edge-bfs (set-add seen this-v))]
                                [else
                                 (for ([move (filter (lambda (move) (not (set-member? seen move))) (next-moves maze this-v one-ways))])
                                   (enqueue! q (list move (add1 dist) now-one-way-path)))
                                 (edge-bfs (set-add seen this-v))]))]))])
      (enqueue! q (list v 0 #f))
      (edge-bfs (set)))))

(define (maze->graph maze start-v end-v [one-ways #f])
  (let ([g (find-vertices maze start-v end-v)])
    (find-edges maze g one-ways)
    g))

(define (next-moves maze pos [one-ways #f])
  (let ([cur-tile (matrix-ref maze (pos-x pos) (pos-y pos))])
    (cond
      [(or (not one-ways) (eq? cur-tile #\.))
       (filter (lambda (pos) (valid-pos? pos maze))
               (map (lambda (neighbor) (pos+ pos neighbor)) neighbors))]
      [else
       (list (pos+ pos (hash-ref slides cur-tile)))])))

;;; Graph searching functions

(define (find-path g start-v end-v)
  (define longest-path 0)
  (define (traverse g v dist visited)
    (cond
      [(equal? v end-v)
       (set! longest-path (max longest-path dist))]
      [(not (set-member? visited v))
       (let ([new-visited (set-add visited v)])
         (for-each (lambda (next-v) (traverse g (car next-v) (+ dist (cdr next-v)) new-visited))
                   (hash-ref g v)))]))

  (traverse g start-v 0 (set))
  longest-path)

;;; Part 1 and 2!

(define (find-longest-path maze one-ways)
  (match-let* ([(list start-pos end-pos) (find-start-end maze)]
               [g (maze->graph maze start-pos end-pos one-ways)])
    (find-path g start-pos end-pos)))

;;; Entry point

(module+ main
  (printf "AoC 2023 Day 23 - A Long Walk~n")
  (let ([maze (read-maze "input.txt")])
    (printf "Part 1: ~a~n" (find-longest-path maze #t))
    (printf "Part 2: ~a~n" (find-longest-path maze #f))))
