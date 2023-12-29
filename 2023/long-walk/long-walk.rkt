#lang racket/base
(require racket/match)
(require racket/set)
(require racket/vector)

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

(define (find-path maze pos end-pos next-move-fn)
  (define longest-path 0)
  (define (traverse maze pos end-pos len visited)
    (cond
      [(equal? pos end-pos)
       (set! longest-path (max longest-path len))]
      [(not (set-member? visited pos))
       (let ([new-visited (set-add visited pos)])
         (for-each (lambda (next-pos) (traverse maze next-pos end-pos (add1 len) new-visited))
                   (next-move-fn maze pos)))]))

  (traverse maze pos end-pos 0 (set))
  longest-path)

;;; Part 1

(define (next-moves maze pos)
  (let ([cur-tile (matrix-ref maze (pos-x pos) (pos-y pos))])
    (cond
      [(eq? cur-tile #\.)
       (filter (lambda (pos) (valid-pos? pos maze))
               (map (lambda (neighbor) (pos+ pos neighbor)) neighbors))]
      [else
       (list (pos+ pos (hash-ref slides cur-tile)))])))

(define (find-longest-path maze)
  (match-let ([(list start-pos end-pos) (find-start-end maze)])
    (find-path maze start-pos end-pos next-moves)))

;;; Part 2

(define (next-moves-no-ice maze pos)
  (filter (lambda (pos) (valid-pos? pos maze))
          (map (lambda (neighbor) (pos+ pos neighbor)) neighbors)))

(define (find-longest-path-no-ice maze)
  (match-let ([(list start-pos end-pos) (find-start-end maze)])
    (find-path maze start-pos end-pos next-moves-no-ice)))

;;; Entry point

(define maze (read-maze "input1.txt"))

(module+ main
  (let ([maze (read-maze "input.txt")])
    (printf "AoC 2023 Day 23 - A Long Walk~n")
    (printf "Part 1: ~a~n" (find-longest-path maze))))
