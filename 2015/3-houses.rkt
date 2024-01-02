#lang racket/base
(require racket/list)
(require racket/match)
(require racket/string)

(struct vec (x y) #:transparent)

(define (vec+ p1 p2)
  (vec (+ (vec-x p1) (vec-x p2))
       (+ (vec-y p1) (vec-y p2))))

(define (neighbor node dir)
  (match dir
    [#\^ (vec+ node (vec 0 1))]
    [#\> (vec+ node (vec 1 0))]
    [#\v (vec+ node (vec 0 -1))]
    [#\< (vec+ node (vec -1 0))]))

(define (read-input input-file)
  (call-with-input-file input-file
    (lambda (port)
      (let ([s (read-line port)])
        (string->list (string-trim s))))))

;;; Part 1

(define (count-houses dirs)
  (define m (hash (vec 0 0) (hash)))
  (define cur-node (vec 0 0))
  (define (iter dirs m cur-node)
    (cond
      [(empty? dirs) (hash-count m)]
      [else
       (let ([neighbors (hash-ref m cur-node)]
             [dir (first dirs)])
         (cond
           [(hash-has-key? neighbors dir)
            (iter (rest dirs) m (hash-ref neighbors dir))]
           [(hash-has-key? m (neighbor cur-node dir))
            (let ([next-node (neighbor cur-node dir)])
              (iter (rest dirs) (add-edge m cur-node dir next-node) next-node))]
           [else
            (let ([next-node (neighbor cur-node dir)])
              (iter (rest dirs) (add-node m cur-node dir next-node) next-node))]))]))
  (define (add-edge m node dir next-node)
    (hash-update m node (lambda (edges) (hash-set edges dir next-node))))
  (define (add-node m node dir next-node)
    (let ([new-m (hash-set m next-node (hash))])
      (hash-update new-m node (lambda (edges) (hash-set edges dir next-node)))))
  (iter dirs m cur-node))

(module+ main
  (printf "AoC 2015 Day 3 - Perfectly Spherical Houses in a Vacuum~n")
  (let ([dirs (read-input "input/3.txt")])
    (printf "1: ~a~n" (count-houses dirs))))

(module+ test
  (require rackunit)

  (check-equal? (count-houses (list #\>)) 2)
  (check-equal? (count-houses (list #\^ #\> #\v #\<)) 4)
  (check-equal? (count-houses (list #\^ #\v #\^ #\v #\^ #\v #\^ #\v)) 2))


