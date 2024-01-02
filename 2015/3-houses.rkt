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

(define (deliver dir m cur-node)
  (define (add-edge m node dir next-node)
    (hash-update m node (lambda (edges) (hash-set edges dir next-node))))
  (define (add-node m node dir next-node)
    (let ([new-m (hash-set m next-node (hash))])
      (hash-update new-m node (lambda (edges) (hash-set edges dir next-node)))))
  (let ([neighbors (hash-ref m cur-node)])
    (cond
      [(hash-has-key? neighbors dir)
       (values m (hash-ref neighbors dir))]
      [(hash-has-key? m (neighbor cur-node dir))
       (let ([next-node (neighbor cur-node dir)])
         (values (add-edge m cur-node dir next-node) next-node))]
      [else
       (let ([next-node (neighbor cur-node dir)])
         (values (add-node m cur-node dir next-node) next-node))])))

;;; Part 1

(define (count-houses dirs)
  (define m (hash (vec 0 0) (hash)))
  (define cur-node (vec 0 0))
  (define (iter dirs m cur-node)
    (cond
      [(empty? dirs) (hash-count m)]
      [else
       (let-values ([(new-map next-node) (deliver (first dirs) m cur-node)])
         (iter (rest dirs) new-map next-node))]))
  (iter dirs m cur-node))

;;; Part 2

(define (count-houses-2 dirs)
  (define m (hash (vec 0 0) (hash)))
  (define s-node (vec 0 0))
  (define r-node (vec 0 0))
  (define (iter dirs m s-node r-node)
    (cond
      [(empty? dirs) (hash-count m)]
      [else
       (let*-values ([(int-map next-s-node) (deliver (first dirs) m s-node)]
                     [(next-map next-r-node) (deliver (second dirs) int-map r-node)])
         (iter (drop dirs 2) next-map next-s-node next-r-node))]))
  (iter dirs m s-node r-node))

(module+ main
  (printf "AoC 2015 Day 3 - Perfectly Spherical Houses in a Vacuum~n")
  (let ([dirs (read-input "input/3.txt")])
    (printf "1: ~a~n" (count-houses dirs))
    (printf "2: ~a~n" (count-houses-2 dirs))))

(module+ test
  (require rackunit)

  (check-equal? (count-houses (list #\>)) 2)
  (check-equal? (count-houses (list #\^ #\> #\v #\<)) 4)
  (check-equal? (count-houses (list #\^ #\v #\^ #\v #\^ #\v #\^ #\v)) 2)

  (check-equal? (count-houses-2 (list #\^ #\v)) 3)
  (check-equal? (count-houses-2 (list #\^ #\> #\v #\<)) 3)
  (check-equal? (count-houses-2 (list #\^ #\v #\^ #\v #\^ #\v #\^ #\v #\^ #\v)) 11))

