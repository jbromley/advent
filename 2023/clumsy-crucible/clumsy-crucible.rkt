#lang racket/base
(require racket/list)
(require racket/match)
(require racket/port)
(require data/heap)

(struct pos [r c] #:transparent)

(struct node [pos dir steps] #:transparent)

(define *up*    (pos -1 0))
(define *right* (pos 0 1))
(define *down*  (pos 1 0))
(define *left*  (pos 0 -1))

(define *next-dirs* (hash
                     (pos -1 0) (list *left* *right*)
                     (pos 0 1)  (list *up* *down*)
                     (pos 1 0)  (list *right* *left*)
                     (pos 0 -1) (list *down* *up*)))
  
(define (pos+ p1 p2)
  (pos (+ (pos-r p1) (pos-r p2))
       (+ (pos-c p1) (pos-c p2))))

(define (read-input input-file)
  (let ([in (port->lines (open-input-file input-file) #:line-mode 'linefeed)])
    (list->vector (map (lambda (line) (list->vector (map char->digit line))) (map string->list in)))))

(define (char->digit ch)
  (- (char->integer ch) (char->integer #\0)))

(define (matrix-ref mat row col)
  (vector-ref (vector-ref mat row) col))

(define (valid-pos? mat p)
  (let ([rows (vector-length mat)]
        [cols (vector-length (vector-ref mat 0))])
    (and (< -1 (pos-r p) rows)
         (< -1 (pos-c p) cols))))

(define (forward mat pos dir steps)
  (let ([new-pos (pos+ pos dir)])
    (if (valid-pos? mat new-pos)
      (begin
        (list (cons (node new-pos dir (add1 steps))
                    (matrix-ref mat (pos-r new-pos) (pos-c new-pos)))))
        '())))

(define (turn mat pos dir)
  (append* (map (lambda (new-dir) (forward mat pos new-dir 0)) (hash-ref *next-dirs* dir))))

(define (edge-of mat min-steps max-steps)
  (lambda (n)
    (match n
      [(node pos dir (== max-steps))
       (turn mat pos dir)]
      [(node pos dir steps)
       #:when (< steps min-steps)
       (forward mat pos dir steps)]
      [(node pos dir steps)
       (append (forward mat pos dir steps)
               (turn mat pos dir))])))

(define (done? mat)
  (let ([rows (vector-length mat)]
        [cols (vector-length (vector-ref mat 0))])
    (lambda (n)
      (equal? (node-pos n) (pos (sub1 rows) (sub1 cols))))))

(define (done-ultra? mat)
  (letrec ([end? (done? mat)])
    (lambda (n) 
      (and (end? n) (>= (node-steps n) 4)))))
  

(define (djikstra start-nodes end? edge-of)
  (letrec ([dist (make-hash)]
           [recurse (lambda (h)
                      (match-let ([(cons d closest) (heap-min h)])
                        (heap-remove-min! h)
                        (cond
                          [(end? closest) d]
                          [else
                           (for ([edge (edge-of closest)])
                             (match-let* ([(cons to cost) edge]
                                          [d1 (+ d cost)])
                                (when (or (not (hash-has-key? dist to))
                                          (< d1 (hash-ref dist to)))
                                  (hash-set! dist to d1)
                                  (heap-add! h (cons d1 to)))))
                           (if (zero? (heap-count h))
                             #f
                             (recurse h))])))]
            [h (make-heap (lambda (a b) (<= (car a) (car b))))])
           (for ([s start-nodes])
             (heap-add! h (cons 0 s)))
           (recurse h)))

(define (search pos edge-of end?)
  (djikstra (list (node pos *right* 0)
                  (node pos *down* 0))
            end?
            edge-of))

;;; Part 1

(define (minimize-heat-loss mat)
  (search (pos 0 0) (edge-of mat 0 3) (done? mat)))

;;; Part 2

(define (minimize-ultra-heat-loss mat)
  (search (pos 0 0) (edge-of mat 4 10) (done-ultra? mat)))

;;; Entry point

(define (main)
  (let ([mat (read-input "input.txt")])
    (printf "AoC 2023 Day 17 - Clumsy Crucibles~n")
    (printf "Part 1: ~a~n" (minimize-heat-loss mat))
    (printf "Part 2: ~a~n" (minimize-ultra-heat-loss mat))))

(main)
