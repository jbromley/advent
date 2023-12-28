#lang racket/base
(require racket/list)
(require racket/match)
(require racket/port)
(require racket/set)
(require racket/string)
(require racket/vector)

;;; Data structures

(struct pos [r c] #:transparent)

(define dirs (list (pos -1 0) (pos 0 1) (pos 1 0) (pos 0 -1)))

(define (add-pos p1 p2)
  (pos (+ (pos-r p1) (pos-r p2)) (+ (pos-c p1) (pos-c p2))))

;;; Read the input map and access coordinates
(define (read-input input-file)
  (let ([in (port->lines (open-input-file input-file) #:line-mode 'linefeed)])
    (list->vector (map list->vector (map string->list in)))))

(define (open-tile? garden-map tile)
  (eq? (matrix-ref garden-map (pos-r tile) (pos-c tile)) #\.))
 
(define (find-start mat)
  (for/or ([r (in-range (vector-length mat))])
    (let ([c (vector-member #\S (vector-ref mat r))])
      (if (eq? c #f) #f (pos r c)))))
    
(define (matrix-ref m r c)
  (vector-ref (vector-ref m r) c))

(define (dims m)
  (cons (vector-length m) (vector-length (vector-ref m 0))))

;;; Common code

(define (valid-tile? tile garden-map)
  (match-let ([(cons rows cols) (dims garden-map)]
              [r (pos-r tile)]
              [c (pos-c tile)])
    (and (< -1 r rows)
         (< -1 c cols)
         (open-tile? garden-map tile))))

(define (explore garden-map frontier visited)
  (cond
    [(empty? frontier) visited]
    [(hash-has-key? visited (car (first frontier))) (explore garden-map (rest frontier) visited)]
    [else
     (let* ([tile (car (first frontier))]
            [dist (cdr (first frontier))]
            [new-visited (hash-set visited tile dist)])
       (explore garden-map
                (append (rest frontier) (explore-step garden-map tile dist visited))
                new-visited))]))

(define (explore-step garden-map tile dist visited)
 (map
  (λ (tile) (cons tile (add1 dist)))
  (filter (λ (tile) (and (valid-tile? tile garden-map) (not (hash-has-key? visited tile))))
          (map (λ (dir) (add-pos tile dir)) dirs))))

;;; Part 1

(define (count-reachable-tiles garden-map total-steps)
  (let* ([start (find-start garden-map)]
         [visited (explore garden-map (list (cons start 0)) (hash))])
    (length (filter (λ (dist) (and (<= dist total-steps) (zero? (modulo dist 2))))
                    (hash-values visited)))))

;;; Part 2

;; This solution only works for certain numbers of steps. In particular, where
;; steps = floor(width / 2) + n * width
;; where n is an even integer. Odd n doesn't work as it changes the parity of each
;; map tile and so even? and odd? would have to be exchanged in the code.
(define (count-reachable-infinite garden-map total-steps)
  (let* ([start (find-start garden-map)]
         [size (car (dims garden-map))]
         [half-size (quotient size 2)]
         [visited (explore garden-map (list (cons start 0)) (hash))]
         [odd-tiles (length (filter odd? (hash-values visited)))]
         [even-tiles (length (filter even? (hash-values visited)))]
         [odd-corners (length (filter (λ (dist) (and (odd? dist) (> dist half-size))) (hash-values visited)))]
         [even-corners (length (filter (λ (dist) (and (even? dist) (> dist half-size))) (hash-values visited)))]
         [n (/ (- total-steps half-size) size)]
         [num-odd (* (add1 n) (add1 n))]
         [num-even (* n n)])
    (- (+ (* num-odd odd-tiles) (* num-even even-tiles) (* n even-corners)) (* (add1 n) odd-corners))))
  
;;; Entry point

(module+ main
  (let ([garden-map (read-input "input.txt")])
    (printf "AoC 2023 Day 21 - Step Counter~n")
    (printf "Part 1: ~a~n" (count-reachable-tiles garden-map 64))
    (printf "Part 1: ~a~n" (count-reachable-infinite garden-map 26501365))))