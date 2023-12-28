#lang racket/base
(require racket/list)
(require racket/match)
(require racket/port)
(require racket/set)
(require)

(define (read-input input-file)
  (let ([in (port->lines (open-input-file input-file) #:line-mode 'linefeed)])
    (list->vector (map list->vector (map string->list in)))))

(define (matrix-ref m row col)
  (vector-ref (vector-ref m row) col))

(struct ray (r c dir) #:transparent)

(define (ray-pos r)
  (cons (ray-r r) (ray-c r)))

(define *move-table* (hash
                      '(#\. up)    '(up)
                      '(#\. right) '(right)
                      '(#\. down)  '(down)
                      '(#\. left)  '(left)
                    
                      '(#\\ up)    '(left)
                      '(#\\ right) '(down)
                      '(#\\ down)  '(right)
                      '(#\\ left)  '(up)
                    
                      '(#\/ up)    '(right)
                      '(#\/ right) '(up)
                      '(#\/ down)  '(left)
                      '(#\/ left)  '(down)
                    
                      '(#\- up)    '(left right)
                      '(#\- right) '(right)
                      '(#\- down)  '(left right)
                      '(#\- left)  '(left)
                    
                      '(#\| up)    '(up)
                      '(#\| right) '(up down)
                      '(#\| down)  '(down)
                      '(#\| left)  '(up down)))

(define (move-ray r move)
  (match move
    ['up    (ray (sub1 (ray-r r)) (ray-c r) 'up)]
    ['right (ray (ray-r r) (add1 (ray-c r)) 'right)]
    ['down  (ray (add1 (ray-r r)) (ray-c r) 'down)]
    ['left  (ray (ray-r r) (sub1 (ray-c r)) 'left)]))

(define (valid-ray? ray mat)
  (let ([num-rows (vector-length mat)]
        [num-cols (vector-length (vector-ref mat 0))]
        [r (ray-r ray)]
        [c (ray-c ray)])
    (and (>= r 0)
         (< r num-rows)
         (>= c 0)
         (< c num-cols))))

(define (continue-trace? r mat visited)
  (and (not (set-member? visited r))
       (valid-ray? r mat)))
    
(define (trace-rays mat rays [visited (set)])
  (cond
    [(empty? rays)
     (set-count (list->set (set-map visited ray-pos)))]
    [else
     (let* ([r (first rays)]
            [other-rays (rest rays)]
            [tile (matrix-ref mat (ray-r r) (ray-c r))]
            [moves (hash-ref *move-table* (list tile (ray-dir r)))]
            [next-rays (filter (lambda (r) (continue-trace? r mat visited))
                               (map (lambda (m) (move-ray r m)) moves))])
       (trace-rays mat (append next-rays other-rays) (set-union visited (list->set next-rays))))]))

;;; Part 1

(define (count-activated-tiles mat)
  (let ([start (ray 0 0 'right)])
    (trace-rays mat (list start) (set start))))

;;; Part 2

(define (maximize-activated-tiles mat)
  (let ([starts (generate-starts mat)])
    (for/fold ([max-tiles 0])
              ([start starts])
      (max max-tiles (trace-rays mat (list start) (set start))))))

(define (generate-starts mat)
  (let ([max-row (sub1 (vector-length mat))]
        [max-col (sub1 (vector-length (vector-ref mat 0)))])
    (append (list (ray 0 0 'down) (ray 0 0 'right)
                  (ray 0 max-col 'down) (ray 0 max-col 'left)
                  (ray max-row 0 'up) (ray max-row 0 'right)
                  (ray max-row max-col 'up) (ray max-row max-col 'left))
            (for/list ([r (in-range 1 max-row)])
              (ray r 0 'right))
            (for/list ([r (in-range 1 max-row)])
              (ray r max-col 'left))
            (for/list ([c (in-range 1 max-col)])
              (ray 0 c 'down))
            (for/list ([c (in-range 1 max-col)])
              (ray max-row c 'up)))))
    
;;; Main entry point

(module+ main
  (let ([mat (read-input "input.txt")])
    (begin
      (printf "AoC 2023 Day 16 - The Floor Will Be Lava~n")
      (printf "Part 1: ~a~n" (count-activated-tiles mat))
      (printf "Part 2: ~a~n" (maximize-activated-tiles mat)))))

;;; Tests

(module+ test
  (require rackunit)

  (define mat (read-input "input1.txt"))

  (check-equal? (count-activated-tiles mat) 46)
  (check-equal? (maximize-activated-tiles mat) 51))
