#lang racket/base
(require racket/port)

(struct pos [r c] #:transparent)

(struct state [pos dir count] #:transparent)

(define *dirs* (hash
                'up    (pos -1 0)
                'right (pos 0 1)
                'down  (pos 1 0)
                'left  (pos 0 -1)))

(define *next-dirs* (hash
                     'up    (list 'left 'right)
                     'right (list 'up 'down)
                     'down  (list 'right 'left)
                     'left  (list 'down 'up)
                     'none    (list 'right 'down)))
  
(define (add-pos p1 p2)
  (pos (+ (pos-r p1) (pos-r p2))
       (+ (pos-c p1) (pos-c p2))))

(define (read-input input-file)
  (let ([in (port->lines (open-input-file input-file) #:line-mode 'linefeed)])
    (list->vector (map (lambda (line) (list->vector (map char->digit line))) (map string->list in)))))

(define (char->digit ch)
  (- (char->integer ch) (char->integer #\0)))

(define (matrix-ref mat row col)
  (vector-ref (vector-ref mat row) col))

(define (valid-pos? p mat)
  (let ([r (pos-r p)]
        [c (pos-c p)]
        [rows (vector-length mat)]
        [cols (vector-length (vector-ref mat 0))])
    (and (>= r 0)
         (>= c 0)
         (< r rows)
         (< c cols))))

(define (neighbors mat st)
  (let ([pos (state-pos st)]
        [dir (state-dir st)]
        [count (state-count st)])
    #f))

(define (straight-neighbor mat pos dir count)
  (cond
    [(eq? dir 'none) '()]
    [else
     (let ([new-pos (add-pos pos (hash-ref *dirs* dir))])
       (if (and (valid-pos? new-pos mat)
                  (<= (add1 count) 3))
         (list (matrix-ref mat (pos-r new-pos) (pos-c new-pos)) (state new-pos dir (add1 count)))
         '()))]))
      
;;; Part 1

(define (a-star mat start) #f)

(define mat (read-input "input1.txt"))
