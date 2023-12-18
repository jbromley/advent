#lang racket/base
(require racket/list)
(require racket/match)
(require racket/port)
(require racket/string)

(struct pos [x y] #:transparent)

(struct node [pos dir steps] #:transparent)

(define *up*    (pos -1 0))
(define *right* (pos 0 1))
(define *down*  (pos 1 0))
(define *left*  (pos 0 -1))

(define *plan-dirs* (hash
                      #\U *up*
                      #\R *right*
                      #\D *down*
                      #\L *left*))

(define (pos+ p1 p2 steps)
  (pos (+ (pos-x p1) (* steps (pos-x p2)))
       (+ (pos-y p1) (* steps (pos-y p2)))))

(define (read-input input-file)
  (let ([lines (port->lines (open-input-file input-file) #:line-mode 'linefeed)])
    (for/list ([line lines])
      (match (string-split line #px"[ ()]" #:repeat? #t)
        [(list dir steps color)
         (list (string-ref dir 0) (string->number steps) color)]))))

(define (build-vertices plans)
  (for/fold ([vertices '()]
             [vertex (pos 0 0)]
             #:result (cons (pos 0 0) vertices))
             ([plan plans])
    (match-let ([(list dir steps _) plan])
      (values (cons vertex vertices) (pos+ vertex (hash-ref *plan-dirs* dir) steps)))))

(define (shoelace-area pts)
  (abs (/ (for/sum ([p1 pts]
                    [p2 (rest pts)])
            (- (* (pos-x p1) (pos-y p2))
               (* (pos-y p1) (pos-x p2))))
          2)))

(define (count-area pts)
  (let ([inside (shoelace-area pts)]
        [boundaries (+ 4
                       (for/sum ([p1 pts]
                                 [p2 (rest pts)])
                         (+ (abs (- (pos-x p1) (pos-x p2)))
                            (abs (- (pos-y p1) (pos-y p2))))))])
    (+ inside (/ boundaries 2) -1)))

;;; Part 1

(define (lagoon-volume plans)
  (count-area (build-vertices plans)))

;;; Part 2

(define (decode-hex-plans plans)
  (for/list ([plan plans])
    (let ([hex (third plan)])
      (list (match (string-ref hex 6)
              [#\0 #\R]
              [#\1 #\D]
              [#\2 #\L]
              [#\3 #\U])
            (string->number (substring hex 1 6) 16)
            hex))))

(define (lagoon-volume-decoded plans)
  (count-area (build-vertices (decode-hex-plans plans))))

;;; Entry point

(define (main)
  (let ([plans (read-input "input.txt")])
    (printf "AoC 2023 Day 18 - Lavaduct Lagoon~n")
    (printf "Part 1: ~a~n" (lagoon-volume plans))
    (printf "Part 2: ~a~n" (lagoon-volume-decoded plans))))
 
(main)
