#lang racket
(require racket/list)

(define (read-instructions input-file)
  (call-with-input-file input-file
    (lambda (port)
      (for/list ([line (in-lines port)])
        (let ([parts (flatten (regexp-match* #rx"([a-z ]+)([0-9]+),([0-9]+)[a-z ]*([0-9]+),([0-9]+)"
                                             line
                                             #:match-select cdr))])
          (cons (string->symbol (string-trim (first parts)))
                (map string->number (rest parts))))))))

(define (make-matrix columns rows init-val)
  (build-vector rows (lambda (_) (make-vector columns init-val))))

(define (matrix-set! m x y value)
  (vector-set! (vector-ref m y) x value))

(define (matrix-ref m x y)
  (vector-ref (vector-ref m y) x))

(define (config-lights lights instructions trans-fn)
  (for ([i instructions])
    (match-let* ([(list command x1 y1 x2 y2) i]
                 [f (trans-fn command lights)])
      (for* ([y (in-range y1 (add1 y2))]
             [x (in-range x1 (add1 x2))])
        (f x y)))))

(define (count-lights lights count-fn)
  (let ([max-y (vector-length lights)]
        [max-x (vector-length (vector-ref lights 0))])
    (for*/sum ([y max-y]
               [x max-x])
      (count-fn lights x y))))

(define (setup-count instructions init-val trans-fn count-fn)
  (let ([lights (make-matrix 1000 1000 init-val)])
    (config-lights lights instructions trans-fn)
    (count-lights lights count-fn)))

;;; Part 1

(define (trans-state command lights)
  (match command
    ['|turn on| (lambda (x y) (matrix-set! lights x y #t))]
    ['|turn off| (lambda (x y) (matrix-set! lights x y #f))]
    ['toggle (lambda (x y) (matrix-set! lights x y
                                        (not (matrix-ref lights x y))))]))

(define (count-state lights x y)
  (if (matrix-ref lights x y) 1 0))

;;; Part 2

(define (trans-brightness command lights)
  (match command
    ['|turn on| (lambda (x y) (matrix-set! lights x y (add1 (matrix-ref lights x y))))]
    ['|turn off| (lambda (x y) (matrix-set! lights x y (max 0 (sub1 (matrix-ref lights x y)))))]
    ['toggle (lambda (x y) (matrix-set! lights x y (+ 2 (matrix-ref lights x y))))]))

(define (count-brightness lights x y)
  (matrix-ref lights x y))

;;; Entry point

(module+ main
  (printf "AoC 2015 Day 6 - Probably a Fire Hazard~n")
  (let ([instructions (read-instructions "input/6.txt")])
    (printf "1: ~a~n" (setup-count instructions #f trans-state count-state))
    (printf "1: ~a~n" (setup-count instructions 0 trans-brightness count-brightness))))

