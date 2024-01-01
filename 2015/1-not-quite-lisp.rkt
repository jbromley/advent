#lang racket/base
(require racket/list)
(require racket/match)
(require racket/string)

(define (read-input input-file)
  (call-with-input-file input-file
    (lambda (port)
      (let ([s (read-line port)])
        (string->list (string-trim s))))))

(define input (read-input "1.txt"))

;;; Part 1 

(define (count-levels lst [level 0])
  (cond
    [(empty? lst) level]
    [else
      (match (first lst)
        [#\( (count-levels (rest lst) (add1 level))]
        [#\) (count-levels (rest lst) (sub1 level))])]))

;;; Part 2 

(define (find-basement lst [pos 1] [level 0])
  (cond
    [(= level -1) (sub1 pos)]
    [(char=? #\( (first lst))
     (find-basement (rest lst) (add1 pos) (add1 level))]
    [(char=? #\) (first lst))
     (find-basement (rest lst) (add1 pos) (sub1 level))]))

(module+ main 
  (printf "AoC 2015 Day 1 - Not Quite Lisp~n")
  (let ([input (read-input "1.txt")])
    (printf "1: ~a~n" (count-levels input))
    (printf "2: ~a~n" (find-basement input))))
