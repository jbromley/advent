#lang racket/base
(require racket/list)
(require racket/string)

(define (read-input input-file)
  (call-with-input-file input-file
    (lambda (port)
      (for/list ([line (in-lines port)])
        (map string->number (string-split line "x"))))))

;;; Part 1

(define (total-area dim-lst)
  (for/sum ([dims dim-lst])
    (apply gift-area dims)))

(define (gift-area l w h)
  (for/fold ([area 0]
             [min-side #f]
             #:result (+ area min-side))
            ([sides (list (cons l w) (cons l h) (cons w h))])
    (let ([side-area (* (car sides) (cdr sides))])
      (values (+ area (* 2 side-area))
              (if (not min-side) side-area (min min-side side-area))))))

;;; Part 2

(define (total-ribbon-length dim-lst)
  (for/sum ([dims dim-lst])
    (apply ribbon-length dims)))

(define (ribbon-length l w h)
  (for/fold ([min-perim #f]
             #:result (+ min-perim (* l w h)))
            ([sides (list (cons l w) (cons l h) (cons w h))])
    (let ([perim (* 2 (+ (car sides) (cdr sides)))])
      (if (not min-perim) perim (min perim min-perim)))))

;;; Entry point

(module+ main
  (printf "Aoc 205 Day 2 - I Was Told There Would Be No Math~n")
  (let ([input (read-input "2.txt")])
    (printf "Part 1: ~a~n" (total-area input))
    (printf "Part 2: ~a~n" (total-ribbon-length input))))
