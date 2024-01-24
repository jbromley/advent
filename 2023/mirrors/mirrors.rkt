#lang racket/base

(require racket/contract)
(require racket/list)
(require racket/match)
(require racket/port)
(require racket/string)

;;; Common code

(define/contract (read-input input-file)
  (-> string? list?)
  (let ([in (port->lines (open-input-file input-file) #:line-mode 'linefeed)])
    (parse-input in '() '())))

(define/contract (parse-input in current-image images)
  (-> list? list? list? list?)
  (cond
    [(empty? in)
     (reverse (cons (reverse current-image) images))]
    [(not (non-empty-string? (first in)))
     (parse-input (rest in) '() (cons (reverse current-image) images))]
    [else
     (parse-input (rest in) (cons (string->list (first in)) current-image) images)]))

(define/contract (sublist lst start len)
  (-> list? integer? integer? list?)
  (take (drop lst start) len))

(define-syntax-rule
  (for/count iter
    body ...
    lastexpr)
  (for/sum iter
    body ...
    (if lastexpr
        1
        0)))

(define/contract (reflect? rows above diffs)
  (-> list? integer? integer? boolean?)
  (let* ([below (- (length rows) above)]
         [min-len (min above below)]
         [skip-above (- above min-len)])
    (= diffs
       (for/sum ([above-rows (sublist rows skip-above min-len)]
                 [below-rows (reverse (sublist rows above min-len))])
         (for/count ([v1 above-rows]
                     [v2 below-rows])
           (not (char=? v1 v2)))))))

(define/contract (find-reflect-line-rows type rows diffs)
  (-> symbol? list? integer? (or/c list? #f))
  (for/first ([above-rows (in-range 1 (length rows))]
              #:when (reflect? rows above-rows diffs))
    (list type above-rows)))
  
(define/contract (transpose image)
  (-> list? list?)
  (apply map list image))

(define/contract (find-reflect-line rows diffs)
  (-> list? integer? (or/c list? #f))
  (or (find-reflect-line-rows 'row rows diffs)
      (find-reflect-line-rows 'col (transpose rows) diffs)))

(define/contract (solve mirrors diffs)
  (-> list? integer? integer?)
  (for/sum ([mirror mirrors])
    (match (find-reflect-line mirror diffs)
      [(list 'row above) (* 100 above)]
      [(list 'col left) left])))

;;; Entry point

(module+ main
  (let ([mirrors (read-input "input.txt")])
    (begin
     (printf "AoC 2023 Day 13 - Point of Incidence~n")
     (printf "Part 1: ~a~n" (solve mirrors 0))
     (printf "Part 2: ~a~n" (solve mirrors 1)))))

;;; Tests
(module+ test
  (require rackunit)

  (define test-cases '((0 405)
                       (1 400)))
  (define mirrors (read-input "input1.txt"))

  (for ([test test-cases])
    (let ([smudges (first test)]
          [result (second test)])
      (check-equal? (solve mirrors smudges) result))))
