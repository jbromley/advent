#lang racket/base
(require racket/list)
(require racket/match)
(require racket/string)

(define (read-records input-file)
  (call-with-input-file input-file
    (λ (port)
      (for/list ([line (in-lines port)])
        (match (string-split line #px"[ ,]")
          [(cons springs groups)
           (list (list->vector (string->list springs)) (list->vector (map string->number groups)))])))))

(define (count-arrangements springs groups)
  (letrec ([springs-len (vector-length springs)]
           [groups-len (vector-length groups)]
           [next-group
            (λ (i-spring i-group)
              (cond
                [(>= i-spring springs-len)
                 (if (= i-group groups-len) 1 0)]
                [else
                 (match (vector-ref springs i-spring)
                   [#\. (next-group (add1 i-spring) i-group)]
                   [#\# (match-group i-spring i-group)]
                   [#\? (+ (next-group (add1 i-spring) i-group)
                           (match-group i-spring i-group))])]))]
           [match-group
            (λ (i-spring i-group)
              (cond
                [(= i-group groups-len) 0]
                [else
                 (let ([i-end (+ i-spring (vector-ref groups i-group))])
                   (if (and (for/and ([i (in-range i-spring i-end)])
                              (and (< i springs-len)
                                   (not (char=? #\. (vector-ref springs i)))))
                            (or (= i-end springs-len)
                                (not (char=? #\# (vector-ref springs i-end)))))
                      (next-group (add1 i-end) (add1 i-group))
                      0))]))])
    (next-group 0 0)))

;;; Part 1

(define (count-all-arrangements records)
  (for/sum ([r records])
    (let ([springs (first r)]
          [groups (second r)])
     (count-arrangements springs groups))))

;;; Entry point

(define recs1 (read-records "input1.txt"))
(define recs (read-records "input.txt"))

(define (main)
  (let ([records (read-records "input.txt")])
    (printf "AoC 2023 day 12 - Hot Springs~n")
    (printf "Part 1: ~a~n" (count-all-arrangements records))))
