#lang racket/base
(require racket/list)
(require racket/match)
(require racket/string)

(struct vec [x y z] #:transparent)

(define (read-snowflakes input-file)
  (call-with-input-file input-file
    (lambda (port)
      (for/vector ([line (in-lines port)])
        (map (lambda (vlist) (apply vec vlist))
             (map (lambda (part) (map (lambda (p) (string->number (string-trim p)))
                                      (string-split part ", " #:trim? #t)))
                  (string-split line "@" #:trim? #t)))))))

(define (det flk1 flk2)
  (match-let ([(list _p1 v1) flk1]
              [(list _p2 v2) flk2])
    (- (* (vec-y v1) (vec-x v2)) (* (vec-x v1) (vec-y v2)))))

(define (det-t0 flk1 flk2)
  (match-let ([(list p1 _v1) flk1]
              [(list p2 v2) flk2])
    (- (* (vec-x v2) (- (vec-y p2) (vec-y p1))) (* (vec-y v2) (- (vec-x p2) (vec-x p1))))))

(define (det-t1 flk1 flk2)
  (match-let ([(list p1 v1) flk1]
              [(list p2 _v2) flk2])
    (- (* (vec-x v1) (- (vec-y p2) (vec-y p1))) (* (vec-y v1) (- (vec-x p2) (vec-x p1))))))

(define (intersect? flake1 flake2 bounds)
  (match-let* ([d (det flake1 flake2)]
               [(list p0 v0) flake1]
               [lo (first bounds)]
               [hi (second bounds)])
    (if (zero? d)
        0
        (let* ([t0 (/ (det-t0 flake1 flake2) d)]
               [t1 (/ (det-t1 flake1 flake2) d)]
               [x (+ (vec-x p0) (* t0 (vec-x v0)))]
               [y (+ (vec-y p0) (* t0 (vec-y v0)))])
          (if (and (>= t0 1) (>= t1 1) (<= lo x hi) (<= lo y hi))
              1
              0)))))

;;; Part 1

(define (count-intersections flakes bounds)
  (let ([n (vector-length flakes)])
    (for*/sum ([i n])
      (for*/sum ([j (in-range (add1 i) n)])
        (intersect? (vector-ref flakes i) (vector-ref flakes j) bounds)))))

;;; Entry point

(define flakes (read-snowflakes "input1.txt"))

(module+ main
  (printf "AoC 2023 Day 24 - Never Tell Me the Odds~n")
  (let ([flakes (read-snowflakes "input.txt")]
        [bounds (list 200000000000000 400000000000000)])
    (printf "Part 1: ~a~n" (count-intersections flakes bounds))))

