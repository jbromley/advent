#lang racket/base
(require racket/contract)
(require racket/list)
(require racket/match)
(require racket/port)
(require racket/string)

;;; Part 1

(define/contract (total-distance input-file)
  (-> string? integer?)
  (let* ([map-in (expand (read-input input-file))]
         [galaxies (chart-galaxies map-in)])
    (for/fold ([distance 0])
              ([galaxy-pairs (combinations galaxies 2)])
      (match galaxy-pairs
        [(list (cons r1 c1) (cons r2 c2))
         (+ distance (+ (abs (- r2 r1)) (abs (- c2 c1))))]))))

;;; Common code
(define/contract (expand-vertical map-in map-out)
   (-> list? list? list?)
   (match map-in
     ['() (reverse map-out)]
     [(cons row rows)
      (let ([new-map-out (cons row map-out)])
        (if (non-empty-string? (string-replace row "." ""))
          (expand-vertical rows new-map-out)
          (expand-vertical rows (cons row new-map-out))))]))

(define/contract (expand-horizontal map-in)
  (-> list? list?)
  (transpose (expand-vertical (transpose map-in) '())))

(define/contract (expand map-in)
  (-> list? list?)
  (expand-horizontal (expand-vertical map-in '())))
  
(define/contract (read-input input-file)
  (-> string? list?)
  (string-split 
    (port->string (open-input-file input-file) #:close? #t) "\n" #:trim? #t))

(define/contract (transpose lst)
  (-> list? list?)
  (let ([s-list (map string->list lst)])
    (map list->string (apply map list s-list))))

(define/contract (chart-galaxies map-in)
  (-> list? list?)
  (for/fold ([galaxies '()])
            ([row map-in]
             [index (in-naturals)])
    (let ([new-galaxies (map (lambda (col) (cons index col)) (map (lambda (cell) (car cell)) (regexp-match-positions* #rx"#" row)))])
      (values (append galaxies new-galaxies)))))

;;; Entry point
(total-distance "input.txt")

;;; Tests
(module+ test
  (require rackunit)

  (define test-cases '(("input1.txt" 374)))

  (for ([test test-cases])
    (let ([input-file (first test)]
          [result (second test)])
      (check-equal? (total-distance input-file) result))))