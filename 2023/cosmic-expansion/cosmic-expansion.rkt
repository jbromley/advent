#lang racket/base
(require racket/contract)
(require racket/list)
(require racket/match)
(require racket/port)
(require racket/set)
(require racket/string)

;;; Part 1 and 2 (only expansion changes)

(define/contract (total-distance input-file expansion)
  (-> string? integer? integer?)
  (let ([galaxies (read-input input-file expansion)])
    (sum-distances galaxies)))

;;; Common code

(define/contract (sum-distances galaxies)
  (-> list? integer?)
  (for/fold ([distance 0])
              ([galaxy-pairs (combinations galaxies 2)])
      (match galaxy-pairs
        [(list (cons r1 c1) (cons r2 c2))
         (+ distance (+ (abs (- r2 r1)) (abs (- c2 c1))))])))
  
(define/contract (read-input input-file expansion)
  (-> string? integer? list?)
  (let ([map-in (port->lines (open-input-file input-file) #:line-mode 'linefeed)])
    (chart-galaxies map-in expansion)))        

(define/contract (chart-galaxies map-in expansion)
  (-> list? integer? list?)
  (let ([num-columns (string-length (first map-in))])
    (for*/fold ([galaxy-map '()]
                [row 0]
                [empty-columns (list->set (in-range num-columns))]
                #:result (expand-horizontal galaxy-map empty-columns expansion))
               ([line map-in])
      (let* ([galaxy-columns (map car (regexp-match-positions* #rx"#" line))]
             [new-galaxies (map (lambda (col) (cons row col)) galaxy-columns)]
             [row-increment (if (empty? galaxy-columns) expansion 1)])
        (values (append galaxy-map new-galaxies)
                (+ row row-increment)
                (set-subtract empty-columns (list->set galaxy-columns)))))))

(define/contract (expand-horizontal map-in empty-columns expansion)
  (-> list? set? integer? list?)
  (for/fold ([chart map-in]
             #:result (sort chart < #:key car))
            ([column (set->list empty-columns)])
    (let-values ([(shifted fixed) (partition (lambda (pos) (> (cdr pos) column)) chart)])
      (append fixed (map (lambda (pos) (cons (car pos) (+ (cdr pos) (- expansion 1)))) shifted)))))
  
;;; Entry point

(printf "Day 11 - Cosmic Expansion~n")
(printf "Total distance, expansion 2: ~a~n" (total-distance "input.txt" 2))
(printf "Total distance, expansion 1,000,000: ~a~n" (total-distance "input.txt" 1000000))


;;; Tests
(module+ test
  (require rackunit)

  (define test-cases '(("input1.txt" 2 374)
                       ("input1.txt" 10 1030)
                       ("input1.txt" 100 8410)))

  (for ([test test-cases])
    (let ([input-file (first test)]
          [expansion (second test)]
          [result (third test)])
      (check-equal? (total-distance input-file expansion) result))))
