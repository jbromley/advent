#lang racket/base
(require racket/list)
(require racket/match)
(require racket/string)

(struct vec [x y z] #:transparent)

(define (read-snowflakes input-file)
  (call-with-input-file input-file
    (lambda (port)
      (for/vector ([line (in-lines port)])
        (map string->number
             (filter non-empty-string? (string-split line #rx"[@, ]" #:trim? #t)))))))

(define (det flk1 flk2)
  (match-let ([(list _px1 _py1 _pz1 vx1 vy1 _vz1) flk1]
              [(list _px2 _py2 _pz2 vx2 vy2 _vz2) flk2])
    (- (* vy1 vx2) (* vx1 vy2))))

(define (det-t0 flk1 flk2)
  (match-let ([(list px1 py1 _pz1 _vx1 _vy1 _vz1) flk1]
              [(list px2 py2 _pz2 vx2 vy2 _vz2) flk2])
    (- (* vx2 (- py2 py1)) (* vy2 (- px2 px1)))))

(define (det-t1 flk1 flk2)
  (match-let ([(list px1 py1 _pz1 vx1 vy1 _vz1) flk1]
              [(list px2 py2 _pz2 _vx2 _vy2 _vz2) flk2])
    (- (* vx1 (- py2 py1)) (* vy1 (- px2 px1)))))

(define (intersect? flake1 flake2 bounds)
  (match-let* ([d (det flake1 flake2)]
               [(list px0 py0 _pz0 vx0 vy0 _vz0) flake1]
               [lo (first bounds)]
               [hi (second bounds)])
    (if (zero? d)
        0
        (let* ([t0 (/ (det-t0 flake1 flake2) d)]
               [t1 (/ (det-t1 flake1 flake2) d)]
               [x (+ px0 (* t0 vx0))]
               [y (+ py0 (* t0 vy0))])
          (if (and (>= t0 1) (>= t1 1) (<= lo x hi) (<= lo y hi))
              1
              0)))))

;;; Part 1

(define (count-intersections flakes bounds)
  (let ([n (vector-length flakes)])
    (for*/sum ([i n])
      (for*/sum ([j (in-range (add1 i) n)])
        (intersect? (vector-ref flakes i) (vector-ref flakes j) bounds)))))

;;; Part 2

;; https://rosettacode.org/wiki/Reduced_row_echelon_form#Scheme
(define (reduced-row-echelon-form matrix)
  (define (clean-down matrix from-row column)
    (cons (car matrix)
          (if (zero? from-row)
              (map (lambda (row)
                     (map -
                          row
                          (map (lambda (element)
                                 (/ (* element (list-ref row column))
                                    (list-ref (car matrix) column)))
                               (car matrix))))
                   (cdr matrix))
              (clean-down (cdr matrix) (- from-row 1) column))))
  (define (clean-up matrix until-row column)
    (if (zero? until-row)
        matrix
        (cons (map -
                   (car matrix)
                   (map (lambda (element)
                          (/ (* element (list-ref (car matrix) column))
                             (list-ref (list-ref matrix until-row) column)))
                        (list-ref matrix until-row)))
              (clean-up (cdr matrix) (- until-row 1) column))))
  (define (normalise matrix row with-column)
    (if (zero? row)
        (cons (map (lambda (element)
                     (/ element (list-ref (car matrix) with-column)))
                   (car matrix))
              (cdr matrix))
        (cons (car matrix) (normalise (cdr matrix) (- row 1) with-column))))
  (define (repeat procedure matrix indices)
    (if (null? indices)
        matrix
        (repeat procedure
                (procedure matrix (car indices) (car indices))
                (cdr indices))))
  (define (iota start stop)
    (if (> start stop)
        (list)
        (cons start (iota (+ start 1) stop))))
  (let ((indices (iota 0 (- (length matrix) 1))))
    (repeat normalise
            (repeat clean-up
                    (repeat clean-down
                            matrix
                            indices)
                    indices)
            indices)))

(define (calculate-rock-trajectory flakes)
  (match-let* ([(list px1 py1 pz1 vx1 vy1 vz1) (vector-ref flakes 0)]
               [(list px2 py2 pz2 vx2 vy2 vz2) (vector-ref flakes 1)]
               [(list px3 py3 pz3 vx3 vy3 vz3) (vector-ref flakes 2)]
               [A (list (list (- vy2 vy1) (- vx1 vx2) 0 (- py1 py2) (- px2 px1) 0)
                        (list (- vy3 vy1) (- vx1 vx3) 0 (- py1 py3) (- px3 px1) 0)
                        (list 0 (- vz2 vz1) (- vy1 vy2) 0 (- pz1 pz2) (- py2 py1))
                        (list 0 (- vz3 vz1) (- vy1 vy3) 0 (- pz1 pz3) (- py3 py1))
                        (list (- vz2 vz1) 0 (- vx1 vx2) (- pz1 pz2) 0 (- px2 px1))
                        (list (- vz3 vz1) 0 (- vx1 vx3) (- pz1 pz3) 0 (- px3 px1)))]
               [b (list (- (- (* py1 vx1) (* py2 vx2)) (- (* px1 vy1) (* px2 vy2)))
                        (- (- (* py1 vx1) (* py3 vx3)) (- (* px1 vy1) (* px3 vy3)))
                        (- (- (* pz1 vy1) (* pz2 vy2)) (- (* py1 vz1) (* py2 vz2)))
                        (- (- (* pz1 vy1) (* pz3 vy3)) (- (* py1 vz1) (* py3 vz3)))
                        (- (- (* pz1 vx1) (* pz2 vx2)) (- (* px1 vz1) (* px2 vz2)))
                        (- (- (* pz1 vx1) (* pz3 vx3)) (- (* px1 vz1) (* px3 vz3))))]
               [aug-mat (map (lambda (a b) (append a (list b))) A b)])
    (apply + (take (map last (reduced-row-echelon-form aug-mat)) 3))))

;;; Entry point

(define flakes (read-snowflakes "input1.txt"))

(module+ main
  (printf "AoC 2023 Day 24 - Never Tell Me the Odds~n")
  (let ([flakes (read-snowflakes "input.txt")]
        [bounds (list 200000000000000 400000000000000)])
    (printf "Part 1: ~a~n" (count-intersections flakes bounds))
    (printf "Part 2: ~a~n" (calculate-rock-trajectory flakes))))

