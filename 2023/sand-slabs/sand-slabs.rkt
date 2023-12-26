#lang racket
(require racket/hash)

(define (brick-zs brick)
  (list (vector-ref brick 2) (vector-ref brick 5)))

(define (brick-x-range brick)
  (in-range (vector-ref brick 0) (add1 (vector-ref brick 3))))

(define (brick-y-range brick)
  (in-range (vector-ref brick 1) (add1 (vector-ref brick 4))))

(define (brick-z-range brick)
  (in-range (vector-ref brick 2) (add1 (vector-ref brick 5))))

(define (update-dims dims brick)
  (list (max (first dims) (add1 (vector-ref brick 0)) (add1 (vector-ref brick 3)))
        (max (second dims) (add1 (vector-ref brick 1)) (add1 (vector-ref brick 4)))
        (max (third dims) (add1 (vector-ref brick 2)) (add1 (vector-ref brick 5)))))

(define (make-space dims)
  (match-let ([(list x y z) dims])
    (build-vector z (λ (altitude) (build-vector y (λ (row) (make-vector x #f)))))))

(define (space-ref space x y z)
  (vector-ref (vector-ref (vector-ref space z) y) x))

(define (space-set! space x y z value)
  (vector-set! (vector-ref (vector-ref space z) y) x value))

(define (read-slabs input-file)
  (call-with-input-file input-file 
    (lambda (port)
      (for*/fold ([bricks '()]
                  [bricks-by-z (hash)]
                  [dims '(0 0 0)]
                  [id 0]
                  #:result (list
                            (list->vector (reverse bricks))
                            bricks-by-z
                            (map-space (make-space dims) (reverse bricks))))
                 ([line (in-lines port)])
        (let ([brick (list->vector (map string->number (string-split line #rx"[,~]")))])
          (values (cons brick bricks)
                  (hash-update bricks-by-z (apply min (brick-zs brick)) (λ (bricks) (cons id bricks)) (list))
                  (update-dims dims brick)
                  (add1 id)))))))

(define (brick-apply brick fn)
  (for ([z (brick-z-range brick)])
    (for ([y (brick-y-range brick)])
      (for ([x (brick-x-range brick)])
        (fn x y z)))))

(define (map-space space bricks)
  (for ([brick bricks]
        [index (in-naturals)])
    (brick-apply brick (λ (x y z) (space-set! space x y z index))))
  space)

;;; Part 1

(define (count-breakable-bricks bricks bricks-by-z space)
  (let ([deps (find-bricks-supports bricks bricks-by-z space)])
    (- (vector-length bricks) (set-count (foldl (λ (s acc) (set-union! acc s) acc) (mutable-set) (filter (λ (s) (= 1 (set-count s))) (hash-values deps)))))))

(define (find-bricks-supports bricks bricks-by-z space)
  (for/fold ([deps (hash)])
            ([z (sort (hash-keys bricks-by-z) <)])
    (hash-union deps
                (for/hash ([id (hash-ref bricks-by-z z)])
                  (let ([brick (vector-ref bricks id)])
                    (find-brick-support brick id space))))))

(define (find-brick-support brick id space)
  (let ([orig-brick (vector-copy brick)]
        [supporters (mutable-set)])
    (for ([_ (in-naturals)])
      #:break (or (zero? (vector-ref brick 2))
                  (zero? (vector-ref brick 5))
                  (not (set-empty? supporters)))
      (vector-set! brick 2 (sub1 (vector-ref brick 2)))
      (vector-set! brick 5 (sub1 (vector-ref brick 5)))
      (brick-apply brick (λ (x y z)
                           (when (and (space-ref space x y z)
                                      (not (= id (space-ref space x y z))))
                                 (set-add! supporters (space-ref space x y z))))))
    (vector-set! brick 2 (add1 (vector-ref brick 2)))
    (vector-set! brick 5 (add1 (vector-ref brick 5)))
    (when (not (eq? brick orig-brick))
      (brick-apply orig-brick (λ (x y z) (space-set! space x y z #f)))
      (brick-apply brick (λ (x y z) (space-set! space x y z id))))
    (values id supporters)))
      
      
      
  
    
    

;;; Entry point

(define results (read-slabs "input.txt"))
(define bricks (first results))
(define by-z (second results))
(define space (third results))
