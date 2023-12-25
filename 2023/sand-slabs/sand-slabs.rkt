#lang racket

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
    (build-vector z (λ (altitude) (build-vector y (λ (row) (make-vector x 'none)))))))

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
                  [id 1]
                  #:result (list
                            (list->vector (reverse bricks))
                            bricks-by-z
                            (map-space (make-space dims) (reverse bricks))))
                 ([line (in-lines port)])
        (let ([brick (list->vector (map string->number (string-split line #rx"[,~]")))])
          (display brick)
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
        [index (in-naturals 1)])
    (brick-apply brick (λ (x y z) (space-set! space x y z index))))
  space) 
    

;;; Entry point

(define results (read-slabs "input1.txt"))
(define bricks (first results))
(define by-z (second results))
(define space (third results))
