#lang racket
(require data/queue)

(define (brick-zs brick)
  (list (vector-ref brick 2) (vector-ref brick 5)))

(define (brick-x-range brick)
  (in-range (vector-ref brick 0) (add1 (vector-ref brick 3))))

(define (brick-y-range brick)
  (in-range (vector-ref brick 1) (add1 (vector-ref brick 4))))

(define (brick-z-range brick)
  (in-range (vector-ref brick 2) (add1 (vector-ref brick 5))))

(define (brick-down! brick)
  (vector-set! brick 2 (sub1 (vector-ref brick 2)))
  (vector-set! brick 5 (sub1 (vector-ref brick 5))))

(define (brick-up! brick)
  (vector-set! brick 2 (add1 (vector-ref brick 2)))
  (vector-set! brick 5 (add1 (vector-ref brick 5))))

(define (brick-hit-ground? brick)
  (or (zero? (vector-ref brick 2))
      (zero? (vector-ref brick 5))))

(define (brick-move! space id start-pos end-pos)
  (when (not (eq? start-pos end-pos))
    (brick-apply start-pos (λ (x y z) (space-set! space x y z #f)))
    (brick-apply end-pos (λ (x y z) (space-set! space x y z id)))))

(define (update-dims dims brick)
  (list (max (first dims) (add1 (vector-ref brick 0)) (add1 (vector-ref brick 3)))
        (max (second dims) (add1 (vector-ref brick 1)) (add1 (vector-ref brick 4)))
        (max (third dims) (add1 (vector-ref brick 2)) (add1 (vector-ref brick 5)))))

(define (make-space dims)
  (match-let ([(list x y z) dims])
    (build-vector z (λ (_) (build-vector y (λ (_) (make-vector x #f)))))))

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

(define (find-bricks-supports bricks bricks-by-z space)
  (for*/fold ([bricks-under (hash)]
              [bricks-on-top (hash)]
              #:result (list bricks-under bricks-on-top))
             ([z (sort (hash-keys bricks-by-z) <)]
              [id (hash-ref bricks-by-z z)])
    (let* ([brick (vector-ref bricks id)]
           [supports (find-brick-support brick id space)])
      (values (hash-set bricks-under id supports)
              (update-bricks-on-top bricks-on-top id supports)))))


(define (find-brick-support brick id space)
  (let ([orig-brick (vector-copy brick)]
        [supports (mutable-set)])
    (for ([_ (in-naturals)])
      #:break (or (brick-hit-ground? brick)
                  (not (set-empty? supports)))
      (brick-down! brick)
      (brick-apply brick (λ (x y z)
                           (when (and (space-ref space x y z)
                                      (not (= id (space-ref space x y z))))
                             (set-add! supports (space-ref space x y z))))))
    (brick-up! brick)
    (brick-move! space id orig-brick brick)
    supports))

(define (update-bricks-on-top on-top id supports)
  (for/fold ([bricks-on-top on-top])
            ([support supports])
    (hash-update bricks-on-top support (λ (v) (cons id v)) (list))))

;;; Part 1

(define (count-breakable-bricks bricks bricks-by-z space)
  (let ([deps (first (find-bricks-supports bricks bricks-by-z space))])
    (- (vector-length bricks)
       (count-unbreakable-bricks deps))))

(define (count-unbreakable-bricks deps-hash)
  (let ([deps (hash-values deps-hash)])
    (set-count (foldl (λ (s acc) (set-union! acc s) acc)
                      (mutable-set)
                      (filter (λ (s) (= 1 (set-count s))) deps)))))

;;; Part 2

(define (count-all-bricks-to-fall bricks bricks-by-z space)
  (match-let ([(list bricks-under bricks-on-top)
               (find-bricks-supports bricks bricks-by-z space)]
              [q (make-queue)])
    (for/sum ([brick (in-range (vector-length bricks))])
      (count-bricks-to-fall (update-queue q brick bricks-on-top)
                            bricks-on-top
                            bricks-under
                            (set brick)))))
      

(define (count-bricks-to-fall brick-queue bricks-on-top bricks-under [fallen (set)])
  (cond
    [(queue-empty? brick-queue)
     (sub1 (set-count fallen))]
    [else
     (let ([brick (dequeue! brick-queue)])
       (if (for/and ([dep (hash-ref bricks-under brick)])
             (set-member? fallen dep))
           (count-bricks-to-fall (update-queue brick-queue brick bricks-on-top) bricks-on-top bricks-under (set-add fallen brick))
           (count-bricks-to-fall brick-queue bricks-on-top bricks-under fallen)))]))

(define (update-queue q brick bricks-on-top)
  (for ([brick (hash-ref bricks-on-top brick (list))])
    (when brick (enqueue! q brick)))
  q)

;;; Entry point

(define inputs (read-slabs "input.txt"))
(define bricks (first inputs))
(define by-z (second inputs))
(define space (third inputs))
(define g (find-bricks-supports bricks by-z space))
(define under (first g))
(define on-top (second g))
(define q (make-queue))
(enqueue! q 1)
(enqueue! q 2)


(module+ main
  (let* ([inputs (read-slabs "input.txt")]
         [bricks (first inputs)]
         [by-z (second inputs)]
         [space (third inputs)])
    (printf "AoC 2023 Day 22 - Sand Slabs~n")
    (printf "Part 1: ~a~n" (count-breakable-bricks bricks by-z space))))

