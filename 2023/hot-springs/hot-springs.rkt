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
           (list springs (map string->number groups))])))))

(define-syntax-rule (memoize-fn! fn)
  (set! fn (memoize-fn fn)))

(define (memoize-fn fn)
  (let ([cache (make-hash)]
        [orig-fn fn])
    (λ args
      (when (not (hash-has-key? cache args))
        (hash-set! cache args (apply orig-fn args)))
      (hash-ref cache args))))
      
(define (count-arrangements springs-str groups-lst)
  (letrec ([springs (list->vector (string->list springs-str))]
           [springs-len (vector-length springs)]
           [groups (list->vector groups-lst)]
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

    (memoize-fn! next-group)
    (memoize-fn! match-group)
    
    (next-group 0 0)))

;;; Part 1

(define (count-all-arrangements records)
  (for/sum ([r records])
    (let ([springs (first r)]
          [groups (second r)])
     (count-arrangements springs groups))))

;;; Part 2

(define (repeat lst n)
  (if (= n 1)
      lst
      (append lst (repeat lst (sub1 n)))))

(define (unfold record)
  (list (string-join (make-list 5 (first record)) "?")
        (repeat (second record) 5)))

(define (count-unfolded-arrangements records)
  (for/sum ([r records])
    (let* ([unfolded-rec (unfold r)]
           [springs (first unfolded-rec)]
           [groups (second unfolded-rec)])
      (count-arrangements springs groups))))
  

;;; Entry point

(module+ main
  (let ([records (read-records "input.txt")])
    (printf "AoC 2023 day 12 - Hot Springs~n")
    (printf "Part 1: ~a~n" (count-all-arrangements records))
    (printf "Part 2: ~a~n" (count-unfolded-arrangements records))))
