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
           (list springs (list->vector (map string->number groups)))])))))

(define (scan-groups springs spring-index groups group-index)
  (let ([springs-len (string-length springs)]
        [groups-len (vector-length groups)])
    (cond
      [(>= spring-index springs-len)
       (if (= group-index groups-len) 1 0)]
      [else
       (match (string-ref springs spring-index)
         [#\. (scan-groups springs (add1 spring-index) groups group-index)]
         [#\# (match-group springs spring-index groups group-index)]
         [#\? (+ (scan-groups springs (add1 spring-index) groups group-index)
                 (match-group springs spring-index groups group-index))])])))

(define (match-group springs spring-index groups group-index)
  (let ([springs-len (string-length springs)]
        [groups-len (vector-length groups)])
    (cond
      [(= group-index groups-len) 0]
      [else
       (let ([end-index (+ spring-index (vector-ref groups group-index))])
         (if (and (for/and ([i (in-range spring-index end-index)])
                    (and (< i springs-len) (not (char=? #\. (string-ref springs i)))))
                  (or (= end-index springs-len)
                      (not (char=? #\# (string-ref springs end-index)))))
             (scan-groups springs (add1 spring-index) groups (add1 group-index))
             0))])))

;;; Part 1

(define (count-rec-arrangements rec)
  (let* ([springs (first rec)]
         [groups (second rec)])
  (scan-groups springs 0 groups 0)))
  
(define (count-arrangements records)
  (for/sum ([r records])
    (count-rec-arrangements r)))
  

(define recs1 (read-records "input1.txt"))
(define recs (read-records "input.txt"))

