#lang racket/base
(require racket/list)
(require racket/match)
(require racket/string)
(require threading)

(define (read-input input-file)
  (call-with-input-file input-file
    (lambda (port)
      (for/list ([line (in-lines port)])
        (string->list line)))))

(define (transpose lstlst)
  (apply map list lstlst))

(define (roll-row row order-by)
  (~> row 
      list->string
      (string-split "#" #:trim? #f)
      (map string->list _)
      (map (λ (lst) (sort lst order-by)) _)
      (map list->string _)
      (string-join _ "#")
      string->list))

(define (roll-row-west row)
  (roll-row row char>?))

(define (roll-row-east row)
  (roll-row row char<?))

(define (roll-platform platform dir)
  (match dir
    ['west (map roll-row-west platform)]
    ['east (map roll-row-east platform)]
    ['north (~> platform transpose (map roll-row-west _) transpose)]
    ['south (~> platform transpose (map roll-row-east _) transpose)]))

(define (total-load platform)
  (for/sum ([row (reverse platform)]
            [row-num (in-naturals 1)])
    (* row-num (count (lambda (ch) (char=? ch #\O)) row))))

;;; Part 1

(define (total-north-load platform)
  (total-load (roll-platform platform 'north)))

;;; Part 2

(define (cycle-platform platform)
  (~> platform
      (roll-platform 'north)
      (roll-platform 'west)
      (roll-platform 'south)
      (roll-platform 'east)))

(define track-cycles
  (let ([platform-id (make-hash)]
        [id-platform (make-hash)])
    (lambda (platform id num-cycles)
      (cond
        [(hash-has-key? platform-id platform)
         (let* ([cycle-start (hash-ref platform-id platform)]
                [cycle-len (- id cycle-start)]
                [remainder (modulo id cycle-len)])
             (hash-ref id-platform (+ cycle-start (modulo (- num-cycles cycle-start) cycle-len))))]
        [else
          (hash-set! platform-id platform id)
          (hash-set! id-platform id platform)
          (track-cycles (cycle-platform platform) (add1 id) num-cycles)]))))

(define (total-cycle-load platform)
  (total-load (track-cycles platform 0 1000000000)))

;;; Entry point

(module+ main
  (let ([platform (read-input "input.txt")])
    (printf "AoC 2023 Day 14 - Parabolic Reflector Dish~n")
    (printf "Part 1: ~a~n" (total-north-load platform))
    (printf "Part 2: ~a~n" (total-cycle-load platform))))
