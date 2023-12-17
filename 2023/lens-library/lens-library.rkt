#lang racket/base
(require racket/list)
(require racket/match)
(require racket/port)
(require racket/string)

(define (read-input input-file)
  (let ([in (string-trim (port->string (open-input-file input-file)))])
    (string-split in "," #:trim? #t)))

(define (holiday-hash s)
  (for/fold ([code 0])
            ([ch (string->list s)])
    (modulo (* (+ code (char->integer ch)) 17) 256)))

;;; Part 1

(define (sum-holiday-hashes init-seqs)
  (for/sum ([seq init-seqs])
    (holiday-hash seq)))

;;; Part 2

(struct lens (label focal-length) #:transparent)

(define (lens=? lens label)
  (string=? label (lens-label lens)))
                
(define (remove-lens box label)
  (match (index-of box label lens=?)
    [#f box]
    [index (append (take box index) (drop box (add1 index)))]))

(define (install-lens box lens)
  (match (index-of box (lens-label lens) lens=?)
    [#f (cons lens box)]
    [index (append (take box index) (list lens) (drop box (add1 index)))]))

(define (parse-init-seq seq)
  (match (flatten (regexp-match* #rx"([a-z]+)([-|=])([0-9]?)" seq #:match-select cdr))
    [(list label "-" "") (list 'remove (holiday-hash label) label)]
    [(list label "=" focal-length) (list 'install (holiday-hash label) label (string->number focal-length))])) 
  
(define (install-lenses init-seqs)
  (for/fold ([boxes (hash)])
            ([seq init-seqs])
    (match (parse-init-seq seq)
      [(list 'install box label focal-length)
       (let ([new-lens (lens label focal-length)])
         (hash-update boxes box (lambda (lenses) (install-lens lenses new-lens)) (list new-lens)))]
      [(list 'remove box label)
       (hash-update boxes box (lambda (lenses) (remove-lens lenses label)) '())])))
  
(define (focusing-power boxes)
  (for/fold ([lens-power 0])
            ([box-num (hash-keys boxes)])
    (let ([lens-list (reverse (hash-ref boxes box-num))])
      (+ lens-power (* (add1 box-num) (for/sum ([lens (map lens-focal-length lens-list)]
                                                 [slot (in-naturals 1)])
                                        (* slot lens)))))))

(define (calculate-focusing-power init-seq)
  (focusing-power (install-lenses init-seq)))
  

;;; Entry point

(define (main)
  (printf "AoC 2023 Day 15 - Lens Library~n")
  (let ([init-seq (read-input "input.txt")])
    (printf "Part 1: ~a~n" (sum-holiday-hashes init-seq))
    (printf "Part 2: ~a~n" (calculate-focusing-power init-seq))))

(main)

;;; Tests

(module+ test
  (require rackunit)

  (define init-seq (read-input "input1.txt"))

  (check-equal? (sum-holiday-hashes init-seq) 1320)
  (check-equal? (calculate-focusing-power init-seq) 145))