#lang racket/base
(require racket/string)

(define (read-strings input-file)
  (call-with-input-file input-file
    (lambda (port)
      (for/list ([line (in-lines port)])
        (string-trim line)))))

(define (count-nice-strings strings nice-fn?)
  (for/sum ([s strings])
    (if (nice-fn? s) 1 0)))

;;; Part 1

(define (nice-string? s)
  (define bad-strings (list "ab" "cd" "pq" "xy"))
  (define vowels (list #\a #\e #\i #\o #\u))
  (define (search s index n-vowels double? bad-string?)
    (cond
      [bad-string? #f]
      [(>= index (string-length s)) (and (not bad-string?) (>= n-vowels 3) double?)]
      [else
       (let ([hist (substring s (max (sub1 index) 0) (add1 index))])
         (search s
                 (add1 index)
                 (if (member (string-ref s index) vowels) (add1 n-vowels) n-vowels)
                 (or double? (and (not (zero? index)) (char=? (string-ref hist 0) (string-ref hist 1))))
                 (member hist bad-strings)))]))
  (search s 0 0 #f #f))

;;; Part 2

(define (nice-string*? s)
  (define s-len (string-length s))
  (define (has-repeat? s)
    (char=? (string-ref s 0) (string-ref s 2)))
  (define (search s index pairs last-pair repeat?)
    (cond
      [(= index s-len)
       (let ([has-pairs (>= (apply max (hash-values pairs)) 2)]
             [result (and (>= (apply max (hash-values pairs)) 2) repeat?)])
         result)]
      [else
       (let ([pair (substring s (sub1 index) (add1 index))])
         (search s
                 (add1 index)
                 (if (not (string=? pair last-pair)) (hash-update pairs pair add1 0) pairs)
                 (if (string=? pair last-pair) "" pair)
                 (or repeat? (and (> index 1)
                                  (has-repeat? (substring s (- index 2) (add1 index)))))))]))
  (search s 1 (hash) "" #f))

(module+ main
  (printf "AoC 2015 Day 5 - Intern Elves?~n")
  (let ([strings (read-strings "input/5.txt")])
    (printf "1: ~a~n" (count-nice-strings strings nice-string?))
    (printf "2: ~a~n" (count-nice-strings strings nice-string*?))))

(module+ test
  (require rackunit)

  (check-equal? (nice-string? "ugknbfddgicrmopn") #t)
  (check-equal? (nice-string? "aaa") #t)
  (check-equal? (nice-string? "jchzalrnumimnmhp") #f)
  (check-equal? (nice-string? "haegwjzuvuyypxyu") #f)
  (check-equal? (nice-string? "dvszwmarrgswjxmb") #f)

  (check-equal? (nice-string*? "qjhvhtzxzqqjkmpb") #t)
  (check-equal? (nice-string*? "xxyxx") #t)
  (check-equal? (nice-string*? "uurcxstgmygtbstg") #f)
  (check-equal? (nice-string*? "ieodomkazucvgmuy") #f))
