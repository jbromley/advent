#lang racket/base
(require racket/list)
(require racket/string)
(require openssl/md5)

(define key "yzbqklnj")

(define (mine key prefix)
  (define (do-md5 key n)
    (md5 (open-input-string (string-append key (number->string n)))))
  (let loop ([n 1]
             [csum (do-md5 key 1)])
    (if (string-prefix? csum prefix)
        n
        (loop (add1 n) (do-md5 key (add1 n))))))

(module+ main
  (printf "AoC 2015 Day 4 - The Ideal Stocking Stuffer~n")
  (printf "1: ~a~n" (mine key "00000"))
  (printf "1: ~a~n" (mine key "000000")))

(module+ test
  (require rackunit)

  (check-equal? (mine "abcdef" "00000") 609043)
  (check-equal? (mine "pqrstuv" "00000") 1048970))

