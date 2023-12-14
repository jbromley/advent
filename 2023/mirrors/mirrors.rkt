#lang racket/base

(require racket/contract)
(require racket/list)
(require racket/port)
(require racket/string)

;;; Common code

(define/contract (read-input input-file)
  (-> string? list?)
  (let ([in (port->lines (open-input-file input-file) #:line-mode 'linefeed)])
    (parse-input in '() '())))

(define/contract (parse-input in current-image images)
  (-> list? list? list? list?)
  (cond
    [(empty? in)
     (reverse (cons (reverse current-image) images))]
    [(not (non-empty-string? (first in)))
     (parse-input (rest in) '() (cons (reverse current-image) images))]
    [else
     (parse-input (rest in) (cons (first in) current-image) images)]))

(define/contract (transpose image)
  (-> list? list?)
  (map list->string (apply map list (map string->list image))))

(define/contract (in-weave image)
  (-> list? list?)
  (define (iter image result)
    (cond
      [(empty? image)
       (reverse result)]
      [else
       (iter (rest image) (cons "" (cons (first image) result)))]))
  (iter image '("")))