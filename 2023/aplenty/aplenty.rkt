#lang racket/base
(require racket/list)
(require racket/match)
(require racket/port)
(require racket/string)

(struct expr [var op value result] #:transparent)

(define (make-result-expr result)
  (expr 'none 'none 'none result))

(define (part-cat part cat)
  (hash-ref part cat))

;;; Read and parse workflows and parts

(define (read-input input-file)
  (let-values ([(workflow-lines part-lines)
               (splitf-at (port->lines (open-input-file input-file) #:line-mode 'linefeed)
                          non-empty-string?)])
    (list (parse-workflows workflow-lines) (parse-parts (drop part-lines 1)))))

(define (parse-workflows lines)
  (for/hash ([line lines])
    (match-let* ([(list label expr-str) (string-split (string-replace line #rx"[{|}]" " ") #:trim? #t)]
                 [expr-lst (string-split expr-str "," #:trim? #t)]
                 [exprs (map parse-expr expr-lst)])
      (values label exprs))))

(define (parse-expr expr-str) 
  (if (string-contains? expr-str ":")
    (match-let ([(list pred result) (string-split expr-str ":")])
      (expr (string-ref pred 0) 
            (string-ref pred 1)
            (string->number (substring pred 2))
            result))
    (make-result-expr expr-str)))

(define (parse-parts lines)
  (for/list ([line lines])
    (apply hash
           (flatten
             (map (lambda (lst) (list (string-ref (first lst) 0) (string->number (second lst))))
                  (regexp-match* #rx"([a-z])=([0-9]+)" line #:match-select cdr))))))

;;; Evaluate expressions and workflows

(define (op->fn op)
  (match op
    [#\< <]
    [#\> >]))

(define (eval-workflow flow part all-flows)
  (match (eval-expr (first flow) part)
    [#f (eval-workflow (rest flow) part all-flows)]
    ["A" #t]
    ["R" #f]
    [new-flow (eval-workflow (hash-ref all-flows new-flow) part all-flows)]))

(define (eval-expr e part)
  (cond
    [(eq? (expr-var e) 'none) (expr-result e)]
    [else
     (if ((op->fn (expr-op e)) (part-cat part (expr-var e)) (expr-value e))
         (expr-result e)
         #f)]))

;;; Part 1

(define (sum-accepted flows parts)
  (define in-flow (hash-ref flows "in"))
  (for/fold ([accepted-sum 0])
            ([part parts])
    (if (eval-workflow in-flow part flows)
        (+ accepted-sum (part-value part))
        accepted-sum)))

(define (part-value part)
    (for/sum ([val (hash-values part)]) val))
                        
;;; Part 2

(define (partition-parts part e)
  (let ([part-range (part-cat part (expr-var e))]
        [split-value (expr-value e)]
        [op (expr-op e)])
    (match op
      [#\< (list)]
      [#\> (list)])))

(define (split-range r op split)
  (match-let ([(cons lo hi) r])
    (match op
      [#\<
       (cond
         [(< hi split) (list r 'none)]
         [(<= split lo) (list 'none r)]
         [else (list (cons lo (sub1 split)) (cons split hi))])]
      [#\>
       (cond
         [(<= hi split) (list 'none r)]
         [(<= split lo) (list r 'none)]
         [else (list (cons split hi) (cons lo (sub1 split)))])])))
      
;;; Entry point

(define inputs (read-input "input1.txt"))
(define workflows (first inputs))
(define parts (second inputs))
(define p1 (first parts))
(define in-flow (hash-ref workflows "in"))

(define (main)
  (match-let ([(list flows parts) (read-input "input.txt")])
    (printf "AoC 2023 Day 19 - Aplenty~n")
    (printf "Part 1: ~a~n" (sum-accepted flows parts))))